//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Kmeans.hpp"
#include "ClassName.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Interface.hpp"
#include "Rand.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <cmath>
#include <cwctype>
#include <limits>
#include <numeric>
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
// clang-format off
enum class Distance { SQUAREDEUCLIDEAN, CITYBLOCK, COSINE, CORRELATION, HAMMING };
enum class StartMethod { PLUS, SAMPLE, UNIFORM, CLUSTER, NUMERIC };
enum class EmptyAction { SINGLETON, DROP, ERROR };
enum class DisplayMode { OFF, FINAL, ITER };
struct Options {
    Distance distance { Distance::SQUAREDEUCLIDEAN }; StartMethod startMethod { StartMethod::PLUS };
    EmptyAction emptyAction { EmptyAction::SINGLETON }; DisplayMode display { DisplayMode::OFF };
    ArrayOf startValue; bool hasStart { false }, hasReplicates { false }, hasMaxIter { false },
        hasEmptyAction { false }, hasDisplay { false }, hasOnlinePhase { false }, onlinePhase { true },
        useParallel { false }, useSubstreams { false }, hasStreams { false };
    ArrayOf streamsValue;
    indexType replicates { 1 }; indexType maxIter { 100 };
};
//=============================================================================
struct KmeansData {
    ArrayOf value; std::vector<double> owned; std::vector<indexType> sourceRows;
    const double* x { nullptr }; indexType originalRows { 0 }, rows { 0 }, cols { 0 };
};
//=============================================================================
// clang-format on
struct RunResult
{
    std::vector<double> labels;
    std::vector<double> centers;
    std::vector<double> distances;
    std::vector<double> sumd;
    double total { std::numeric_limits<double>::infinity() };
    indexType iterations { 0 };
    bool converged { false };
};
//=============================================================================
bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
std::wstring
textScalar(const ArrayOf& value, const std::wstring& message)
{
    if (!isTextScalar(value)) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    std::wstring text = value.asWideString();
    std::transform(text.begin(), text.end(), text.begin(), std::towlower);
    return text;
}
//=============================================================================
bool
matches(const std::wstring& value, const std::wstring& name)
{
    return !value.empty() && name.rfind(value, 0) == 0;
}
//=============================================================================
ArrayOfVector
callRng(Evaluator* eval, const ArrayOfVector& args, int nLhs)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        return {};
    }
    FunctionDefPtr funcDef = nullptr;
    if (!eval->getContext()->lookupFunction("rng", funcDef) || funcDef == nullptr) {
        Error(L"Nelson:statistics:functionNotFound", L"Function not found: rng");
    }
    return funcDef->evaluateFunction(eval, args, nLhs);
}
//=============================================================================
bool
isRandStreamObject(const ArrayOf& value)
{
    std::string className;
    ClassName(value, className);
    return className == "RandStream";
}
//=============================================================================
void
syncRandStreamFromCurrentRng(Evaluator* eval, const ArrayOf& stream)
{
    if (eval == nullptr || !isRandStreamObject(stream)) {
        return;
    }
    ArrayOfVector params;
    ArrayOfVector result;
    eval->bytecodeInvokeObjectMethodIfExists(stream, "syncFromGlobalState", params, 0, result);
}
//=============================================================================
ArrayOf
firstStreamValue(const ArrayOf& streams)
{
    if (streams.isEmpty()) {
        return ArrayOf();
    }
    if (streams.isCell()) {
        const auto* values = static_cast<const ArrayOf*>(streams.dataPointer());
        if (values == nullptr || streams.elementCount() == 0) {
            return ArrayOf();
        }
        return values[0];
    }
    return streams;
}
//=============================================================================
struct ScopedKmeansRng
{
    Evaluator* eval { nullptr };
    bool active { false };
    bool syncStream { false };
    ArrayOf previous;
    ArrayOf stream;

    ScopedKmeansRng(Evaluator* evaluator, const Options& options) : eval(evaluator)
    {
        if (!options.hasStreams) {
            return;
        }
        stream = firstStreamValue(options.streamsValue);
        if (stream.isEmpty()) {
            return;
        }
        syncStream = isRandStreamObject(stream);
        ArrayOfVector args;
        args << stream;
        ArrayOfVector result = callRng(eval, args, 1);
        if (!result.empty()) {
            previous = result[0];
            active = true;
        }
    }

    ~ScopedKmeansRng()
    {
        if (!active) {
            return;
        }
        try {
            if (syncStream) {
                syncRandStreamFromCurrentRng(eval, stream);
            }
            ArrayOfVector args;
            args << previous;
            callRng(eval, args, 0);
        } catch (...) {
        }
    }
};
//=============================================================================
template <typename T>
T
choiceFromText(const std::wstring& value, const std::vector<std::pair<std::wstring, T>>& names,
    const std::wstring& invalidMessage, const std::wstring& ambiguousMessage)
{
    T found = names[0].second;
    bool haveMatch = false;
    for (const auto& item : names) {
        if (matches(value, item.first)) {
            if (haveMatch) {
                RaiseError(ambiguousMessage, L"Nelson:statistics:ambiguousNameValue");
            }
            found = item.second;
            haveMatch = true;
        }
    }
    if (!haveMatch) {
        RaiseError(invalidMessage, L"Nelson:statistics:unknownNameValue");
    }
    return found;
}
//=============================================================================
double
positiveScalar(const ArrayOf& value, const std::wstring& message, bool integerOnly)
{
    if (!value.isNumeric() || value.isSparse() || !value.isReal() || !value.isScalar()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    double scalar = value.asDoubleScalar();
    if (!std::isfinite(scalar) || scalar <= 0.0) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    if (integerOnly && std::floor(scalar) != scalar) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    return scalar;
}
//=============================================================================
ArrayOf
realDoubleMatrix(ArrayOf value, const std::wstring& message)
{
    if (!value.isNumeric() || value.isSparse() || !value.isReal() || !value.is2D()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    value.promoteType(NLS_DOUBLE);
    return value;
}
//=============================================================================
ArrayOf
tableToMatrix(ArrayOf value)
{
    indexType rows = value.tableHeight();
    stringVector names = value.tableVariableNames();
    std::vector<ArrayOf> columns;
    indexType cols = 0;
    for (const std::string& name : names) {
        ArrayOf col = value.tableColumn(name);
        if (!col.isNumeric() && !col.isLogical()) {
            Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
                L"Table variables");
        }
        if (col.isSparse() || !col.isReal() || !col.is2D() || col.rows() != rows) {
            Error(L"Nelson:statistics:tableVariablesNotRealFull",
                L"Table variables must be real full arrays with matching rows.");
        }
        col.promoteType(NLS_DOUBLE);
        cols += col.columns();
        columns.push_back(col);
    }
    std::vector<double> values(rows * cols, 0.0);
    indexType outCol = 0;
    for (const ArrayOf& col : columns) {
        const double* ptr = static_cast<const double*>(col.dataPointer());
        for (indexType c = 0; c < col.columns(); ++c) {
            for (indexType r = 0; r < rows; ++r) {
                values[r + outCol * rows] = ptr[r + c * rows];
            }
            ++outCol;
        }
    }
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    std::copy(values.begin(), values.end(), out);
    return ArrayOf(NLS_DOUBLE, Dimensions(rows, cols), out);
}
//=============================================================================
double
at(const double* x, indexType rows, indexType row, indexType col)
{
    return x[row + col * rows];
}
//=============================================================================
double
centerAt(const std::vector<double>& centers, indexType k, indexType cluster, indexType col)
{
    return centers[cluster + col * k];
}
//=============================================================================
void
setCenterAt(
    std::vector<double>& centers, indexType k, indexType cluster, indexType col, double value)
{
    centers[cluster + col * k] = value;
}
//=============================================================================
double
randomUnit()
{
    ArrayOf r = Rand(NLS_DOUBLE);
    return *static_cast<const double*>(r.dataPointer());
}
//=============================================================================
std::vector<double>
randomVector(indexType count)
{
    Dimensions dims(1, count);
    ArrayOf r = Rand(dims, NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(r.dataPointer());
    return std::vector<double>(ptr, ptr + count);
}
//=============================================================================
indexType
randomIndex(indexType n)
{
    double value = randomUnit();
    indexType idx = static_cast<indexType>(std::floor(value * static_cast<double>(n)));
    return std::min(idx, n - 1);
}
//=============================================================================
std::vector<indexType>
randomPermutation(indexType n)
{
    std::vector<double> keys = randomVector(n);
    std::vector<indexType> order(n);
    std::iota(order.begin(), order.end(), 0);
    std::sort(
        order.begin(), order.end(), [&](indexType a, indexType b) { return keys[a] < keys[b]; });
    return order;
}
//=============================================================================
Distance
parseDistance(const ArrayOf& value)
{
    std::wstring text = textScalar(value,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidValue", L"Invalid %s value."),
            L"Distance"));
    // clang-format off
    const std::vector<std::pair<std::wstring, Distance>> names = {
        { L"sqeuclidean", Distance::SQUAREDEUCLIDEAN }, { L"squaredeuclidean", Distance::SQUAREDEUCLIDEAN },
        { L"cityblock", Distance::CITYBLOCK }, { L"cosine", Distance::COSINE },
        { L"correlation", Distance::CORRELATION }, { L"hamming", Distance::HAMMING },
    };
    // clang-format on
    return choiceFromText(text, names,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidValue", L"Invalid %s value."),
            L"Distance"),
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:statistics:ambiguousValue", L"Ambiguous %s value."),
            L"Distance"));
}
//=============================================================================
StartMethod
parseStartMethod(const ArrayOf& value)
{
    if (!isTextScalar(value)) {
        return StartMethod::NUMERIC;
    }
    std::wstring text = textScalar(value,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidValue", L"Invalid %s value."),
            L"Start"));
    // clang-format off
    const std::vector<std::pair<std::wstring, StartMethod>> names = {
        { L"plus", StartMethod::PLUS }, { L"sample", StartMethod::SAMPLE },
        { L"uniform", StartMethod::UNIFORM }, { L"cluster", StartMethod::CLUSTER },
    };
    // clang-format on
    return choiceFromText(text, names,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidValue", L"Invalid %s value."),
            L"Start"),
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:statistics:ambiguousValue", L"Ambiguous %s value."),
            L"Start"));
}
//=============================================================================
EmptyAction
parseEmptyAction(const ArrayOf& value)
{
    std::wstring text = textScalar(value,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidValue", L"Invalid %s value."),
            L"EmptyAction"));
    const std::vector<std::pair<std::wstring, EmptyAction>> names = {
        { L"singleton", EmptyAction::SINGLETON },
        { L"drop", EmptyAction::DROP },
        { L"error", EmptyAction::ERROR },
    };
    return choiceFromText(text, names,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidValue", L"Invalid %s value."),
            L"EmptyAction"),
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:statistics:ambiguousValue", L"Ambiguous %s value."),
            L"EmptyAction"));
}
//=============================================================================
DisplayMode
parseDisplay(const ArrayOf& value)
{
    std::wstring text = textScalar(value,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidValue", L"Invalid %s value."),
            L"Display"));
    const std::vector<std::pair<std::wstring, DisplayMode>> names = {
        { L"off", DisplayMode::OFF },
        { L"final", DisplayMode::FINAL },
        { L"iter", DisplayMode::ITER },
    };
    return choiceFromText(text, names,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidValue", L"Invalid %s value."),
            L"Display"),
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:statistics:ambiguousValue", L"Ambiguous %s value."),
            L"Display"));
}
//=============================================================================
bool
parseOnOff(const ArrayOf& value, const std::wstring& message)
{
    std::wstring text = textScalar(value, message);
    if (matches(text, L"on")) {
        return true;
    }
    if (matches(text, L"off")) {
        return false;
    }
    Error(L"Nelson:statistics:invalidValue", message);
    return false;
}
//=============================================================================
std::wstring
parseName(const ArrayOf& value)
{
    std::wstring text = textScalar(value, _W("Invalid name-value argument name."));
    // clang-format off
    const std::vector<std::pair<std::wstring, std::wstring>> names = {
        { L"distance", L"distance" }, { L"start", L"start" },
        { L"replicates", L"replicates" }, { L"maxiter", L"maxiter" },
        { L"emptyaction", L"emptyaction" }, { L"display", L"display" },
        { L"onlinephase", L"onlinephase" }, { L"options", L"options" },
    };
    // clang-format on
    return choiceFromText(text, names, _W("Invalid name-value argument name."),
        _W("Ambiguous name-value argument name."));
}
//=============================================================================
bool
applyOptionField(Options& options, const std::string& field, const ArrayOf& value, bool ifUnset)
{
    if (ifUnset && value.isEmpty(true)) {
        return true;
    }
    if (field == "Distance" && (!ifUnset || options.distance == Distance::SQUAREDEUCLIDEAN)) {
        options.distance = parseDistance(value);
    } else if (field == "Start" && (!ifUnset || !options.hasStart)) {
        options.hasStart = true;
        options.startMethod = parseStartMethod(value);
        options.startValue = value;
    } else if (field == "Replicates" && (!ifUnset || !options.hasReplicates)) {
        if (value.isEmpty(true)) {
            return true;
        }
        options.hasReplicates = true;
        options.replicates = static_cast<indexType>(positiveScalar(value,
            formatI18nMessage(
                Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                    L"%s must be a positive integer scalar."),
                L"Replicates"),
            true));
    } else if (field == "MaxIter" && (!ifUnset || !options.hasMaxIter)) {
        if (value.isEmpty(true)) {
            return true;
        }
        options.hasMaxIter = true;
        options.maxIter = static_cast<indexType>(positiveScalar(value,
            formatI18nMessage(
                Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                    L"%s must be a positive integer scalar."),
                L"MaxIter"),
            true));
    } else if (field == "EmptyAction" && (!ifUnset || !options.hasEmptyAction)) {
        options.hasEmptyAction = true;
        options.emptyAction = parseEmptyAction(value);
    } else if (field == "Display" && (!ifUnset || !options.hasDisplay)) {
        options.hasDisplay = true;
        options.display = parseDisplay(value);
    } else if (field == "OnlinePhase" && (!ifUnset || !options.hasOnlinePhase)) {
        options.hasOnlinePhase = true;
        options.onlinePhase = parseOnOff(value,
            formatI18nMessage(Nelson::resolveMessageTemplate(
                                  L"Nelson:statistics:invalidValue", L"Invalid %s value."),
                L"OnlinePhase"));
    } else if (field == "UseParallel") {
        options.useParallel = value.isLogical() && value.isScalar()
            ? value.asLogicalScalar()
            : parseOnOff(value,
                  formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:statistics:invalidValue", L"Invalid %s value."),
                      L"UseParallel"));
    } else if (field == "UseSubstreams") {
        options.useSubstreams = value.isLogical() && value.isScalar()
            ? value.asLogicalScalar()
            : parseOnOff(value,
                  formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:statistics:invalidValue", L"Invalid %s value."),
                      L"UseSubstreams"));
    } else if (field == "Streams") {
        options.hasStreams = !value.isEmpty();
        options.streamsValue = value;
    } else {
        return false;
    }
    return true;
}
//=============================================================================
void
applyOptionsStruct(Options& options, const ArrayOf& value)
{
    if (!value.isStruct() || !value.isScalar()) {
        Error(L"Nelson:statistics:invalidOptions", L"%s must be a scalar structure.", L"Options");
    }
    const stringVector fields = value.fieldNames();
    for (const std::string& field : fields) {
        if (value.isField(field)) {
            applyOptionField(options, field, value.field(field), true);
        }
    }
}
//=============================================================================
void
parseNameValues(Options& options, const ArrayOfVector& argIn)
{
    if ((argIn.size() - 2) % 2 != 0) {
        Error(L"Nelson:statistics:nameValueNotPaired", L"Name-value arguments must be paired.");
    }
    for (size_t k = 2; k < argIn.size(); k += 2) {
        if (parseName(argIn[k]) == L"options") {
            applyOptionsStruct(options, argIn[k + 1]);
        }
    }
    for (size_t k = 2; k < argIn.size(); k += 2) {
        std::wstring name = parseName(argIn[k]);
        const ArrayOf& value = argIn[k + 1];
        if (name == L"distance") {
            options.distance = parseDistance(value);
        } else if (name == L"start") {
            options.hasStart = true;
            options.startMethod = parseStartMethod(value);
            options.startValue = value;
        } else if (name == L"replicates") {
            options.hasReplicates = true;
            options.replicates = static_cast<indexType>(positiveScalar(value,
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                        L"%s must be a positive integer scalar."),
                    L"Replicates"),
                true));
        } else if (name == L"maxiter") {
            options.hasMaxIter = true;
            options.maxIter = static_cast<indexType>(positiveScalar(value,
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                        L"%s must be a positive integer scalar."),
                    L"MaxIter"),
                true));
        } else if (name == L"emptyaction") {
            options.hasEmptyAction = true;
            options.emptyAction = parseEmptyAction(value);
        } else if (name == L"display") {
            options.hasDisplay = true;
            options.display = parseDisplay(value);
        } else if (name == L"onlinephase") {
            options.hasOnlinePhase = true;
            options.onlinePhase = parseOnOff(value,
                formatI18nMessage(Nelson::resolveMessageTemplate(
                                      L"Nelson:statistics:invalidValue", L"Invalid %s value."),
                    L"OnlinePhase"));
        }
    }
}
//=============================================================================
KmeansData
parseData(ArrayOf value)
{
    KmeansData data;
    if (value.isTable()) {
        value = tableToMatrix(value);
    }
    data.value = realDoubleMatrix(value,
        resolveMessageTemplate(L"Nelson:statistics:xRealNumericFull",
            L"X must be a real numeric full matrix or table."));
    data.originalRows = data.value.rows();
    data.cols = data.value.columns();
    const double* ptr = static_cast<const double*>(data.value.dataPointer());
    for (indexType r = 0; r < data.originalRows; ++r) {
        bool omit = false;
        for (indexType c = 0; c < data.cols; ++c) {
            double v = ptr[r + c * data.originalRows];
            if (std::isnan(v)) {
                omit = true;
            } else if (!std::isfinite(v)) {
                Error(L"Nelson:statistics:xMustNotBeInf", L"%s must not contain Inf values.", L"X");
            }
        }
        if (!omit) {
            data.sourceRows.push_back(r);
        }
    }
    data.rows = static_cast<indexType>(data.sourceRows.size());
    data.owned.assign(data.rows * data.cols, 0.0);
    for (indexType c = 0; c < data.cols; ++c) {
        for (indexType r = 0; r < data.rows; ++r) {
            data.owned[r + c * data.rows] = ptr[data.sourceRows[r] + c * data.originalRows];
        }
    }
    data.x = data.owned.data();
    return data;
}
//=============================================================================
indexType
parseK(const ArrayOf& value, const Options& options)
{
    if (value.isEmpty(true) && options.startMethod == StartMethod::NUMERIC) {
        return 0;
    }
    return static_cast<indexType>(positiveScalar(value,
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                              L"%s must be a positive integer scalar."),
            L"K"),
        true));
}
//=============================================================================
double
distanceToCenter(const KmeansData& data, const std::vector<double>& centers, indexType k,
    indexType row, indexType cluster, Distance distance)
{
    if (distance == Distance::CITYBLOCK) {
        double s = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            s += std::abs(at(data.x, data.rows, row, c) - centerAt(centers, k, cluster, c));
        }
        return s;
    }
    if (distance == Distance::HAMMING) {
        indexType count = 0;
        for (indexType c = 0; c < data.cols; ++c) {
            if (at(data.x, data.rows, row, c) != centerAt(centers, k, cluster, c)) {
                ++count;
            }
        }
        return static_cast<double>(count) / static_cast<double>(data.cols);
    }
    double dot = 0.0;
    double nx = 0.0;
    double nc = 0.0;
    double sx = 0.0;
    double sc = 0.0;
    if (distance == Distance::CORRELATION) {
        for (indexType c = 0; c < data.cols; ++c) {
            sx += at(data.x, data.rows, row, c);
            sc += centerAt(centers, k, cluster, c);
        }
        sx /= static_cast<double>(data.cols);
        sc /= static_cast<double>(data.cols);
    }
    for (indexType c = 0; c < data.cols; ++c) {
        double xv = at(data.x, data.rows, row, c) - sx;
        double cv = centerAt(centers, k, cluster, c) - sc;
        if (distance == Distance::SQUAREDEUCLIDEAN) {
            double d = xv - cv;
            dot += d * d;
        } else {
            dot += xv * cv;
            nx += xv * xv;
            nc += cv * cv;
        }
    }
    if (distance == Distance::SQUAREDEUCLIDEAN) {
        return dot;
    }
    double denom = std::sqrt(nx) * std::sqrt(nc);
    return denom == 0.0 ? std::numeric_limits<double>::infinity() : 1.0 - dot / denom;
}
//=============================================================================
void
copyRowToCenter(const KmeansData& data, std::vector<double>& centers, indexType k,
    indexType cluster, indexType row)
{
    for (indexType c = 0; c < data.cols; ++c) {
        setCenterAt(centers, k, cluster, c, at(data.x, data.rows, row, c));
    }
}
//=============================================================================
double
median(std::vector<double> values)
{
    if (values.empty()) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    std::sort(values.begin(), values.end());
    indexType n = static_cast<indexType>(values.size());
    return (n % 2) ? values[n / 2] : (values[n / 2 - 1] + values[n / 2]) / 2.0;
}
//=============================================================================
std::vector<double>
sampleCenters(const KmeansData& data, indexType k)
{
    if (data.rows < k) {
        Error(L"Nelson:statistics:kTooLarge", L"K must not exceed the number of rows in X.");
    }
    std::vector<double> centers(k * data.cols, 0.0);
    std::vector<indexType> order = randomPermutation(data.rows);
    for (indexType cluster = 0; cluster < k; ++cluster) {
        copyRowToCenter(data, centers, k, cluster, order[cluster]);
    }
    return centers;
}
//=============================================================================
std::vector<double>
uniformCenters(const KmeansData& data, indexType k)
{
    std::vector<double> centers(k * data.cols, 0.0);
    std::vector<double> mins(data.cols, std::numeric_limits<double>::infinity());
    std::vector<double> maxs(data.cols, -std::numeric_limits<double>::infinity());
    for (indexType c = 0; c < data.cols; ++c) {
        for (indexType r = 0; r < data.rows; ++r) {
            double v = at(data.x, data.rows, r, c);
            mins[c] = std::min(mins[c], v);
            maxs[c] = std::max(maxs[c], v);
        }
    }
    std::vector<double> u = randomVector(k * data.cols);
    for (indexType c = 0; c < data.cols; ++c) {
        for (indexType cluster = 0; cluster < k; ++cluster) {
            double value = mins[c] + u[cluster + c * k] * (maxs[c] - mins[c]);
            setCenterAt(centers, k, cluster, c, value);
        }
    }
    return centers;
}
//=============================================================================
std::vector<double>
plusCenters(const KmeansData& data, indexType k)
{
    std::vector<double> centers(k * data.cols, 0.0);
    std::vector<double> minD(data.rows, std::numeric_limits<double>::infinity());
    copyRowToCenter(data, centers, k, 0, randomIndex(data.rows));
    for (indexType cluster = 1; cluster < k; ++cluster) {
        double total = 0.0;
        for (indexType r = 0; r < data.rows; ++r) {
            double d
                = distanceToCenter(data, centers, k, r, cluster - 1, Distance::SQUAREDEUCLIDEAN);
            minD[r] = std::min(minD[r], d);
            total += minD[r];
        }
        double threshold = randomUnit() * total;
        double cumulative = 0.0;
        indexType chosen = data.rows - 1;
        for (indexType r = 0; r < data.rows; ++r) {
            cumulative += minD[r];
            if (cumulative >= threshold) {
                chosen = r;
                break;
            }
        }
        copyRowToCenter(data, centers, k, cluster, chosen);
    }
    return centers;
}
//=============================================================================
std::vector<double>
numericCenters(
    ArrayOf startValue, indexType k, indexType cols, indexType replicate, indexType& pages)
{
    if (!startValue.isNumeric() || startValue.isSparse() || !startValue.isReal()) {
        Error(L"Nelson:statistics:invalidStart", L"%s must be a real numeric array.", L"Start");
    }
    startValue.promoteType(NLS_DOUBLE);
    Dimensions dims = startValue.dimensions();
    indexType rows = dims.rows();
    indexType startCols = dims.columns();
    pages = dims.dimensionLength(2);
    if (rows == 0 || startCols != cols || (k != 0 && rows != k)) {
        Error(L"Nelson:statistics:invalidStartSize", L"Start size must be K-by-P or K-by-P-by-R.");
    }
    if (replicate >= pages) {
        Error(L"Nelson:statistics:invalidReplicates",
            L"Replicates must match the number of pages in Start.");
    }
    k = rows;
    std::vector<double> centers(k * cols, 0.0);
    const double* ptr = static_cast<const double*>(startValue.dataPointer());
    indexType pageOffset = replicate * k * cols;
    std::copy(ptr + pageOffset, ptr + pageOffset + k * cols, centers.begin());
    return centers;
}
//=============================================================================
std::vector<double>
initialCenters(const KmeansData& data, const Options& options, indexType k, indexType replicate,
    indexType& pages)
{
    pages = 1;
    if (options.startMethod == StartMethod::NUMERIC) {
        return numericCenters(options.startValue, k, data.cols, replicate, pages);
    }
    if (k == 0) {
        Error(L"Nelson:statistics:kRequired", L"K must be specified unless Start is numeric.");
    }
    if (options.startMethod == StartMethod::SAMPLE) {
        return sampleCenters(data, k);
    }
    if (options.startMethod == StartMethod::UNIFORM) {
        return uniformCenters(data, k);
    }
    if (options.startMethod == StartMethod::CLUSTER) {
        return sampleCenters(data, k);
    }
    return plusCenters(data, k);
}
//=============================================================================
void
assignPoints(const KmeansData& data, const Options& options, const std::vector<double>& centers,
    indexType k, std::vector<double>& labels, std::vector<double>& distances,
    std::vector<double>& sumd)
{
    std::fill(sumd.begin(), sumd.end(), 0.0);
    std::vector<double> bestDistances(data.rows, 0.0);
    std::vector<indexType> bestClusters(data.rows, 0);
    ompIndexType rows = static_cast<ompIndexType>(data.rows);
    OMP_PARALLEL_FOR_LOOP(options.useParallel ? rows : 0)
    for (ompIndexType rr = 0; rr < rows; ++rr) {
        indexType r = static_cast<indexType>(rr);
        double best = std::numeric_limits<double>::infinity();
        for (indexType cluster = 0; cluster < k; ++cluster) {
            double d = distanceToCenter(data, centers, k, r, cluster, options.distance);
            distances[r + cluster * data.rows] = d;
            if (d < best) {
                best = d;
                bestClusters[r] = cluster;
            }
        }
        bestDistances[r] = best;
        labels[r] = static_cast<double>(bestClusters[r] + 1);
    }
    for (indexType r = 0; r < data.rows; ++r) {
        sumd[bestClusters[r]] += bestDistances[r];
    }
}
//=============================================================================
bool
recomputeCenters(const KmeansData& data, const Options& options, std::vector<double>& centers,
    indexType k, std::vector<double>& labels, const std::vector<double>& distances)
{
    std::vector<double> next(k * data.cols, 0.0);
    std::vector<indexType> counts(k, 0);
    for (indexType r = 0; r < data.rows; ++r) {
        indexType cluster = static_cast<indexType>(labels[r]) - 1;
        ++counts[cluster];
        for (indexType c = 0; c < data.cols; ++c) {
            next[cluster + c * k] += at(data.x, data.rows, r, c);
        }
    }
    for (indexType cluster = 0; cluster < k; ++cluster) {
        if (counts[cluster] == 0) {
            if (options.emptyAction == EmptyAction::ERROR) {
                Error(L"Nelson:statistics:emptyCluster", L"Empty cluster created.");
            }
            if (options.emptyAction == EmptyAction::SINGLETON) {
                double maxDistance = -std::numeric_limits<double>::infinity();
                indexType row = 0;
                for (indexType r = 0; r < data.rows; ++r) {
                    indexType assigned = static_cast<indexType>(labels[r]) - 1;
                    double d = distances[r + assigned * data.rows];
                    if (counts[assigned] > 1 && d > maxDistance) {
                        maxDistance = d;
                        row = r;
                    }
                }
                indexType oldCluster = static_cast<indexType>(labels[row]) - 1;
                labels[row] = static_cast<double>(cluster + 1);
                --counts[oldCluster];
                copyRowToCenter(data, next, k, cluster, row);
                counts[cluster] = 1;
            } else {
                for (indexType c = 0; c < data.cols; ++c) {
                    setCenterAt(next, k, cluster, c, std::numeric_limits<double>::quiet_NaN());
                }
            }
        } else {
            for (indexType c = 0; c < data.cols; ++c) {
                if (options.distance == Distance::CITYBLOCK
                    || options.distance == Distance::HAMMING) {
                    std::vector<double> values;
                    for (indexType r = 0; r < data.rows; ++r) {
                        if (static_cast<indexType>(labels[r]) - 1 == cluster) {
                            values.push_back(at(data.x, data.rows, r, c));
                        }
                    }
                    double m = median(values);
                    next[cluster + c * k]
                        = options.distance == Distance::HAMMING ? std::round(m) : m;
                } else {
                    next[cluster + c * k] /= static_cast<double>(counts[cluster]);
                }
            }
            if (options.distance == Distance::COSINE || options.distance == Distance::CORRELATION) {
                double meanValue = 0.0;
                if (options.distance == Distance::CORRELATION) {
                    for (indexType c = 0; c < data.cols; ++c) {
                        meanValue += next[cluster + c * k];
                    }
                    meanValue /= static_cast<double>(data.cols);
                }
                double nrm = 0.0;
                for (indexType c = 0; c < data.cols; ++c) {
                    double v = next[cluster + c * k] - meanValue;
                    next[cluster + c * k] = v;
                    nrm += v * v;
                }
                nrm = std::sqrt(nrm);
                if (nrm != 0.0) {
                    for (indexType c = 0; c < data.cols; ++c) {
                        next[cluster + c * k] /= nrm;
                    }
                }
            }
        }
    }
    bool changed = next != centers;
    centers.swap(next);
    return changed;
}
#include "KmeansOnlineUpdate.hpp"
void
displayIteration(Evaluator* eval, indexType replicate, indexType iter, indexType phase,
    indexType moved, double total, DisplayMode mode)
{
    if (mode == DisplayMode::ITER && eval && eval->getInterface()) {
        eval->getInterface()->outputMessage(L"Replicate " + std::to_wstring(replicate + 1)
            + L", iteration " + std::to_wstring(iter) + L", phase " + std::to_wstring(phase) + L", "
            + std::to_wstring(moved) + L" points reassigned, total sum of distances = "
            + std::to_wstring(total) + L".\n");
    }
}
//=============================================================================
void
displayFinal(Evaluator* eval, indexType replicate, const RunResult& result, DisplayMode mode)
{
    if ((mode == DisplayMode::FINAL || mode == DisplayMode::ITER) && eval && eval->getInterface()) {
        eval->getInterface()->outputMessage(L"Replicate " + std::to_wstring(replicate + 1) + L", "
            + std::to_wstring(result.iterations) + L" iterations, total sum of distances = "
            + std::to_wstring(result.total) + L".\n");
    }
}
//=============================================================================
RunResult
runOne(const KmeansData& data, const Options& options, std::vector<double> centers, indexType k,
    indexType replicate, Evaluator* eval)
{
    RunResult result;
    result.centers = std::move(centers);
    result.labels.assign(data.rows, 0.0);
    result.distances.assign(data.rows * k, std::numeric_limits<double>::infinity());
    result.sumd.assign(k, 0.0);
    std::vector<double> previous(data.rows, -1.0);
    for (indexType iter = 1; iter <= options.maxIter; ++iter) {
        assignPoints(
            data, options, result.centers, k, result.labels, result.distances, result.sumd);
        result.total = std::accumulate(result.sumd.begin(), result.sumd.end(), 0.0);
        indexType batchMoved = 0;
        for (indexType r = 0; r < data.rows; ++r) {
            batchMoved += result.labels[r] != previous[r] ? 1 : 0;
        }
        bool labelsStable = result.labels == previous;
        bool centersChanged
            = recomputeCenters(data, options, result.centers, k, result.labels, result.distances);
        displayIteration(eval, replicate, iter, 1, batchMoved, result.total, options.display);
        if (options.onlinePhase) {
            indexType onlineMoved
                = onlineUpdate(data, options, result.centers, k, result.labels, result.distances);
            centersChanged = onlineMoved != 0 || centersChanged;
            assignPoints(
                data, options, result.centers, k, result.labels, result.distances, result.sumd);
            result.total = std::accumulate(result.sumd.begin(), result.sumd.end(), 0.0);
            displayIteration(eval, replicate, iter, 2, onlineMoved, result.total, options.display);
        }
        result.iterations = iter;
        if (labelsStable || !centersChanged) {
            result.converged = true;
            break;
        }
        previous = result.labels;
    }
    assignPoints(data, options, result.centers, k, result.labels, result.distances, result.sumd);
    result.total = std::accumulate(result.sumd.begin(), result.sumd.end(), 0.0);
    displayFinal(eval, replicate, result, options.display);
    return result;
}
//=============================================================================
ArrayOf
doubleArray(const std::vector<double>& values, const Dimensions& dims)
{
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    std::copy(values.begin(), values.end(), out);
    return ArrayOf(NLS_DOUBLE, dims, out);
}
//=============================================================================
ArrayOfVector
makeOutput(const RunResult& best, const KmeansData& data, indexType k, int nLhs)
{
    ArrayOfVector out;
    std::vector<double> labels(data.originalRows, std::numeric_limits<double>::quiet_NaN());
    for (indexType r = 0; r < data.rows; ++r) {
        labels[data.sourceRows[r]] = best.labels[r];
    }
    out << doubleArray(labels, Dimensions(data.originalRows, 1));
    if (nLhs > 1) {
        out << doubleArray(best.centers, Dimensions(k, data.cols));
    }
    if (nLhs > 2) {
        out << doubleArray(best.sumd, Dimensions(k, 1));
    }
    if (nLhs > 3) {
        std::vector<double> distances(
            data.originalRows * k, std::numeric_limits<double>::quiet_NaN());
        for (indexType c = 0; c < k; ++c) {
            for (indexType r = 0; r < data.rows; ++r) {
                distances[data.sourceRows[r] + c * data.originalRows]
                    = best.distances[r + c * data.rows];
            }
        }
        out << doubleArray(distances, Dimensions(data.originalRows, k));
    }
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::Kmeans(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 4);
    Options options;
    parseNameValues(options, argIn);
    ScopedKmeansRng rngGuard(eval, options);
    KmeansData data = parseData(argIn[0]);
    if (data.rows == 0) {
        Error(L"Nelson:statistics:xNeedsOneRow", L"X must have at least one row.");
    }
    indexType k = parseK(argIn[1], options);
    indexType pages = 1;
    std::vector<double> centers = initialCenters(data, options, k, 0, pages);
    k = static_cast<indexType>(centers.size() / data.cols);
    if (options.startMethod == StartMethod::NUMERIC && pages > 1) {
        options.replicates = pages;
    }
    RunResult best;
    bool haveBest = false;
    for (indexType replicate = 0; replicate < options.replicates; ++replicate) {
        if (replicate > 0) {
            centers = initialCenters(data, options, k, replicate, pages);
        }
        RunResult current = runOne(data, options, centers, k, replicate, eval);
        if (!current.converged) {
            Warning(L"Nelson:kmeans:failedToConverge", L"Failed to converge in %d iterations.",
                (int)options.maxIter);
        }
        if (!haveBest || current.total < best.total) {
            best = std::move(current);
            haveBest = true;
        }
    }
    bool showBest = options.display == DisplayMode::FINAL || options.display == DisplayMode::ITER;
    if (showBest && eval && eval->getInterface()) {
        eval->getInterface()->outputMessage(
            L"Best total sum of distances = " + std::to_wstring(best.total) + L"\n");
    }
    return makeOutput(best, data, k, nLhs);
}
//=============================================================================
