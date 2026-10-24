//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Dbscan.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <cwctype>
#include <limits>
#include <numeric>
#include <queue>
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
enum class Metric
{
    EUCLIDEAN,
    SQUAREDEUCLIDEAN,
    SEUCLIDEAN,
    CITYBLOCK,
    CHEBYCHEV,
    MINKOWSKI,
    MAHALANOBIS,
    COSINE,
    CORRELATION,
    SPEARMAN,
    HAMMING,
    JACCARD,
    PRECOMPUTED,
    CUSTOM
};
//=============================================================================
struct Options
{
    Metric metric { Metric::EUCLIDEAN };
    double p { 2.0 };
    std::vector<double> scale;
    std::vector<double> cov;
    ArrayOf distanceFunction;
    bool hasP { false };
    bool hasScale { false };
    bool hasCov { false };
};
//=============================================================================
struct DbscanData
{
    ArrayOf originalX;
    ArrayOf numericX;
    const double* x { nullptr };
    indexType rows { 0 };
    indexType cols { 0 };
    std::vector<double> work;
    std::vector<double> rowNorms;
    std::vector<double> invCov;
    const logical* logicalX { nullptr };
    bool precomputedVector { false };
    bool precomputedLogical { false };
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
Metric
metricFromText(const std::wstring& value)
{
    const std::vector<std::pair<std::wstring, Metric>> names = {
        { L"euclidean", Metric::EUCLIDEAN },
        { L"squaredeuclidean", Metric::SQUAREDEUCLIDEAN },
        { L"seuclidean", Metric::SEUCLIDEAN },
        { L"cityblock", Metric::CITYBLOCK },
        { L"chebychev", Metric::CHEBYCHEV },
        { L"minkowski", Metric::MINKOWSKI },
        { L"mahalanobis", Metric::MAHALANOBIS },
        { L"cosine", Metric::COSINE },
        { L"correlation", Metric::CORRELATION },
        { L"spearman", Metric::SPEARMAN },
        { L"hamming", Metric::HAMMING },
        { L"jaccard", Metric::JACCARD },
        { L"precomputed", Metric::PRECOMPUTED },
    };
    Metric found = Metric::EUCLIDEAN;
    bool haveMatch = false;
    for (const auto& item : names) {
        if (matches(value, item.first)) {
            if (haveMatch) {
                Error(L"Nelson:statistics:ambiguousValue", L"Ambiguous %s value.", L"Distance");
            }
            found = item.second;
            haveMatch = true;
        }
    }
    if (!haveMatch) {
        Error(L"Nelson:statistics:invalidDistance", L"Invalid %s value.", L"Distance");
    }
    return found;
}
//=============================================================================
bool
nameMatches(const std::wstring& value, const std::wstring& name)
{
    return matches(value, name);
}
//=============================================================================
ArrayOf
realDoubleArray(ArrayOf value, const std::wstring& message)
{
    if (value.isSparse() || !value.isReal() || !value.canBePromotedTo(NLS_DOUBLE)) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    value.promoteType(NLS_DOUBLE);
    return value;
}
//=============================================================================
std::vector<double>
realVector(ArrayOf value, const std::wstring& message)
{
    value = realDoubleArray(value, message);
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return std::vector<double>(ptr, ptr + value.elementCount());
}
//=============================================================================
double
positiveScalar(const ArrayOf& value, const std::wstring& message, bool integerOnly)
{
    if (!value.isNumeric() || !value.isReal() || !value.isScalar()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    double scalar = value.asDoubleScalar();
    if (!std::isfinite(scalar) || scalar <= 0) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    if (integerOnly && std::floor(scalar) != scalar) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    return scalar;
}
//=============================================================================
indexType
precomputedVectorSizeToRows(indexType length)
{
    double nAsDouble = (1.0 + std::sqrt(1.0 + 8.0 * static_cast<double>(length))) / 2.0;
    indexType n = static_cast<indexType>(std::floor(nAsDouble + 0.5));
    if (n < 1 || n * (n - 1) / 2 != length) {
        Error(L"Nelson:statistics:precomputedDistanceLength",
            L"Precomputed distance vector has invalid length.");
    }
    return n;
}
//=============================================================================
double
parseEpsilon(const ArrayOf& value, const Options& options, const DbscanData& data)
{
    if (data.precomputedLogical) {
        if (!value.isEmpty(true)) {
            Error(L"Nelson:statistics:epsilonMustBeEmpty",
                L"Epsilon must be empty for logical precomputed distances.");
        }
        return 0.0;
    }
    if (options.metric == Metric::PRECOMPUTED && value.isEmpty(true)) {
        Error(L"Nelson:statistics:invalidEpsilon", L"%s must be a positive scalar.", L"Epsilon");
    }
    return positiveScalar(value,
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositive",
                              L"%s must be a positive scalar."),
            L"Epsilon"),
        false);
}
//=============================================================================
Options
parseOptions(const ArrayOfVector& argIn)
{
    Options options;
    if ((argIn.size() - 3) % 2 != 0) {
        Error(L"Nelson:statistics:nameValueNotPaired", L"Name-value arguments must be paired.");
    }
    for (size_t k = 3; k < argIn.size(); k += 2) {
        std::wstring name = textScalar(argIn[k], _W("Invalid name-value argument name."));
        const ArrayOf& value = argIn[k + 1];
        if (nameMatches(name, L"distance")) {
            if (value.isFunctionHandle()) {
                options.metric = Metric::CUSTOM;
                options.distanceFunction = value;
            } else {
                options.metric = metricFromText(textScalar(value,
                    formatI18nMessage(Nelson::resolveMessageTemplate(
                                          L"Nelson:statistics:invalidValue", L"Invalid %s value."),
                        L"Distance")));
            }
        } else if (nameMatches(name, L"p")) {
            options.hasP = true;
            options.p = positiveScalar(value,
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(
                        L"Nelson:statistics:mustBePositive", L"%s must be a positive scalar."),
                    L"P"),
                false);
        } else if (nameMatches(name, L"scale")) {
            options.hasScale = true;
            options.scale = realVector(value,
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBeRealVector",
                        L"%s must be a real numeric vector."),
                    L"Scale"));
        } else if (nameMatches(name, L"cov")) {
            options.hasCov = true;
            options.cov = realVector(value,
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:realNumericMatrix",
                        L"%s must be a real numeric matrix."),
                    L"Cov"));
        } else {
            Error(L"Nelson:statistics:unknownNameValue", L"Invalid name-value argument name.");
        }
    }
    return options;
}
//=============================================================================
void
validateOptions(const Options& options)
{
    if (options.hasP && options.metric != Metric::MINKOWSKI) {
        Error(L"Nelson:statistics:pRequiresMinkowski", L"%s is valid only with %s.", L"P",
            L"Minkowski distance");
    }
    if (options.hasScale && options.metric != Metric::SEUCLIDEAN) {
        Error(L"Nelson:statistics:scaleRequiresSeuclidean", L"%s is valid only with %s.", L"Scale",
            L"standardized Euclidean distance");
    }
    if (options.hasCov && options.metric != Metric::MAHALANOBIS) {
        Error(L"Nelson:statistics:covRequiresMahalanobis", L"%s is valid only with %s.", L"Cov",
            L"Mahalanobis distance");
    }
}
//=============================================================================
DbscanData
parseData(ArrayOf value, const Options& options)
{
    DbscanData data;
    data.originalX = value;
    if (options.metric == Metric::PRECOMPUTED) {
        if (value.isSparse() || !value.isReal()) {
            Error(L"Nelson:statistics:invalidPrecomputedDistance",
                L"Precomputed distances must be %s.", L"a real full matrix or vector");
        }
        data.precomputedLogical = value.isLogical();
        if (data.precomputedLogical) {
            data.logicalX = static_cast<const logical*>(value.dataPointer());
            if (value.is2D() && value.rows() == value.columns()) {
                data.rows = value.rows();
                data.cols = value.columns();
            } else if (value.isVector()) {
                data.precomputedVector = true;
                data.rows = precomputedVectorSizeToRows(value.elementCount());
                data.cols = value.elementCount();
            } else {
                Error(L"Nelson:statistics:invalidPrecomputedDistance",
                    L"Precomputed distances must be %s.", L"a square matrix or vector");
            }
            return data;
        }
        data.numericX = realDoubleArray(value,
            formatI18nMessage(resolveMessageTemplate(
                                  L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                L"Precomputed distances"));
        if (data.numericX.is2D() && data.numericX.rows() == data.numericX.columns()) {
            data.rows = data.numericX.rows();
            data.cols = data.numericX.columns();
        } else if (data.numericX.isVector()) {
            data.precomputedVector = true;
            data.rows = precomputedVectorSizeToRows(data.numericX.elementCount());
            data.cols = data.numericX.elementCount();
        } else {
            Error(L"Nelson:statistics:invalidPrecomputedDistance",
                L"Precomputed distances must be %s.", L"a square matrix or vector");
        }
        data.x = static_cast<const double*>(data.numericX.dataPointer());
        return data;
    }
    data.numericX = realDoubleArray(value,
        Nelson::resolveMessageTemplate(
            L"Nelson:statistics:xMustBeRealMatrix", L"X must be a real numeric full matrix."));
    if (!data.numericX.is2D()) {
        Error(L"Nelson:statistics:xMustBeRealMatrix", L"X must be a real numeric full matrix.");
    }
    data.rows = data.numericX.rows();
    data.cols = data.numericX.columns();
    data.x = static_cast<const double*>(data.numericX.dataPointer());
    return data;
}
//=============================================================================
double
at(const double* x, indexType rows, indexType row, indexType col)
{
    return x[row + col * rows];
}
//=============================================================================
indexType
precomputedVectorIndex(indexType rows, indexType a, indexType b)
{
    if (a == b) {
        return 0;
    }
    if (a < b) {
        std::swap(a, b);
    }
    return b * rows - (b * (b + 1)) / 2 + (a - b - 1);
}
//=============================================================================
double
precomputedDistance(const DbscanData& data, indexType a, indexType b)
{
    if (a == b) {
        return 0.0;
    }
    if (data.precomputedVector) {
        return data.x[precomputedVectorIndex(data.rows, a, b)];
    }
    return at(data.x, data.rows, a, b);
}
//=============================================================================
bool
precomputedLogicalNeighbor(const DbscanData& data, indexType a, indexType b)
{
    if (a == b) {
        return true;
    }
    if (data.precomputedVector) {
        return data.logicalX[precomputedVectorIndex(data.rows, a, b)] != 0;
    }
    return data.logicalX[a + b * data.rows] != 0;
}
//=============================================================================
void
computeScale(DbscanData& data, Options& options)
{
    if (!options.scale.empty()) {
        if (static_cast<indexType>(options.scale.size()) != data.cols) {
            Error(L"Nelson:statistics:scaleSizeMismatch",
                L"%s must match the number of columns in X.", L"Scale length");
        }
        for (double v : options.scale) {
            if (!std::isfinite(v) || v <= 0) {
                Error(L"Nelson:statistics:invalidScale",
                    L"Scale values must be positive and finite.");
            }
        }
        return;
    }
    options.scale.assign(data.cols, 0.0);
    for (indexType c = 0; c < data.cols; ++c) {
        double sum = 0.0;
        double sumsq = 0.0;
        indexType count = 0;
        for (indexType r = 0; r < data.rows; ++r) {
            double v = at(data.x, data.rows, r, c);
            if (std::isfinite(v)) {
                sum += v;
                sumsq += v * v;
                ++count;
            }
        }
        if (count > 1) {
            double var = (sumsq - sum * sum / static_cast<double>(count)) / (count - 1);
            options.scale[c] = std::sqrt(std::max(0.0, var));
        }
    }
}
//=============================================================================
bool
invertMatrix(const std::vector<double>& a, indexType n, std::vector<double>& inv)
{
    std::vector<double> m(a);
    inv.assign(n * n, 0.0);
    for (indexType i = 0; i < n; ++i) {
        inv[i + i * n] = 1.0;
    }
    for (indexType col = 0; col < n; ++col) {
        indexType pivot = col;
        double best = std::abs(m[col + col * n]);
        for (indexType row = col + 1; row < n; ++row) {
            double candidate = std::abs(m[row + col * n]);
            if (candidate > best) {
                best = candidate;
                pivot = row;
            }
        }
        if (best == 0.0 || !std::isfinite(best)) {
            return false;
        }
        if (pivot != col) {
            for (indexType c = 0; c < n; ++c) {
                std::swap(m[col + c * n], m[pivot + c * n]);
                std::swap(inv[col + c * n], inv[pivot + c * n]);
            }
        }
        double divisor = m[col + col * n];
        for (indexType c = 0; c < n; ++c) {
            m[col + c * n] /= divisor;
            inv[col + c * n] /= divisor;
        }
        for (indexType row = 0; row < n; ++row) {
            if (row == col) {
                continue;
            }
            double factor = m[row + col * n];
            for (indexType c = 0; c < n; ++c) {
                m[row + c * n] -= factor * m[col + c * n];
                inv[row + c * n] -= factor * inv[col + c * n];
            }
        }
    }
    return true;
}
//=============================================================================
bool
isSymmetricMatrix(const std::vector<double>& a, indexType n)
{
    for (indexType c = 0; c < n; ++c) {
        for (indexType r = c + 1; r < n; ++r) {
            double left = a[r + c * n];
            double right = a[c + r * n];
            double scale = std::max({ 1.0, std::abs(left), std::abs(right) });
            if (std::abs(left - right) > 100.0 * std::numeric_limits<double>::epsilon() * scale) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
bool
isPositiveDefiniteMatrix(const std::vector<double>& a, indexType n)
{
    std::vector<double> lower(n * n, 0.0);
    for (indexType c = 0; c < n; ++c) {
        for (indexType r = c; r < n; ++r) {
            double sum = a[r + c * n];
            for (indexType k = 0; k < c; ++k) {
                sum -= lower[r + k * n] * lower[c + k * n];
            }
            if (r == c) {
                if (sum <= 0.0 || !std::isfinite(sum)) {
                    return false;
                }
                lower[r + c * n] = std::sqrt(sum);
            } else {
                lower[r + c * n] = sum / lower[c + c * n];
            }
        }
    }
    return true;
}
//=============================================================================
void
computeCovariance(DbscanData& data, Options& options)
{
    if (!options.cov.empty()) {
        if (static_cast<indexType>(options.cov.size()) != data.cols * data.cols) {
            Error(L"Nelson:statistics:covColumnMismatch",
                L"%s must match the number of columns in X.", L"Cov size");
        }
        for (double v : options.cov) {
            if (!std::isfinite(v)) {
                Error(L"Nelson:statistics:invalidCov", L"Cov values must be finite.");
            }
        }
        if (!isSymmetricMatrix(options.cov, data.cols)
            || !isPositiveDefiniteMatrix(options.cov, data.cols)) {
            Error(L"Nelson:statistics:covNotPositiveDefinite",
                L"Cov must be symmetric positive definite.");
        }
        if (!invertMatrix(options.cov, data.cols, data.invCov)) {
            Error(L"Nelson:statistics:covMustBeNonsingular", L"Cov must be nonsingular.");
        }
        return;
    }
    std::vector<double> means(data.cols, 0.0);
    indexType count = 0;
    for (indexType r = 0; r < data.rows; ++r) {
        bool valid = true;
        for (indexType c = 0; c < data.cols; ++c) {
            valid = valid && std::isfinite(at(data.x, data.rows, r, c));
        }
        if (valid) {
            for (indexType c = 0; c < data.cols; ++c) {
                means[c] += at(data.x, data.rows, r, c);
            }
            ++count;
        }
    }
    if (count < 2) {
        Error(L"Nelson:statistics:covNotComputable", L"Cov cannot be computed from X.");
    }
    for (double& mean : means) {
        mean /= static_cast<double>(count);
    }
    options.cov.assign(data.cols * data.cols, 0.0);
    for (indexType r = 0; r < data.rows; ++r) {
        bool valid = true;
        for (indexType c = 0; c < data.cols; ++c) {
            valid = valid && std::isfinite(at(data.x, data.rows, r, c));
        }
        if (valid) {
            for (indexType a = 0; a < data.cols; ++a) {
                double da = at(data.x, data.rows, r, a) - means[a];
                for (indexType b = 0; b < data.cols; ++b) {
                    options.cov[a + b * data.cols] += da * (at(data.x, data.rows, r, b) - means[b]);
                }
            }
        }
    }
    for (double& v : options.cov) {
        v /= static_cast<double>(count - 1);
    }
    if (!invertMatrix(options.cov, data.cols, data.invCov)) {
        Error(L"Nelson:statistics:covMustBeNonsingular", L"Cov must be nonsingular.");
    }
}
//=============================================================================
void
centerRows(DbscanData& data)
{
    data.work.assign(data.rows * data.cols, std::numeric_limits<double>::quiet_NaN());
    data.rowNorms.assign(data.rows, std::numeric_limits<double>::quiet_NaN());
    for (indexType r = 0; r < data.rows; ++r) {
        double mean = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            mean += at(data.x, data.rows, r, c);
        }
        mean /= static_cast<double>(data.cols);
        double norm = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            double v = at(data.x, data.rows, r, c) - mean;
            data.work[r + c * data.rows] = v;
            norm += v * v;
        }
        data.rowNorms[r] = std::sqrt(norm);
    }
}
//=============================================================================
void
normalizeRows(DbscanData& data)
{
    data.rowNorms.assign(data.rows, 0.0);
    for (indexType r = 0; r < data.rows; ++r) {
        double norm = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            double v = at(data.x, data.rows, r, c);
            norm += v * v;
        }
        data.rowNorms[r] = std::sqrt(norm);
    }
}
//=============================================================================
void
rankRows(DbscanData& data)
{
    std::vector<double> ranks(data.rows * data.cols, std::numeric_limits<double>::quiet_NaN());
    std::vector<indexType> order(data.cols);
    for (indexType r = 0; r < data.rows; ++r) {
        std::iota(order.begin(), order.end(), 0);
        std::sort(order.begin(), order.end(), [&](indexType a, indexType b) {
            return at(data.x, data.rows, r, a) < at(data.x, data.rows, r, b);
        });
        for (indexType k = 0; k < data.cols;) {
            indexType end = k + 1;
            double value = at(data.x, data.rows, r, order[k]);
            if (std::isnan(value)) {
                break;
            }
            while (end < data.cols && at(data.x, data.rows, r, order[end]) == value) {
                ++end;
            }
            double rank = (static_cast<double>(k + 1) + static_cast<double>(end)) / 2.0;
            for (indexType p = k; p < end; ++p) {
                ranks[r + order[p] * data.rows] = rank;
            }
            k = end;
        }
    }
    data.work.assign(data.rows * data.cols, std::numeric_limits<double>::quiet_NaN());
    data.rowNorms.assign(data.rows, std::numeric_limits<double>::quiet_NaN());
    for (indexType r = 0; r < data.rows; ++r) {
        double mean = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            mean += at(ranks.data(), data.rows, r, c);
        }
        mean /= static_cast<double>(data.cols);
        double norm = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            double v = at(ranks.data(), data.rows, r, c) - mean;
            data.work[r + c * data.rows] = v;
            norm += v * v;
        }
        data.rowNorms[r] = std::sqrt(norm);
    }
}
//=============================================================================
void
prepareMetric(DbscanData& data, Options& options)
{
    if (options.metric == Metric::SEUCLIDEAN) {
        computeScale(data, options);
    } else if (options.metric == Metric::MAHALANOBIS) {
        computeCovariance(data, options);
    } else if (options.metric == Metric::COSINE) {
        normalizeRows(data);
    } else if (options.metric == Metric::CORRELATION) {
        centerRows(data);
    } else if (options.metric == Metric::SPEARMAN) {
        rankRows(data);
    }
}
//=============================================================================
double
distanceValue(
    const DbscanData& data, const Options& options, indexType a, indexType b, double limit)
{
    if (options.metric == Metric::PRECOMPUTED) {
        return precomputedDistance(data, a, b);
    }
    const double* x = data.work.empty() ? data.x : data.work.data();
    switch (options.metric) {
    case Metric::EUCLIDEAN: {
        double s = 0.0;
        double limit2 = limit * limit;
        for (indexType c = 0; c < data.cols && s <= limit2; ++c) {
            double d = at(x, data.rows, a, c) - at(x, data.rows, b, c);
            s += d * d;
        }
        return std::sqrt(s);
    }
    case Metric::SQUAREDEUCLIDEAN: {
        double s = 0.0;
        for (indexType c = 0; c < data.cols && s <= limit; ++c) {
            double d = at(x, data.rows, a, c) - at(x, data.rows, b, c);
            s += d * d;
        }
        return s;
    }
    case Metric::SEUCLIDEAN: {
        double s = 0.0;
        double limit2 = limit * limit;
        for (indexType c = 0; c < data.cols && s <= limit2; ++c) {
            double d = at(data.x, data.rows, a, c) - at(data.x, data.rows, b, c);
            if (options.scale[c] != 0.0) {
                d /= options.scale[c];
            } else if (d != 0.0) {
                return std::numeric_limits<double>::infinity();
            }
            s += d * d;
        }
        return std::sqrt(s);
    }
    case Metric::CITYBLOCK: {
        double s = 0.0;
        for (indexType c = 0; c < data.cols && s <= limit; ++c) {
            s += std::abs(at(data.x, data.rows, a, c) - at(data.x, data.rows, b, c));
        }
        return s;
    }
    case Metric::CHEBYCHEV: {
        double s = 0.0;
        for (indexType c = 0; c < data.cols && s <= limit; ++c) {
            s = std::max(s, std::abs(at(data.x, data.rows, a, c) - at(data.x, data.rows, b, c)));
        }
        return s;
    }
    case Metric::MINKOWSKI: {
        double s = 0.0;
        double limitP = std::pow(limit, options.p);
        for (indexType c = 0; c < data.cols && s <= limitP; ++c) {
            s += std::pow(
                std::abs(at(data.x, data.rows, a, c) - at(data.x, data.rows, b, c)), options.p);
        }
        return std::pow(s, 1.0 / options.p);
    }
    case Metric::MAHALANOBIS: {
        double s = 0.0;
        for (indexType i = 0; i < data.cols; ++i) {
            double di = at(data.x, data.rows, a, i) - at(data.x, data.rows, b, i);
            double rowSum = 0.0;
            for (indexType j = 0; j < data.cols; ++j) {
                double dj = at(data.x, data.rows, a, j) - at(data.x, data.rows, b, j);
                rowSum += data.invCov[i + j * data.cols] * dj;
            }
            s += di * rowSum;
        }
        return std::sqrt(std::max(0.0, s));
    }
    case Metric::COSINE:
    case Metric::CORRELATION:
    case Metric::SPEARMAN: {
        double denom = data.rowNorms[a] * data.rowNorms[b];
        if (denom == 0.0 || !std::isfinite(denom)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double dot = 0.0;
        for (indexType c = 0; c < data.cols; ++c) {
            dot += at(x, data.rows, a, c) * at(x, data.rows, b, c);
        }
        return 1.0 - dot / denom;
    }
    case Metric::HAMMING: {
        indexType count = 0;
        for (indexType c = 0; c < data.cols; ++c) {
            if (at(data.x, data.rows, a, c) != at(data.x, data.rows, b, c)) {
                ++count;
            }
        }
        return static_cast<double>(count) / static_cast<double>(data.cols);
    }
    case Metric::JACCARD: {
        indexType unequal = 0;
        indexType nonzero = 0;
        for (indexType c = 0; c < data.cols; ++c) {
            double va = at(data.x, data.rows, a, c);
            double vb = at(data.x, data.rows, b, c);
            if (va != 0.0 || vb != 0.0) {
                ++nonzero;
                if (va != vb) {
                    ++unequal;
                }
            }
        }
        return nonzero == 0 ? std::numeric_limits<double>::quiet_NaN()
                            : static_cast<double>(unequal) / static_cast<double>(nonzero);
    }
    default:
        return std::numeric_limits<double>::quiet_NaN();
    }
}
//=============================================================================
ArrayOf
rowAsDouble(const DbscanData& data, indexType row)
{
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, data.cols));
    for (indexType c = 0; c < data.cols; ++c) {
        out[c] = at(data.x, data.rows, row, c);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, data.cols), out);
}
//=============================================================================
std::vector<double>
callDistance(Evaluator* eval, const DbscanData& data, const Options& options, indexType row)
{
    FunctionDef* fevalDef = nullptr;
    if (!eval || !eval->getContext() || !eval->getContext()->lookupFunction("feval", fevalDef)
        || fevalDef == nullptr) {
        Error(L"Nelson:statistics:evaluatorNotAvailable", L"Function evaluator is not available.");
    }
    ArrayOfVector args;
    args << options.distanceFunction;
    args << rowAsDouble(data, row);
    args << data.numericX;
    ArrayOfVector result = fevalDef->evaluateFunction(eval, args, 1);
    if (result.empty()) {
        Error(L"Nelson:statistics:invalidDistanceFunctionOutput",
            L"Distance function must return %s.", L"a distance vector");
    }
    ArrayOf distances = realDoubleArray(result[0],
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidDistanceFunctionOutput",
                L"Distance function must return %s."),
            L"real numeric values"));
    if (!distances.isVector() || distances.elementCount() != data.rows) {
        Error(L"Nelson:statistics:invalidDistanceFunctionOutput",
            L"Distance function must return %s.", L"one distance per row");
    }
    const double* ptr = static_cast<const double*>(distances.dataPointer());
    return std::vector<double>(ptr, ptr + data.rows);
}
//=============================================================================
std::vector<indexType>
regionQuery(
    Evaluator* eval, const DbscanData& data, const Options& options, indexType row, double eps)
{
    std::vector<indexType> neighbors;
    neighbors.reserve(static_cast<size_t>(std::min<indexType>(data.rows, 256)));
    if (options.metric == Metric::PRECOMPUTED && data.precomputedLogical) {
        for (indexType j = 0; j < data.rows; ++j) {
            if (precomputedLogicalNeighbor(data, row, j)) {
                neighbors.push_back(j);
            }
        }
        return neighbors;
    }
    if (options.metric == Metric::CUSTOM) {
        std::vector<double> distances = callDistance(eval, data, options, row);
        for (indexType j = 0; j < data.rows; ++j) {
            if (!std::isnan(distances[j]) && distances[j] <= eps) {
                neighbors.push_back(j);
            }
        }
        return neighbors;
    }
    for (indexType j = 0; j < data.rows; ++j) {
        double d = distanceValue(data, options, row, j, eps);
        if (!std::isnan(d) && d <= eps) {
            neighbors.push_back(j);
        }
    }
    return neighbors;
}
//=============================================================================
ArrayOfVector
runDbscan(
    Evaluator* eval, DbscanData& data, Options& options, double eps, indexType minpts, int nLhs)
{
    prepareMetric(data, options);
    std::vector<std::vector<indexType>> neighborhoods(data.rows);
    std::vector<logical> computed(data.rows, 0);
    std::vector<logical> visited(data.rows, 0);
    std::vector<logical> core(data.rows, 0);
    std::vector<double> labels(data.rows, 0.0);
    auto getNeighbors = [&](indexType row) -> const std::vector<indexType>& {
        if (!computed[row]) {
            neighborhoods[row] = regionQuery(eval, data, options, row, eps);
            core[row] = neighborhoods[row].size() >= static_cast<size_t>(minpts);
            computed[row] = 1;
        }
        return neighborhoods[row];
    };
    double cluster = 0.0;
    for (indexType i = 0; i < data.rows; ++i) {
        if (visited[i]) {
            continue;
        }
        visited[i] = 1;
        const std::vector<indexType>& seed = getNeighbors(i);
        if (!core[i]) {
            labels[i] = -1.0;
            continue;
        }
        cluster += 1.0;
        labels[i] = cluster;
        std::queue<indexType> queue;
        for (indexType neighbor : seed) {
            if (neighbor != i) {
                queue.push(neighbor);
            }
        }
        while (!queue.empty()) {
            indexType current = queue.front();
            queue.pop();
            if (!visited[current]) {
                visited[current] = 1;
                const std::vector<indexType>& currentNeighbors = getNeighbors(current);
                if (core[current]) {
                    for (indexType neighbor : currentNeighbors) {
                        if (!visited[neighbor] || labels[neighbor] <= 0.0) {
                            queue.push(neighbor);
                        }
                    }
                }
            }
            if (labels[current] <= 0.0) {
                labels[current] = cluster;
            }
        }
    }
    if (nLhs > 1) {
        for (indexType i = 0; i < data.rows; ++i) {
            getNeighbors(i);
        }
    }
    double* labelsOut = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, data.rows));
    std::copy(labels.begin(), labels.end(), labelsOut);
    ArrayOfVector out;
    out << ArrayOf(NLS_DOUBLE, Dimensions(data.rows, 1), labelsOut);
    if (nLhs > 1) {
        logical* coreOut = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, data.rows));
        std::copy(core.begin(), core.end(), coreOut);
        out << ArrayOf(NLS_LOGICAL, Dimensions(data.rows, 1), coreOut);
    }
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::Dbscan(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3);
    nargoutcheck(nLhs, 0, 2);
    indexType minpts = static_cast<indexType>(positiveScalar(argIn[2],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                              L"%s must be a positive integer scalar."),
            L"MinPts"),
        true));
    Options options = parseOptions(argIn);
    validateOptions(options);
    DbscanData data = parseData(argIn[0], options);
    double eps = parseEpsilon(argIn[1], options, data);
    return runDbscan(eval, data, options, eps, minpts, nLhs);
}
//=============================================================================
