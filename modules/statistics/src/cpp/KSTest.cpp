//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StatisticsDistributions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FunctionDef.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr double INV_SQRT2 = 0.70710678118654752440084436210484903928;
//=============================================================================
static bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
static std::wstring
lowerText(const ArrayOf& value)
{
    std::wstring text = value.asWideString();
    std::transform(text.begin(), text.end(), text.begin(), [](wchar_t c) { return towlower(c); });
    return text;
}
//=============================================================================
static std::wstring
matchOption(const ArrayOf& value, const std::vector<std::wstring>& names)
{
    if (!isTextScalar(value)) {
        Error("Nelson:validators:mustBeTextScalar",
            "%s must be a character vector or string scalar.", "Name");
    }
    std::wstring text = lowerText(value);
    std::vector<std::wstring> matches;
    for (const auto& name : names) {
        if (name.rfind(text, 0) == 0) {
            matches.push_back(name);
        }
    }
    if (matches.empty()) {
        Error("Nelson:statistics:unknownNameValue", "Invalid name-value argument name.");
    }
    if (matches.size() > 1) {
        Error("Nelson:statistics:ambiguousNameValue", "Ambiguous name-value argument name.");
    }
    return matches[0];
}
//=============================================================================
static std::wstring
matchValue(
    const ArrayOf& value, const std::vector<std::wstring>& names, const std::wstring& message)
{
    if (!isTextScalar(value)) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    std::wstring text = lowerText(value);
    std::vector<std::wstring> matches;
    for (const auto& name : names) {
        if (name.rfind(text, 0) == 0) {
            matches.push_back(name);
        }
    }
    if (matches.empty() || matches.size() > 1) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    return matches[0];
}
//=============================================================================
static bool
isRealNumeric(const ArrayOf& value)
{
    return value.isNumeric() && !value.isSparse() && !value.isComplex();
}
//=============================================================================
static ArrayOf
asDoubleArray(const ArrayOf& value, const std::wstring& message)
{
    if (!isRealNumeric(value)) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    ArrayOf copy = value;
    copy.promoteType(NLS_DOUBLE);
    return copy;
}
//=============================================================================
static double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleArray(value, message);
    return copy.asDoubleScalar(false, false);
}
//=============================================================================
static double
normalCdf01(double z)
{
    return 0.5 * std::erfc(-z * INV_SQRT2);
}
//=============================================================================
static double
kolmogorovPValue(double z)
{
    if (z < 0 || std::isnan(z)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (z == 0) {
        return 1.0;
    }
    double sum = 0;
    for (int k = 1; k < 200; ++k) {
        double term = std::exp(-2.0 * k * k * z * z);
        sum += (k % 2 ? 1.0 : -1.0) * term;
        if (term < 1e-14) {
            break;
        }
    }
    return std::min(1.0, std::max(0.0, 2.0 * sum));
}
//=============================================================================
static double
ksCritical(double alpha, double n, bool twoSided)
{
    if (twoSided) {
        if (alpha < 0.01 || alpha > 0.2) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        return std::sqrt(-0.5 * std::log(alpha / 2.0) / n);
    }
    if (alpha < 0.005 || alpha > 0.1) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return std::sqrt(-0.5 * std::log(alpha) / n);
}
//=============================================================================
static bool
isCallableCdf(const ArrayOf& value)
{
    return value.isFunctionHandle() || value.isClassType() || value.isHandle();
}
//=============================================================================
static ArrayOf
columnVectorFrom(const std::vector<double>& values)
{
    double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    std::copy(values.begin(), values.end(), ptr);
    return ArrayOf(NLS_DOUBLE, Dimensions(static_cast<indexType>(values.size()), 1), ptr);
}
//=============================================================================
static ArrayOfVector
evaluateFunctionHandle(Evaluator* eval, const ArrayOf& cdfSpec, const ArrayOf& x)
{
    function_handle fh = cdfSpec.asFunctionHandle();
    FunctionDef* funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
    if (funcDef == nullptr) {
        Error("Nelson:statistics:invalidCdf:invalidCDFFunctionHandle",
            "Invalid CDF function handle.");
    }
    ArrayOfVector input;
    input.push_back(x);
    return funcDef->evaluateFunction(eval, input, 1);
}
//=============================================================================
static std::vector<double>
evaluateCallableCdf(Evaluator* eval, const ArrayOf& cdfSpec, const std::vector<double>& x)
{
    if (eval == nullptr) {
        Error("Nelson:statistics:cdfEvaluatorRequired", "CDF evaluation requires an evaluator.");
    }
    ArrayOf xInput = columnVectorFrom(x);
    ArrayOfVector outputs;
    if (cdfSpec.isFunctionHandle()) {
        outputs = evaluateFunctionHandle(eval, cdfSpec, xInput);
    } else {
        ArrayOfVector params;
        params.push_back(xInput);
        if (!eval->bytecodeInvokeObjectMethodIfExists(cdfSpec, "cdf", params, 1, outputs)) {
            Error("Nelson:statistics:invalidCdf:cDFObjectDefineACdfMethod",
                "CDF object must define a cdf method.");
        }
    }
    if (outputs.empty()) {
        Error("Nelson:statistics:invalidCdfOutput", "CDF function must return %s.", "a value");
    }
    ArrayOf values = asDoubleArray(outputs[0],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:invalidCdfOutput",
                              L"CDF function must return %s."),
            L"real numeric values"));
    if (values.elementCount() != x.size()) {
        Error("Nelson:statistics:invalidCdfOutput", "CDF function must return %s.",
            "one value per sample");
    }
    const double* ptr = static_cast<const double*>(values.dataPointer());
    return std::vector<double>(ptr, ptr + values.elementCount());
}
//=============================================================================
static double
interpolateCdf(const std::vector<std::pair<double, double>>& cdf, double x)
{
    if (x < cdf.front().first || x > cdf.back().first) {
        Error("Nelson:statistics:invalidCdf:cDFCoverAllSampleValues",
            "CDF must cover all sample values.");
    }
    auto it = std::lower_bound(
        cdf.begin(), cdf.end(), std::make_pair(x, -std::numeric_limits<double>::infinity()));
    if (it == cdf.begin()) {
        return it->second;
    }
    if (it != cdf.end() && it->first == x) {
        return it->second;
    }
    auto hi = it;
    auto lo = it - 1;
    double frac = (x - lo->first) / (hi->first - lo->first);
    return lo->second + frac * (hi->second - lo->second);
}
//=============================================================================
static void
parseCdf(const ArrayOf& value, std::vector<std::pair<double, double>>& cdf, ArrayOf& callableCdf)
{
    if (isCallableCdf(value)) {
        callableCdf = value;
        return;
    }
    ArrayOf cdfArray = asDoubleArray(value,
        resolveMessageTemplate(L"Nelson:statistics:cdfTwoColumnMatrix",
            L"CDF must be a two-column matrix, function handle, or object."));
    if (cdfArray.columns() != 2) {
        Error(
            "Nelson:statistics:invalidCdf:cDFATwoColumnMatrix", "CDF must be a two-column matrix.");
    }
    const double* cp = static_cast<const double*>(cdfArray.dataPointer());
    cdf.resize(cdfArray.rows());
    for (indexType k = 0; k < cdfArray.rows(); ++k) {
        cdf[k] = { cp[k], cp[k + cdfArray.rows()] };
    }
    std::sort(cdf.begin(), cdf.end());
}
//=============================================================================
static double
hypothesizedCdf(const std::vector<std::pair<double, double>>& cdf,
    const std::vector<double>& callableValues, double x, indexType k)
{
    if (!callableValues.empty()) {
        double value = callableValues[k];
        if (std::isnan(value) || value < 0 || value > 1) {
            Error("Nelson:statistics:invalidCdf", "%s must be in the range [0,1].", "CDF values");
        }
        return value;
    }
    return cdf.empty() ? normalCdf01(x) : interpolateCdf(cdf, x);
}
//=============================================================================
KSTestResult
KSTest(Evaluator* eval, const ArrayOfVector& args)
{
    ArrayOf xArray = asDoubleArray(args[0],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBeRealVector",
                              L"%s must be a real numeric vector."),
            L"X"));
    if (!xArray.isVector()) {
        Error("Nelson:validators:mustBeVector", "%s must be a vector.", "X");
    }
    double alpha = 0.05;
    std::wstring tail = L"unequal";
    std::vector<std::pair<double, double>> cdf;
    ArrayOf callableCdf;
    for (sizeType pos = 1; pos < args.size();) {
        if (pos + 1 >= args.size()) {
            Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
        }
        std::wstring name = matchOption(args[pos], { L"alpha", L"cdf", L"tail" });
        if (name == L"alpha") {
            alpha = scalarDouble(args[pos + 1],
                formatI18nMessage(Nelson::resolveMessageTemplate(
                                      L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                    L"ALPHA"));
            if (!(alpha > 0 && alpha < 1)) {
                Error("Nelson:statistics:alphaOutOfRange",
                    "%s must be a scalar in the range (0,1).", "ALPHA");
            }
        } else if (name == L"tail") {
            tail = matchValue(args[pos + 1], { L"unequal", L"larger", L"smaller" },
                resolveMessageTemplate(L"Nelson:statistics:tailUnequalLargerSmaller",
                    L"TAIL must be 'unequal', 'larger', or 'smaller'."));
        } else {
            parseCdf(args[pos + 1], cdf, callableCdf);
        }
        pos += 2;
    }
    const double* xp = static_cast<const double*>(xArray.dataPointer());
    std::vector<double> x;
    x.reserve(xArray.elementCount());
    for (indexType k = 0; k < xArray.elementCount(); ++k) {
        if (!std::isnan(xp[k])) {
            x.push_back(xp[k]);
        }
    }
    if (x.empty()) {
        Error("Nelson:statistics:xAllNaN", "X must contain at least one non-NaN value.");
    }
    std::sort(x.begin(), x.end());
    std::vector<double> callableValues;
    if (isCallableCdf(callableCdf)) {
        callableValues = evaluateCallableCdf(eval, callableCdf, x);
    }
    double n = static_cast<double>(x.size());
    double dPlus = 0;
    double dMinus = 0;
    for (indexType k = 0; k < x.size(); ++k) {
        double hypoth = hypothesizedCdf(cdf, callableValues, x[k], k);
        double fBefore = static_cast<double>(k) / n;
        double fAt = static_cast<double>(k + 1) / n;
        dPlus = std::max(dPlus, fAt - hypoth);
        dMinus = std::max(dMinus, hypoth - fBefore);
    }
    double stat
        = tail == L"larger" ? dPlus : (tail == L"smaller" ? dMinus : std::max(dPlus, dMinus));
    double p = tail == L"unequal"
        ? kolmogorovPValue((std::sqrt(n) + 0.12 + 0.11 / std::sqrt(n)) * stat)
        : std::exp(-2.0 * n * stat * stat);
    double cv = ksCritical(alpha, n, tail == L"unequal");
    KSTestResult result;
    result.h = ArrayOf::logicalScalar(!std::isnan(p) && p < alpha);
    result.p = ArrayOf::doubleScalar(p);
    result.statistic = ArrayOf::doubleScalar(stat);
    result.criticalValue = ArrayOf::doubleScalar(cv);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
