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
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
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
    if (matches.size() != 1) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    return matches[0];
}
//=============================================================================
static ArrayOf
asDoubleArray(const ArrayOf& value, const std::wstring& message)
{
    if (!value.isNumeric() || value.isSparse() || value.isComplex()) {
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
static indexType
firstNonSingleton(const Dimensions& dims)
{
    for (indexType k = 0; k < dims.length(); ++k) {
        if (dims.dimensionLength(k) != 1) {
            return k;
        }
    }
    return 0;
}
//=============================================================================
static void
planeShape(const Dimensions& dims, indexType dim, indexType& planes, indexType& planeSize,
    indexType& lineSize)
{
    lineSize = dims.dimensionLength(dim);
    planeSize = 1;
    for (indexType k = 0; k < dim; ++k) {
        planeSize *= dims.dimensionLength(k);
    }
    planes = 1;
    for (indexType k = dim + 1; k < dims.length(); ++k) {
        planes *= dims.dimensionLength(k);
    }
}
//=============================================================================
static void
parseZTest(const ArrayOfVector& args, ArrayOf& x, double& m, double& sigma, double& alpha,
    std::wstring& tail, indexType& dim)
{
    x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    m = scalarDouble(args[1],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:validators:mustBeScalar", L"%s must be a scalar."),
            L"M"));
    sigma = scalarDouble(args[2],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:validators:mustBeScalar", L"%s must be a scalar."),
            L"SIGMA"));
    if (!(sigma > 0)) {
        Error("Nelson:validators:mustBePositive", "%s must be positive.", "SIGMA");
    }
    alpha = 0.05;
    tail = L"both";
    dim = firstNonSingleton(x.dimensions());
    if ((args.size() - 3) % 2 != 0) {
        Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
    }
    for (sizeType pos = 3; pos < args.size(); pos += 2) {
        std::wstring name = matchOption(args[pos], { L"alpha", L"tail", L"dim" });
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
            tail = matchValue(args[pos + 1], { L"both", L"right", L"left" },
                resolveMessageTemplate(L"Nelson:statistics:tailBothRightLeft",
                    L"TAIL must be 'both', 'right', or 'left'."));
        } else {
            dim = args[pos + 1].asScalarIndex(false) - 1;
        }
    }
}
//=============================================================================
static void
computeValues(double meanValue, double n, double m, double sigma, double alpha,
    const std::wstring& tail, double& z, double& p, double& lo, double& hi)
{
    if (n == 0) {
        z = p = lo = hi = std::numeric_limits<double>::quiet_NaN();
        return;
    }
    double se = sigma / std::sqrt(n);
    z = (meanValue - m) / se;
    if (tail == L"right") {
        p = 1.0 - NormalCdf01(z);
        lo = meanValue - NormalInv01(1.0 - alpha) * se;
        hi = std::numeric_limits<double>::infinity();
    } else if (tail == L"left") {
        p = NormalCdf01(z);
        lo = -std::numeric_limits<double>::infinity();
        hi = meanValue + NormalInv01(1.0 - alpha) * se;
    } else {
        p = 2.0 * std::min(NormalCdf01(z), 1.0 - NormalCdf01(z));
        double crit = NormalInv01(1.0 - alpha / 2.0);
        lo = meanValue - crit * se;
        hi = meanValue + crit * se;
    }
}
//=============================================================================
TTestResult
ZTest(const ArrayOfVector& args)
{
    ArrayOf x;
    double m;
    double sigma;
    double alpha;
    std::wstring tail;
    indexType dim;
    parseZTest(args, x, m, sigma, alpha, tail, dim);
    Dimensions outDims = x.dimensions();
    outDims.setDimensionLength(dim, 1);
    outDims.simplify();
    indexType outCount = outDims.elementCount();
    indexType planes, planeSize, lineSize;
    planeShape(x.dimensions(), dim, planes, planeSize, lineSize);
    logical* h = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, outCount));
    double* p = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* zval = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* ci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * outCount));
    const double* xp = static_cast<const double*>(x.dataPointer());
#if WITH_OPENMP
#pragma omp parallel for if (outCount > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType out = 0; out < (ompIndexType)outCount; ++out) {
        indexType plane = out / planeSize;
        indexType j = out % planeSize;
        indexType base = plane * planeSize * lineSize + j;
        double sum = 0;
        double n = 0;
        for (indexType k = 0; k < lineSize; ++k) {
            double value = xp[base + k * planeSize];
            if (!std::isnan(value)) {
                sum += value;
                n += 1;
            }
        }
        double lo;
        double hi;
        computeValues(sum / n, n, m, sigma, alpha, tail, zval[out], p[out], lo, hi);
        h[out] = (!std::isnan(p[out]) && p[out] < alpha) ? 1 : 0;
        ci[2 * out] = lo;
        ci[2 * out + 1] = hi;
    }
    TTestResult result;
    result.h = ArrayOf(NLS_LOGICAL, outDims, h);
    result.p = ArrayOf(NLS_DOUBLE, outDims, p);
    result.ci = ArrayOf(NLS_DOUBLE, Dimensions(2, outCount), ci);
    result.stats = ArrayOf(NLS_DOUBLE, outDims, zval);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
