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
parseVarTest(const ArrayOfVector& args, ArrayOf& x, double& v, double& alpha, std::wstring& tail,
    indexType& dim)
{
    x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    v = scalarDouble(args[1],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:validators:mustBeScalar", L"%s must be a scalar."),
            L"V"));
    if (!(v >= 0)) {
        Error("Nelson:validators:mustBeNonnegative", "%s must be nonnegative.", "V");
    }
    alpha = 0.05;
    tail = L"both";
    dim = firstNonSingleton(x.dimensions());
    if ((args.size() - 2) % 2 != 0) {
        Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
    }
    for (sizeType pos = 2; pos < args.size(); pos += 2) {
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
computeValues(double sum, double sumsq, double n, double v, double alpha, const std::wstring& tail,
    double& stat, double& p, double& lo, double& hi, double& df)
{
    if (n < 2) {
        stat = p = lo = hi = df = std::numeric_limits<double>::quiet_NaN();
        return;
    }
    df = n - 1.0;
    double centered = std::max(0.0, sumsq - sum * sum / n);
    stat = v == 0 ? (centered == 0 ? std::numeric_limits<double>::quiet_NaN()
                                   : std::numeric_limits<double>::infinity())
                  : centered / v;
    double cdf = ChiSquareCdf(stat, df);
    double upper = ChiSquareUpper(stat, df);
    if (tail == L"right") {
        p = upper;
        lo = centered / ChiSquareInv(1.0 - alpha, df);
        hi = std::numeric_limits<double>::infinity();
    } else if (tail == L"left") {
        p = cdf;
        lo = 0;
        hi = centered / ChiSquareInv(alpha, df);
    } else {
        p = std::min(1.0, 2.0 * std::min(cdf, upper));
        lo = centered / ChiSquareInv(1.0 - alpha / 2.0, df);
        hi = centered / ChiSquareInv(alpha / 2.0, df);
    }
}
//=============================================================================
TTestResult
VarTest(const ArrayOfVector& args)
{
    ArrayOf x;
    double v;
    double alpha;
    std::wstring tail;
    indexType dim;
    parseVarTest(args, x, v, alpha, tail, dim);
    Dimensions outDims = x.dimensions();
    outDims.setDimensionLength(dim, 1);
    outDims.simplify();
    indexType outCount = outDims.elementCount();
    indexType planes, planeSize, lineSize;
    planeShape(x.dimensions(), dim, planes, planeSize, lineSize);
    logical* h = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, outCount));
    double* p = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* stat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* df = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
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
        double sumsq = 0;
        double n = 0;
        for (indexType k = 0; k < lineSize; ++k) {
            double value = xp[base + k * planeSize];
            if (!std::isnan(value)) {
                sum += value;
                sumsq += value * value;
                n += 1;
            }
        }
        double lo;
        double hi;
        computeValues(sum, sumsq, n, v, alpha, tail, stat[out], p[out], lo, hi, df[out]);
        h[out] = (!std::isnan(p[out]) && p[out] < alpha) ? 1 : 0;
        ci[2 * out] = lo;
        ci[2 * out + 1] = hi;
    }
    stringVector names { "chisqstat", "df" };
    ArrayOfVector values;
    values << ArrayOf(NLS_DOUBLE, outDims, stat);
    values << ArrayOf(NLS_DOUBLE, outDims, df);
    TTestResult result;
    result.h = ArrayOf(NLS_LOGICAL, outDims, h);
    result.p = ArrayOf(NLS_DOUBLE, outDims, p);
    result.ci = ArrayOf(NLS_DOUBLE, Dimensions(2, outCount), ci);
    result.stats = ArrayOf::structArray(names, values);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
