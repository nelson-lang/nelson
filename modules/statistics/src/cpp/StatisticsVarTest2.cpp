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
parseVarTest2(const ArrayOfVector& args, ArrayOf& x, ArrayOf& y, double& alpha, std::wstring& tail,
    indexType& dim)
{
    x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    y = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"Y"));
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
static Dimensions
outputDimensions(const ArrayOf& x, const ArrayOf& y, indexType dim, bool& vectorPair)
{
    vectorPair = x.isVector() && y.isVector();
    if (vectorPair) {
        return Dimensions(1, 1);
    }
    Dimensions xd = x.dimensions();
    Dimensions yd = y.dimensions();
    indexType length = std::max(std::max(xd.length(), yd.length()), dim + 1);
    for (indexType k = 0; k < length; ++k) {
        if (k != dim && xd.dimensionLength(k) != yd.dimensionLength(k)) {
            Error("Nelson:statistics:xySizeMismatch", "X and Y must have %s.", "compatible sizes");
        }
    }
    Dimensions outDims = xd;
    outDims.setDimensionLength(dim, 1);
    outDims.simplify();
    return outDims;
}
//=============================================================================
struct SliceStats
{
    double variance;
    double df;
};
//=============================================================================
static SliceStats
sliceStats(const double* data, indexType base, indexType stride, indexType count)
{
    double sum = 0;
    double sumsq = 0;
    double n = 0;
    for (indexType k = 0; k < count; ++k) {
        double value = data[base + k * stride];
        if (!std::isnan(value)) {
            sum += value;
            sumsq += value * value;
            n += 1;
        }
    }
    SliceStats stats;
    stats.df = n - 1.0;
    stats.variance = n < 2 ? std::numeric_limits<double>::quiet_NaN()
                           : std::max(0.0, sumsq - sum * sum / n) / stats.df;
    return stats;
}
//=============================================================================
static void
computeValues(const SliceStats& xs, const SliceStats& ys, double alpha, const std::wstring& tail,
    double& f, double& p, double& lo, double& hi)
{
    if (xs.df < 1 || ys.df < 1 || std::isnan(xs.variance) || std::isnan(ys.variance)) {
        f = p = lo = hi = std::numeric_limits<double>::quiet_NaN();
        return;
    }
    if (ys.variance == 0) {
        f = xs.variance == 0 ? std::numeric_limits<double>::quiet_NaN()
                             : std::numeric_limits<double>::infinity();
    } else {
        f = xs.variance / ys.variance;
    }
    double cdf = FCdf(f, xs.df, ys.df);
    double upper = FUpper(f, xs.df, ys.df);
    if (tail == L"right") {
        p = upper;
        lo = f / FInv(1.0 - alpha, xs.df, ys.df);
        hi = std::numeric_limits<double>::infinity();
    } else if (tail == L"left") {
        p = cdf;
        lo = 0;
        hi = f / FInv(alpha, xs.df, ys.df);
    } else {
        p = std::min(1.0, 2.0 * std::min(cdf, upper));
        lo = f / FInv(1.0 - alpha / 2.0, xs.df, ys.df);
        hi = f / FInv(alpha / 2.0, xs.df, ys.df);
    }
}
//=============================================================================
TTestResult
VarTest2(const ArrayOfVector& args)
{
    ArrayOf x;
    ArrayOf y;
    double alpha;
    std::wstring tail;
    indexType dim;
    parseVarTest2(args, x, y, alpha, tail, dim);
    bool vectorPair;
    Dimensions outDims = outputDimensions(x, y, dim, vectorPair);
    indexType outCount = outDims.elementCount();
    indexType xPlanes, xPlaneSize, xLineSize, yPlanes, yPlaneSize, yLineSize;
    planeShape(x.dimensions(), dim, xPlanes, xPlaneSize, xLineSize);
    planeShape(y.dimensions(), dim, yPlanes, yPlaneSize, yLineSize);
    if (vectorPair) {
        xPlanes = yPlanes = xPlaneSize = yPlaneSize = 1;
        xLineSize = x.elementCount();
        yLineSize = y.elementCount();
    }
    logical* h = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, outCount));
    double* p = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* f = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* df1 = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* df2 = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* ci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * outCount));
    const double* xp = static_cast<const double*>(x.dataPointer());
    const double* yp = static_cast<const double*>(y.dataPointer());
#if WITH_OPENMP
#pragma omp parallel for if (outCount > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType out = 0; out < (ompIndexType)outCount; ++out) {
        indexType plane = vectorPair ? 0 : out / xPlaneSize;
        indexType j = vectorPair ? 0 : out % xPlaneSize;
        indexType xb = plane * xPlaneSize * xLineSize + j;
        indexType yb = plane * yPlaneSize * yLineSize + j;
        SliceStats xs = sliceStats(xp, xb, xPlaneSize, xLineSize);
        SliceStats ys = sliceStats(yp, yb, yPlaneSize, yLineSize);
        df1[out] = xs.df;
        df2[out] = ys.df;
        double lo;
        double hi;
        computeValues(xs, ys, alpha, tail, f[out], p[out], lo, hi);
        h[out] = (!std::isnan(p[out]) && p[out] < alpha) ? 1 : 0;
        ci[2 * out] = lo;
        ci[2 * out + 1] = hi;
    }
    stringVector names { "fstat", "df1", "df2" };
    ArrayOfVector values;
    values << ArrayOf(NLS_DOUBLE, outDims, f);
    values << ArrayOf(NLS_DOUBLE, outDims, df1);
    values << ArrayOf(NLS_DOUBLE, outDims, df2);
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
