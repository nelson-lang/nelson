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
parseTTest2(const ArrayOfVector& args, ArrayOf& x, ArrayOf& y, double& alpha, std::wstring& tail,
    std::wstring& vartype, indexType& dim)
{
    if (args.size() < 2) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
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
    vartype = L"equal";
    dim = firstNonSingleton(x.dimensions());
    if ((args.size() - 2) % 2 != 0) {
        Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
    }
    for (sizeType pos = 2; pos < args.size(); pos += 2) {
        std::wstring name = matchOption(args[pos], { L"alpha", L"tail", L"dim", L"vartype" });
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
        } else if (name == L"vartype") {
            vartype = matchValue(args[pos + 1], { L"equal", L"unequal" },
                resolveMessageTemplate(L"Nelson:statistics:vartypeEqualUnequal",
                    L"VARTYPE must be 'equal' or 'unequal'."));
        } else {
            dim = args[pos + 1].asScalarIndex(false) - 1;
        }
    }
}
//=============================================================================
static Dimensions
validateOutputDimensions(const ArrayOf& x, const ArrayOf& y, indexType dim, bool& vectorPair)
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
struct SampleStats
{
    double mean;
    double variance;
    double n;
};
//=============================================================================
static SampleStats
sampleStats(const double* data, indexType base, indexType stride, indexType count)
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
    SampleStats stats;
    stats.mean = sum / n;
    stats.variance = (sumsq - sum * sum / n) / (n - 1);
    stats.n = n;
    return stats;
}
//=============================================================================
static void
tailValues(double diff, double se, double nu, double alpha, const std::wstring& tail, double& t,
    double& pv, double& lo, double& hi)
{
    t = diff / se;
    if (tail == L"right") {
        pv = 1.0 - StudentTCdf(t, nu);
        lo = diff - StudentTInv(1.0 - alpha, nu) * se;
        hi = std::numeric_limits<double>::infinity();
    } else if (tail == L"left") {
        pv = StudentTCdf(t, nu);
        lo = -std::numeric_limits<double>::infinity();
        hi = diff + StudentTInv(1.0 - alpha, nu) * se;
    } else {
        pv = 2.0 * std::min(StudentTCdf(t, nu), 1.0 - StudentTCdf(t, nu));
        double crit = StudentTInv(1.0 - alpha / 2.0, nu);
        lo = diff - crit * se;
        hi = diff + crit * se;
    }
}
//=============================================================================
static void
zeroSeValues(double diff, const std::wstring& tail, double& t, double& pv, double& lo, double& hi)
{
    if (diff == 0) {
        t = std::numeric_limits<double>::quiet_NaN();
        pv = std::numeric_limits<double>::quiet_NaN();
        lo = std::numeric_limits<double>::quiet_NaN();
        hi = std::numeric_limits<double>::quiet_NaN();
        return;
    }
    t = diff > 0 ? std::numeric_limits<double>::infinity()
                 : -std::numeric_limits<double>::infinity();
    if (tail == L"right") {
        pv = diff > 0 ? 0.0 : 1.0;
        lo = diff;
        hi = std::numeric_limits<double>::infinity();
    } else if (tail == L"left") {
        pv = diff < 0 ? 0.0 : 1.0;
        lo = -std::numeric_limits<double>::infinity();
        hi = diff;
    } else {
        pv = 0.0;
        lo = diff;
        hi = diff;
    }
}
//=============================================================================
static void
computeTest(const SampleStats& xs, const SampleStats& ys, const std::wstring& vartype,
    const std::wstring& tail, double alpha, double& t, double& pv, double& lo, double& hi,
    double& nu, double& sd, double& sdx, double& sdy)
{
    double vx = std::max(0.0, xs.variance);
    double vy = std::max(0.0, ys.variance);
    double diff = xs.mean - ys.mean;
    sdx = std::sqrt(vx);
    sdy = std::sqrt(vy);
    if (xs.n < 2 || ys.n < 2) {
        t = pv = lo = hi = nu = sd = std::numeric_limits<double>::quiet_NaN();
        return;
    }
    if (vartype == L"equal") {
        nu = xs.n + ys.n - 2;
        sd = std::sqrt(((xs.n - 1) * vx + (ys.n - 1) * vy) / nu);
        double se = sd * std::sqrt(1.0 / xs.n + 1.0 / ys.n);
        se == 0 ? zeroSeValues(diff, tail, t, pv, lo, hi)
                : tailValues(diff, se, nu, alpha, tail, t, pv, lo, hi);
    } else {
        double sx = vx / xs.n;
        double sy = vy / ys.n;
        double se = std::sqrt(sx + sy);
        double denom = sx * sx / (xs.n - 1) + sy * sy / (ys.n - 1);
        nu = denom == 0 ? xs.n + ys.n - 2 : (sx + sy) * (sx + sy) / denom;
        sd = std::numeric_limits<double>::quiet_NaN();
        se == 0 ? zeroSeValues(diff, tail, t, pv, lo, hi)
                : tailValues(diff, se, nu, alpha, tail, t, pv, lo, hi);
    }
}
//=============================================================================
TTestResult
TTest2(const ArrayOfVector& args)
{
    ArrayOf x;
    ArrayOf y;
    double alpha;
    std::wstring tail;
    std::wstring vartype;
    indexType dim;
    parseTTest2(args, x, y, alpha, tail, vartype, dim);
    bool vectorPair;
    Dimensions outDims = validateOutputDimensions(x, y, dim, vectorPair);
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
    double* tstat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* df = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* sd = vartype == L"equal"
        ? static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount))
        : nullptr;
    double* ci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * outCount));
    double* sdUnequal = vartype == L"unequal"
        ? static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * outCount))
        : nullptr;
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
        SampleStats xs = sampleStats(xp, xb, xPlaneSize, xLineSize);
        SampleStats ys = sampleStats(yp, yb, yPlaneSize, yLineSize);
        double lo, hi, nu, sdValue, sx, sy;
        computeTest(xs, ys, vartype, tail, alpha, tstat[out], p[out], lo, hi, nu, sdValue, sx, sy);
        h[out] = (!std::isnan(p[out]) && p[out] < alpha) ? 1 : 0;
        df[out] = nu;
        if (sd != nullptr) {
            sd[out] = sdValue;
        }
        ci[2 * out] = lo;
        ci[2 * out + 1] = hi;
        if (sdUnequal != nullptr) {
            sdUnequal[2 * out] = sx;
            sdUnequal[2 * out + 1] = sy;
        }
    }
    stringVector names { "tstat", "df", "sd" };
    ArrayOfVector values;
    values << ArrayOf(NLS_DOUBLE, outDims, tstat);
    values << ArrayOf(NLS_DOUBLE, outDims, df);
    values << (vartype == L"equal" ? ArrayOf(NLS_DOUBLE, outDims, sd)
                                   : ArrayOf(NLS_DOUBLE, Dimensions(2, outCount), sdUnequal));
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
