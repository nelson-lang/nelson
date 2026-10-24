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
//=============================================================================
namespace Nelson {
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
static Dimensions
commonDimensions(const ArrayOf& a, const ArrayOf& b)
{
    if (!a.isScalar() && !b.isScalar() && !a.dimensions().equals(b.dimensions())) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
    return !a.isScalar() ? a.dimensions() : (!b.isScalar() ? b.dimensions() : Dimensions(1, 1));
}
//=============================================================================
static double
valueAt(const ArrayOf& value, indexType k)
{
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return ptr[value.isScalar() ? 0 : k];
}
//=============================================================================
static bool
isNonnegativeInteger(double value)
{
    return std::isfinite(value) && value >= 0 && std::floor(value) == value;
}
//=============================================================================
static void
validateCounts(double x, double n)
{
    if (std::isnan(x) || std::isnan(n)) {
        return;
    }
    if (!isNonnegativeInteger(x)) {
        Error("Nelson:statistics:xMustBeNonnegativeInteger",
            "%s must contain nonnegative integer finite values.", "X");
    }
    if (!isNonnegativeInteger(n)) {
        Error("Nelson:statistics:nMustBeNonnegativeInteger",
            "%s must contain nonnegative integer finite values.", "N");
    }
    if (x > n) {
        Error("Nelson:statistics:xExceedsN", "X must not exceed N.");
    }
}
//=============================================================================
static void
validateCountsArray(const ArrayOf& x, const ArrayOf& n, indexType count)
{
    for (indexType k = 0; k < count; ++k) {
        validateCounts(valueAt(x, k), valueAt(n, k));
    }
}
//=============================================================================
static double
alphaValue(const ArrayOfVector& args)
{
    if (args.size() < 3 || args[2].isEmpty(false)) {
        return 0.05;
    }
    double alpha = scalarDouble(args[2],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
            L"ALPHA"));
    if (!(alpha >= 0 && alpha <= 1)) {
        Error("Nelson:statistics:alphaOutOfRangeClosed", "%s must be a scalar in the range [0,1].",
            "ALPHA");
    }
    return alpha;
}
//=============================================================================
static double
betaFraction(double a, double b, double x)
{
    const int maxIterations = 200;
    const double eps = 3.0e-14;
    const double fpmin = std::numeric_limits<double>::min() / eps;
    double qab = a + b;
    double qap = a + 1.0;
    double qam = a - 1.0;
    double c = 1.0;
    double d = 1.0 - qab * x / qap;
    if (std::fabs(d) < fpmin) {
        d = fpmin;
    }
    d = 1.0 / d;
    double h = d;
    for (int m = 1; m <= maxIterations; ++m) {
        int m2 = 2 * m;
        double aa = m * (b - m) * x / ((qam + m2) * (a + m2));
        d = 1.0 + aa * d;
        if (std::fabs(d) < fpmin) {
            d = fpmin;
        }
        c = 1.0 + aa / c;
        if (std::fabs(c) < fpmin) {
            c = fpmin;
        }
        d = 1.0 / d;
        h *= d * c;
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
        d = 1.0 + aa * d;
        if (std::fabs(d) < fpmin) {
            d = fpmin;
        }
        c = 1.0 + aa / c;
        if (std::fabs(c) < fpmin) {
            c = fpmin;
        }
        d = 1.0 / d;
        double del = d * c;
        h *= del;
        if (std::fabs(del - 1.0) <= eps) {
            break;
        }
    }
    return h;
}
//=============================================================================
static double
regularizedBeta(double x, double a, double b)
{
    if (std::isnan(x) || std::isnan(a) || std::isnan(b) || a <= 0 || b <= 0 || x < 0 || x > 1) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x == 0) {
        return 0;
    }
    if (x == 1) {
        return 1;
    }
    double bt = std::exp(std::lgamma(a + b) - std::lgamma(a) - std::lgamma(b) + a * std::log(x)
        + b * std::log1p(-x));
    if (x < (a + 1.0) / (a + b + 2.0)) {
        return bt * betaFraction(a, b, x) / a;
    }
    return 1.0 - bt * betaFraction(b, a, 1.0 - x) / b;
}
//=============================================================================
static double
betaInvValue(double p, double a, double b)
{
    if (std::isnan(p) || std::isnan(a) || std::isnan(b) || p < 0 || p > 1 || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 0) {
        return 0;
    }
    if (p == 1) {
        return 1;
    }
    double lo = 0;
    double hi = 1;
    for (int k = 0; k < 120; ++k) {
        double mid = 0.5 * (lo + hi);
        if (regularizedBeta(mid, a, b) < p) {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    return 0.5 * (lo + hi);
}
//=============================================================================
BinomialFitResult
BinomialFit(const ArrayOfVector& args)
{
    ArrayOf x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    ArrayOf n = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"N"));
    if (x.isEmpty(false) || n.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X and N");
    }
    Dimensions dims = commonDimensions(x, n);
    double alpha = alphaValue(args);
    indexType count = dims.elementCount();
    validateCountsArray(x, n, count);
    double* pHat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, count));
    double* pCI = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, count * 2));
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double xv = valueAt(x, k);
        double nv = valueAt(n, k);
        if (std::isnan(xv) || std::isnan(nv) || nv == 0) {
            pHat[k] = std::numeric_limits<double>::quiet_NaN();
            pCI[k] = pHat[k];
            pCI[k + count] = pHat[k];
            continue;
        }
        pHat[k] = xv / nv;
        pCI[k] = xv == 0 ? 0 : betaInvValue(alpha / 2.0, xv, nv - xv + 1.0);
        pCI[k + count] = xv == nv ? 1 : betaInvValue(1.0 - alpha / 2.0, xv + 1.0, nv - xv);
    }
    BinomialFitResult result;
    result.pHat = ArrayOf(NLS_DOUBLE, dims, pHat);
    result.pCI = ArrayOf(NLS_DOUBLE, Dimensions(count, 2), pCI);
    return result;
}
//=============================================================================
static double
binomialNegativeLogLikelihood(double p, const ArrayOf& x, const ArrayOf& n, indexType count)
{
    if (std::isnan(p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!std::isfinite(p) || p < 0 || p > 1) {
        return std::numeric_limits<double>::infinity();
    }
    double nlogL = 0;
    for (indexType k = 0; k < count; ++k) {
        double xv = valueAt(x, k);
        double nv = valueAt(n, k);
        validateCounts(xv, nv);
        if (std::isnan(xv) || std::isnan(nv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        if ((p == 0 && xv != 0) || (p == 1 && xv != nv)) {
            return std::numeric_limits<double>::infinity();
        }
        if (p == 0 || p == 1) {
            continue;
        }
        double logc = std::lgamma(nv + 1.0) - std::lgamma(xv + 1.0) - std::lgamma(nv - xv + 1.0);
        nlogL -= logc + xv * std::log(p) + (nv - xv) * std::log1p(-p);
    }
    return nlogL;
}
//=============================================================================
static BinomialLikeResult
makeLikeResult(double nlogL, double variance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* varPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    varPtr[0] = variance;
    BinomialLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), varPtr);
    return result;
}
//=============================================================================
BinomialLikeResult
BinomialLike(const ArrayOfVector& args)
{
    double p = scalarDouble(args[0],
        resolveMessageTemplate(
            L"Nelson:statistics:pRealNumericScalar", L"P must be a real numeric scalar."));
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    ArrayOf n = asDoubleArray(args[2],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"N"));
    if (x.isEmpty(false) || n.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X and N");
    }
    Dimensions dims = commonDimensions(x, n);
    indexType count = dims.elementCount();
    validateCountsArray(x, n, count);
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    double nlogL = binomialNegativeLogLikelihood(p, x, n, count);
    if (std::isnan(nlogL) || std::isinf(nlogL)) {
        return makeLikeResult(nlogL, nanValue);
    }
    double totalN = 0;
    for (indexType k = 0; k < count; ++k) {
        totalN += valueAt(n, k);
    }
    double variance = (p > 0 && p < 1 && totalN > 0) ? p * (1.0 - p) / totalN : 0;
    return makeLikeResult(nlogL, variance);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
