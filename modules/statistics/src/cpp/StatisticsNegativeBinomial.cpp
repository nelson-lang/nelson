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
#include "StatisticsHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Rand.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace StatisticsHelpers;
//=============================================================================
static bool
isNonnegativeInteger(double value)
{
    return std::isfinite(value) && value >= 0 && std::floor(value) == value;
}
//=============================================================================
static bool
validParameters(double r, double p)
{
    return std::isfinite(r) && r > 0 && std::isfinite(p) && p >= 0 && p <= 1;
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
nbinPdfValue(double x, double r, double p)
{
    if (std::isnan(x) || !validParameters(r, p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!isNonnegativeInteger(x)) {
        return 0;
    }
    if (p == 0) {
        return 0;
    }
    if (p == 1) {
        return x == 0 ? 1 : 0;
    }
    double logc = std::lgamma(x + r) - std::lgamma(r) - std::lgamma(x + 1.0);
    return std::exp(logc + r * std::log(p) + x * std::log1p(-p));
}
//=============================================================================
static double
nbinCdfValue(double x, double r, double p, bool upper)
{
    if (std::isnan(x) || !validParameters(r, p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double k = std::floor(x);
    if (k < 0) {
        return upper ? 1 : 0;
    }
    if (p == 0) {
        return upper ? 1 : 0;
    }
    if (p == 1) {
        return upper ? 0 : 1;
    }
    if (std::isinf(k)) {
        return upper ? 0 : 1;
    }
    double cdf = regularizedBeta(p, r, k + 1.0);
    return upper ? std::max(0.0, 1.0 - cdf) : cdf;
}
//=============================================================================
static double
nbinInvValue(double y, double r, double p)
{
    if (std::isnan(y) || y < 0 || y > 1 || !validParameters(r, p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (y == 0 || p == 1) {
        return 0;
    }
    if (p == 0 || y == 1) {
        return std::numeric_limits<double>::infinity();
    }
    double mean = r * (1.0 - p) / p;
    double variance = mean / p;
    double hi = std::max(1.0, std::ceil(mean + 10.0 * std::sqrt(variance) + 10.0));
    while (nbinCdfValue(hi, r, p, false) < y && hi < std::numeric_limits<double>::max() / 2.0) {
        hi = 2.0 * hi + 1.0;
    }
    double lo = 0;
    while (lo < hi) {
        double mid = std::floor(0.5 * (lo + hi));
        if (nbinCdfValue(mid, r, p, false) >= y) {
            hi = mid;
        } else {
            lo = mid + 1.0;
        }
    }
    return lo;
}
//=============================================================================
static ArrayOf
ternaryNBinFunction(
    const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf r = asDoubleArray(args[1],
        resolveMessageTemplate(
            L"Nelson:statistics:rPositiveNumericArray", L"R must be a positive numeric array."));
    ArrayOf p = asDoubleArray(args[2],
        resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
            L"P must be a numeric array in the range [0,1]."));
    Dimensions dims = commonDimensions(first, r, p);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double x = valueAt(first, k);
        double rv = valueAt(r, k);
        double pv = valueAt(p, k);
        double value = mode == 0
            ? nbinPdfValue(x, rv, pv)
            : (mode == 1 ? nbinCdfValue(x, rv, pv, upper) : nbinInvValue(x, rv, pv));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
NegativeBinomialPdf(const ArrayOfVector& args)
{
    return ternaryNBinFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
NegativeBinomialCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"negative binomial"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 3) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 3) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return ternaryNBinFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
NegativeBinomialInv(const ArrayOfVector& args)
{
    return ternaryNBinFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"Y"),
        2, false);
}
//=============================================================================
ArrayOf
NegativeBinomialRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0] << args[1];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf rparam = asDoubleArray(args[0],
        resolveMessageTemplate(
            L"Nelson:statistics:rPositiveNumericArray", L"R must be a positive numeric array."));
    ArrayOf p = asDoubleArray(args[1],
        resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
            L"P must be a numeric array in the range [0,1]."));
    Dimensions dims
        = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(rparam, p, p);
    if (args.size() > 2) {
        validateRequestedDimensions(rparam, p, dims);
    }
    if (dims.isEmpty(false)) {
        ArrayOf result = ArrayOf::empty(dims);
        result.promoteType(outputType);
        return result;
    }
    ArrayOf r = Rand(dims, outputType);
    void* ptr = r.readWriteDataPointer();
    indexType count = dims.elementCount();
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double value
            = nbinInvValue(getFloatingValue(ptr, outputType, k), valueAt(rparam, k), valueAt(p, k));
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
NegativeBinomialStatResult
NegativeBinomialStat(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf r = asDoubleArray(args[0],
        resolveMessageTemplate(
            L"Nelson:statistics:rPositiveNumericArray", L"R must be a positive numeric array."));
    ArrayOf p = asDoubleArray(args[1],
        resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
            L"P must be a numeric array in the range [0,1]."));
    Dimensions dims = commonDimensions(r, p, p);
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double rv = valueAt(r, k);
        double pv = valueAt(p, k);
        double mean = std::numeric_limits<double>::quiet_NaN();
        double variance = mean;
        if (validParameters(rv, pv)) {
            if (pv == 0) {
                mean = std::numeric_limits<double>::infinity();
                variance = mean;
            } else {
                mean = rv * (1.0 - pv) / pv;
                variance = mean / pv;
            }
        }
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    NegativeBinomialStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
