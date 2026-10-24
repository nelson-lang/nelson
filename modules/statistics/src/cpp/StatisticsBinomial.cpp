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
validParameters(double n, double p)
{
    return isNonnegativeInteger(n) && std::isfinite(p) && p >= 0 && p <= 1;
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
binomialPdfValue(double x, double n, double p)
{
    if (std::isnan(x) || !validParameters(n, p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!isNonnegativeInteger(x) || x > n) {
        return 0;
    }
    if (p == 0) {
        return x == 0 ? 1 : 0;
    }
    if (p == 1) {
        return x == n ? 1 : 0;
    }
    double logc = std::lgamma(n + 1.0) - std::lgamma(x + 1.0) - std::lgamma(n - x + 1.0);
    return std::exp(logc + x * std::log(p) + (n - x) * std::log1p(-p));
}
//=============================================================================
static double
binomialCdfValue(double x, double n, double p)
{
    if (std::isnan(x) || !validParameters(n, p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double k = std::floor(x);
    if (k < 0) {
        return 0;
    }
    if (k >= n) {
        return 1;
    }
    if (p == 0) {
        return 1;
    }
    if (p == 1) {
        return 0;
    }
    return regularizedBeta(1.0 - p, n - k, k + 1.0);
}
//=============================================================================
static double
binomialInvValue(double y, double n, double p)
{
    if (std::isnan(y) || y < 0 || y > 1 || !validParameters(n, p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (y == 0 || p == 0 || n == 0) {
        return 0;
    }
    if (y == 1 || p == 1) {
        return n;
    }
    double lo = 0;
    double hi = n;
    while (lo < hi) {
        double mid = std::floor(0.5 * (lo + hi));
        if (binomialCdfValue(mid, n, p) >= y) {
            hi = mid;
        } else {
            lo = mid + 1;
        }
    }
    return lo;
}
//=============================================================================
static ArrayOf
ternaryBinomialFunction(
    const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf n = asDoubleArray(args[1],
        resolveMessageTemplate(L"Nelson:statistics:nNonnegativeIntegerNumeric",
            L"N must be a nonnegative integer numeric array."));
    ArrayOf p = asDoubleArray(args[2],
        resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
            L"P must be a numeric array in the range [0,1]."));
    Dimensions dims = commonDimensions(first, n, p);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double x = valueAt(first, k);
        double nv = valueAt(n, k);
        double pv = valueAt(p, k);
        double value;
        if (mode == 0) {
            value = binomialPdfValue(x, nv, pv);
        } else if (mode == 1) {
            value = binomialCdfValue(x, nv, pv);
            value = upper && !std::isnan(value) ? std::max(0.0, 1.0 - value) : value;
        } else {
            value = binomialInvValue(x, nv, pv);
        }
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
BinomialPdf(const ArrayOfVector& args)
{
    return ternaryBinomialFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
BinomialCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"binomial"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 3) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 3) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return ternaryBinomialFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
BinomialInv(const ArrayOfVector& args)
{
    return ternaryBinomialFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"Y"),
        2, false);
}
//=============================================================================
ArrayOf
BinomialRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0] << args[1];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf n = asDoubleArray(args[0],
        resolveMessageTemplate(L"Nelson:statistics:nNonnegativeIntegerNumeric",
            L"N must be a nonnegative integer numeric array."));
    ArrayOf p = asDoubleArray(args[1],
        resolveMessageTemplate(L"Nelson:statistics:pNumericArrayRange",
            L"P must be a numeric array in the range [0,1]."));
    Dimensions dims = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(n, p, p);
    if (args.size() > 2) {
        validateRequestedDimensions(n, p, dims);
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
            = binomialInvValue(getFloatingValue(ptr, outputType, k), valueAt(n, k), valueAt(p, k));
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
