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
betaPdfValue(double x, double a, double b)
{
    if (std::isnan(x) || std::isnan(a) || std::isnan(b) || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0 || x > 1) {
        return 0;
    }
    if (x == 0) {
        if (a < 1) {
            return std::numeric_limits<double>::infinity();
        }
        return a == 1 ? b : 0;
    }
    if (x == 1) {
        if (b < 1) {
            return std::numeric_limits<double>::infinity();
        }
        return b == 1 ? a : 0;
    }
    return std::exp((a - 1.0) * std::log(x) + (b - 1.0) * std::log1p(-x) + std::lgamma(a + b)
        - std::lgamma(a) - std::lgamma(b));
}
//=============================================================================
static double
betaCdfValue(double x, double a, double b)
{
    if (std::isnan(x) || std::isnan(a) || std::isnan(b) || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x <= 0) {
        return 0;
    }
    if (x >= 1) {
        return 1;
    }
    return regularizedBeta(x, a, b);
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
static ArrayOf
ternaryBetaFunction(
    const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf a = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"A"));
    ArrayOf b = asDoubleArray(args[2],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    Dimensions dims = commonDimensions(first, a, b);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double x = valueAt(first, k);
        double av = valueAt(a, k);
        double bv = valueAt(b, k);
        double value;
        if (mode == 0) {
            value = betaPdfValue(x, av, bv);
        } else if (mode == 1) {
            value = betaCdfValue(x, av, bv);
            value = upper && !std::isnan(value) ? std::max(0.0, 1.0 - value) : value;
        } else {
            value = betaInvValue(x, av, bv);
        }
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
BetaPdf(const ArrayOfVector& args)
{
    return ternaryBetaFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
BetaCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"beta"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 3) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 3) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return ternaryBetaFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
BetaInv(const ArrayOfVector& args)
{
    return ternaryBetaFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
ArrayOf
BetaRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0] << args[1];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf a = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"A"));
    ArrayOf b = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
    Dimensions dims = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(a, b, b);
    if (args.size() > 2) {
        validateRequestedDimensions(a, b, dims);
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
            = betaInvValue(getFloatingValue(ptr, outputType, k), valueAt(a, k), valueAt(b, k));
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
