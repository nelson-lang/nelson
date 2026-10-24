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
chi2PdfValue(double x, double nu)
{
    if (std::isnan(x) || std::isnan(nu) || nu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0 || std::isinf(x)) {
        return 0;
    }
    if (x == 0) {
        if (nu < 2) {
            return std::numeric_limits<double>::infinity();
        }
        if (nu == 2) {
            return 0.5;
        }
        return 0;
    }
    double halfNu = 0.5 * nu;
    return std::exp(
        (halfNu - 1.0) * std::log(x) - 0.5 * x - halfNu * std::log(2.0) - std::lgamma(halfNu));
}
//=============================================================================
static double
fPdfFinite(double x, double df1, double df2)
{
    if (x < 0) {
        return 0;
    }
    if (std::isinf(x)) {
        return 0;
    }
    if (x == 0) {
        if (df1 < 2) {
            return std::numeric_limits<double>::infinity();
        }
        if (df1 == 2) {
            return 1;
        }
        return 0;
    }
    double halfDf1 = 0.5 * df1;
    double halfDf2 = 0.5 * df2;
    double logBeta = std::lgamma(halfDf1) + std::lgamma(halfDf2) - std::lgamma(halfDf1 + halfDf2);
    return std::exp(halfDf1 * std::log(df1) + halfDf2 * std::log(df2)
        + (halfDf1 - 1.0) * std::log(x) - (halfDf1 + halfDf2) * std::log(df2 + df1 * x) - logBeta);
}
//=============================================================================
static double
fPdfValue(double x, double df1, double df2)
{
    if (std::isnan(x) || std::isnan(df1) || std::isnan(df2) || df1 <= 0 || df2 <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (std::isinf(df1) && std::isinf(df2)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (std::isinf(df2)) {
        return df1 * chi2PdfValue(df1 * x, df1);
    }
    if (std::isinf(df1)) {
        if (x <= 0 || std::isinf(x)) {
            return 0;
        }
        return (df2 / (x * x)) * chi2PdfValue(df2 / x, df2);
    }
    return fPdfFinite(x, df1, df2);
}
//=============================================================================
static double
fCdfValue(double x, double df1, double df2)
{
    if (std::isnan(x) || std::isnan(df1) || std::isnan(df2) || df1 <= 0 || df2 <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x <= 0) {
        return 0;
    }
    if (std::isinf(x)) {
        return 1;
    }
    if (std::isinf(df1) && std::isinf(df2)) {
        return x < 1 ? 0 : 1;
    }
    if (std::isinf(df2)) {
        return ChiSquareCdf(df1 * x, df1);
    }
    if (std::isinf(df1)) {
        return ChiSquareUpper(df2 / x, df2);
    }
    return FCdf(x, df1, df2);
}
//=============================================================================
static double
fInvValue(double p, double df1, double df2)
{
    if (std::isnan(p) || std::isnan(df1) || std::isnan(df2) || p < 0 || p > 1 || df1 <= 0
        || df2 <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (std::isinf(df1) && std::isinf(df2)) {
        return 1;
    }
    if (std::isinf(df2)) {
        return ChiSquareInv(p, df1) / df1;
    }
    if (std::isinf(df1)) {
        return df2 / ChiSquareInv(1.0 - p, df2);
    }
    return FInv(p, df1, df2);
}
//=============================================================================
static ArrayOf
ternaryFFunction(const ArrayOfVector& args, const std::wstring& firstMessage, int mode, bool upper)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf df1 = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V1"));
    ArrayOf df2 = asDoubleArray(args[2],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V2"));
    Dimensions dims = commonDimensions(first, df1, df2);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double a = valueAt(first, k);
        double v1 = valueAt(df1, k);
        double v2 = valueAt(df2, k);
        double value;
        if (mode == 0) {
            value = fPdfValue(a, v1, v2);
        } else if (mode == 1) {
            value = fCdfValue(a, v1, v2);
            value = upper && !std::isnan(value) ? std::max(0.0, 1.0 - value) : value;
        } else {
            value = fInvValue(a, v1, v2);
        }
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
double
FCdf(double x, double df1, double df2)
{
    if (std::isnan(x) || std::isnan(df1) || std::isnan(df2) || df1 <= 0 || df2 <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x <= 0) {
        return 0;
    }
    if (std::isinf(x)) {
        return 1;
    }
    double ratio = (df1 * x) / (df1 * x + df2);
    return regularizedBeta(ratio, 0.5 * df1, 0.5 * df2);
}
//=============================================================================
double
FUpper(double x, double df1, double df2)
{
    double p = FCdf(x, df1, df2);
    return std::isnan(p) ? p : std::max(0.0, 1.0 - p);
}
//=============================================================================
double
FInv(double p, double df1, double df2)
{
    if (std::isnan(p) || std::isnan(df1) || std::isnan(df2) || p < 0 || p > 1 || df1 <= 0
        || df2 <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 0) {
        return 0;
    }
    if (p == 1) {
        return std::numeric_limits<double>::infinity();
    }
    double lo = 0;
    double hi = 1;
    while (FCdf(hi, df1, df2) < p && hi < std::numeric_limits<double>::max() / 2.0) {
        hi *= 2.0;
    }
    for (int k = 0; k < 120; ++k) {
        double mid = 0.5 * (lo + hi);
        if (FCdf(mid, df1, df2) < p) {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    return 0.5 * (lo + hi);
}
//=============================================================================
ArrayOf
FPdf(const ArrayOfVector& args)
{
    return ternaryFFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0, false);
}
//=============================================================================
ArrayOf
FCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        upper = matchValue(numericArgs.back(), { L"upper" },
                    formatI18nMessage(
                        resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                            L"Invalid %s distribution option."),
                        L"F"))
            == L"upper";
        numericArgs.pop_back();
    }
    if (numericArgs.size() < 3) {
        Error("Nelson:notEnoughInputs", "Not enough input arguments.");
    }
    if (numericArgs.size() > 3) {
        Error("Nelson:tooManyInputs", "Too many input arguments.");
    }
    return ternaryFFunction(numericArgs,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1, upper);
}
//=============================================================================
ArrayOf
FInv(const ArrayOfVector& args)
{
    return ternaryFFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
