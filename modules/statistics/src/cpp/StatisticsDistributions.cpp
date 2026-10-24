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
#include "RandNormal.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr double INV_SQRT2 = 0.70710678118654752440084436210484903928;
static constexpr double SQRT2 = 1.41421356237309504880168872420969807857;
static constexpr double SQRT2PI = 2.50662827463100050241576528481104525301;
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
static NelsonType
floatingOutputType(const std::vector<ArrayOf>& values)
{
    for (const auto& value : values) {
        if (value.isSingleClass()) {
            return NLS_SINGLE;
        }
    }
    return NLS_DOUBLE;
}
//=============================================================================
static NelsonType
floatingOutputType(const ArrayOfVector& values)
{
    for (const auto& value : values) {
        if (value.isSingleClass()) {
            return NLS_SINGLE;
        }
    }
    return NLS_DOUBLE;
}
//=============================================================================
static void
setFloatingValue(void* ptr, NelsonType outputType, indexType index, double value)
{
    if (outputType == NLS_SINGLE) {
        static_cast<single*>(ptr)[index] = static_cast<single>(value);
    } else {
        static_cast<double*>(ptr)[index] = value;
    }
}
//=============================================================================
static double
getFloatingValue(const void* ptr, NelsonType outputType, indexType index)
{
    if (outputType == NLS_SINGLE) {
        return static_cast<const single*>(ptr)[index];
    }
    return static_cast<const double*>(ptr)[index];
}
//=============================================================================
static Dimensions
commonDimensions(const std::vector<ArrayOf>& values)
{
    Dimensions dims(1, 1);
    bool haveArray = false;
    for (const auto& value : values) {
        if (!value.isScalar()) {
            if (!haveArray) {
                dims = value.dimensions();
                haveArray = true;
            } else if (!dims.equals(value.dimensions())) {
                Error("Nelson:dimagree:sizeMismatchInputArguments",
                    "Size mismatch on input arguments");
            }
        }
    }
    return dims;
}
//=============================================================================
static double
valueAt(const ArrayOf& value, indexType k)
{
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return ptr[value.isScalar() ? 0 : k];
}
//=============================================================================
static double
normalPdf(double z)
{
    return std::exp(-0.5 * z * z) / SQRT2PI;
}
//=============================================================================
static double
normalCdf01(double z)
{
    return 0.5 * std::erfc(-z * INV_SQRT2);
}
//=============================================================================
static double
normalInv01(double p)
{
    if (std::isnan(p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p < 0.0 || p > 1.0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 0.0) {
        return -std::numeric_limits<double>::infinity();
    }
    if (p == 1.0) {
        return std::numeric_limits<double>::infinity();
    }
    const double a[] = { -3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02,
        1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00 };
    const double b[] = { -5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02,
        6.680131188771972e+01, -1.328068155288572e+01 };
    const double c[] = { -7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00,
        -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00 };
    const double d[] = { 7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00,
        3.754408661907416e+00 };
    const double plow = 0.02425;
    const double phigh = 1.0 - plow;
    double q;
    double x;
    if (p < plow) {
        q = std::sqrt(-2.0 * std::log(p));
        x = (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5])
            / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1.0);
    } else if (p <= phigh) {
        q = p - 0.5;
        double r = q * q;
        x = (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q
            / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1.0);
    } else {
        q = std::sqrt(-2.0 * std::log(1.0 - p));
        x = -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5])
            / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1.0);
    }
    for (int k = 0; k < 2; ++k) {
        double err = normalCdf01(x) - p;
        double pdf = normalPdf(x);
        if (pdf == 0) {
            break;
        }
        x -= err / pdf;
    }
    return x;
}
//=============================================================================
double
NormalCdf01(double z)
{
    return normalCdf01(z);
}
//=============================================================================
double
NormalInv01(double p)
{
    return normalInv01(p);
}
//=============================================================================
static double
normalValue(double x, double mu, double sigma)
{
    if (sigma <= 0 || std::isnan(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return normalCdf01((x - mu) / sigma);
}
//=============================================================================
static double
normalInverseValue(double p, double mu, double sigma)
{
    if (sigma <= 0 || std::isnan(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return mu + sigma * normalInv01(p);
}
//=============================================================================
static double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleArray(value, message);
    return copy.asDoubleScalar(false, false);
}
//=============================================================================
static bool
isEmptyDefault(const ArrayOf& value)
{
    return value.isEmpty(false);
}
//=============================================================================
static double
studentBetaFraction(double a, double b, double x)
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
        return bt * studentBetaFraction(a, b, x) / a;
    }
    return 1.0 - bt * studentBetaFraction(b, a, 1.0 - x) / b;
}
//=============================================================================
double
StudentTCdf(double t, double nu)
{
    if (std::isnan(t) || std::isnan(nu) || nu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (std::isinf(t)) {
        return t < 0 ? 0.0 : 1.0;
    }
    double x = nu / (nu + t * t);
    double ib = regularizedBeta(x, 0.5 * nu, 0.5);
    if (t >= 0) {
        return 1.0 - 0.5 * ib;
    }
    return 0.5 * ib;
}
//=============================================================================
double
StudentTInv(double p, double nu)
{
    if (std::isnan(p) || std::isnan(nu) || p < 0 || p > 1 || nu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 0) {
        return -std::numeric_limits<double>::infinity();
    }
    if (p == 1) {
        return std::numeric_limits<double>::infinity();
    }
    if (p == 0.5) {
        return 0.0;
    }
    double lo = -1.0;
    double hi = 1.0;
    while (StudentTCdf(lo, nu) > p) {
        lo *= 2.0;
    }
    while (StudentTCdf(hi, nu) < p) {
        hi *= 2.0;
    }
    for (int k = 0; k < 100; ++k) {
        double mid = 0.5 * (lo + hi);
        if (StudentTCdf(mid, nu) < p) {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    return 0.5 * (lo + hi);
}
//=============================================================================
static double
gammaRegularizedLower(double x, double a)
{
    if (std::isnan(x) || std::isnan(a) || x < 0 || a <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x == 0) {
        return 0;
    }
    const int maxIterations = 200;
    const double eps = 3.0e-14;
    double ap = a;
    double del = 1.0 / a;
    double sum = del;
    for (int n = 1; n <= maxIterations; ++n) {
        ap += 1.0;
        del *= x / ap;
        sum += del;
        if (std::fabs(del) < std::fabs(sum) * eps) {
            break;
        }
    }
    return sum * std::exp(-x + a * std::log(x) - std::lgamma(a));
}
//=============================================================================
static double
gammaRegularizedUpperFraction(double x, double a)
{
    const int maxIterations = 200;
    const double eps = 3.0e-14;
    const double fpmin = std::numeric_limits<double>::min() / eps;
    double b = x + 1.0 - a;
    double c = 1.0 / fpmin;
    double d = 1.0 / std::max(std::fabs(b), fpmin);
    double h = d;
    for (int i = 1; i <= maxIterations; ++i) {
        double an = -static_cast<double>(i) * (static_cast<double>(i) - a);
        b += 2.0;
        d = an * d + b;
        if (std::fabs(d) < fpmin) {
            d = fpmin;
        }
        c = b + an / c;
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
    return std::exp(-x + a * std::log(x) - std::lgamma(a)) * h;
}
//=============================================================================
static double
gammaRegularizedUpper(double x, double a)
{
    if (std::isnan(x) || std::isnan(a) || x < 0 || a <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x == 0) {
        return 1;
    }
    if (x < a + 1.0) {
        return 1.0 - gammaRegularizedLower(x, a);
    }
    return gammaRegularizedUpperFraction(x, a);
}
//=============================================================================
double
ChiSquareCdf(double x, double nu)
{
    if (std::isnan(x) || std::isnan(nu) || nu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0) {
        return 0;
    }
    if (std::isinf(x)) {
        return 1;
    }
    return gammaRegularizedLower(0.5 * x, 0.5 * nu);
}
//=============================================================================
double
ChiSquareUpper(double x, double nu)
{
    if (std::isnan(x) || std::isnan(nu) || nu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 0) {
        return 1;
    }
    if (std::isinf(x)) {
        return 0;
    }
    return gammaRegularizedUpper(0.5 * x, 0.5 * nu);
}
//=============================================================================
double
ChiSquareInv(double p, double nu)
{
    if (std::isnan(p) || std::isnan(nu) || p < 0 || p > 1 || nu <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 0) {
        return 0;
    }
    if (p == 1) {
        return std::numeric_limits<double>::infinity();
    }
    double lo = 0;
    double hi = std::max(1.0, nu);
    while (ChiSquareCdf(hi, nu) < p && hi < std::numeric_limits<double>::max() / 2.0) {
        hi *= 2.0;
    }
    for (int k = 0; k < 120; ++k) {
        double mid = 0.5 * (lo + hi);
        if (ChiSquareCdf(mid, nu) < p) {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    return 0.5 * (lo + hi);
}
//=============================================================================
NormalBounds
NormalCdf(const ArrayOfVector& args)
{
    bool upper = false;
    ArrayOfVector numericArgs = args;
    if (!numericArgs.empty() && isTextScalar(numericArgs.back())) {
        std::wstring flag = matchValue(numericArgs.back(), { L"upper" },
            formatI18nMessage(resolveMessageTemplate(L"Nelson:statistics:invalidDistributionOption",
                                  L"Invalid %s distribution option."),
                L"normal"));
        upper = flag == L"upper";
        numericArgs.pop_back();
    }
    NelsonType outputType = floatingOutputType(numericArgs);
    ArrayOf x = asDoubleArray(numericArgs[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    ArrayOf mu = numericArgs.size() > 1 && !isEmptyDefault(numericArgs[1])
        ? asDoubleArray(numericArgs[1],
              formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"MU"))
        : ArrayOf::doubleScalar(0);
    ArrayOf sigma = numericArgs.size() > 2 && !isEmptyDefault(numericArgs[2])
        ? asDoubleArray(numericArgs[2],
              formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"SIGMA"))
        : ArrayOf::doubleScalar(1);
    Dimensions dims = commonDimensions({ x, mu, sigma });
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double z = (valueAt(x, k) - valueAt(mu, k)) / valueAt(sigma, k);
        double value = valueAt(sigma, k) <= 0 || std::isnan(valueAt(sigma, k))
            ? std::numeric_limits<double>::quiet_NaN()
            : (upper ? 0.5 * std::erfc(z * INV_SQRT2) : normalCdf01(z));
        setFloatingValue(out, outputType, k, value);
    }
    NormalBounds result;
    result.value = ArrayOf(outputType, dims, out);
    if (numericArgs.size() > 3) {
        ArrayOf pCov = asDoubleArray(numericArgs[3], _W("PCOV must be a 2-by-2 numeric matrix."));
        if (pCov.rows() != 2 || pCov.columns() != 2) {
            Error("Nelson:statistics:invalidPcov", "PCOV must be a 2-by-2 numeric matrix.");
        }
        double alpha = numericArgs.size() > 4
            ? scalarDouble(numericArgs[4],
                  formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                      L"ALPHA"))
            : 0.05;
        if (!(alpha > 0 && alpha < 1)) {
            Error("Nelson:statistics:alphaOutOfRange", "%s must be a scalar in the range (0,1).",
                "ALPHA");
        }
        double zcrit = normalInv01(1.0 - alpha / 2.0);
        const double* cov = static_cast<const double*>(pCov.dataPointer());
        void* lo = ArrayOf::allocateArrayOf(outputType, count);
        void* hi = ArrayOf::allocateArrayOf(outputType, count);
        for (indexType k = 0; k < count; ++k) {
            double s = valueAt(sigma, k);
            double z = (valueAt(x, k) - valueAt(mu, k)) / s;
            double g0 = -1.0 / s;
            double g1 = -z / s;
            double varz = g0 * g0 * cov[0] + 2.0 * g0 * g1 * cov[1] + g1 * g1 * cov[3];
            double dz = zcrit * std::sqrt(std::max(0.0, varz));
            setFloatingValue(lo, outputType, k, normalCdf01(z - dz));
            setFloatingValue(hi, outputType, k, normalCdf01(z + dz));
        }
        result.lower = ArrayOf(outputType, dims, lo);
        result.upper = ArrayOf(outputType, dims, hi);
    }
    return result;
}
//=============================================================================
NormalBounds
NormalInv(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf p = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"));
    ArrayOf mu = args.size() > 1 && !isEmptyDefault(args[1])
        ? asDoubleArray(args[1],
              formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"MU"))
        : ArrayOf::doubleScalar(0);
    ArrayOf sigma = args.size() > 2 && !isEmptyDefault(args[2])
        ? asDoubleArray(args[2],
              formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"SIGMA"))
        : ArrayOf::doubleScalar(1);
    Dimensions dims = commonDimensions({ p, mu, sigma });
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double value = normalInverseValue(valueAt(p, k), valueAt(mu, k), valueAt(sigma, k));
        setFloatingValue(out, outputType, k, value);
    }
    NormalBounds result;
    result.value = ArrayOf(outputType, dims, out);
    if (args.size() > 3) {
        ArrayOf pCov = asDoubleArray(args[3], _W("PCOV must be a 2-by-2 numeric matrix."));
        if (pCov.rows() != 2 || pCov.columns() != 2) {
            Error("Nelson:statistics:invalidPcov", "PCOV must be a 2-by-2 numeric matrix.");
        }
        double alpha = args.size() > 4
            ? scalarDouble(args[4],
                  formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                      L"ALPHA"))
            : 0.05;
        if (!(alpha > 0 && alpha < 1)) {
            Error("Nelson:statistics:alphaOutOfRange", "%s must be a scalar in the range (0,1).",
                "ALPHA");
        }
        double zcrit = normalInv01(1.0 - alpha / 2.0);
        const double* cov = static_cast<const double*>(pCov.dataPointer());
        void* lo = ArrayOf::allocateArrayOf(outputType, count);
        void* hi = ArrayOf::allocateArrayOf(outputType, count);
        for (indexType k = 0; k < count; ++k) {
            double z = normalInv01(valueAt(p, k));
            double g0 = 1.0;
            double g1 = z;
            double varx = g0 * g0 * cov[0] + 2.0 * g0 * g1 * cov[1] + g1 * g1 * cov[3];
            double dx = zcrit * std::sqrt(std::max(0.0, varx));
            double value = getFloatingValue(out, outputType, k);
            setFloatingValue(lo, outputType, k, value - dx);
            setFloatingValue(hi, outputType, k, value + dx);
        }
        result.lower = ArrayOf(outputType, dims, lo);
        result.upper = ArrayOf(outputType, dims, hi);
    }
    return result;
}
//=============================================================================
static Dimensions
dimensionsFromSizeArgs(const ArrayOfVector& args, sizeType first)
{
    auto sizeValue = [](double value) {
        if (!std::isfinite(value) || value < 0 || std::floor(value) != value) {
            Error("Nelson:statistics:sizeNotNonnegative",
                "%s must contain nonnegative integer values.", "Size");
        }
        return static_cast<indexType>(value);
    };
    if (args.size() == first) {
        return Dimensions(1, 1);
    }
    if (args.size() == first + 1 && args[first].isRowVector()) {
        ArrayOf dimsArg = asDoubleArray(args[first],
            formatI18nMessage(Nelson::resolveMessageTemplate(
                                  L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                L"Size"));
        const double* ptr = static_cast<const double*>(dimsArg.dataPointer());
        Dimensions dims;
        for (indexType k = 0; k < dimsArg.elementCount(); ++k) {
            dims[k] = sizeValue(ptr[k]);
        }
        if (dims.length() == 1) {
            dims[1] = dims[0];
        }
        dims.simplify();
        return dims;
    }
    Dimensions dims;
    for (sizeType k = first; k < args.size(); ++k) {
        double d = scalarDouble(args[k],
            formatI18nMessage(Nelson::resolveMessageTemplate(
                                  L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                L"Size"));
        dims[k - first] = sizeValue(d);
    }
    if (dims.length() == 1) {
        dims[1] = dims[0];
    }
    dims.simplify();
    return dims;
}
//=============================================================================
ArrayOf
NormalRnd(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType({ args[0], args[1] });
    ArrayOf mu = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
    ArrayOf sigma = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"SIGMA"));
    Dimensions dims
        = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions({ mu, sigma });
    if (args.size() > 2) {
        for (const auto& value : { mu, sigma }) {
            if (!value.isScalar() && !value.dimensions().equals(dims)) {
                Error("Nelson:dimagree:sizeMismatchInputArguments",
                    "Size mismatch on input arguments");
            }
        }
    }
    indexType count = dims.elementCount();
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(outputType);
        return res;
    }
    ArrayOf z = RandNormal(dims, outputType);
    void* ptr = z.readWriteDataPointer();
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double s = valueAt(sigma, k);
        double value = valueAt(mu, k) + s * getFloatingValue(ptr, outputType, k);
        if (s < 0 || std::isnan(s)) {
            value = std::numeric_limits<double>::quiet_NaN();
        }
        setFloatingValue(ptr, outputType, k, value);
    }
    return z;
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
parseTTest(const ArrayOfVector& args, ArrayOf& x, ArrayOf& y, bool& pairedSample, double& nullMean,
    double& alpha, std::wstring& tail, indexType& dim)
{
    x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    pairedSample = false;
    nullMean = 0;
    sizeType pos = 1;
    if (args.size() > 1 && !isTextScalar(args[1])) {
        if (!args[1].isEmpty()) {
            y = asDoubleArray(args[1],
                formatI18nMessage(resolveMessageTemplate(L"Nelson:statistics:realNumeric",
                                      L"%s must be real numeric."),
                    L"Y"));
            if (y.isScalar()) {
                nullMean = valueAt(y, 0);
            } else {
                pairedSample = true;
            }
        }
        pos = 2;
    }
    alpha = 0.05;
    tail = L"both";
    dim = firstNonSingleton(x.dimensions());
    if ((args.size() - pos) % 2 != 0) {
        Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
    }
    while (pos < args.size()) {
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
        pos += 2;
    }
}
//=============================================================================
TTestResult
TTest(const ArrayOfVector& args)
{
    ArrayOf x;
    ArrayOf y;
    bool pairedSample;
    double nullMean;
    double alpha;
    std::wstring tail;
    indexType dim;
    parseTTest(args, x, y, pairedSample, nullMean, alpha, tail, dim);
    if (pairedSample && !x.dimensions().equals(y.dimensions())) {
        Error("Nelson:statistics:xySizeMismatch", "X and Y must have %s.", "the same size");
    }
    Dimensions dims = x.dimensions();
    indexType planes;
    indexType planeSize;
    indexType lineSize;
    planeShape(dims, dim, planes, planeSize, lineSize);
    Dimensions outDims = dims;
    outDims.setDimensionLength(dim, 1);
    outDims.simplify();
    indexType outCount = outDims.elementCount();
    logical* h = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, outCount));
    double* p = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* tstat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* df = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    double* sd = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    Dimensions ciDims(2, outCount);
    double* ci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * outCount));
    const double* xp = static_cast<const double*>(x.dataPointer());
    const double* yp = pairedSample ? static_cast<const double*>(y.dataPointer()) : nullptr;
#if WITH_OPENMP
#pragma omp parallel for if (outCount > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType out = 0; out < (ompIndexType)outCount; ++out) {
        indexType plane = out / planeSize;
        indexType j = out % planeSize;
        double sum = 0;
        double sumsq = 0;
        double n = 0;
        for (indexType k = 0; k < lineSize; ++k) {
            indexType idx = plane * planeSize * lineSize + j + k * planeSize;
            double v = pairedSample ? xp[idx] - yp[idx] : xp[idx] - nullMean;
            if (!std::isnan(v)) {
                sum += v;
                sumsq += v * v;
                n += 1;
            }
        }
        double mean = sum / n;
        double variance = (sumsq - sum * sum / n) / (n - 1);
        double s = std::sqrt(std::max(0.0, variance));
        double se = s / std::sqrt(n);
        double t = mean / se;
        double nu = n - 1;
        double pv;
        double lo;
        double hi;
        if (n < 2 || std::isnan(t)) {
            t = std::numeric_limits<double>::quiet_NaN();
            pv = std::numeric_limits<double>::quiet_NaN();
            lo = std::numeric_limits<double>::quiet_NaN();
            hi = std::numeric_limits<double>::quiet_NaN();
        } else if (se == 0) {
            if (mean == 0) {
                t = std::numeric_limits<double>::quiet_NaN();
                pv = std::numeric_limits<double>::quiet_NaN();
                lo = std::numeric_limits<double>::quiet_NaN();
                hi = std::numeric_limits<double>::quiet_NaN();
            } else {
                t = mean > 0 ? std::numeric_limits<double>::infinity()
                             : -std::numeric_limits<double>::infinity();
                if (tail == L"right") {
                    pv = mean > 0 ? 0.0 : 1.0;
                    lo = mean;
                    hi = std::numeric_limits<double>::infinity();
                } else if (tail == L"left") {
                    pv = mean < 0 ? 0.0 : 1.0;
                    lo = -std::numeric_limits<double>::infinity();
                    hi = mean;
                } else {
                    pv = 0.0;
                    lo = mean;
                    hi = mean;
                }
            }
        } else if (tail == L"right") {
            pv = 1.0 - StudentTCdf(t, nu);
            double crit = StudentTInv(1.0 - alpha, nu);
            lo = mean - crit * se;
            hi = std::numeric_limits<double>::infinity();
        } else if (tail == L"left") {
            pv = StudentTCdf(t, nu);
            double crit = StudentTInv(1.0 - alpha, nu);
            lo = -std::numeric_limits<double>::infinity();
            hi = mean + crit * se;
        } else {
            pv = 2.0 * std::min(StudentTCdf(t, nu), 1.0 - StudentTCdf(t, nu));
            double crit = StudentTInv(1.0 - alpha / 2.0, nu);
            lo = mean - crit * se;
            hi = mean + crit * se;
        }
        h[out] = (!std::isnan(pv) && pv < alpha) ? 1 : 0;
        p[out] = pv;
        tstat[out] = t;
        df[out] = nu;
        sd[out] = s;
        ci[2 * out] = lo;
        ci[2 * out + 1] = hi;
    }
    stringVector names { "tstat", "df", "sd" };
    ArrayOfVector values;
    values << ArrayOf(NLS_DOUBLE, outDims, tstat);
    values << ArrayOf(NLS_DOUBLE, outDims, df);
    values << ArrayOf(NLS_DOUBLE, outDims, sd);
    TTestResult result;
    result.h = ArrayOf(NLS_LOGICAL, outDims, h);
    result.p = ArrayOf(NLS_DOUBLE, outDims, p);
    result.ci = ArrayOf(NLS_DOUBLE, ciDims, ci);
    result.stats = ArrayOf::structArray(names, values);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
