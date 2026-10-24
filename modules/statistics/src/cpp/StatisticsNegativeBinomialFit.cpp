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
#include <array>
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
struct NBinColumnEstimate
{
    double r;
    double p;
    double negativeLogLikelihood;
    std::array<double, 4> covariance;
};
//=============================================================================
struct NBinFitOptions
{
    int maxIter { 160 };
    double tolX { 1e-10 };
};
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
static ArrayOf
expandedArray(
    const ArrayOf& value, const Dimensions& dims, double defaultValue, const std::wstring& message)
{
    if (value.isEmpty(false)) {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
        std::fill(ptr, ptr + dims.elementCount(), defaultValue);
        return ArrayOf(NLS_DOUBLE, dims, ptr);
    }
    ArrayOf copy = asDoubleArray(value, message);
    if (!copy.isScalar() && !copy.dimensions().equals(dims)) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
    if (copy.isScalar() && dims.elementCount() != 1) {
        double scalarValue = static_cast<const double*>(copy.dataPointer())[0];
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
        std::fill(ptr, ptr + dims.elementCount(), scalarValue);
        return ArrayOf(NLS_DOUBLE, dims, ptr);
    }
    return copy;
}
//=============================================================================
static bool
isNonnegativeInteger(double value)
{
    return std::isfinite(value) && value >= 0 && std::floor(value) == value;
}
//=============================================================================
static void
validateDataValues(const ArrayOf& x)
{
    const double* ptr = static_cast<const double*>(x.dataPointer());
    for (indexType k = 0; k < x.elementCount(); ++k) {
        double value = ptr[k];
        if (!std::isnan(value) && !isNonnegativeInteger(value)) {
            Error("Nelson:statistics:xMustBeNonnegativeInteger",
                "%s must contain nonnegative integer finite values.", "X");
        }
    }
}
//=============================================================================
static void
validateCensoring(const ArrayOf& censoring)
{
    const double* ptr = static_cast<const double*>(censoring.dataPointer());
    for (indexType k = 0; k < censoring.elementCount(); ++k) {
        double value = ptr[k];
        if (std::isnan(value) || (value != 0.0 && value != 1.0)) {
            Error("Nelson:statistics:invalidCensoring", "CENSORING must contain 0 or 1 values.");
        }
    }
}
//=============================================================================
static void
validateFrequency(const ArrayOf& frequency)
{
    const double* ptr = static_cast<const double*>(frequency.dataPointer());
    for (indexType k = 0; k < frequency.elementCount(); ++k) {
        double value = ptr[k];
        if (!std::isfinite(value) || value < 0) {
            Error("Nelson:statistics:invalidFreq", "%s must contain nonnegative finite values.",
                "FREQ");
        }
    }
}
//=============================================================================
static double
alphaValue(const ArrayOfVector& args)
{
    if (args.size() < 2 || args[1].isEmpty(false)) {
        return 0.05;
    }
    double alpha = scalarDouble(args[1],
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
scalarOptionField(const ArrayOf& options, const std::string& field, double defaultValue)
{
    if (!options.isField(field)) {
        return defaultValue;
    }
    ArrayOf value = options.field(field);
    if (value.isEmpty(false)) {
        return defaultValue;
    }
    if (!value.isNumeric() || !value.isScalar() || value.isComplex()) {
        Error("Nelson:statistics:optionsFieldsNotNumeric",
            "OPTIONS fields must be numeric scalar values.");
    }
    return value.asDoubleScalar(false, false);
}
//=============================================================================
static NBinFitOptions
fitOptionsValue(const ArrayOfVector& args)
{
    NBinFitOptions options;
    if (args.size() > 4 && !args[4].isEmpty(false) && !args[4].isStruct()) {
        Error("Nelson:statistics:invalidOptions", "%s must be a scalar structure.", "OPTIONS");
    }
    if (args.size() > 4 && !args[4].isEmpty(false)) {
        double maxIter = scalarOptionField(args[4], "MaxIter", options.maxIter);
        double tolX = scalarOptionField(args[4], "TolX", options.tolX);
        if (!std::isfinite(maxIter) || maxIter <= 0 || std::floor(maxIter) != maxIter) {
            Error("Nelson:statistics:optionsMaxIterPositiveIntegerScalar",
                "%s must be a positive integer scalar.", "OPTIONS MaxIter");
        }
        if (!std::isfinite(tolX) || tolX <= 0) {
            Error("Nelson:statistics:optionsTolXPositiveScalar", "%s must be a positive scalar.",
                "OPTIONS TolX");
        }
        options.maxIter = static_cast<int>(maxIter);
        options.tolX = tolX;
    }
    return options;
}
//=============================================================================
static void
prepareCensoringFrequency(const ArrayOfVector& args, sizeType firstOptional, const Dimensions& dims,
    ArrayOf& censoring, ArrayOf& frequency)
{
    censoring = args.size() > firstOptional
        ? expandedArray(args[firstOptional], dims, 0.0,
              formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"CENSORING"))
        : expandedArray(ArrayOf::empty(), dims, 0.0, L"");
    frequency = args.size() > firstOptional + 1
        ? expandedArray(args[firstOptional + 1], dims, 1.0,
              formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
                  L"FREQ"))
        : expandedArray(ArrayOf::empty(), dims, 1.0, L"");
    validateCensoring(censoring);
    validateFrequency(frequency);
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
        c = 1.0 + aa / c;
        if (std::fabs(d) < fpmin) {
            d = fpmin;
        }
        if (std::fabs(c) < fpmin) {
            c = fpmin;
        }
        d = 1.0 / d;
        h *= d * c;
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
        d = 1.0 + aa * d;
        c = 1.0 + aa / c;
        if (std::fabs(d) < fpmin) {
            d = fpmin;
        }
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
negativeBinomialLogSurvival(double x, double r, double p)
{
    if (p == 0) {
        return 0;
    }
    if (p == 1) {
        return -std::numeric_limits<double>::infinity();
    }
    double cdf = regularizedBeta(p, r, std::floor(x) + 1.0);
    double survival = std::max(0.0, 1.0 - cdf);
    return survival > 0 ? std::log(survival) : -std::numeric_limits<double>::infinity();
}
//=============================================================================
static double
negativeBinomialNegativeLogLikelihood(double r, double p, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    if (std::isnan(r) || std::isnan(p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!std::isfinite(r) || r <= 0 || !std::isfinite(p) || p < 0 || p > 1) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = 0;
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double weight = frequency[k];
        if (weight == 0) {
            continue;
        }
        if (censoring[k] != 0) {
            logLikelihood += weight * negativeBinomialLogSurvival(xv, r, p);
        } else if (p == 0 || (p == 1 && xv != 0)) {
            return std::numeric_limits<double>::infinity();
        } else if (p == 1) {
            continue;
        } else {
            double logc = std::lgamma(xv + r) - std::lgamma(r) - std::lgamma(xv + 1.0);
            logLikelihood += weight * (logc + r * std::log(p) + xv * std::log1p(-p));
        }
    }
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 2>
toNative(double theta0, double theta1)
{
    double r = std::exp(theta0);
    double p = 1.0 / (1.0 + std::exp(-theta1));
    return { r, p };
}
//=============================================================================
static double
transformedNLogL(const std::array<double, 2>& theta, const double* x, const double* c,
    const double* f, indexType count)
{
    std::array<double, 2> params = toNative(theta[0], theta[1]);
    return negativeBinomialNegativeLogLikelihood(params[0], params[1], x, c, f, count);
}
//=============================================================================
static NBinColumnEstimate
nanEstimate()
{
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    return { nanValue, nanValue, nanValue, { nanValue, nanValue, nanValue, nanValue } };
}
//=============================================================================
static std::array<double, 2>
initialTheta(const double* x, const double* censoring, const double* frequency, indexType rows)
{
    double weightSum = 0;
    double sum = 0;
    double sumSquares = 0;
    for (indexType k = 0; k < rows; ++k) {
        if (censoring[k] == 0 && frequency[k] > 0) {
            weightSum += frequency[k];
            sum += frequency[k] * x[k];
            sumSquares += frequency[k] * x[k] * x[k];
        }
    }
    if (weightSum <= 0 || sum <= 0) {
        return { std::numeric_limits<double>::quiet_NaN(),
            std::numeric_limits<double>::quiet_NaN() };
    }
    double mean = sum / weightSum;
    double variance = std::max(sumSquares / weightSum - mean * mean, mean * 1.05);
    double r = std::max(mean * mean / std::max(variance - mean, mean * 0.05), 1e-6);
    double p = std::min(std::max(r / (r + mean), 1e-8), 1.0 - 1e-8);
    return { std::log(r), std::log(p / (1.0 - p)) };
}
//=============================================================================
static std::array<double, 4>
negativeBinomialCovariance(
    double r, double p, const double* x, const double* c, const double* f, indexType count)
{
    double hr = std::max(1e-5 * std::abs(r), 1e-6);
    double hp = std::max(1e-5 * std::min(p, 1.0 - p), 1e-7);
    if (r - hr <= 0) {
        hr = r * 0.5;
    }
    if (p - hp <= 0) {
        hp = p * 0.5;
    }
    if (p + hp >= 1) {
        hp = (1.0 - p) * 0.5;
    }
    double f00 = negativeBinomialNegativeLogLikelihood(r, p, x, c, f, count);
    double frp = negativeBinomialNegativeLogLikelihood(r + hr, p, x, c, f, count);
    double frm = negativeBinomialNegativeLogLikelihood(r - hr, p, x, c, f, count);
    double fpp = negativeBinomialNegativeLogLikelihood(r, p + hp, x, c, f, count);
    double fpm = negativeBinomialNegativeLogLikelihood(r, p - hp, x, c, f, count);
    double fPP = negativeBinomialNegativeLogLikelihood(r + hr, p + hp, x, c, f, count);
    double fPM = negativeBinomialNegativeLogLikelihood(r + hr, p - hp, x, c, f, count);
    double fMP = negativeBinomialNegativeLogLikelihood(r - hr, p + hp, x, c, f, count);
    double fMM = negativeBinomialNegativeLogLikelihood(r - hr, p - hp, x, c, f, count);
    double info00 = (frp - 2.0 * f00 + frm) / (hr * hr);
    double info11 = (fpp - 2.0 * f00 + fpm) / (hp * hp);
    double info01 = (fPP - fPM - fMP + fMM) / (4.0 * hr * hp);
    double det = info00 * info11 - info01 * info01;
    std::array<double, 4> covariance { std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN() };
    if (std::isfinite(det) && det > 0) {
        covariance[0] = info11 / det;
        covariance[1] = -info01 / det;
        covariance[2] = covariance[1];
        covariance[3] = info00 / det;
    }
    return covariance;
}
//=============================================================================
static NBinColumnEstimate
fitNegativeBinomialColumn(const double* x, const double* censoring, const double* frequency,
    indexType rows, const NBinFitOptions& options)
{
    for (indexType k = 0; k < rows; ++k) {
        if (std::isnan(x[k])) {
            return nanEstimate();
        }
    }
    std::array<double, 2> start = initialTheta(x, censoring, frequency, rows);
    if (!std::isfinite(start[0]) || !std::isfinite(start[1])) {
        return nanEstimate();
    }
    std::array<std::array<double, 2>, 3> vertex { { start, { start[0] + 0.2, start[1] },
        { start[0], start[1] + 0.2 } } };
    std::array<double, 3> value;
    for (int k = 0; k < 3; ++k) {
        value[k] = transformedNLogL(vertex[k], x, censoring, frequency, rows);
    }
    for (int iter = 0; iter < options.maxIter; ++iter) {
        std::array<int, 3> order { 0, 1, 2 };
        std::sort(
            order.begin(), order.end(), [&value](int a, int b) { return value[a] < value[b]; });
        std::array<double, 2> centroid { 0.5 * (vertex[order[0]][0] + vertex[order[1]][0]),
            0.5 * (vertex[order[0]][1] + vertex[order[1]][1]) };
        double spread = std::max(std::abs(vertex[order[2]][0] - vertex[order[0]][0]),
            std::abs(vertex[order[2]][1] - vertex[order[0]][1]));
        if (spread <= options.tolX * (1.0 + std::abs(vertex[order[0]][0]))) {
            break;
        }
        std::array<double, 2> reflected { 2.0 * centroid[0] - vertex[order[2]][0],
            2.0 * centroid[1] - vertex[order[2]][1] };
        double reflectedValue = transformedNLogL(reflected, x, censoring, frequency, rows);
        if (reflectedValue < value[order[1]]) {
            vertex[order[2]] = reflected;
            value[order[2]] = reflectedValue;
        } else {
            std::array<double, 2> contracted { centroid[0]
                    + 0.5 * (vertex[order[2]][0] - centroid[0]),
                centroid[1] + 0.5 * (vertex[order[2]][1] - centroid[1]) };
            vertex[order[2]] = contracted;
            value[order[2]] = transformedNLogL(contracted, x, censoring, frequency, rows);
        }
    }
    int best = value[0] < value[1] ? (value[0] < value[2] ? 0 : 2) : (value[1] < value[2] ? 1 : 2);
    std::array<double, 2> params = toNative(vertex[best][0], vertex[best][1]);
    NBinColumnEstimate result;
    result.r = params[0];
    result.p = params[1];
    result.negativeLogLikelihood = value[best];
    result.covariance
        = negativeBinomialCovariance(result.r, result.p, x, censoring, frequency, rows);
    return result;
}
//=============================================================================
NegativeBinomialFitResult
NegativeBinomialFit(const ArrayOfVector& args)
{
    ArrayOf x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false) || x.dimensions().length() > 2) {
        Error("Nelson:statistics:xMustBeNonemptyVectorOrMatrix",
            "X must be a nonempty vector or matrix.");
    }
    validateDataValues(x);
    double alpha = alphaValue(args);
    NBinFitOptions options = fitOptionsValue(args);
    Dimensions dims = x.dimensions();
    bool vectorInput = x.isVector();
    indexType rows = vectorInput ? x.elementCount() : dims.rows();
    indexType cols = vectorInput ? 1 : dims.columns();
    ArrayOf data = x;
    if (vectorInput && dims.rows() == 1) {
        Dimensions reshaped(rows, 1);
        data.reshape(reshaped);
    }
    ArrayOf censoring;
    ArrayOf frequency;
    prepareCensoringFrequency(args, 2, dims, censoring, frequency);
    if (vectorInput && dims.rows() == 1) {
        Dimensions reshaped(rows, 1);
        censoring.reshape(reshaped);
        frequency.reshape(reshaped);
    }
    const double* xp = static_cast<const double*>(data.dataPointer());
    const double* cp = static_cast<const double*>(censoring.dataPointer());
    const double* fp = static_cast<const double*>(frequency.dataPointer());
    double* params = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 2));
    double* pci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 4));
    double z = NormalInv01(1.0 - alpha / 2.0);
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        indexType offset = c * rows;
        NBinColumnEstimate estimate
            = fitNegativeBinomialColumn(xp + offset, cp + offset, fp + offset, rows, options);
        params[c] = estimate.r;
        params[c + cols] = estimate.p;
        indexType base = c * 4;
        if (!std::isfinite(estimate.r) || !std::isfinite(estimate.p)) {
            pci[base] = std::numeric_limits<double>::quiet_NaN();
            pci[base + 1] = std::numeric_limits<double>::quiet_NaN();
            pci[base + 2] = std::numeric_limits<double>::quiet_NaN();
            pci[base + 3] = std::numeric_limits<double>::quiet_NaN();
            continue;
        }
        double rLogSe = estimate.covariance[0] >= 0 && estimate.r > 0
            ? std::sqrt(estimate.covariance[0]) / estimate.r
            : std::numeric_limits<double>::quiet_NaN();
        double pSe = estimate.covariance[3] >= 0 ? std::sqrt(estimate.covariance[3])
                                                 : std::numeric_limits<double>::quiet_NaN();
        pci[base] = estimate.r * std::exp(-z * rLogSe);
        pci[base + 1] = estimate.r * std::exp(z * rLogSe);
        pci[base + 2] = std::max(0.0, estimate.p - z * pSe);
        pci[base + 3] = std::min(1.0, estimate.p + z * pSe);
    }
    NegativeBinomialFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 2) : Dimensions(cols, 2), params);
    result.parameterCI = ArrayOf(NLS_DOUBLE,
        vectorInput ? Dimensions(2, 2) : Dimensions(std::vector<indexType> { 2, 2, cols }), pci);
    return result;
}
//=============================================================================
static NegativeBinomialLikeResult
makeLikeResult(double nlogL, const std::array<double, 4>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    for (indexType k = 0; k < 4; ++k) {
        covPtr[k] = covariance[k];
    }
    NegativeBinomialLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(2, 2), covPtr);
    return result;
}
//=============================================================================
NegativeBinomialLikeResult
NegativeBinomialLike(const ArrayOfVector& args)
{
    ArrayOf params = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"PARAMS"));
    if (!params.isVector() || params.elementCount() != 2) {
        Error("Nelson:statistics:invalidParams", "PARAMS must be a %s vector.", "two-element");
    }
    const double* pp = static_cast<const double*>(params.dataPointer());
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    validateDataValues(x);
    ArrayOf censoring;
    ArrayOf frequency;
    prepareCensoringFrequency(args, 2, x.dimensions(), censoring, frequency);
    std::array<double, 4> nanCov { std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN() };
    if (std::isnan(pp[0]) || std::isnan(pp[1])) {
        return makeLikeResult(std::numeric_limits<double>::quiet_NaN(), nanCov);
    }
    indexType count = x.elementCount();
    ArrayOf flatX = x;
    ArrayOf flatC = censoring;
    ArrayOf flatF = frequency;
    Dimensions flatDims(count, 1);
    flatX.reshape(flatDims);
    flatC.reshape(flatDims);
    flatF.reshape(flatDims);
    const double* xp = static_cast<const double*>(flatX.dataPointer());
    const double* cp = static_cast<const double*>(flatC.dataPointer());
    const double* fp = static_cast<const double*>(flatF.dataPointer());
    double nlogL = negativeBinomialNegativeLogLikelihood(pp[0], pp[1], xp, cp, fp, count);
    if (std::isnan(nlogL) || std::isinf(nlogL)) {
        return makeLikeResult(nlogL, nanCov);
    }
    return makeLikeResult(nlogL, negativeBinomialCovariance(pp[0], pp[1], xp, cp, fp, count));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
