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
#include <array>
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace StatisticsHelpers;
//=============================================================================
static constexpr double PI_VALUE = 3.141592653589793238462643383279502884;
//=============================================================================
struct WeibullColumnEstimate
{
    double scale;
    double shape;
    double negativeLogLikelihood;
    std::array<double, 4> covariance;
};
//=============================================================================
struct WeibullFitOptions
{
    int maxIter { 120 };
    double tolX { 1e-10 };
};
//=============================================================================
static void
validateDataValues(const ArrayOf& x)
{
    const double* ptr = static_cast<const double*>(x.dataPointer());
    for (indexType k = 0; k < x.elementCount(); ++k) {
        double value = ptr[k];
        if (!std::isnan(value) && (!std::isfinite(value) || value <= 0)) {
            Error("Nelson:statistics:xMustBePositive", "X must contain positive finite values.");
        }
    }
}
//=============================================================================
static double
normalInvApprox(double p)
{
    static constexpr double a1 = -3.969683028665376e+01;
    static constexpr double a2 = 2.209460984245205e+02;
    static constexpr double a3 = -2.759285104469687e+02;
    static constexpr double a4 = 1.383577518672690e+02;
    static constexpr double a5 = -3.066479806614716e+01;
    static constexpr double a6 = 2.506628277459239e+00;
    static constexpr double b1 = -5.447609879822406e+01;
    static constexpr double b2 = 1.615858368580409e+02;
    static constexpr double b3 = -1.556989798598866e+02;
    static constexpr double b4 = 6.680131188771972e+01;
    static constexpr double b5 = -1.328068155288572e+01;
    static constexpr double c1 = -7.784894002430293e-03;
    static constexpr double c2 = -3.223964580411365e-01;
    static constexpr double c3 = -2.400758277161838e+00;
    static constexpr double c4 = -2.549732539343734e+00;
    static constexpr double c5 = 4.374664141464968e+00;
    static constexpr double c6 = 2.938163982698783e+00;
    static constexpr double d1 = 7.784695709041462e-03;
    static constexpr double d2 = 3.224671290700398e-01;
    static constexpr double d3 = 2.445134137142996e+00;
    static constexpr double d4 = 3.754408661907416e+00;
    if (p <= 0) {
        return -std::numeric_limits<double>::infinity();
    }
    if (p >= 1) {
        return std::numeric_limits<double>::infinity();
    }
    if (p < 0.02425) {
        double q = std::sqrt(-2.0 * std::log(p));
        return (((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6)
            / ((((d1 * q + d2) * q + d3) * q + d4) * q + 1.0);
    }
    if (p > 0.97575) {
        double q = std::sqrt(-2.0 * std::log1p(-p));
        return -(((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6)
            / ((((d1 * q + d2) * q + d3) * q + d4) * q + 1.0);
    }
    double q = p - 0.5;
    double r = q * q;
    return (((((a1 * r + a2) * r + a3) * r + a4) * r + a5) * r + a6) * q
        / (((((b1 * r + b2) * r + b3) * r + b4) * r + b5) * r + 1.0);
}
//=============================================================================
static WeibullFitOptions
fitOptionsValue(const ArrayOfVector& args)
{
    FitOptions commonOptions = StatisticsHelpers::parseFitOptions(args);
    return { commonOptions.maxIter, commonOptions.tolX };
}
//=============================================================================
static double
profileLogSum(
    const double* x, const double* frequency, indexType rows, indexType column, double shape)
{
    indexType offset = column * rows;
    double maxValue = -std::numeric_limits<double>::infinity();
    for (indexType r = 0; r < rows; ++r) {
        double weight = frequency[offset + r];
        if (weight > 0) {
            maxValue = std::max(maxValue, shape * std::log(x[offset + r]));
        }
    }
    if (!std::isfinite(maxValue)) {
        return maxValue;
    }
    double sum = 0;
    for (indexType r = 0; r < rows; ++r) {
        double weight = frequency[offset + r];
        if (weight > 0) {
            sum += weight * std::exp(shape * std::log(x[offset + r]) - maxValue);
        }
    }
    return maxValue + std::log(sum);
}
//=============================================================================
static double
profileNegativeLogLikelihood(double logShape, const double* x, const double* censoring,
    const double* frequency, indexType rows, indexType column, double uncensoredWeight,
    double uncensoredLogSum)
{
    double shape = std::exp(logShape);
    double logSum = profileLogSum(x, frequency, rows, column, shape);
    if (!std::isfinite(logSum)) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = uncensoredWeight * std::log(shape)
        - uncensoredWeight * (logSum - std::log(uncensoredWeight))
        + (shape - 1.0) * uncensoredLogSum - uncensoredWeight;
    return -logLikelihood;
}
//=============================================================================
static double
weibullNegativeLogLikelihood(double scale, double shape, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    if (!std::isfinite(scale) || !std::isfinite(shape) || scale <= 0 || shape <= 0) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = 0;
    double logScale = std::log(scale);
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double weight = frequency[k];
        if (weight == 0) {
            continue;
        }
        double z = std::pow(xv / scale, shape);
        if (censoring[k] == 0) {
            logLikelihood
                += weight * (std::log(shape) - shape * logScale + (shape - 1.0) * std::log(xv) - z);
        } else {
            logLikelihood -= weight * z;
        }
    }
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 4>
weibullCovariance(double scale, double shape, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    double hScale = std::max(1e-5 * std::abs(scale), 1e-6);
    double hShape = std::max(1e-5 * std::abs(shape), 1e-6);
    if (scale - hScale <= 0) {
        hScale = scale * 0.5;
    }
    if (shape - hShape <= 0) {
        hShape = shape * 0.5;
    }
    double f00 = weibullNegativeLogLikelihood(scale, shape, x, censoring, frequency, count);
    double fScalePlus
        = weibullNegativeLogLikelihood(scale + hScale, shape, x, censoring, frequency, count);
    double fScaleMinus
        = weibullNegativeLogLikelihood(scale - hScale, shape, x, censoring, frequency, count);
    double fShapePlus
        = weibullNegativeLogLikelihood(scale, shape + hShape, x, censoring, frequency, count);
    double fShapeMinus
        = weibullNegativeLogLikelihood(scale, shape - hShape, x, censoring, frequency, count);
    double fPP = weibullNegativeLogLikelihood(
        scale + hScale, shape + hShape, x, censoring, frequency, count);
    double fPM = weibullNegativeLogLikelihood(
        scale + hScale, shape - hShape, x, censoring, frequency, count);
    double fMP = weibullNegativeLogLikelihood(
        scale - hScale, shape + hShape, x, censoring, frequency, count);
    double fMM = weibullNegativeLogLikelihood(
        scale - hScale, shape - hShape, x, censoring, frequency, count);
    double info00 = (fScalePlus - 2.0 * f00 + fScaleMinus) / (hScale * hScale);
    double info11 = (fShapePlus - 2.0 * f00 + fShapeMinus) / (hShape * hShape);
    double info01 = (fPP - fPM - fMP + fMM) / (4.0 * hScale * hShape);
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
static WeibullColumnEstimate
fitWeibullColumn(const double* x, const double* censoring, const double* frequency, indexType rows,
    indexType column, const WeibullFitOptions& options)
{
    indexType offset = column * rows;
    double totalWeight = 0;
    double uncensoredWeight = 0;
    double uncensoredLogSum = 0;
    double weightedLogSum = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[offset + r];
        double weight = frequency[offset + r];
        if (std::isnan(xv)) {
            hasNaN = true;
        }
        totalWeight += weight;
        weightedLogSum += weight * std::log(xv);
        if (censoring[offset + r] == 0 && weight > 0) {
            uncensoredWeight += weight;
            uncensoredLogSum += weight * std::log(xv);
        }
    }
    WeibullColumnEstimate estimate;
    estimate.scale = std::numeric_limits<double>::quiet_NaN();
    estimate.shape = estimate.scale;
    estimate.negativeLogLikelihood = estimate.scale;
    estimate.covariance = { estimate.scale, estimate.scale, estimate.scale, estimate.scale };
    if (hasNaN || totalWeight <= 0 || uncensoredWeight <= 0) {
        return estimate;
    }
    double logMean = weightedLogSum / totalWeight;
    double logVariance = 0;
    for (indexType r = 0; r < rows; ++r) {
        double delta = std::log(x[offset + r]) - logMean;
        logVariance += frequency[offset + r] * delta * delta;
    }
    double shape0 = logVariance > 0 ? PI_VALUE / std::sqrt(6.0 * logVariance / totalWeight) : 1.0;
    double center = std::log(std::max(shape0, std::numeric_limits<double>::min()));
    double lo = center - 8.0;
    double hi = center + 8.0;
    double gr = (std::sqrt(5.0) - 1.0) / 2.0;
    double x1 = hi - gr * (hi - lo);
    double x2 = lo + gr * (hi - lo);
    double f1 = profileNegativeLogLikelihood(
        x1, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredLogSum);
    double f2 = profileNegativeLogLikelihood(
        x2, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredLogSum);
    for (int iter = 0;
        iter < options.maxIter && std::abs(hi - lo) > options.tolX * (1.0 + std::abs(center));
        ++iter) {
        if (f1 > f2) {
            lo = x1;
            x1 = x2;
            f1 = f2;
            x2 = lo + gr * (hi - lo);
            f2 = profileNegativeLogLikelihood(
                x2, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredLogSum);
        } else {
            hi = x2;
            x2 = x1;
            f2 = f1;
            x1 = hi - gr * (hi - lo);
            f1 = profileNegativeLogLikelihood(
                x1, x, censoring, frequency, rows, column, uncensoredWeight, uncensoredLogSum);
        }
    }
    double shape = std::exp((lo + hi) / 2.0);
    double logScale
        = (profileLogSum(x, frequency, rows, column, shape) - std::log(uncensoredWeight)) / shape;
    double scale = std::exp(logScale);
    estimate.scale = scale;
    estimate.shape = shape;
    estimate.negativeLogLikelihood = weibullNegativeLogLikelihood(
        scale, shape, x + offset, censoring + offset, frequency + offset, rows);
    estimate.covariance
        = weibullCovariance(scale, shape, x + offset, censoring + offset, frequency + offset, rows);
    return estimate;
}
//=============================================================================
WeibullFitResult
WeibullFit(const ArrayOfVector& args)
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
    WeibullFitOptions options = fitOptionsValue(args);
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
    double z = normalInvApprox(1.0 - alpha / 2.0);
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        WeibullColumnEstimate estimate = fitWeibullColumn(xp, cp, fp, rows, c, options);
        params[c] = estimate.scale;
        params[c + cols] = estimate.shape;
        double scaleLogSe = estimate.covariance[0] >= 0 && estimate.scale > 0
            ? std::sqrt(estimate.covariance[0]) / estimate.scale
            : std::numeric_limits<double>::quiet_NaN();
        double shapeLogSe = estimate.covariance[3] >= 0 && estimate.shape > 0
            ? std::sqrt(estimate.covariance[3]) / estimate.shape
            : std::numeric_limits<double>::quiet_NaN();
        indexType base = c * 4;
        pci[base] = estimate.scale * std::exp(-z * scaleLogSe);
        pci[base + 1] = estimate.scale * std::exp(z * scaleLogSe);
        pci[base + 2] = estimate.shape * std::exp(-z * shapeLogSe);
        pci[base + 3] = estimate.shape * std::exp(z * shapeLogSe);
    }
    WeibullFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 2) : Dimensions(cols, 2), params);
    result.parameterCI = ArrayOf(NLS_DOUBLE,
        vectorInput ? Dimensions(2, 2) : Dimensions(std::vector<indexType> { 2, 2, cols }), pci);
    return result;
}
//=============================================================================
static WeibullLikeResult
makeLikeResult(double nlogL, const std::array<double, 4>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    for (indexType k = 0; k < 4; ++k) {
        covPtr[k] = covariance[k];
    }
    WeibullLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(2, 2), covPtr);
    return result;
}
//=============================================================================
WeibullLikeResult
WeibullLike(const ArrayOfVector& args)
{
    ArrayOf params = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"PARAMS"));
    if (!params.isVector() || params.elementCount() != 2) {
        Error("Nelson:statistics:invalidParams", "PARAMS must be a %s vector.", "two-element");
    }
    const double* pp = static_cast<const double*>(params.dataPointer());
    double scale = pp[0];
    double shape = pp[1];
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    validateDataValues(x);
    Dimensions dims = x.dimensions();
    ArrayOf censoring;
    ArrayOf frequency;
    prepareCensoringFrequency(args, 2, dims, censoring, frequency);
    std::array<double, 4> nanCov { std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN() };
    if (std::isnan(scale) || std::isnan(shape)) {
        return makeLikeResult(std::numeric_limits<double>::quiet_NaN(), nanCov);
    }
    if (!std::isfinite(scale) || !std::isfinite(shape) || scale <= 0 || shape <= 0) {
        return makeLikeResult(std::numeric_limits<double>::infinity(), nanCov);
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
    double nlogL = weibullNegativeLogLikelihood(scale, shape, xp, cp, fp, count);
    if (std::isnan(nlogL)) {
        return makeLikeResult(nlogL, nanCov);
    }
    std::array<double, 4> covariance = weibullCovariance(scale, shape, xp, cp, fp, count);
    return makeLikeResult(nlogL, covariance);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
