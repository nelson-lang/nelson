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
struct GammaColumnEstimate
{
    double shape;
    double scale;
    double negativeLogLikelihood;
    std::array<double, 4> covariance;
};
//=============================================================================
struct GammaFitOptions
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
digammaApprox(double x)
{
    double result = 0;
    while (x < 8.0) {
        result -= 1.0 / x;
        x += 1.0;
    }
    double inv = 1.0 / x;
    double inv2 = inv * inv;
    return result + std::log(x) - 0.5 * inv
        - inv2 * (1.0 / 12.0 - inv2 * (1.0 / 120.0 - inv2 / 252.0));
}
//=============================================================================
static double
trigammaApprox(double x)
{
    double result = 0;
    while (x < 8.0) {
        result += 1.0 / (x * x);
        x += 1.0;
    }
    double inv = 1.0 / x;
    double inv2 = inv * inv;
    return result + inv + 0.5 * inv2 + inv2 * inv / 6.0 - inv2 * inv2 * inv / 30.0
        + inv2 * inv2 * inv2 * inv / 42.0;
}
//=============================================================================
static GammaFitOptions
fitOptionsValue(const ArrayOfVector& args)
{
    FitOptions commonOptions = StatisticsHelpers::parseFitOptions(args);
    return { commonOptions.maxIter, commonOptions.tolX };
}
//=============================================================================
static double
gammaLogSurvival(double x, double shape, double scale)
{
    double value = ChiSquareUpper(2.0 * x / scale, 2.0 * shape);
    return value > 0 ? std::log(value) : -std::numeric_limits<double>::infinity();
}
//=============================================================================
static double
gammaNegativeLogLikelihood(double shape, double scale, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    if (!std::isfinite(shape) || !std::isfinite(scale) || shape <= 0 || scale <= 0) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = 0;
    double logScale = std::log(scale);
    double logGamma = std::lgamma(shape);
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double weight = frequency[k];
        if (weight == 0) {
            continue;
        }
        if (censoring[k] == 0) {
            logLikelihood += weight
                * ((shape - 1.0) * std::log(xv) - xv / scale - logGamma - shape * logScale);
        } else {
            logLikelihood += weight * gammaLogSurvival(xv, shape, scale);
        }
    }
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 4>
gammaCovariance(double shape, double scale, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    double hShape = std::max(1e-5 * std::abs(shape), 1e-6);
    double hScale = std::max(1e-5 * std::abs(scale), 1e-6);
    if (shape - hShape <= 0) {
        hShape = shape * 0.5;
    }
    if (scale - hScale <= 0) {
        hScale = scale * 0.5;
    }
    double f00 = gammaNegativeLogLikelihood(shape, scale, x, censoring, frequency, count);
    double fShapePlus
        = gammaNegativeLogLikelihood(shape + hShape, scale, x, censoring, frequency, count);
    double fShapeMinus
        = gammaNegativeLogLikelihood(shape - hShape, scale, x, censoring, frequency, count);
    double fScalePlus
        = gammaNegativeLogLikelihood(shape, scale + hScale, x, censoring, frequency, count);
    double fScaleMinus
        = gammaNegativeLogLikelihood(shape, scale - hScale, x, censoring, frequency, count);
    double fPP = gammaNegativeLogLikelihood(
        shape + hShape, scale + hScale, x, censoring, frequency, count);
    double fPM = gammaNegativeLogLikelihood(
        shape + hShape, scale - hScale, x, censoring, frequency, count);
    double fMP = gammaNegativeLogLikelihood(
        shape - hShape, scale + hScale, x, censoring, frequency, count);
    double fMM = gammaNegativeLogLikelihood(
        shape - hShape, scale - hScale, x, censoring, frequency, count);
    double info00 = (fShapePlus - 2.0 * f00 + fShapeMinus) / (hShape * hShape);
    double info11 = (fScalePlus - 2.0 * f00 + fScaleMinus) / (hScale * hScale);
    double info01 = (fPP - fPM - fMP + fMM) / (4.0 * hShape * hScale);
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
static GammaColumnEstimate
initialGammaEstimate(
    const double* x, const double* censoring, const double* frequency, indexType rows)
{
    double totalWeight = 0;
    double uncensoredWeight = 0;
    double uncensoredSum = 0;
    double uncensoredLogSum = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[r];
        double weight = frequency[r];
        if (std::isnan(xv)) {
            hasNaN = true;
        }
        totalWeight += weight;
        if (censoring[r] == 0 && weight > 0) {
            uncensoredWeight += weight;
            uncensoredSum += weight * xv;
            uncensoredLogSum += weight * std::log(xv);
        }
    }
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    GammaColumnEstimate estimate { nanValue, nanValue, nanValue,
        { nanValue, nanValue, nanValue, nanValue } };
    if (hasNaN || totalWeight <= 0 || uncensoredWeight <= 0) {
        return estimate;
    }
    double mean = uncensoredSum / uncensoredWeight;
    double meanLog = uncensoredLogSum / uncensoredWeight;
    double s = std::log(mean) - meanLog;
    double shape
        = s > 0 ? (3.0 - s + std::sqrt((s - 3.0) * (s - 3.0) + 24.0 * s)) / (12.0 * s) : 1e8;
    for (int iter = 0; iter < 100; ++iter) {
        double f = std::log(shape) - digammaApprox(shape) - s;
        double fp = 1.0 / shape - trigammaApprox(shape);
        double next = shape - f / fp;
        if (!std::isfinite(next) || next <= 0) {
            next = shape * 0.5;
        }
        if (std::abs(next - shape) <= 1e-12 * (1.0 + shape)) {
            shape = next;
            break;
        }
        shape = next;
    }
    double scale = mean / shape;
    estimate.shape = shape;
    estimate.scale = scale;
    estimate.negativeLogLikelihood
        = gammaNegativeLogLikelihood(shape, scale, x, censoring, frequency, rows);
    estimate.covariance = gammaCovariance(shape, scale, x, censoring, frequency, rows);
    return estimate;
}
//=============================================================================
static GammaColumnEstimate
fitCensoredGammaColumn(const double* x, const double* censoring, const double* frequency,
    indexType rows, const GammaFitOptions& options)
{
    GammaColumnEstimate start = initialGammaEstimate(x, censoring, frequency, rows);
    if (!std::isfinite(start.shape) || !std::isfinite(start.scale)) {
        return start;
    }
    std::array<std::array<double, 2>, 3> vertex {
        { { std::log(start.shape), std::log(start.scale) },
            { std::log(start.shape * 1.25), std::log(start.scale) },
            { std::log(start.shape), std::log(start.scale * 1.25) } }
    };
    std::array<double, 3> value;
    for (int k = 0; k < 3; ++k) {
        value[k] = gammaNegativeLogLikelihood(
            std::exp(vertex[k][0]), std::exp(vertex[k][1]), x, censoring, frequency, rows);
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
        std::array<double, 2> reflected { centroid[0] + centroid[0] - vertex[order[2]][0],
            centroid[1] + centroid[1] - vertex[order[2]][1] };
        double reflectedValue = gammaNegativeLogLikelihood(
            std::exp(reflected[0]), std::exp(reflected[1]), x, censoring, frequency, rows);
        if (reflectedValue < value[order[0]]) {
            std::array<double, 2> expanded { centroid[0] + 2.0 * (reflected[0] - centroid[0]),
                centroid[1] + 2.0 * (reflected[1] - centroid[1]) };
            double expandedValue = gammaNegativeLogLikelihood(
                std::exp(expanded[0]), std::exp(expanded[1]), x, censoring, frequency, rows);
            vertex[order[2]] = expandedValue < reflectedValue ? expanded : reflected;
            value[order[2]] = std::min(expandedValue, reflectedValue);
        } else if (reflectedValue < value[order[1]]) {
            vertex[order[2]] = reflected;
            value[order[2]] = reflectedValue;
        } else {
            std::array<double, 2> contracted { centroid[0]
                    + 0.5 * (vertex[order[2]][0] - centroid[0]),
                centroid[1] + 0.5 * (vertex[order[2]][1] - centroid[1]) };
            value[order[2]] = gammaNegativeLogLikelihood(
                std::exp(contracted[0]), std::exp(contracted[1]), x, censoring, frequency, rows);
            vertex[order[2]] = contracted;
        }
    }
    int best = value[0] < value[1] ? (value[0] < value[2] ? 0 : 2) : (value[1] < value[2] ? 1 : 2);
    start.shape = std::exp(vertex[best][0]);
    start.scale = std::exp(vertex[best][1]);
    start.negativeLogLikelihood = value[best];
    start.covariance = gammaCovariance(start.shape, start.scale, x, censoring, frequency, rows);
    return start;
}
//=============================================================================
static GammaColumnEstimate
fitGammaColumn(const double* x, const double* censoring, const double* frequency, indexType rows,
    indexType column, const GammaFitOptions& options)
{
    indexType offset = column * rows;
    bool hasCensored = false;
    for (indexType r = 0; r < rows; ++r) {
        hasCensored = hasCensored || (censoring[offset + r] != 0 && frequency[offset + r] > 0);
    }
    if (hasCensored) {
        return fitCensoredGammaColumn(
            x + offset, censoring + offset, frequency + offset, rows, options);
    }
    return initialGammaEstimate(x + offset, censoring + offset, frequency + offset, rows);
}
//=============================================================================
GammaFitResult
GammaFit(const ArrayOfVector& args)
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
    GammaFitOptions options = fitOptionsValue(args);
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
        GammaColumnEstimate estimate = fitGammaColumn(xp, cp, fp, rows, c, options);
        params[c] = estimate.shape;
        params[c + cols] = estimate.scale;
        double shapeLogSe = estimate.covariance[0] >= 0 && estimate.shape > 0
            ? std::sqrt(estimate.covariance[0]) / estimate.shape
            : std::numeric_limits<double>::quiet_NaN();
        double scaleLogSe = estimate.covariance[3] >= 0 && estimate.scale > 0
            ? std::sqrt(estimate.covariance[3]) / estimate.scale
            : std::numeric_limits<double>::quiet_NaN();
        indexType base = c * 4;
        pci[base] = estimate.shape * std::exp(-z * shapeLogSe);
        pci[base + 1] = estimate.shape * std::exp(z * shapeLogSe);
        pci[base + 2] = estimate.scale * std::exp(-z * scaleLogSe);
        pci[base + 3] = estimate.scale * std::exp(z * scaleLogSe);
    }
    GammaFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 2) : Dimensions(cols, 2), params);
    result.parameterCI = ArrayOf(NLS_DOUBLE,
        vectorInput ? Dimensions(2, 2) : Dimensions(std::vector<indexType> { 2, 2, cols }), pci);
    return result;
}
//=============================================================================
static GammaLikeResult
makeLikeResult(double nlogL, const std::array<double, 4>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    for (indexType k = 0; k < 4; ++k) {
        covPtr[k] = covariance[k];
    }
    GammaLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(2, 2), covPtr);
    return result;
}
//=============================================================================
GammaLikeResult
GammaLike(const ArrayOfVector& args)
{
    ArrayOf params = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"PARAMS"));
    if (!params.isVector() || params.elementCount() != 2) {
        Error("Nelson:statistics:invalidParams", "PARAMS must be a %s vector.", "two-element");
    }
    const double* pp = static_cast<const double*>(params.dataPointer());
    double shape = pp[0];
    double scale = pp[1];
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
    if (std::isnan(shape) || std::isnan(scale)) {
        return makeLikeResult(std::numeric_limits<double>::quiet_NaN(), nanCov);
    }
    if (!std::isfinite(shape) || !std::isfinite(scale) || shape <= 0 || scale <= 0) {
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
    double nlogL = gammaNegativeLogLikelihood(shape, scale, xp, cp, fp, count);
    if (std::isnan(nlogL)) {
        return makeLikeResult(nlogL, nanCov);
    }
    std::array<double, 4> covariance = gammaCovariance(shape, scale, xp, cp, fp, count);
    return makeLikeResult(nlogL, covariance);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
