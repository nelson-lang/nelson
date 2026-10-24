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
static constexpr double LOG_SQRT_2PI = 0.91893853320467274178032973640561763986;
static constexpr double INV_SQRT2 = 0.70710678118654752440084436210484903928;
//=============================================================================
struct LognormalColumnEstimate
{
    double mu;
    double sigma;
    double negativeLogLikelihood;
    std::array<double, 4> covariance;
};
//=============================================================================
struct LognormalFitOptions
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
static LognormalFitOptions
fitOptionsValue(const ArrayOfVector& args)
{
    FitOptions commonOptions = StatisticsHelpers::parseFitOptions(args);
    return { commonOptions.maxIter, commonOptions.tolX };
}
//=============================================================================
static double
normalLogSurvival(double z)
{
    double value = 0.5 * std::erfc(z * INV_SQRT2);
    return value > 0 ? std::log(value) : -std::numeric_limits<double>::infinity();
}
//=============================================================================
static double
lognormalNegativeLogLikelihood(double mu, double sigma, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    if (!std::isfinite(mu) || !std::isfinite(sigma) || sigma <= 0) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = 0;
    double logSigma = std::log(sigma);
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double weight = frequency[k];
        if (weight == 0) {
            continue;
        }
        double y = std::log(xv);
        double z = (y - mu) / sigma;
        if (censoring[k] == 0) {
            logLikelihood -= weight * (std::log(xv) + logSigma + LOG_SQRT_2PI + 0.5 * z * z);
        } else {
            logLikelihood += weight * normalLogSurvival(z);
        }
    }
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 4>
lognormalCovariance(double mu, double sigma, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    double hMu = std::max(1e-5 * std::max(std::abs(mu), 1.0), 1e-6);
    double hSigma = std::max(1e-5 * std::abs(sigma), 1e-6);
    if (sigma - hSigma <= 0) {
        hSigma = sigma * 0.5;
    }
    double f00 = lognormalNegativeLogLikelihood(mu, sigma, x, censoring, frequency, count);
    double fMuPlus
        = lognormalNegativeLogLikelihood(mu + hMu, sigma, x, censoring, frequency, count);
    double fMuMinus
        = lognormalNegativeLogLikelihood(mu - hMu, sigma, x, censoring, frequency, count);
    double fSigmaPlus
        = lognormalNegativeLogLikelihood(mu, sigma + hSigma, x, censoring, frequency, count);
    double fSigmaMinus
        = lognormalNegativeLogLikelihood(mu, sigma - hSigma, x, censoring, frequency, count);
    double fPP
        = lognormalNegativeLogLikelihood(mu + hMu, sigma + hSigma, x, censoring, frequency, count);
    double fPM
        = lognormalNegativeLogLikelihood(mu + hMu, sigma - hSigma, x, censoring, frequency, count);
    double fMP
        = lognormalNegativeLogLikelihood(mu - hMu, sigma + hSigma, x, censoring, frequency, count);
    double fMM
        = lognormalNegativeLogLikelihood(mu - hMu, sigma - hSigma, x, censoring, frequency, count);
    double info00 = (fMuPlus - 2.0 * f00 + fMuMinus) / (hMu * hMu);
    double info11 = (fSigmaPlus - 2.0 * f00 + fSigmaMinus) / (hSigma * hSigma);
    double info01 = (fPP - fPM - fMP + fMM) / (4.0 * hMu * hSigma);
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
static LognormalColumnEstimate
initialLognormalEstimate(
    const double* x, const double* censoring, const double* frequency, indexType rows)
{
    double totalWeight = 0;
    double uncensoredWeight = 0;
    double weightedLogSum = 0;
    bool hasNaN = false;
    bool hasCensored = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[r];
        double weight = frequency[r];
        if (std::isnan(xv)) {
            hasNaN = true;
        }
        totalWeight += weight;
        if (censoring[r] != 0 && weight > 0) {
            hasCensored = true;
        }
        if (censoring[r] == 0 && weight > 0) {
            uncensoredWeight += weight;
            weightedLogSum += weight * std::log(xv);
        }
    }
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    LognormalColumnEstimate estimate { nanValue, nanValue, nanValue,
        { nanValue, nanValue, nanValue, nanValue } };
    if (hasNaN || totalWeight <= 0 || uncensoredWeight <= 0) {
        return estimate;
    }
    double mu = weightedLogSum / uncensoredWeight;
    double variance = 0;
    for (indexType r = 0; r < rows; ++r) {
        if (censoring[r] == 0 && frequency[r] > 0) {
            double delta = std::log(x[r]) - mu;
            variance += frequency[r] * delta * delta;
        }
    }
    double sigma = std::sqrt(std::max(variance / uncensoredWeight, 0.0));
    if (sigma <= 0) {
        sigma = std::numeric_limits<double>::epsilon();
    }
    estimate.mu = mu;
    estimate.sigma = sigma;
    estimate.negativeLogLikelihood
        = lognormalNegativeLogLikelihood(mu, sigma, x, censoring, frequency, rows);
    if (!hasCensored) {
        estimate.covariance
            = { sigma * sigma / uncensoredWeight, 0, 0, sigma * sigma / (2.0 * uncensoredWeight) };
    }
    return estimate;
}
//=============================================================================
static LognormalColumnEstimate
fitCensoredLognormalColumn(const double* x, const double* censoring, const double* frequency,
    indexType rows, const LognormalFitOptions& options)
{
    LognormalColumnEstimate start = initialLognormalEstimate(x, censoring, frequency, rows);
    if (!std::isfinite(start.mu) || !std::isfinite(start.sigma)) {
        return start;
    }
    std::array<std::array<double, 2>, 3> vertex { { { start.mu, std::log(start.sigma) },
        { start.mu + std::max(start.sigma * 0.25, 0.1), std::log(start.sigma) },
        { start.mu, std::log(start.sigma * 1.25) } } };
    std::array<double, 3> value;
    for (int k = 0; k < 3; ++k) {
        value[k] = lognormalNegativeLogLikelihood(
            vertex[k][0], std::exp(vertex[k][1]), x, censoring, frequency, rows);
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
        double reflectedValue = lognormalNegativeLogLikelihood(
            reflected[0], std::exp(reflected[1]), x, censoring, frequency, rows);
        if (reflectedValue < value[order[0]]) {
            std::array<double, 2> expanded { centroid[0] + 2.0 * (reflected[0] - centroid[0]),
                centroid[1] + 2.0 * (reflected[1] - centroid[1]) };
            double expandedValue = lognormalNegativeLogLikelihood(
                expanded[0], std::exp(expanded[1]), x, censoring, frequency, rows);
            vertex[order[2]] = expandedValue < reflectedValue ? expanded : reflected;
            value[order[2]] = std::min(expandedValue, reflectedValue);
        } else if (reflectedValue < value[order[1]]) {
            vertex[order[2]] = reflected;
            value[order[2]] = reflectedValue;
        } else {
            std::array<double, 2> contracted { centroid[0]
                    + 0.5 * (vertex[order[2]][0] - centroid[0]),
                centroid[1] + 0.5 * (vertex[order[2]][1] - centroid[1]) };
            value[order[2]] = lognormalNegativeLogLikelihood(
                contracted[0], std::exp(contracted[1]), x, censoring, frequency, rows);
            vertex[order[2]] = contracted;
        }
    }
    int best = value[0] < value[1] ? (value[0] < value[2] ? 0 : 2) : (value[1] < value[2] ? 1 : 2);
    start.mu = vertex[best][0];
    start.sigma = std::exp(vertex[best][1]);
    start.negativeLogLikelihood = value[best];
    start.covariance = lognormalCovariance(start.mu, start.sigma, x, censoring, frequency, rows);
    return start;
}
//=============================================================================
static LognormalColumnEstimate
fitLognormalColumn(const double* x, const double* censoring, const double* frequency,
    indexType rows, indexType column, const LognormalFitOptions& options)
{
    indexType offset = column * rows;
    bool hasCensored = false;
    for (indexType r = 0; r < rows; ++r) {
        hasCensored = hasCensored || (censoring[offset + r] != 0 && frequency[offset + r] > 0);
    }
    if (hasCensored) {
        return fitCensoredLognormalColumn(
            x + offset, censoring + offset, frequency + offset, rows, options);
    }
    LognormalColumnEstimate estimate
        = initialLognormalEstimate(x + offset, censoring + offset, frequency + offset, rows);
    if (std::isfinite(estimate.mu) && std::isfinite(estimate.sigma)) {
        estimate.covariance = lognormalCovariance(
            estimate.mu, estimate.sigma, x + offset, censoring + offset, frequency + offset, rows);
    }
    return estimate;
}
//=============================================================================
LognormalFitResult
LognormalFit(const ArrayOfVector& args)
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
    LognormalFitOptions options = fitOptionsValue(args);
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
        LognormalColumnEstimate estimate = fitLognormalColumn(xp, cp, fp, rows, c, options);
        params[c] = estimate.mu;
        params[c + cols] = estimate.sigma;
        double muSe = estimate.covariance[0] >= 0 ? std::sqrt(estimate.covariance[0])
                                                  : std::numeric_limits<double>::quiet_NaN();
        double sigmaSe = estimate.covariance[3] >= 0 ? std::sqrt(estimate.covariance[3])
                                                     : std::numeric_limits<double>::quiet_NaN();
        indexType base = c * 4;
        pci[base] = estimate.mu - z * muSe;
        pci[base + 1] = estimate.mu + z * muSe;
        pci[base + 2] = estimate.sigma - z * sigmaSe;
        pci[base + 3] = estimate.sigma + z * sigmaSe;
    }
    LognormalFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 2) : Dimensions(cols, 2), params);
    result.parameterCI = ArrayOf(NLS_DOUBLE,
        vectorInput ? Dimensions(2, 2) : Dimensions(std::vector<indexType> { 2, 2, cols }), pci);
    return result;
}
//=============================================================================
static LognormalLikeResult
makeLikeResult(double nlogL, const std::array<double, 4>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    for (indexType k = 0; k < 4; ++k) {
        covPtr[k] = covariance[k];
    }
    LognormalLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(2, 2), covPtr);
    return result;
}
//=============================================================================
LognormalLikeResult
LognormalLike(const ArrayOfVector& args)
{
    ArrayOf params = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"PARAMS"));
    if (!params.isVector() || params.elementCount() != 2) {
        Error("Nelson:statistics:invalidParams", "PARAMS must be a %s vector.", "two-element");
    }
    const double* pp = static_cast<const double*>(params.dataPointer());
    double mu = pp[0];
    double sigma = pp[1];
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
    if (std::isnan(mu) || std::isnan(sigma)) {
        return makeLikeResult(std::numeric_limits<double>::quiet_NaN(), nanCov);
    }
    if (!std::isfinite(mu) || !std::isfinite(sigma) || sigma <= 0) {
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
    double nlogL = lognormalNegativeLogLikelihood(mu, sigma, xp, cp, fp, count);
    if (std::isnan(nlogL)) {
        return makeLikeResult(nlogL, nanCov);
    }
    std::array<double, 4> covariance = lognormalCovariance(mu, sigma, xp, cp, fp, count);
    return makeLikeResult(nlogL, covariance);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
