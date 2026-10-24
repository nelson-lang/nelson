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
struct NormalColumnEstimate
{
    double mu;
    double sigma;
    double negativeLogLikelihood;
    std::array<double, 4> covariance;
};
//=============================================================================
struct NormalFitOptions
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
        if (!std::isnan(value) && !std::isfinite(value)) {
            Error("Nelson:statistics:xMustBeFinite", "%s must contain finite values.", "X");
        }
    }
}
//=============================================================================
static NormalFitOptions
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
normalNegativeLogLikelihood(double mu, double sigma, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    if (std::isnan(mu) || std::isnan(sigma)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
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
        double z = (xv - mu) / sigma;
        if (censoring[k] == 0) {
            logLikelihood -= weight * (logSigma + LOG_SQRT_2PI + 0.5 * z * z);
        } else {
            logLikelihood += weight * normalLogSurvival(z);
        }
    }
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 4>
normalCovariance(double mu, double sigma, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    double hMu = std::max(1e-5 * std::max(std::abs(mu), 1.0), 1e-6);
    double hSigma = std::max(1e-5 * std::abs(sigma), 1e-6);
    if (sigma - hSigma <= 0) {
        hSigma = sigma * 0.5;
    }
    double f00 = normalNegativeLogLikelihood(mu, sigma, x, censoring, frequency, count);
    double fMuPlus = normalNegativeLogLikelihood(mu + hMu, sigma, x, censoring, frequency, count);
    double fMuMinus = normalNegativeLogLikelihood(mu - hMu, sigma, x, censoring, frequency, count);
    double fSigmaPlus
        = normalNegativeLogLikelihood(mu, sigma + hSigma, x, censoring, frequency, count);
    double fSigmaMinus
        = normalNegativeLogLikelihood(mu, sigma - hSigma, x, censoring, frequency, count);
    double fPP
        = normalNegativeLogLikelihood(mu + hMu, sigma + hSigma, x, censoring, frequency, count);
    double fPM
        = normalNegativeLogLikelihood(mu + hMu, sigma - hSigma, x, censoring, frequency, count);
    double fMP
        = normalNegativeLogLikelihood(mu - hMu, sigma + hSigma, x, censoring, frequency, count);
    double fMM
        = normalNegativeLogLikelihood(mu - hMu, sigma - hSigma, x, censoring, frequency, count);
    double h00 = (fMuPlus - 2.0 * f00 + fMuMinus) / (hMu * hMu);
    double h11 = (fSigmaPlus - 2.0 * f00 + fSigmaMinus) / (hSigma * hSigma);
    double h01 = (fPP - fPM - fMP + fMM) / (4.0 * hMu * hSigma);
    double det = h00 * h11 - h01 * h01;
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    if (!std::isfinite(det) || det <= 0) {
        return { nanValue, nanValue, nanValue, nanValue };
    }
    return { h11 / det, -h01 / det, -h01 / det, h00 / det };
}
//=============================================================================
static NormalColumnEstimate
initialNormalEstimate(
    const double* x, const double* censoring, const double* frequency, indexType rows)
{
    double totalWeight = 0;
    double uncensoredWeight = 0;
    double weightedSum = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[r];
        double weight = frequency[r];
        if (std::isnan(xv)) {
            hasNaN = true;
        }
        if (censoring[r] == 0) {
            uncensoredWeight += weight;
            weightedSum += weight * xv;
        }
        totalWeight += weight;
    }
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    NormalColumnEstimate estimate { nanValue, nanValue, nanValue,
        { nanValue, nanValue, nanValue, nanValue } };
    if (hasNaN || totalWeight <= 0 || uncensoredWeight <= 0) {
        return estimate;
    }
    double mu = weightedSum / uncensoredWeight;
    double weightedSquare = 0;
    for (indexType r = 0; r < rows; ++r) {
        if (censoring[r] == 0) {
            double d = x[r] - mu;
            weightedSquare += frequency[r] * d * d;
        }
    }
    double sigma = std::sqrt(weightedSquare / uncensoredWeight);
    estimate.mu = mu;
    estimate.sigma = sigma;
    estimate.negativeLogLikelihood
        = normalNegativeLogLikelihood(mu, sigma, x, censoring, frequency, rows);
    estimate.covariance = normalCovariance(mu, sigma, x, censoring, frequency, rows);
    return estimate;
}
//=============================================================================
static double
normalNegativeLogLikelihoodLogSigma(double mu, double logSigma, const double* x,
    const double* censoring, const double* frequency, indexType rows)
{
    return normalNegativeLogLikelihood(mu, std::exp(logSigma), x, censoring, frequency, rows);
}
//=============================================================================
static NormalColumnEstimate
fitCensoredNormalColumn(const double* x, const double* censoring, const double* frequency,
    indexType rows, const NormalFitOptions& options)
{
    NormalColumnEstimate start = initialNormalEstimate(x, censoring, frequency, rows);
    if (!std::isfinite(start.mu) || !std::isfinite(start.sigma)) {
        return start;
    }
    std::array<std::array<double, 2>, 3> vertex { { { start.mu, std::log(start.sigma) },
        { start.mu + 0.2 * start.sigma, std::log(start.sigma) },
        { start.mu, std::log(start.sigma * 1.2) } } };
    std::array<double, 3> value;
    for (int k = 0; k < 3; ++k) {
        value[k] = normalNegativeLogLikelihoodLogSigma(
            vertex[k][0], vertex[k][1], x, censoring, frequency, rows);
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
        double reflectedValue = normalNegativeLogLikelihoodLogSigma(
            reflected[0], reflected[1], x, censoring, frequency, rows);
        if (reflectedValue < value[order[0]]) {
            std::array<double, 2> expanded { centroid[0] + 2.0 * (reflected[0] - centroid[0]),
                centroid[1] + 2.0 * (reflected[1] - centroid[1]) };
            double expandedValue = normalNegativeLogLikelihoodLogSigma(
                expanded[0], expanded[1], x, censoring, frequency, rows);
            vertex[order[2]] = expandedValue < reflectedValue ? expanded : reflected;
            value[order[2]] = std::min(expandedValue, reflectedValue);
        } else if (reflectedValue < value[order[1]]) {
            vertex[order[2]] = reflected;
            value[order[2]] = reflectedValue;
        } else {
            vertex[order[2]][0] = 0.5 * (vertex[order[2]][0] + vertex[order[0]][0]);
            vertex[order[2]][1] = 0.5 * (vertex[order[2]][1] + vertex[order[0]][1]);
            value[order[2]] = normalNegativeLogLikelihoodLogSigma(
                vertex[order[2]][0], vertex[order[2]][1], x, censoring, frequency, rows);
        }
    }
    int best = value[0] <= value[1] && value[0] <= value[2] ? 0 : (value[1] <= value[2] ? 1 : 2);
    start.mu = vertex[best][0];
    start.sigma = std::exp(vertex[best][1]);
    start.negativeLogLikelihood = value[best];
    start.covariance = normalCovariance(start.mu, start.sigma, x, censoring, frequency, rows);
    return start;
}
//=============================================================================
static NormalColumnEstimate
fitNormalColumn(const double* x, const double* censoring, const double* frequency, indexType rows,
    indexType column, const NormalFitOptions& options)
{
    indexType offset = column * rows;
    bool hasCensored = false;
    for (indexType r = 0; r < rows; ++r) {
        hasCensored = hasCensored || censoring[offset + r] != 0;
    }
    if (hasCensored) {
        return fitCensoredNormalColumn(
            x + offset, censoring + offset, frequency + offset, rows, options);
    }
    return initialNormalEstimate(x + offset, censoring + offset, frequency + offset, rows);
}
//=============================================================================
NormalFitResult
NormalFit(const ArrayOfVector& args)
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
    NormalFitOptions options = fitOptionsValue(args);
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
    double* mu = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols));
    double* sigma = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols));
    double* muCI = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * cols));
    double* sigmaCI = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2 * cols));
    double z = NormalInv01(1.0 - alpha / 2.0);
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        NormalColumnEstimate estimate = fitNormalColumn(xp, cp, fp, rows, c, options);
        mu[c] = estimate.mu;
        sigma[c] = estimate.sigma;
        double muSe = estimate.covariance[0] >= 0 ? std::sqrt(estimate.covariance[0])
                                                  : std::numeric_limits<double>::quiet_NaN();
        double sigmaLogSe = estimate.covariance[3] >= 0 && estimate.sigma > 0
            ? std::sqrt(estimate.covariance[3]) / estimate.sigma
            : std::numeric_limits<double>::quiet_NaN();
        muCI[2 * c] = estimate.mu - z * muSe;
        muCI[2 * c + 1] = estimate.mu + z * muSe;
        if (estimate.sigma == 0) {
            sigmaCI[2 * c] = 0;
            sigmaCI[2 * c + 1] = 0;
        } else {
            sigmaCI[2 * c] = estimate.sigma * std::exp(-z * sigmaLogSe);
            sigmaCI[2 * c + 1] = estimate.sigma * std::exp(z * sigmaLogSe);
        }
    }
    Dimensions outDims = vectorInput ? Dimensions(1, 1) : Dimensions(1, cols);
    NormalFitResult result;
    result.muHat = ArrayOf(NLS_DOUBLE, outDims, mu);
    result.sigmaHat = ArrayOf(NLS_DOUBLE, outDims, sigma);
    result.muCI = ArrayOf(NLS_DOUBLE, Dimensions(2, cols), muCI);
    result.sigmaCI = ArrayOf(NLS_DOUBLE, Dimensions(2, cols), sigmaCI);
    return result;
}
//=============================================================================
static NormalLikeResult
makeLikeResult(double nlogL, const std::array<double, 4>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    for (int k = 0; k < 4; ++k) {
        covPtr[k] = covariance[k];
    }
    NormalLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(2, 2), covPtr);
    return result;
}
//=============================================================================
NormalLikeResult
NormalLike(const ArrayOfVector& args)
{
    ArrayOf params = asDoubleArray(args[0],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:statistics:invalidParams", L"PARAMS must be a %s vector."),
            L"two-element real numeric"));
    if (params.elementCount() != 2) {
        Error("Nelson:statistics:invalidParams", "PARAMS must be a %s vector.",
            "two-element real numeric");
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
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    std::array<double, 4> nanCov { nanValue, nanValue, nanValue, nanValue };
    if (std::isnan(mu) || std::isnan(sigma)) {
        return makeLikeResult(nanValue, nanCov);
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
    double nlogL = normalNegativeLogLikelihood(mu, sigma, xp, cp, fp, count);
    if (std::isnan(nlogL)) {
        return makeLikeResult(nlogL, nanCov);
    }
    return makeLikeResult(nlogL, normalCovariance(mu, sigma, xp, cp, fp, count));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
