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
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace StatisticsHelpers;
//=============================================================================
struct BetaColumnEstimate
{
    double a;
    double b;
    double negativeLogLikelihood;
    std::array<double, 4> covariance;
};
//=============================================================================
static void
validateDataValues(const ArrayOf& x)
{
    const double* ptr = static_cast<const double*>(x.dataPointer());
    for (indexType k = 0; k < x.elementCount(); ++k) {
        double value = ptr[k];
        if (!std::isnan(value) && (!std::isfinite(value) || value <= 0 || value >= 1)) {
            Error("Nelson:statistics:xMustBeInUnitInterval",
                "X must contain finite values in the open interval (0,1).");
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
        - inv2 * (1.0 / 12.0 - inv2 * (1.0 / 120.0 + -inv2 * (1.0 / 252.0 - inv2 * (1.0 / 240.0))));
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
    return result + inv + 0.5 * inv2
        + inv2 * inv * (1.0 / 6.0 - inv2 * (1.0 / 30.0 + -inv2 * (1.0 / 42.0)));
}
//=============================================================================
static double
betaNegativeLogLikelihood(double a, double b, const double* x, indexType count)
{
    if (std::isnan(a) || std::isnan(b)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!std::isfinite(a) || !std::isfinite(b) || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = 0;
    double logNorm = std::lgamma(a + b) - std::lgamma(a) - std::lgamma(b);
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        logLikelihood += (a - 1.0) * std::log(xv) + (b - 1.0) * std::log1p(-xv) + logNorm;
    }
    return -logLikelihood;
}
//=============================================================================
static std::array<double, 4>
betaCovariance(double a, double b, indexType count)
{
    double s = a + b;
    double n = static_cast<double>(count);
    double h00 = n * (trigammaApprox(a) - trigammaApprox(s));
    double h11 = n * (trigammaApprox(b) - trigammaApprox(s));
    double h01 = -n * trigammaApprox(s);
    double det = h00 * h11 - h01 * h01;
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    if (!std::isfinite(det) || det <= 0) {
        return { nanValue, nanValue, nanValue, nanValue };
    }
    return { h11 / det, -h01 / det, -h01 / det, h00 / det };
}
//=============================================================================
static BetaColumnEstimate
fitBetaColumn(const double* x, indexType rows, indexType column)
{
    indexType offset = column * rows;
    double sum = 0;
    double sumSquares = 0;
    double meanLog = 0;
    double meanLog1p = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[offset + r];
        if (std::isnan(xv)) {
            hasNaN = true;
            continue;
        }
        sum += xv;
        sumSquares += xv * xv;
        meanLog += std::log(xv);
        meanLog1p += std::log1p(-xv);
    }
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    BetaColumnEstimate estimate { nanValue, nanValue, nanValue,
        { nanValue, nanValue, nanValue, nanValue } };
    if (hasNaN || rows == 0) {
        return estimate;
    }
    double n = static_cast<double>(rows);
    double mean = sum / n;
    double variance = sumSquares / n - mean * mean;
    double common = variance > 0 ? mean * (1.0 - mean) / variance - 1.0 : 2.0;
    double a = common > 0 ? std::max(mean * common, 1e-3) : 1.0;
    double b = common > 0 ? std::max((1.0 - mean) * common, 1e-3) : 1.0;
    meanLog /= n;
    meanLog1p /= n;
    for (int iter = 0; iter < 80; ++iter) {
        double s = a + b;
        double f0 = digammaApprox(a) - digammaApprox(s) - meanLog;
        double f1 = digammaApprox(b) - digammaApprox(s) - meanLog1p;
        double h00 = trigammaApprox(a) - trigammaApprox(s);
        double h11 = trigammaApprox(b) - trigammaApprox(s);
        double h01 = -trigammaApprox(s);
        double det = h00 * h11 - h01 * h01;
        if (!std::isfinite(det) || det == 0) {
            break;
        }
        double da = (h11 * f0 - h01 * f1) / det;
        double db = (-h01 * f0 + h00 * f1) / det;
        double step = 1.0;
        while ((a - step * da) <= 0 || (b - step * db) <= 0) {
            step *= 0.5;
        }
        a -= step * da;
        b -= step * db;
        if (std::max(std::abs(step * da), std::abs(step * db)) <= 1e-10 * (1.0 + a + b)) {
            break;
        }
    }
    estimate.a = a;
    estimate.b = b;
    estimate.negativeLogLikelihood = betaNegativeLogLikelihood(a, b, x + offset, rows);
    estimate.covariance = betaCovariance(a, b, rows);
    return estimate;
}
//=============================================================================
BetaFitResult
BetaFit(const ArrayOfVector& args)
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
    Dimensions dims = x.dimensions();
    bool vectorInput = x.isVector();
    indexType rows = vectorInput ? x.elementCount() : dims.rows();
    indexType cols = vectorInput ? 1 : dims.columns();
    ArrayOf data = x;
    if (vectorInput && dims.rows() == 1) {
        Dimensions reshaped(rows, 1);
        data.reshape(reshaped);
    }
    const double* xp = static_cast<const double*>(data.dataPointer());
    double* params = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 2));
    double* pci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 4));
    double z = NormalInv01(1.0 - alpha / 2.0);
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        BetaColumnEstimate estimate = fitBetaColumn(xp, rows, c);
        params[c] = estimate.a;
        params[c + cols] = estimate.b;
        double aLogSe = estimate.covariance[0] >= 0 && estimate.a > 0
            ? std::sqrt(estimate.covariance[0]) / estimate.a
            : std::numeric_limits<double>::quiet_NaN();
        double bLogSe = estimate.covariance[3] >= 0 && estimate.b > 0
            ? std::sqrt(estimate.covariance[3]) / estimate.b
            : std::numeric_limits<double>::quiet_NaN();
        pci[2 * c] = estimate.a * std::exp(-z * aLogSe);
        pci[2 * c + 1] = estimate.a * std::exp(z * aLogSe);
        pci[2 * (c + cols)] = estimate.b * std::exp(-z * bLogSe);
        pci[2 * (c + cols) + 1] = estimate.b * std::exp(z * bLogSe);
    }
    BetaFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 2) : Dimensions(cols, 2), params);
    result.parameterCI = ArrayOf(NLS_DOUBLE,
        vectorInput ? Dimensions(2, 2) : Dimensions(std::vector<indexType> { 2, 2, cols }), pci);
    return result;
}
//=============================================================================
static BetaLikeResult
makeLikeResult(double nlogL, const std::array<double, 4>& covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    for (int k = 0; k < 4; ++k) {
        covPtr[k] = covariance[k];
    }
    BetaLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(2, 2), covPtr);
    return result;
}
//=============================================================================
BetaLikeResult
BetaLike(const ArrayOfVector& args)
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
    double a = pp[0];
    double b = pp[1];
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    validateDataValues(x);
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    std::array<double, 4> nanCov { nanValue, nanValue, nanValue, nanValue };
    if (std::isnan(a) || std::isnan(b)) {
        return makeLikeResult(nanValue, nanCov);
    }
    if (!std::isfinite(a) || !std::isfinite(b) || a <= 0 || b <= 0) {
        return makeLikeResult(std::numeric_limits<double>::infinity(), nanCov);
    }
    indexType count = x.elementCount();
    ArrayOf flatX = x;
    Dimensions flatDims(count, 1);
    flatX.reshape(flatDims);
    const double* xp = static_cast<const double*>(flatX.dataPointer());
    double nlogL = betaNegativeLogLikelihood(a, b, xp, count);
    if (std::isnan(nlogL)) {
        return makeLikeResult(nlogL, nanCov);
    }
    return makeLikeResult(nlogL, betaCovariance(a, b, count));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
