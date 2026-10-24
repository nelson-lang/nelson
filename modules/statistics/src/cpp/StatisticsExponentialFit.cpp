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
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace StatisticsHelpers;
//=============================================================================
struct ExponentialColumnEstimate
{
    double mean;
    double negativeLogLikelihood;
    double variance;
};
//=============================================================================
static void
validateDataValues(const ArrayOf& x)
{
    const double* ptr = static_cast<const double*>(x.dataPointer());
    for (indexType k = 0; k < x.elementCount(); ++k) {
        double value = ptr[k];
        if (!std::isnan(value) && (!std::isfinite(value) || value < 0)) {
            Error("Nelson:statistics:xMustBeNonnegative",
                "%s must contain nonnegative finite values.", "X");
        }
    }
}
//=============================================================================
static double
exponentialNegativeLogLikelihood(
    double mu, const double* x, const double* censoring, const double* frequency, indexType count)
{
    if (std::isnan(mu)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!std::isfinite(mu) || mu <= 0) {
        return std::numeric_limits<double>::infinity();
    }
    double nlogL = 0;
    double logMu = std::log(mu);
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double weight = frequency[k];
        if (weight == 0) {
            continue;
        }
        nlogL += weight * xv / mu;
        if (censoring[k] == 0) {
            nlogL += weight * logMu;
        }
    }
    return nlogL;
}
//=============================================================================
static double
exponentialVariance(
    double mu, const double* x, const double* censoring, const double* frequency, indexType count)
{
    double uncensoredWeight = 0;
    double weightedSum = 0;
    for (indexType k = 0; k < count; ++k) {
        double weight = frequency[k];
        weightedSum += weight * x[k];
        if (censoring[k] == 0) {
            uncensoredWeight += weight;
        }
    }
    double information = -uncensoredWeight / (mu * mu) + 2.0 * weightedSum / (mu * mu * mu);
    return std::isfinite(information) && information > 0 ? 1.0 / information
                                                         : std::numeric_limits<double>::quiet_NaN();
}
//=============================================================================
static ExponentialColumnEstimate
fitExponentialColumn(const double* x, const double* censoring, const double* frequency,
    indexType rows, indexType column)
{
    indexType offset = column * rows;
    double totalWeight = 0;
    double uncensoredWeight = 0;
    double weightedSum = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[offset + r];
        double weight = frequency[offset + r];
        if (std::isnan(xv)) {
            hasNaN = true;
        }
        totalWeight += weight;
        weightedSum += weight * xv;
        if (censoring[offset + r] == 0 && weight > 0) {
            uncensoredWeight += weight;
        }
    }
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    ExponentialColumnEstimate estimate { nanValue, nanValue, nanValue };
    if (hasNaN || totalWeight <= 0 || uncensoredWeight <= 0) {
        return estimate;
    }
    estimate.mean = weightedSum / uncensoredWeight;
    estimate.negativeLogLikelihood = exponentialNegativeLogLikelihood(
        estimate.mean, x + offset, censoring + offset, frequency + offset, rows);
    estimate.variance = exponentialVariance(
        estimate.mean, x + offset, censoring + offset, frequency + offset, rows);
    return estimate;
}
//=============================================================================
ExpFitResult
ExpFit(const ArrayOfVector& args)
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
    validateFitOptions(args);
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
    double* params = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols));
    double* pci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 2));
    double z = NormalInv01(1.0 - alpha / 2.0);
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        ExponentialColumnEstimate estimate = fitExponentialColumn(xp, cp, fp, rows, c);
        params[c] = estimate.mean;
        double logSe = estimate.variance >= 0 && estimate.mean > 0
            ? std::sqrt(estimate.variance) / estimate.mean
            : std::numeric_limits<double>::quiet_NaN();
        pci[c * 2] = estimate.mean * std::exp(-z * logSe);
        pci[c * 2 + 1] = estimate.mean * std::exp(z * logSe);
    }
    ExpFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 1) : Dimensions(cols, 1), params);
    result.parameterCI
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(2, 1) : Dimensions(2, cols), pci);
    return result;
}
//=============================================================================
static ExpLikeResult
makeLikeResult(double nlogL, double variance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* varPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    varPtr[0] = variance;
    ExpLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), varPtr);
    return result;
}
//=============================================================================
ExpLikeResult
ExpLike(const ArrayOfVector& args)
{
    double mu = scalarDouble(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"MU"));
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
    if (std::isnan(mu)) {
        return makeLikeResult(nanValue, nanValue);
    }
    if (!std::isfinite(mu) || mu <= 0) {
        return makeLikeResult(std::numeric_limits<double>::infinity(), nanValue);
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
    double nlogL = exponentialNegativeLogLikelihood(mu, xp, cp, fp, count);
    if (std::isnan(nlogL)) {
        return makeLikeResult(nlogL, nanValue);
    }
    return makeLikeResult(nlogL, exponentialVariance(mu, xp, cp, fp, count));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
