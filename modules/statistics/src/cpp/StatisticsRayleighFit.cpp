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
struct RayleighColumnEstimate
{
    double scale;
    double negativeLogLikelihood;
    double variance;
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
rayleighNegativeLogLikelihood(double scale, const double* x, const double* censoring,
    const double* frequency, indexType count)
{
    if (!std::isfinite(scale) || scale <= 0) {
        return std::numeric_limits<double>::infinity();
    }
    double logLikelihood = 0;
    double scale2 = scale * scale;
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double weight = frequency[k];
        if (weight == 0) {
            continue;
        }
        double z = -(xv * xv) / (2.0 * scale2);
        if (censoring[k] == 0) {
            if (xv == 0) {
                return std::numeric_limits<double>::infinity();
            }
            logLikelihood += weight * (std::log(xv) - 2.0 * std::log(scale) + z);
        } else {
            logLikelihood += weight * z;
        }
    }
    return -logLikelihood;
}
//=============================================================================
static double
rayleighVariance(double scale, const double* x, const double* censoring, const double* frequency,
    indexType count)
{
    double h = std::max(1e-5 * std::abs(scale), 1e-6);
    if (scale - h <= 0) {
        h = scale * 0.5;
    }
    double f0 = rayleighNegativeLogLikelihood(scale, x, censoring, frequency, count);
    double fp = rayleighNegativeLogLikelihood(scale + h, x, censoring, frequency, count);
    double fm = rayleighNegativeLogLikelihood(scale - h, x, censoring, frequency, count);
    double info = (fp - 2.0 * f0 + fm) / (h * h);
    return std::isfinite(info) && info > 0 ? 1.0 / info : std::numeric_limits<double>::quiet_NaN();
}
//=============================================================================
static RayleighColumnEstimate
fitRayleighColumn(const double* x, const double* censoring, const double* frequency, indexType rows,
    indexType column)
{
    indexType offset = column * rows;
    double totalWeight = 0;
    double uncensoredWeight = 0;
    double weightedSquareSum = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[offset + r];
        double weight = frequency[offset + r];
        if (std::isnan(xv)) {
            hasNaN = true;
        }
        totalWeight += weight;
        weightedSquareSum += weight * xv * xv;
        if (censoring[offset + r] == 0 && weight > 0) {
            uncensoredWeight += weight;
        }
    }
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    RayleighColumnEstimate estimate { nanValue, nanValue, nanValue };
    if (hasNaN || totalWeight <= 0 || uncensoredWeight <= 0) {
        return estimate;
    }
    estimate.scale = std::sqrt(weightedSquareSum / (2.0 * uncensoredWeight));
    estimate.negativeLogLikelihood = rayleighNegativeLogLikelihood(
        estimate.scale, x + offset, censoring + offset, frequency + offset, rows);
    estimate.variance = rayleighVariance(
        estimate.scale, x + offset, censoring + offset, frequency + offset, rows);
    return estimate;
}
//=============================================================================
RayleighFitResult
RayleighFit(const ArrayOfVector& args)
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
        RayleighColumnEstimate estimate = fitRayleighColumn(xp, cp, fp, rows, c);
        params[c] = estimate.scale;
        double logSe = estimate.variance >= 0 && estimate.scale > 0
            ? std::sqrt(estimate.variance) / estimate.scale
            : std::numeric_limits<double>::quiet_NaN();
        pci[c * 2] = estimate.scale * std::exp(-z * logSe);
        pci[c * 2 + 1] = estimate.scale * std::exp(z * logSe);
    }
    RayleighFitResult result;
    result.parameterHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 1) : Dimensions(cols, 1), params);
    result.parameterCI
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(2, 1) : Dimensions(2, cols), pci);
    return result;
}
//=============================================================================
static RayleighLikeResult
makeLikeResult(double nlogL, double variance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* varPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    varPtr[0] = variance;
    RayleighLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), varPtr);
    return result;
}
//=============================================================================
RayleighLikeResult
RayleighLike(const ArrayOfVector& args)
{
    double scale = scalarDouble(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"B"));
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
    if (std::isnan(scale)) {
        return makeLikeResult(nanValue, nanValue);
    }
    if (!std::isfinite(scale) || scale <= 0) {
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
    double nlogL = rayleighNegativeLogLikelihood(scale, xp, cp, fp, count);
    if (std::isnan(nlogL)) {
        return makeLikeResult(nlogL, nanValue);
    }
    return makeLikeResult(nlogL, rayleighVariance(scale, xp, cp, fp, count));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
