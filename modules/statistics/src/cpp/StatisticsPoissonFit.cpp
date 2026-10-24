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
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
struct PoissonColumnEstimate
{
    double lambda;
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
static PoissonColumnEstimate
fitPoissonColumn(const double* x, indexType rows, indexType column)
{
    indexType offset = column * rows;
    double sum = 0;
    bool hasNaN = false;
    for (indexType r = 0; r < rows; ++r) {
        double xv = x[offset + r];
        if (std::isnan(xv)) {
            hasNaN = true;
        }
        sum += xv;
    }
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    if (hasNaN || rows == 0) {
        return { nanValue, nanValue };
    }
    double lambda = sum / static_cast<double>(rows);
    return { lambda, lambda / static_cast<double>(rows) };
}
//=============================================================================
PoissonFitResult
PoissonFit(const ArrayOfVector& args)
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
    double* lambda = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols));
    double* ci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 2));
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        PoissonColumnEstimate estimate = fitPoissonColumn(xp, rows, c);
        lambda[c] = estimate.lambda;
        double count = estimate.lambda * static_cast<double>(rows);
        if (std::isnan(estimate.lambda)) {
            ci[2 * c] = estimate.lambda;
            ci[2 * c + 1] = estimate.lambda;
        } else if (count == 0) {
            ci[2 * c] = 0;
            ci[2 * c + 1] = 0.5 * ChiSquareInv(1.0 - alpha / 2.0, 2.0) / static_cast<double>(rows);
        } else {
            ci[2 * c] = 0.5 * ChiSquareInv(alpha / 2.0, 2.0 * count) / static_cast<double>(rows);
            ci[2 * c + 1] = 0.5 * ChiSquareInv(1.0 - alpha / 2.0, 2.0 * (count + 1.0))
                / static_cast<double>(rows);
        }
    }
    PoissonFitResult result;
    result.lambdaHat
        = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 1) : Dimensions(1, cols), lambda);
    result.lambdaCI = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(2, 1) : Dimensions(2, cols), ci);
    return result;
}
//=============================================================================
static double
poissonNegativeLogLikelihood(double lambda, const double* x, indexType count)
{
    if (std::isnan(lambda)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!std::isfinite(lambda) || lambda < 0) {
        return std::numeric_limits<double>::infinity();
    }
    double nlogL = 0;
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        if (lambda == 0) {
            if (xv != 0) {
                return std::numeric_limits<double>::infinity();
            }
            continue;
        }
        nlogL += lambda - xv * std::log(lambda) + std::lgamma(xv + 1.0);
    }
    return nlogL;
}
//=============================================================================
static PoissonLikeResult
makeLikeResult(double nlogL, double variance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* varPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    varPtr[0] = variance;
    PoissonLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), varPtr);
    return result;
}
//=============================================================================
PoissonLikeResult
PoissonLike(const ArrayOfVector& args)
{
    double lambda = scalarDouble(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"LAMBDA"));
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    validateDataValues(x);
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    if (std::isnan(lambda)) {
        return makeLikeResult(nanValue, nanValue);
    }
    if (!std::isfinite(lambda) || lambda < 0) {
        return makeLikeResult(std::numeric_limits<double>::infinity(), nanValue);
    }
    indexType count = x.elementCount();
    ArrayOf flatX = x;
    Dimensions flatDims(count, 1);
    flatX.reshape(flatDims);
    const double* xp = static_cast<const double*>(flatX.dataPointer());
    double nlogL = poissonNegativeLogLikelihood(lambda, xp, count);
    if (std::isnan(nlogL) || std::isinf(nlogL)) {
        return makeLikeResult(nlogL, nanValue);
    }
    double variance = lambda > 0 ? lambda / static_cast<double>(count) : 0;
    return makeLikeResult(nlogL, variance);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
