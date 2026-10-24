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
static double
geometricNegativeLogLikelihood(double p, const double* x, indexType count)
{
    if (std::isnan(p)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!std::isfinite(p) || p < 0 || p > 1) {
        return std::numeric_limits<double>::infinity();
    }
    double nlogL = 0;
    for (indexType k = 0; k < count; ++k) {
        double xv = x[k];
        if (std::isnan(xv)) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        if (p == 0) {
            return std::numeric_limits<double>::infinity();
        }
        if (p == 1) {
            if (xv != 0) {
                return std::numeric_limits<double>::infinity();
            }
            continue;
        }
        nlogL -= std::log(p) + xv * std::log1p(-p);
    }
    return nlogL;
}
//=============================================================================
static double
geometricVariance(double p, const double* x, indexType count)
{
    if (!(p > 0 && p < 1)) {
        return p == 1 ? 0 : std::numeric_limits<double>::quiet_NaN();
    }
    double sum = 0;
    for (indexType k = 0; k < count; ++k) {
        sum += x[k];
    }
    double n = static_cast<double>(count);
    double info = n / (p * p) + sum / ((1.0 - p) * (1.0 - p));
    return info > 0 ? 1.0 / info : std::numeric_limits<double>::quiet_NaN();
}
//=============================================================================
GeometricFitResult
GeometricFit(const ArrayOfVector& args)
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
    double* phat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols));
    double* pci = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 2));
    double z = NormalInv01(1.0 - alpha / 2.0);
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        indexType offset = c * rows;
        double sum = 0;
        bool hasNaN = false;
        for (indexType r = 0; r < rows; ++r) {
            double xv = xp[offset + r];
            hasNaN = hasNaN || std::isnan(xv);
            sum += xv;
        }
        double p = hasNaN ? std::numeric_limits<double>::quiet_NaN()
                          : static_cast<double>(rows) / (static_cast<double>(rows) + sum);
        phat[c] = p;
        double var = hasNaN ? p : geometricVariance(p, xp + offset, rows);
        if (p > 0 && p < 1 && var >= 0) {
            double logit = std::log(p / (1.0 - p));
            double se = std::sqrt(var) / (p * (1.0 - p));
            double lo = logit - z * se;
            double hi = logit + z * se;
            pci[2 * c] = 1.0 / (1.0 + std::exp(-lo));
            pci[2 * c + 1] = 1.0 / (1.0 + std::exp(-hi));
        } else if (p == 1) {
            pci[2 * c] = 1;
            pci[2 * c + 1] = 1;
        } else {
            pci[2 * c] = p;
            pci[2 * c + 1] = p;
        }
    }
    GeometricFitResult result;
    result.pHat = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(1, 1) : Dimensions(1, cols), phat);
    result.pCI = ArrayOf(NLS_DOUBLE, vectorInput ? Dimensions(2, 1) : Dimensions(2, cols), pci);
    return result;
}
//=============================================================================
static GeometricLikeResult
makeLikeResult(double nlogL, double variance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* varPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    varPtr[0] = variance;
    GeometricLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), varPtr);
    return result;
}
//=============================================================================
GeometricLikeResult
GeometricLike(const ArrayOfVector& args)
{
    double p = scalarDouble(args[0],
        resolveMessageTemplate(
            L"Nelson:statistics:pRealNumericScalar", L"P must be a real numeric scalar."));
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    validateDataValues(x);
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    if (std::isnan(p)) {
        return makeLikeResult(nanValue, nanValue);
    }
    if (!std::isfinite(p) || p < 0 || p > 1) {
        return makeLikeResult(std::numeric_limits<double>::infinity(), nanValue);
    }
    indexType count = x.elementCount();
    ArrayOf flatX = x;
    Dimensions flatDims(count, 1);
    flatX.reshape(flatDims);
    const double* xp = static_cast<const double*>(flatX.dataPointer());
    double nlogL = geometricNegativeLogLikelihood(p, xp, count);
    if (std::isnan(nlogL) || std::isinf(nlogL)) {
        return makeLikeResult(nlogL, nanValue);
    }
    return makeLikeResult(nlogL, geometricVariance(p, xp, count));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
