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
#include "Rand.hpp"
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
static NelsonType
floatingOutputType(const ArrayOfVector& values)
{
    for (const auto& value : values) {
        if (value.isSingleClass()) {
            return NLS_SINGLE;
        }
    }
    return NLS_DOUBLE;
}
//=============================================================================
static void
setFloatingValue(void* ptr, NelsonType outputType, indexType index, double value)
{
    if (outputType == NLS_SINGLE) {
        static_cast<single*>(ptr)[index] = static_cast<single>(value);
    } else {
        static_cast<double*>(ptr)[index] = value;
    }
}
//=============================================================================
static double
getFloatingValue(const void* ptr, NelsonType outputType, indexType index)
{
    if (outputType == NLS_SINGLE) {
        return static_cast<const single*>(ptr)[index];
    }
    return static_cast<const double*>(ptr)[index];
}
//=============================================================================
static Dimensions
commonDimensions(const ArrayOf& a, const ArrayOf& b)
{
    if (!a.isScalar() && !b.isScalar() && !a.dimensions().equals(b.dimensions())) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
    return !a.isScalar() ? a.dimensions() : (!b.isScalar() ? b.dimensions() : Dimensions(1, 1));
}
//=============================================================================
static double
valueAt(const ArrayOf& value, indexType index)
{
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return ptr[value.isScalar() ? 0 : index];
}
//=============================================================================
static double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleArray(value, message);
    return copy.asDoubleScalar(false, false);
}
//=============================================================================
static indexType
sizeValue(double value)
{
    if (!std::isfinite(value) || std::floor(value) != value) {
        Error("Nelson:statistics:invalidSize", "Size must contain integer values.");
    }
    return value < 0 ? 0 : static_cast<indexType>(value);
}
//=============================================================================
static Dimensions
dimensionsFromSizeArgs(const ArrayOfVector& args, sizeType first)
{
    if (args.size() == first) {
        return Dimensions(1, 1);
    }
    if (args.size() == first + 1 && args[first].isRowVector()) {
        ArrayOf dimsArg = asDoubleArray(args[first],
            formatI18nMessage(Nelson::resolveMessageTemplate(
                                  L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                L"Size"));
        const double* ptr = static_cast<const double*>(dimsArg.dataPointer());
        Dimensions dims;
        for (indexType k = 0; k < dimsArg.elementCount(); ++k) {
            dims[k] = sizeValue(ptr[k]);
        }
        if (dims.length() == 1) {
            dims[1] = dims[0];
        }
        dims.simplify();
        return dims;
    }
    Dimensions dims;
    for (sizeType k = first; k < args.size(); ++k) {
        dims[k - first] = sizeValue(scalarDouble(args[k],
            formatI18nMessage(Nelson::resolveMessageTemplate(
                                  L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                L"Size")));
    }
    if (dims.length() == 1) {
        dims[1] = dims[0];
    }
    dims.simplify();
    return dims;
}
//=============================================================================
static void
validateRequestedDimensions(const ArrayOf& value, const Dimensions& dims)
{
    if (!value.isScalar() && !value.dimensions().equals(dims)) {
        Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
    }
}
//=============================================================================
static bool
validN(double n)
{
    return std::isfinite(n) && n >= 1 && std::floor(n) == n;
}
//=============================================================================
static double
unidPdfValue(double x, double n)
{
    if (std::isnan(x) || !validN(n)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (!std::isfinite(x) || std::floor(x) != x || x < 1 || x > n) {
        return 0;
    }
    return 1.0 / n;
}
//=============================================================================
static double
unidCdfValue(double x, double n)
{
    if (std::isnan(x) || !validN(n)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (x < 1) {
        return 0;
    }
    if (x >= n) {
        return 1;
    }
    return std::floor(x) / n;
}
//=============================================================================
static double
unidInvValue(double p, double n)
{
    if (std::isnan(p) || p < 0 || p > 1 || !validN(n)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (p == 0) {
        return 1;
    }
    return std::min(n, std::ceil(p * n));
}
//=============================================================================
static ArrayOf
binaryUnidFunction(const ArrayOfVector& args, const std::wstring& firstMessage, int mode)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf first = asDoubleArray(args[0], firstMessage);
    ArrayOf n = asDoubleArray(args[1],
        resolveMessageTemplate(L"Nelson:statistics:nPositiveIntegerNumeric",
            L"N must be a positive integer numeric array."));
    Dimensions dims = commonDimensions(first, n);
    indexType count = dims.elementCount();
    void* out = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double a = valueAt(first, k);
        double nv = valueAt(n, k);
        double value = mode == 0 ? unidPdfValue(a, nv)
                                 : (mode == 1 ? unidCdfValue(a, nv) : unidInvValue(a, nv));
        setFloatingValue(out, outputType, k, value);
    }
    return ArrayOf(outputType, dims, out);
}
//=============================================================================
ArrayOf
DiscreteUniformPdf(const ArrayOfVector& args)
{
    return binaryUnidFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        0);
}
//=============================================================================
ArrayOf
DiscreteUniformCdf(const ArrayOfVector& args)
{
    return binaryUnidFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"),
        1);
}
//=============================================================================
ArrayOf
DiscreteUniformInv(const ArrayOfVector& args)
{
    return binaryUnidFunction(args,
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"P"),
        2);
}
//=============================================================================
ArrayOf
DiscreteUniformRnd(const ArrayOfVector& args)
{
    ArrayOfVector typeArgs;
    typeArgs << args[0];
    NelsonType outputType = floatingOutputType(typeArgs);
    ArrayOf n = asDoubleArray(args[0],
        resolveMessageTemplate(L"Nelson:statistics:nPositiveIntegerNumeric",
            L"N must be a positive integer numeric array."));
    Dimensions dims = args.size() > 1 ? dimensionsFromSizeArgs(args, 1) : n.dimensions();
    if (args.size() > 1) {
        validateRequestedDimensions(n, dims);
    }
    if (dims.isEmpty(false)) {
        ArrayOf result = ArrayOf::empty(dims);
        result.promoteType(outputType);
        return result;
    }
    ArrayOf r = Rand(dims, outputType);
    void* ptr = r.readWriteDataPointer();
    indexType count = dims.elementCount();
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double value = unidInvValue(getFloatingValue(ptr, outputType, k), valueAt(n, k));
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
UniformStatResult
DiscreteUniformStat(const ArrayOfVector& args)
{
    NelsonType outputType = floatingOutputType(args);
    ArrayOf n = asDoubleArray(args[0],
        resolveMessageTemplate(L"Nelson:statistics:nPositiveIntegerNumeric",
            L"N must be a positive integer numeric array."));
    Dimensions dims = n.dimensions();
    indexType count = dims.elementCount();
    void* meanPtr = ArrayOf::allocateArrayOf(outputType, count);
    void* varPtr = ArrayOf::allocateArrayOf(outputType, count);
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double nv = valueAt(n, k);
        double mean = validN(nv) ? 0.5 * (nv + 1.0) : std::numeric_limits<double>::quiet_NaN();
        double variance = std::isnan(mean) ? mean : (nv * nv - 1.0) / 12.0;
        setFloatingValue(meanPtr, outputType, k, mean);
        setFloatingValue(varPtr, outputType, k, variance);
    }
    UniformStatResult result;
    result.mean = ArrayOf(outputType, dims, meanPtr);
    result.variance = ArrayOf(outputType, dims, varPtr);
    return result;
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
static bool
validObservation(double value)
{
    return std::isfinite(value) && value >= 1 && std::floor(value) == value;
}
//=============================================================================
static void
validateObservations(const ArrayOf& x)
{
    const double* ptr = static_cast<const double*>(x.dataPointer());
    for (indexType k = 0; k < x.elementCount(); ++k) {
        double value = ptr[k];
        if (!std::isnan(value) && !validObservation(value)) {
            Error("Nelson:statistics:xMustBePositiveInteger",
                "X must contain positive integer finite values.");
        }
    }
}
//=============================================================================
static Dimensions
fitOutputDimensions(const Dimensions& dims)
{
    return Dimensions(1, dims.columns());
}
//=============================================================================
DiscreteUniformFitResult
DiscreteUniformFit(const ArrayOfVector& args)
{
    ArrayOf x = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false) || x.dimensions().length() > 2) {
        Error("Nelson:statistics:xMustBeNonemptyVectorOrMatrix",
            "X must be a nonempty vector or matrix.");
    }
    validateObservations(x);
    double alpha = alphaValue(args);
    Dimensions dims = x.dimensions();
    bool vectorInput = x.isVector();
    indexType rows = vectorInput ? x.elementCount() : dims.rows();
    indexType cols = vectorInput ? 1 : dims.columns();
    Dimensions outDims = vectorInput ? Dimensions(1, 1) : fitOutputDimensions(dims);
    const double* xp = static_cast<const double*>(x.dataPointer());
    double* nHat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols));
    double* nCI = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, cols * 2));
#if WITH_OPENMP
#pragma omp parallel for if (cols > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType c = 0; c < (ompIndexType)cols; ++c) {
        double mx = 0;
        bool hasNaN = false;
        for (indexType r = 0; r < rows; ++r) {
            double value = xp[c * rows + r];
            if (std::isnan(value)) {
                hasNaN = true;
            } else {
                mx = std::max(mx, value);
            }
        }
        if (hasNaN || mx < 1) {
            nHat[c] = std::numeric_limits<double>::quiet_NaN();
            nCI[c] = nHat[c];
            nCI[c + cols] = nHat[c];
            continue;
        }
        nHat[c] = mx;
        nCI[c] = mx;
        double upper = alpha == 0 ? std::numeric_limits<double>::infinity()
                                  : std::floor(mx * std::pow(alpha, -1.0 / rows));
        nCI[c + cols] = std::max(mx, upper);
    }
    DiscreteUniformFitResult result;
    result.nHat = ArrayOf(NLS_DOUBLE, outDims, nHat);
    result.nCI = ArrayOf(NLS_DOUBLE, Dimensions(cols, 2), nCI);
    return result;
}
//=============================================================================
static DiscreteUniformLikeResult
makeDiscreteUniformLikeResult(double nlogL, double covariance)
{
    double* nlogPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    nlogPtr[0] = nlogL;
    double* covPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 1));
    covPtr[0] = covariance;
    DiscreteUniformLikeResult result;
    result.negativeLogLikelihood = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), nlogPtr);
    result.covariance = ArrayOf(NLS_DOUBLE, Dimensions(1, 1), covPtr);
    return result;
}
//=============================================================================
DiscreteUniformLikeResult
DiscreteUniformLike(const ArrayOfVector& args)
{
    double n = scalarDouble(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"N"));
    ArrayOf x = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"X"));
    if (x.isEmpty(false)) {
        Error("Nelson:validators:mustBeNonempty", "%s must be nonempty.", "X");
    }
    validateObservations(x);
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    if (std::isnan(n)) {
        return makeDiscreteUniformLikeResult(nanValue, nanValue);
    }
    if (!validN(n)) {
        return makeDiscreteUniformLikeResult(std::numeric_limits<double>::infinity(), nanValue);
    }
    const double* xp = static_cast<const double*>(x.dataPointer());
    for (indexType k = 0; k < x.elementCount(); ++k) {
        double value = xp[k];
        if (std::isnan(value)) {
            return makeDiscreteUniformLikeResult(nanValue, nanValue);
        }
        if (value > n) {
            return makeDiscreteUniformLikeResult(std::numeric_limits<double>::infinity(), nanValue);
        }
    }
    double nlogL = static_cast<double>(x.elementCount()) * std::log(n);
    return makeDiscreteUniformLikeResult(nlogL, nanValue);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
