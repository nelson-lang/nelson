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
static Dimensions
commonDimensions(const ArrayOfVector& values)
{
    Dimensions dims(1, 1);
    for (const auto& value : values) {
        if (!value.isScalar()) {
            dims = value.dimensions();
            break;
        }
    }
    for (const auto& value : values) {
        if (!value.isScalar() && !value.dimensions().equals(dims)) {
            Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
        }
    }
    return dims;
}
//=============================================================================
static void
validateRequestedDimensions(const ArrayOfVector& values, const Dimensions& dims)
{
    for (const auto& value : values) {
        if (!value.isScalar() && !value.dimensions().equals(dims)) {
            Error("Nelson:dimagree:sizeMismatchInputArguments", "Size mismatch on input arguments");
        }
    }
}
//=============================================================================
static ArrayOf
inverseRandom(
    const ArrayOfVector& params, const Dimensions& dims, NelsonType outputType, int distribution)
{
    if (dims.isEmpty(false)) {
        ArrayOf result = ArrayOf::empty(dims);
        result.promoteType(outputType);
        return result;
    }
    ArrayOf r = Rand(const_cast<Dimensions&>(dims), outputType);
    void* ptr = r.readWriteDataPointer();
    indexType count = dims.elementCount();
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)count; ++k) {
        double u = getFloatingValue(ptr, outputType, k);
        double value;
        if (distribution == 0) {
            value = ChiSquareInv(u, valueAt(params[0], k));
        } else if (distribution == 1) {
            double nu = valueAt(params[0], k);
            value = std::isinf(nu) ? NormalInv01(u) : StudentTInv(u, nu);
        } else {
            value = FInv(u, valueAt(params[0], k), valueAt(params[1], k));
        }
        setFloatingValue(ptr, outputType, k, value);
    }
    return r;
}
//=============================================================================
ArrayOf
ChiSquareRnd(const ArrayOfVector& args)
{
    ArrayOf nu = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V"));
    ArrayOfVector params;
    params << nu;
    ArrayOfVector typeArgs;
    typeArgs << args[0];
    NelsonType outputType = floatingOutputType(typeArgs);
    Dimensions dims = args.size() > 1 ? dimensionsFromSizeArgs(args, 1) : commonDimensions(params);
    if (args.size() > 1) {
        validateRequestedDimensions(params, dims);
    }
    return inverseRandom(params, dims, outputType, 0);
}
//=============================================================================
ArrayOf
TRnd(const ArrayOfVector& args)
{
    ArrayOf nu = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V"));
    ArrayOfVector params;
    params << nu;
    ArrayOfVector typeArgs;
    typeArgs << args[0];
    NelsonType outputType = floatingOutputType(typeArgs);
    Dimensions dims = args.size() > 1 ? dimensionsFromSizeArgs(args, 1) : commonDimensions(params);
    if (args.size() > 1) {
        validateRequestedDimensions(params, dims);
    }
    return inverseRandom(params, dims, outputType, 1);
}
//=============================================================================
ArrayOf
FRnd(const ArrayOfVector& args)
{
    ArrayOf v1 = asDoubleArray(args[0],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V1"));
    ArrayOf v2 = asDoubleArray(args[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:statistics:realNumeric", L"%s must be real numeric."),
            L"V2"));
    ArrayOfVector params;
    params << v1;
    params << v2;
    ArrayOfVector typeArgs;
    typeArgs << args[0] << args[1];
    NelsonType outputType = floatingOutputType(typeArgs);
    Dimensions dims = args.size() > 2 ? dimensionsFromSizeArgs(args, 2) : commonDimensions(params);
    if (args.size() > 2) {
        validateRequestedDimensions(params, dims);
    }
    return inverseRandom(params, dims, outputType, 2);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
