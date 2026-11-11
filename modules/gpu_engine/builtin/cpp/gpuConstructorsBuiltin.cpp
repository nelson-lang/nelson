//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "gpuConstructorsBuiltin.hpp"
#include "GpuKernels.hpp"
#include "GpuArrayObject.hpp"
#include "GpuTransfer.hpp"
#include "Dimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassName.hpp"
#include "GpuTransfer.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static Dimensions
parseDimensions(const ArrayOfVector& argIn);
static ArrayOfVector
constructorBuiltin(int nLhs, const ArrayOfVector& argIn, float value);
static std::string
resolveTypeAndDims(const ArrayOfVector& argIn, ArrayOfVector& dimArgs, const std::string& fallback);
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayZerosBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return constructorBuiltin(nLhs, argIn, 0.0f);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayOnesBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return constructorBuiltin(nLhs, argIn, 1.0f);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayRandBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    resolveTypeAndDims(argIn, dimArgs, "single");
    ArrayOfVector retval;
    retval << GpuRandom(parseDimensions(dimArgs), false);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayRandnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    resolveTypeAndDims(argIn, dimArgs, "single");
    ArrayOfVector retval;
    retval << GpuRandom(parseDimensions(dimArgs), true);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayEyeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    std::string underlyingType = resolveTypeAndDims(argIn, dimArgs, "single");
    ArrayOfVector retval;
    retval << GpuEye(parseDimensions(dimArgs), underlyingType);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayNanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    resolveTypeAndDims(argIn, dimArgs, "single");
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(dimArgs), std::nanf(""), "single");
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    resolveTypeAndDims(argIn, dimArgs, "single");
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(dimArgs), HUGE_VALF, "single");
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayTrueBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    resolveTypeAndDims(argIn, dimArgs, "single");
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(dimArgs), 1.0f, "logical");
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFalseBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    resolveTypeAndDims(argIn, dimArgs, "single");
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(dimArgs), 0.0f, "logical");
    return retval;
}
//=============================================================================
// static helper functions
//=============================================================================
static Dimensions
parseDimensions(const ArrayOfVector& argIn)
{
    Dimensions dims;
    if (argIn.empty()) {
        dims[0] = 1;
        dims[1] = 1;
        return dims;
    }
    if (argIn.size() == 1) {
        ArrayOf arg = argIn[0];
        if (!arg.isNumeric() || arg.isSparse() || !arg.isRowVector()) {
            Error(L"Nelson:gpuArray:invalidSize", L"Size vector or scalar dimensions expected.");
        }
        if (arg.isScalar()) {
            indexType n = arg.asScalarIndex(true, true, true);
            dims[0] = n;
            dims[1] = n;
            return dims;
        }
        if (arg.isEmpty() || arg.elementCount() >= Nelson::maxDims) {
            Error(L"Nelson:gpuArray:invalidSize", L"Size vector or scalar dimensions expected.");
        }
        ArrayOf dimVector(arg);
        auto* ptrValues = static_cast<indexType*>(dimVector.indexPointer());
        ompIndexType elementCount = dimVector.elementCount();
        for (ompIndexType k = 0; k < elementCount; k++) {
            dims[k] = static_cast<indexType>(ptrValues[k]);
        }
        if (dims.length() == 1) {
            dims[1] = dims[0];
        }
        return dims;
    }
    for (size_t k = 0; k < argIn.size(); k++) {
        dims[k] = argIn[k].asScalarIndex(true, true, true);
    }
    if (dims.length() == 1) {
        dims[1] = dims[0];
    }
    return dims;
}
//=============================================================================
static ArrayOfVector
constructorBuiltin(int nLhs, const ArrayOfVector& argIn, float value)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    // a trailing gpuArray prototype ('like' form) sets the class; otherwise the
    // device default (single) is used
    std::string underlyingType = resolveTypeAndDims(argIn, dimArgs, "single");
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(dimArgs), value, underlyingType);
    return retval;
}
//=============================================================================
// splits sizes from the class options: 'like', P / a trailing prototype / class names / 'gpuArray'
static std::string
resolveTypeAndDims(const ArrayOfVector& argIn, ArrayOfVector& dimArgs, const std::string& fallback)
{
    std::string underlyingType = fallback;
    size_t end = argIn.size();
    if (end >= 2 && argIn[end - 2].isRowVectorCharacterArray()
        && argIn[end - 2].asUtf8String() == "like") {
        GpuArrayObject* prototype = AsGpuArrayObject(argIn[end - 1]);
        underlyingType = prototype ? prototype->underlyingType() : ClassName(argIn[end - 1]);
        end -= 2;
    } else if (end >= 1 && AsGpuArrayObject(argIn[end - 1]) != nullptr && end > 1) {
        underlyingType = AsGpuArrayObject(argIn[end - 1])->underlyingType();
        end -= 1;
    }
    while (end > 0
        && (argIn[end - 1].isRowVectorCharacterArray() || argIn[end - 1].isScalarStringArray())) {
        std::string option = argIn[end - 1].asUtf8String();
        if (option != "gpuArray") {
            underlyingType = (option == "double") ? "single" : option;
        }
        end -= 1;
    }
    for (size_t k = 0; k < end; k++) {
        GpuArrayObject* size = AsGpuArrayObject(argIn[k]);
        dimArgs.push_back(size ? GatherGpuArray(size) : argIn[k]);
    }
    return (underlyingType == "double") ? "single" : underlyingType;
}
//=============================================================================
