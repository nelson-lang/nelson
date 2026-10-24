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
//=============================================================================
using namespace Nelson;
//=============================================================================
static Dimensions
parseDimensions(const ArrayOfVector& argIn);
static ArrayOfVector
constructorBuiltin(int nLhs, const ArrayOfVector& argIn, float value);
static std::string
resolveTypeAndDims(const ArrayOfVector& argIn, ArrayOfVector& dimArgs);
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
    ArrayOfVector retval;
    retval << GpuRandom(parseDimensions(argIn), false);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayRandnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << GpuRandom(parseDimensions(argIn), true);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayEyeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector dimArgs;
    std::string underlyingType = resolveTypeAndDims(argIn, dimArgs);
    ArrayOfVector retval;
    retval << GpuEye(parseDimensions(dimArgs), underlyingType);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayNanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(argIn), std::nanf(""), "single");
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(argIn), HUGE_VALF, "single");
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayTrueBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(argIn), 1.0f, "logical");
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFalseBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(argIn), 0.0f, "logical");
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
    std::string underlyingType = resolveTypeAndDims(argIn, dimArgs);
    ArrayOfVector retval;
    retval << GpuFilled(parseDimensions(dimArgs), value, underlyingType);
    return retval;
}
//=============================================================================
static std::string
resolveTypeAndDims(const ArrayOfVector& argIn, ArrayOfVector& dimArgs)
{
    if (!argIn.empty()) {
        GpuArrayObject* prototype = AsGpuArrayObject(argIn.back());
        if (prototype != nullptr) {
            for (size_t k = 0; k + 1 < argIn.size(); k++) {
                dimArgs.push_back(argIn[k]);
            }
            return prototype->underlyingType();
        }
    }
    dimArgs = argIn;
    return "single";
}
//=============================================================================
