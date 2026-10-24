//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gpuArrayBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuTransfer.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << CreateGpuArray(argIn[0]);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gatherBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    GpuArrayObject* gpuObject = AsGpuArrayObject(argIn[0]);
    if (gpuObject != nullptr) {
        retval << GatherGpuArray(gpuObject);
    } else {
        // gather on a host value is the identity
        retval << argIn[0];
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::isgpuarrayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(AsGpuArrayObject(argIn[0]) != nullptr);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::underlyingTypeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    GpuArrayObject* gpuObject = AsGpuArrayObject(argIn[0]);
    if (gpuObject != nullptr) {
        retval << ArrayOf::characterArray(gpuObject->underlyingType());
    } else {
        retval << ArrayOf::characterArray(ClassName(argIn[0]));
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::isUnderlyingTypeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    std::string expectedType = argIn[1].asUtf8String();
    std::string effectiveType;
    GpuArrayObject* gpuObject = AsGpuArrayObject(argIn[0]);
    if (gpuObject != nullptr) {
        effectiveType = gpuObject->underlyingType();
    } else {
        effectiveType = ClassName(argIn[0]);
    }
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(effectiveType == expectedType);
    return retval;
}
//=============================================================================
