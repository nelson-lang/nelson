//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gpuCastBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
#include "ClassName.hpp"
#include "StringToClass.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
convertTo(int nLhs, const ArrayOfVector& argIn, const std::string& className)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << GpuCast(argIn[0], className);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayDoubleBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "double");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArraySingleBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "single");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayLogicalBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "logical");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInt8Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "int8");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayUint8Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "uint8");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInt16Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "int16");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayUint16Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "uint16");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInt32Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "int32");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayUint32Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "uint32");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInt64Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "int64");
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayUint64Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return convertTo(nLhs, argIn, "uint64");
}
//=============================================================================
// cast(G, className), cast(G, 'like', P) and cast(A, 'like', G)
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCastBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    std::string option = argIn[1].asUtf8String();
    std::string className = option;
    if (argIn.size() == 3) {
        if (option != "like") {
            Error(L"Nelson:cast:invalidOption", L"'like' expected as second argument.");
        }
        GpuArrayObject* prototype = AsGpuArrayObject(argIn[2]);
        className = prototype ? prototype->underlyingType() : ClassName(argIn[2]);
    }
    ArrayOfVector retval;
    if (AsGpuArrayObject(argIn[0]) != nullptr) {
        retval << GpuCast(argIn[0], className);
        return retval;
    }
    // host value with a gpuArray prototype: convert on the host, then upload
    bool haveError = false;
    NelsonType hostClass = StringToClass(className == "double" ? "single" : className, haveError);
    if (haveError) {
        Error(std::string("Nelson:gpuArray:castUnsupported"),
            std::string("Conversion of a gpuArray to %s is not supported."), className);
    }
    ArrayOf host(argIn[0]);
    host.promoteType(hostClass);
    retval << CreateGpuArray(host);
    return retval;
}
//=============================================================================
