//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gpuBesselBuiltin.hpp"
#include "gpuFallbackBuiltin.hpp"
#include "GpuBessel.hpp"
#include "GpuArrayObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// value of a real host scalar (a selector or SCALE argument)
static bool
hostScalar(const ArrayOf& value, double& out)
{
    if (AsGpuArrayObject(value) != nullptr || value.isComplex() || value.isSparse()
        || !value.isScalar()) {
        return false;
    }
    if (value.dataClass() != NLS_DOUBLE && value.dataClass() != NLS_SINGLE) {
        return false;
    }
    ArrayOf v(value);
    v.promoteType(NLS_DOUBLE);
    out = ((const double*)v.dataPointer())[0];
    return true;
}
//=============================================================================
// scale flag at argIn[index] when present; false when it is not 0 or 1
static bool
scaleArgument(const ArrayOfVector& argIn, size_t index, bool& scaled)
{
    scaled = false;
    if (argIn.size() <= index) {
        return true;
    }
    double v = 0;
    if (!hostScalar(argIn[index], v) || (v != 0 && v != 1)) {
        return false;
    }
    scaled = (v == 1);
    return true;
}
//=============================================================================
// device Bessel of kind 0..5 for besselX(nu, Z[, scale]) style arguments
static bool
deviceBessel(int kind, const ArrayOf& nu, const ArrayOf& z, bool scaled, ArrayOf& result)
{
    ArrayOf nuOwner;
    ArrayOf zOwner;
    GpuArrayObject* gNu = GpuEngineGateway::gpuArrayDeviceOperand(nu, nuOwner, false);
    GpuArrayObject* gZ = GpuEngineGateway::gpuArrayDeviceOperand(z, zOwner, false);
    return gNu != nullptr && gZ != nullptr && GpuBessel(kind, gNu, gZ, scaled, result);
}
//=============================================================================
static ArrayOfVector
besselOrHost(Evaluator* eval, int kind, const wchar_t* name, int nLhs, const ArrayOfVector& argIn)
{
    bool scaled = false;
    ArrayOf result;
    if (nLhs <= 1 && (argIn.size() == 2 || argIn.size() == 3) && scaleArgument(argIn, 2, scaled)
        && deviceBessel(kind, argIn[0], argIn[1], scaled, result)) {
        ArrayOfVector retval;
        retval << result;
        return retval;
    }
    return GpuEngineGateway::gpuArrayHostFallback(eval, name, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBesseljBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return besselOrHost(eval, 0, L"besselj", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBesselyBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return besselOrHost(eval, 1, L"bessely", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBesseliBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return besselOrHost(eval, 2, L"besseli", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBesselkBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return besselOrHost(eval, 3, L"besselk", nLhs, argIn);
}
//=============================================================================
// besselh(nu, Z), besselh(nu, K, Z) and besselh(nu, K, Z, scale)
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBesselhBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    double k = 1;
    bool scaled = false;
    bool known = nLhs <= 1 && argIn.size() >= 2 && argIn.size() <= 4;
    if (known && argIn.size() > 2) {
        known = hostScalar(argIn[1], k) && (k == 1 || k == 2) && scaleArgument(argIn, 3, scaled);
    }
    ArrayOf result;
    if (known
        && deviceBessel(
            k == 1 ? 4 : 5, argIn[0], argIn[argIn.size() == 2 ? 1 : 2], scaled, result)) {
        ArrayOfVector retval;
        retval << result;
        return retval;
    }
    return gpuArrayHostFallback(eval, L"besselh", nLhs, argIn);
}
//=============================================================================
// airy(Z), airy(K, Z) and airy(K, Z, scale)
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayAiryBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    double k = 0;
    bool scaled = false;
    bool known = nLhs <= 1 && !argIn.empty() && argIn.size() <= 3;
    if (known && argIn.size() > 1) {
        known = hostScalar(argIn[0], k) && (k == 0 || k == 1 || k == 2 || k == 3)
            && scaleArgument(argIn, 2, scaled);
    }
    ArrayOf result;
    if (known) {
        ArrayOf zOwner;
        GpuArrayObject* gZ = gpuArrayDeviceOperand(argIn[argIn.size() == 1 ? 0 : 1], zOwner, false);
        if (gZ != nullptr && GpuAiry((int)k, gZ, scaled, result)) {
            ArrayOfVector retval;
            retval << result;
            return retval;
        }
    }
    return gpuArrayHostFallback(eval, L"airy", nLhs, argIn);
}
//=============================================================================
