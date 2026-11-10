//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gpuLinearAlgebraBuiltin.hpp"
#include "gpuFallbackBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuElementwiseKernels.hpp"
#include "GpuKernels.hpp"
#include "GpuLinearAlgebra.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
ArrayOfVector
host(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn)
{
    return GpuEngineGateway::gpuArrayHostFallback(eval, name, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
one(const ArrayOf& value)
{
    ArrayOfVector r;
    r << value;
    return r;
}
//=============================================================================
// real single matrix on the device, or null
GpuArrayObject*
matrix(const ArrayOf& value, ArrayOf& owner)
{
    GpuArrayObject* g = GpuEngineGateway::gpuArrayDeviceOperand(value, owner, false);
    return GpuIsRealSingle(g) && g->dimensions().length() <= 2 ? g : nullptr;
}
//=============================================================================
// a \ b: scalar a divides element-wise, square a is solved on the device
bool
leftDivide(const ArrayOf& left, const ArrayOf& right, ArrayOf& result)
{
    ArrayOf ownerA, ownerB;
    GpuArrayObject* a = matrix(left, ownerA);
    GpuArrayObject* b = matrix(right, ownerB);
    if (a == nullptr || b == nullptr) {
        return false;
    }
    if (a->elementCount() == 1) {
        result = GpuLdivide(a, b);
        return true;
    }
    return GpuSolveSquare(a, b, result);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayDetBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf owner, result;
    GpuArrayObject* a = argIn.size() == 1 ? matrix(argIn[0], owner) : nullptr;
    if (a && nLhs <= 1 && GpuDet(a, result)) {
        return one(result);
    }
    return host(eval, L"det", nLhs, argIn);
}
//=============================================================================
// lu(A), lu(A, 'vector'), lu(A, 'matrix')
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayLuBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool vectorForm = false;
    bool known = argIn.size() == 1;
    if (argIn.size() == 2
        && (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray())) {
        std::string option = argIn[1].asUtf8String();
        known = option == "vector" || option == "matrix";
        vectorForm = option == "vector";
    }
    ArrayOf owner;
    GpuArrayObject* a = known ? matrix(argIn[0], owner) : nullptr;
    ArrayOfVector outputs;
    if (a && nLhs <= 3 && GpuLu(a, nLhs, vectorForm, outputs)) {
        return outputs;
    }
    return host(eval, L"lu", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInvBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf owner, result;
    GpuArrayObject* a = argIn.size() == 1 ? matrix(argIn[0], owner) : nullptr;
    if (a && nLhs <= 1) {
        if (a->elementCount() == 1) {
            ArrayOf oneValue = GpuFilled(Dimensions(1, 1), 1.0f, "single");
            return one(GpuRdivide(AsGpuArrayObject(oneValue), a));
        }
        if (GpuInv(a, result)) {
            return one(result);
        }
    }
    return host(eval, L"inv", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMldivideBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf result;
    if (argIn.size() == 2 && nLhs <= 1 && leftDivide(argIn[0], argIn[1], result)) {
        return one(result);
    }
    return host(eval, L"mldivide", nLhs, argIn);
}
//=============================================================================
// b / a = (a.' \ b.').'
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMrdivideBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() == 2 && nLhs <= 1) {
        ArrayOf ownerB, ownerA, result;
        GpuArrayObject* b = matrix(argIn[0], ownerB);
        GpuArrayObject* a = matrix(argIn[1], ownerA);
        if (a && b) {
            if (a->elementCount() == 1) {
                return one(GpuRdivide(b, a));
            }
            ArrayOf at = GpuTranspose(a);
            ArrayOf bt = GpuTranspose(b);
            if (GpuSolveSquare(AsGpuArrayObject(at), AsGpuArrayObject(bt), result)) {
                return one(GpuTranspose(AsGpuArrayObject(result)));
            }
        }
    }
    return host(eval, L"mrdivide", nLhs, argIn);
}
//=============================================================================
// linsolve(A, b) without options solves like A \ b for a square A
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayLinsolveBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf ownerA, ownerB, result;
    if (argIn.size() == 2 && nLhs <= 1) {
        GpuArrayObject* a = matrix(argIn[0], ownerA);
        GpuArrayObject* b = matrix(argIn[1], ownerB);
        if (a && b && GpuSolveSquare(a, b, result)) {
            return one(result);
        }
    }
    return host(eval, L"linsolve", nLhs, argIn);
}
//=============================================================================
// chol(A), chol(A, 'upper'), chol(A, 'lower')
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCholBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool lower = false;
    bool known = argIn.size() == 1;
    if (argIn.size() == 2
        && (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray())) {
        std::string option = argIn[1].asUtf8String();
        known = option == "upper" || option == "lower";
        lower = option == "lower";
    }
    ArrayOf owner, result;
    GpuArrayObject* a = known && !argIn.empty() ? matrix(argIn[0], owner) : nullptr;
    if (a && nLhs <= 1 && GpuChol(a, lower, result)) {
        return one(result);
    }
    return host(eval, L"chol", nLhs, argIn);
}
//=============================================================================
// qr(A), qr(A, 0), qr(A, 'econ') with one or two outputs
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayQrBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool economy = false;
    bool known = argIn.size() == 1;
    if (argIn.size() == 2) {
        const ArrayOf& option = argIn[1];
        if (option.isRowVectorCharacterArray() || option.isScalarStringArray()) {
            known = option.asUtf8String() == "econ";
            economy = known;
        } else if (!AsGpuArrayObject(option) && option.isNumeric() && option.isScalar()
            && !option.isComplex() && option.asDoubleScalar() == 0.0) {
            known = true;
            economy = true;
        }
    }
    ArrayOf owner;
    GpuArrayObject* a = known && !argIn.empty() ? matrix(argIn[0], owner) : nullptr;
    ArrayOfVector outputs;
    if (a && GpuQr(a, nLhs, economy, outputs)) {
        return outputs;
    }
    return host(eval, L"qr", nLhs, argIn);
}
//=============================================================================
// [W, V, ok] = __gpuArray_jacobi_svd__(A): the sweeps of the device svd
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayJacobiSvdBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf owner, w, v;
    GpuArrayObject* a = argIn.size() == 1 ? matrix(argIn[0], owner) : nullptr;
    bool ok = a != nullptr && GpuJacobiSvd(a, w, v);
    ArrayOfVector retval;
    retval << w << v << ArrayOf::logicalScalar(ok);
    return retval;
}
//=============================================================================
