//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "gpuOperatorsBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
binaryGpuOperator(const ArrayOfVector& argIn, ArrayOf (*kernel)(GpuArrayObject*, GpuArrayObject*));
static ArrayOf
unaryGpuOperator(const ArrayOfVector& argIn, ArrayOf (*kernel)(GpuArrayObject*));
//=============================================================================
// Macro-free but compact builtin definitions
//=============================================================================
#define GPU_BINARY_BUILTIN(BUILTIN_NAME, KERNEL_FN)                                                \
    ArrayOfVector Nelson::GpuEngineGateway::BUILTIN_NAME(                                          \
        Evaluator* eval, int nLhs, const ArrayOfVector& argIn)                                     \
    {                                                                                              \
        nargincheck(argIn, 2, 2);                                                                  \
        nargoutcheck(nLhs, 0, 1);                                                                  \
        ArrayOfVector retval;                                                                      \
        retval << binaryGpuOperator(argIn, KERNEL_FN);                                             \
        return retval;                                                                             \
    }
//=============================================================================
#define GPU_UNARY_BUILTIN(BUILTIN_NAME, KERNEL_FN)                                                 \
    ArrayOfVector Nelson::GpuEngineGateway::BUILTIN_NAME(                                          \
        Evaluator* eval, int nLhs, const ArrayOfVector& argIn)                                     \
    {                                                                                              \
        nargincheck(argIn, 1, 1);                                                                  \
        nargoutcheck(nLhs, 0, 1);                                                                  \
        ArrayOfVector retval;                                                                      \
        retval << unaryGpuOperator(argIn, KERNEL_FN);                                              \
        return retval;                                                                             \
    }
//=============================================================================
GPU_BINARY_BUILTIN(gpuArrayPlusBuiltin, GpuPlus)
GPU_BINARY_BUILTIN(gpuArrayMinusBuiltin, GpuMinus)
GPU_BINARY_BUILTIN(gpuArrayTimesBuiltin, GpuTimes)
GPU_BINARY_BUILTIN(gpuArrayRdivideBuiltin, GpuRdivide)
GPU_BINARY_BUILTIN(gpuArrayLdivideBuiltin, GpuLdivide)
GPU_BINARY_BUILTIN(gpuArrayPowerBuiltin, GpuPower)
GPU_BINARY_BUILTIN(gpuArrayMtimesBuiltin, GpuMtimes)
//=============================================================================
GPU_UNARY_BUILTIN(gpuArrayRealBuiltin, GpuReal)
GPU_UNARY_BUILTIN(gpuArrayImagBuiltin, GpuImag)
GPU_UNARY_BUILTIN(gpuArrayConjBuiltin, GpuConj)
GPU_UNARY_BUILTIN(gpuArrayAngleBuiltin, GpuAngle)
GPU_UNARY_BUILTIN(gpuArrayUminusBuiltin, GpuUminus)
GPU_UNARY_BUILTIN(gpuArrayAbsBuiltin, GpuAbs)
GPU_UNARY_BUILTIN(gpuArraySqrtBuiltin, GpuSqrt)
GPU_UNARY_BUILTIN(gpuArrayExpBuiltin, GpuExp)
GPU_UNARY_BUILTIN(gpuArrayLogBuiltin, GpuLog)
GPU_UNARY_BUILTIN(gpuArraySinBuiltin, GpuSin)
GPU_UNARY_BUILTIN(gpuArrayCosBuiltin, GpuCos)
GPU_UNARY_BUILTIN(gpuArrayTanBuiltin, GpuTan)
GPU_UNARY_BUILTIN(gpuArrayFloorBuiltin, GpuFloor)
GPU_UNARY_BUILTIN(gpuArrayCeilBuiltin, GpuCeil)
GPU_UNARY_BUILTIN(gpuArrayRoundBuiltin, GpuRound)
GPU_UNARY_BUILTIN(gpuArrayFixBuiltin, GpuFix)
GPU_UNARY_BUILTIN(gpuArraySignBuiltin, GpuSign)
GPU_BINARY_BUILTIN(gpuArrayModBuiltin, GpuMod)
GPU_BINARY_BUILTIN(gpuArrayRemBuiltin, GpuRem)
GPU_UNARY_BUILTIN(gpuArraySinhBuiltin, GpuSinh)
GPU_UNARY_BUILTIN(gpuArrayCoshBuiltin, GpuCosh)
GPU_UNARY_BUILTIN(gpuArrayTanhBuiltin, GpuTanh)
GPU_UNARY_BUILTIN(gpuArrayAsinBuiltin, GpuAsin)
GPU_UNARY_BUILTIN(gpuArrayAcosBuiltin, GpuAcos)
GPU_UNARY_BUILTIN(gpuArrayAtanBuiltin, GpuAtan)
GPU_UNARY_BUILTIN(gpuArrayLog2Builtin, GpuLog2)
GPU_UNARY_BUILTIN(gpuArrayLog10Builtin, GpuLog10)
GPU_BINARY_BUILTIN(gpuArrayAtan2Builtin, GpuAtan2)
GPU_BINARY_BUILTIN(gpuArrayHypotBuiltin, GpuHypot)
GPU_UNARY_BUILTIN(gpuArrayExpm1Builtin, GpuExpm1)
GPU_UNARY_BUILTIN(gpuArrayLog1pBuiltin, GpuLog1p)
GPU_UNARY_BUILTIN(gpuArrayPow2Builtin, GpuPow2)
GPU_UNARY_BUILTIN(gpuArraySecBuiltin, GpuSec)
GPU_UNARY_BUILTIN(gpuArrayCscBuiltin, GpuCsc)
GPU_UNARY_BUILTIN(gpuArrayCotBuiltin, GpuCot)
GPU_UNARY_BUILTIN(gpuArrayAsecBuiltin, GpuAsec)
GPU_UNARY_BUILTIN(gpuArrayAcscBuiltin, GpuAcsc)
GPU_UNARY_BUILTIN(gpuArrayAcotBuiltin, GpuAcot)
GPU_UNARY_BUILTIN(gpuArraySechBuiltin, GpuSech)
GPU_UNARY_BUILTIN(gpuArrayCschBuiltin, GpuCsch)
GPU_UNARY_BUILTIN(gpuArrayCothBuiltin, GpuCoth)
GPU_UNARY_BUILTIN(gpuArrayAsinhBuiltin, GpuAsinh)
GPU_UNARY_BUILTIN(gpuArrayAcoshBuiltin, GpuAcosh)
GPU_UNARY_BUILTIN(gpuArrayAtanhBuiltin, GpuAtanh)
GPU_UNARY_BUILTIN(gpuArrayDeg2radBuiltin, GpuDeg2rad)
GPU_UNARY_BUILTIN(gpuArrayRad2degBuiltin, GpuRad2deg)
GPU_UNARY_BUILTIN(gpuArrayIsnanBuiltin, GpuIsNaN)
GPU_UNARY_BUILTIN(gpuArrayIsinfBuiltin, GpuIsInf)
GPU_UNARY_BUILTIN(gpuArrayIsfiniteBuiltin, GpuIsFinite)
GPU_BINARY_BUILTIN(gpuArrayBitandBuiltin, GpuBitand)
GPU_BINARY_BUILTIN(gpuArrayBitorBuiltin, GpuBitor)
GPU_BINARY_BUILTIN(gpuArrayBitxorBuiltin, GpuBitxor)
GPU_BINARY_BUILTIN(gpuArrayBitshiftBuiltin, GpuBitshift)
//=============================================================================
GPU_BINARY_BUILTIN(gpuArrayEqBuiltin, GpuEq)
GPU_BINARY_BUILTIN(gpuArrayNeBuiltin, GpuNe)
GPU_BINARY_BUILTIN(gpuArrayLtBuiltin, GpuLt)
GPU_BINARY_BUILTIN(gpuArrayLeBuiltin, GpuLe)
GPU_BINARY_BUILTIN(gpuArrayGtBuiltin, GpuGt)
GPU_BINARY_BUILTIN(gpuArrayGeBuiltin, GpuGe)
GPU_BINARY_BUILTIN(gpuArrayAndBuiltin, GpuAnd)
GPU_BINARY_BUILTIN(gpuArrayOrBuiltin, GpuOr)
GPU_BINARY_BUILTIN(gpuArrayXorBuiltin, GpuXor)
GPU_UNARY_BUILTIN(gpuArrayNotBuiltin, GpuNot)
//=============================================================================
static int
reductionDim(const ArrayOf& dimArg)
{
    return (int)dimArg.asScalarIndex(false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArraySortBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 2);
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    int dim = -1;
    bool descend = false;
    for (size_t k = 1; k < argIn.size(); ++k) {
        if (argIn[k].isRowVectorCharacterArray()) {
            std::string direction = argIn[k].asUtf8String();
            if (direction == "descend") {
                descend = true;
            } else if (direction == "ascend") {
                descend = false;
            } else {
                Error(L"Nelson:sort:invalidDirection",
                    L"sort: direction must be 'ascend' or 'descend'.");
            }
        } else {
            dim = (int)argIn[k].asScalarIndex(false);
        }
    }
    ArrayOfVector sorted = GpuSort(a, dim, descend);
    ArrayOfVector retval;
    retval << sorted[0];
    if (nLhs >= 2) {
        retval << sorted[1];
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFindBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    ArrayOfVector retval;
    retval << GpuFind(a);
    return retval;
}
//=============================================================================
// max/min: max(A), max(A,[],dim), max(A,B)  (single output only for now)
//=============================================================================
static ArrayOfVector
gpuMaxMin(int nLhs, const ArrayOfVector& argIn, ArrayOf (*reduceKernel)(GpuArrayObject*, int),
    ArrayOfVector (*argReduceKernel)(GpuArrayObject*, int),
    ArrayOf (*binaryKernel)(GpuArrayObject*, GpuArrayObject*))
{
    bool wantIndices = (nLhs >= 2);
    if (argIn.size() == 1) {
        GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
        if (a == nullptr) {
            Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
        }
        if (wantIndices) {
            return argReduceKernel(a, -1);
        }
        ArrayOfVector retval;
        retval << reduceKernel(a, -1);
        return retval;
    }
    if (argIn.size() == 2) {
        if (argIn[1].isEmpty()) {
            Error(L"Nelson:GPU:maxMinInvalidSyntax",
                L"Use the three-argument form (A, [], dim) to reduce along a dimension.");
        }
        if (wantIndices) {
            Error(L"Nelson:GPU:maxMinSingleOutput",
                L"max/min with two arrays returns a single output.");
        }
        ArrayOfVector retval;
        retval << binaryGpuOperator(argIn, binaryKernel);
        return retval;
    }
    if (!argIn[1].isEmpty()) {
        Error(L"Nelson:GPU:maxMinInvalidSecondArgument",
            L"Second argument must be [] when a dimension is specified.");
    }
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    int dim = reductionDim(argIn[2]);
    if (wantIndices) {
        return argReduceKernel(a, dim);
    }
    ArrayOfVector retval;
    retval << reduceKernel(a, dim);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 2);
    return gpuMaxMin(nLhs, argIn, GpuMaxAlong, GpuMaxAlong2, GpuMaxBinary);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 2);
    return gpuMaxMin(nLhs, argIn, GpuMinAlong, GpuMinAlong2, GpuMinBinary);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArraySizeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* gpuObject = AsGpuArrayObject(argIn[0]);
    if (gpuObject == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    Dimensions dims = gpuObject->dimensions();
    indexType dimensionCount = dims.length();
    if (dimensionCount < 2) {
        dimensionCount = 2;
    }
    double* values = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimensionCount);
    for (indexType k = 0; k < dimensionCount; ++k) {
        values[k] = (double)dims.dimensionLength(k);
    }
    Dimensions outputDims(1, dimensionCount);
    ArrayOfVector retval;
    retval << ArrayOf(NLS_DOUBLE, outputDims, values);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIsrealBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* gpuObject = AsGpuArrayObject(argIn[0]);
    if (gpuObject == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(!gpuObject->isComplex());
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayDispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    GpuArrayObject* gpuObject = AsGpuArrayObject(argIn[0]);
    if (gpuObject == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    Interface* io = eval->getInterface();
    if (io != nullptr) {
        Dimensions dims = gpuObject->dimensions();
        std::string message
            = "  " + dims.toString() + " gpuArray (" + gpuObject->underlyingType() + ")\n";
        io->outputMessage(message);
    }
    return {};
}
//=============================================================================
// static helper functions
//=============================================================================
static ArrayOf
binaryGpuOperator(const ArrayOfVector& argIn, ArrayOf (*kernel)(GpuArrayObject*, GpuArrayObject*))
{
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    GpuArrayObject* b = AsGpuArrayObject(argIn[1]);
    GpuArrayObject* temporaryOperand = nullptr;
    if (a == nullptr && b == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    // mixed host/device operand: upload the host side as a temporary
    if (a == nullptr) {
        temporaryOperand = UploadGpuArray(argIn[0]);
        a = temporaryOperand;
    } else if (b == nullptr) {
        temporaryOperand = UploadGpuArray(argIn[1]);
        b = temporaryOperand;
    }
    ArrayOf result;
    try {
        result = kernel(a, b);
    } catch (...) {
        delete temporaryOperand;
        throw;
    }
    delete temporaryOperand;
    return result;
}
//=============================================================================
static ArrayOf
unaryGpuOperator(const ArrayOfVector& argIn, ArrayOf (*kernel)(GpuArrayObject*))
{
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    return kernel(a);
}
//=============================================================================
