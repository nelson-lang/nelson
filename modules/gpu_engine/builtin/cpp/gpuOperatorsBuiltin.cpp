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
#include "GpuElementwiseMath.hpp"
#include "gpuFallbackBuiltin.hpp"
#include <cmath>
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
//=============================================================================
// x.^n with a whole host exponent cannot turn complex: no device-side domain check
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayPowerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* base = AsGpuArrayObject(argIn[0]);
    const ArrayOf& exponent = argIn[1];
    ArrayOfVector retval;
    if (base != nullptr && !base->isComplex() && !base->isRawIntStorage()
        && GpuIntegerResultType(base, base).empty() && AsGpuArrayObject(exponent) == nullptr
        && exponent.isScalar() && !exponent.isComplex() && !exponent.isSparse()
        && (exponent.isDoubleType() || exponent.isSingleType())) {
        double n = exponent.asDoubleScalar();
        ArrayOf result;
        if (n == std::floor(n) && std::fabs(n) <= 1048576.0
            && GpuElementwiseMath("power:wholeexponent", argIn, result)) {
            retval << result;
            return retval;
        }
    }
    retval << binaryGpuOperator(argIn, GpuPower);
    return retval;
}
//=============================================================================
GPU_BINARY_BUILTIN(gpuArrayMtimesBuiltin, GpuMtimes)
//=============================================================================
GPU_UNARY_BUILTIN(gpuArrayRealBuiltin, GpuReal)
GPU_UNARY_BUILTIN(gpuArrayImagBuiltin, GpuImag)
GPU_UNARY_BUILTIN(gpuArrayConjBuiltin, GpuConj)
GPU_UNARY_BUILTIN(gpuArrayAngleBuiltin, GpuAngle)
GPU_UNARY_BUILTIN(gpuArrayUminusBuiltin, GpuUminus)
GPU_UNARY_BUILTIN(gpuArrayAbsBuiltin, GpuAbs)
GPU_UNARY_BUILTIN(gpuArrayExpBuiltin, GpuExp)
GPU_UNARY_BUILTIN(gpuArrayFloorBuiltin, GpuFloor)
GPU_UNARY_BUILTIN(gpuArrayCeilBuiltin, GpuCeil)
GPU_UNARY_BUILTIN(gpuArrayRoundBuiltin, GpuRound)
GPU_UNARY_BUILTIN(gpuArrayFixBuiltin, GpuFix)
GPU_UNARY_BUILTIN(gpuArraySignBuiltin, GpuSign)
GPU_BINARY_BUILTIN(gpuArrayModBuiltin, GpuMod)
GPU_BINARY_BUILTIN(gpuArrayRemBuiltin, GpuRem)
GPU_UNARY_BUILTIN(gpuArrayPow2Builtin, GpuPow2)
GPU_UNARY_BUILTIN(gpuArrayDeg2radBuiltin, GpuDeg2rad)
GPU_UNARY_BUILTIN(gpuArrayRad2degBuiltin, GpuRad2deg)
GPU_UNARY_BUILTIN(gpuArrayIsnanBuiltin, GpuIsNaN)
GPU_UNARY_BUILTIN(gpuArrayIsinfBuiltin, GpuIsInf)
GPU_UNARY_BUILTIN(gpuArrayIsfiniteBuiltin, GpuIsFinite)
//=============================================================================
// integer operands run on the device; whole-valued floats and int8/int16 shifts on the host
static bool
bitwiseOnDevice(const ArrayOfVector& argIn, bool shift)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    GpuArrayObject* b = AsGpuArrayObject(argIn[1]);
    auto isInteger = [shift](GpuArrayObject* g) {
        return g != nullptr && !g->isComplex()
            && (g->isRawIntStorage() || (!shift && !GpuIntegerResultType(g, g).empty()));
    };
    if (shift) {
        return isInteger(a);
    }
    return isInteger(a) || isInteger(b) || (a == nullptr && IS_INTEGER_TYPE(argIn[0].dataClass()))
        || (b == nullptr && IS_INTEGER_TYPE(argIn[1].dataClass()));
}
//=============================================================================
// device floats hold double values: the host bit functions get them back as double
static ArrayOfVector
bitwiseHost(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector args;
    bool device = false;
    for (const ArrayOf& value : argIn) {
        GpuArrayObject* g = AsGpuArrayObject(value);
        if (g != nullptr && !g->isComplex() && g->underlyingType() == "single") {
            ArrayOf host = GatherGpuArray(g);
            host.promoteType(NLS_DOUBLE);
            args << host;
            device = true;
        } else {
            args << value;
        }
    }
    ArrayOfVector results = GpuEngineGateway::gpuArrayHostFallback(eval, name, nLhs, args);
    for (size_t k = 0; device && k < results.size(); ++k) {
        if (AsGpuArrayObject(results[k]) == nullptr
            && (results[k].isNumeric() || results[k].isLogical()) && !results[k].isSparse()
            && results[k].dataClass() != NLS_INT64 && results[k].dataClass() != NLS_UINT64) {
            results[k] = CreateGpuArray(results[k]);
        }
    }
    return results;
}
//=============================================================================
#define GPU_BITWISE_BUILTIN(BUILTIN_NAME, KERNEL_FN, NAME, SHIFT)                                      ArrayOfVector Nelson::GpuEngineGateway::BUILTIN_NAME(                                                  Evaluator* eval, int nLhs, const ArrayOfVector& argIn)                                         {                                                                                                      if (argIn.size() != 2 || nLhs > 1 || !bitwiseOnDevice(argIn, SHIFT)) {                                 return bitwiseHost(eval, NAME, nLhs, argIn);                                                   }                                                                                                  ArrayOfVector retval;                                                                              retval << binaryGpuOperator(argIn, KERNEL_FN);                                                     return retval;                                                                                 }
GPU_BITWISE_BUILTIN(gpuArrayBitandBuiltin, GpuBitand, L"bitand", false)
GPU_BITWISE_BUILTIN(gpuArrayBitorBuiltin, GpuBitor, L"bitor", false)
GPU_BITWISE_BUILTIN(gpuArrayBitxorBuiltin, GpuBitxor, L"bitxor", false)
GPU_BITWISE_BUILTIN(gpuArrayBitshiftBuiltin, GpuBitshift, L"bitshift", true)
#undef GPU_BITWISE_BUILTIN
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
    GpuArrayObject* a = argIn.size() == 1 ? AsGpuArrayObject(argIn[0]) : nullptr;
    // subscripts, values, count and direction come from the host
    if (a == nullptr || nLhs > 1) {
        return gpuArrayHostFallback(eval, L"find", nLhs, argIn);
    }
    ArrayOfVector retval;
    retval << GpuFind(a);
    return retval;
}
//=============================================================================
// max/min: max(A), max(A,[],dim), max(A,B)  (single output only for now)
//=============================================================================
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
