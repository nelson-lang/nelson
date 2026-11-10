//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gpuFallbackBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuTransfer.hpp"
#include "GpuElementwiseMath.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FunctionDef.hpp"
#include "Context.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// a gpuArray (also inside a cell) becomes its host copy
static ArrayOf
toHost(const ArrayOf& value, bool& hadGpu)
{
    GpuArrayObject* gpuObject = AsGpuArrayObject(value);
    if (gpuObject != nullptr) {
        hadGpu = true;
        return GatherGpuArray(gpuObject);
    }
    if (value.isCell() && !value.isEmpty()) {
        indexType n = value.elementCount();
        const ArrayOf* elements = (const ArrayOf*)value.dataPointer();
        ArrayOf* copied = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, n);
        ArrayOf result(NLS_CELL_ARRAY, value.dimensions(), copied);
        for (indexType k = 0; k < n; ++k) {
            copied[k] = toHost(elements[k], hadGpu);
        }
        return result;
    }
    return value;
}
//=============================================================================
// numeric results go back to the device; answers to a yes/no question stay on the host
static bool
belongsOnDevice(const ArrayOf& value)
{
    if (value.isSparse() || !(value.isNumeric() || value.isLogical())) {
        return false;
    }
    if (value.isLogical() && value.isScalar()) {
        return false;
    }
    switch (value.dataClass()) {
    case NLS_INT64:
    case NLS_UINT64:
        return false;
    default:
        return true;
    }
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayHostFallback(
    Evaluator* eval, const std::wstring& name, int nLhs, const ArrayOfVector& argIn)
{
    FunctionDefPtr funcDef = nullptr;
    if (!eval->getContext()->lookupFunction(name, funcDef) || funcDef == nullptr) {
        Error(std::wstring(L"Nelson:gpuArray:noHostFunction"),
            std::wstring(L"No function '%s' to run on the host."), name);
    }
    bool hadGpu = false;
    ArrayOfVector hostArgs;
    for (const ArrayOf& arg : argIn) {
        hostArgs << toHost(arg, hadGpu);
    }
    ArrayOfVector outputs = funcDef->evaluateFunction(eval, hostArgs, nLhs);
    for (ArrayOf& output : outputs) {
        if (hadGpu && belongsOnDevice(output)) {
            output = CreateGpuArray(output);
        }
    }
    return outputs;
}
//=============================================================================
#define GPU_FALLBACK_DEFINE(NAME)                                                                  \
    ArrayOfVector Nelson::GpuEngineGateway::gpuArrayFallback_##NAME(                               \
        Evaluator* eval, int nLhs, const ArrayOfVector& argIn)                                     \
    {                                                                                              \
        return gpuArrayHostFallback(eval, L## #NAME, nLhs, argIn);                                 \
    }
GPU_FALLBACK_FUNCTIONS(GPU_FALLBACK_DEFINE)
#undef GPU_FALLBACK_DEFINE
//=============================================================================
static ArrayOfVector
mathOrHost(Evaluator* eval, const char* name, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf result;
    if (nLhs <= 1 && GpuElementwiseMath(name, argIn, result)) {
        ArrayOfVector retval;
        retval << result;
        return retval;
    }
    std::string ascii(name);
    return Nelson::GpuEngineGateway::gpuArrayHostFallback(
        eval, std::wstring(ascii.begin(), ascii.end()), nLhs, argIn);
}
//=============================================================================
// gammainc(x, a) and gammainc(x, a, 'lower' | 'upper'); a > 100 and other forms on the host
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayGammaincBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    std::string name = "gammainc";
    bool known = argIn.size() == 2;
    if (argIn.size() == 3
        && (argIn[2].isRowVectorCharacterArray() || argIn[2].isScalarStringArray())) {
        std::string tail = argIn[2].asUtf8String();
        known = tail == "lower" || tail == "upper";
        if (tail == "upper") {
            name = "gammainc:upper";
        }
    }
    ArrayOf result;
    if (known && nLhs <= 1) {
        ArrayOfVector operands;
        operands << argIn[0] << argIn[1];
        if (GpuElementwiseMath(name, operands, result)) {
            ArrayOfVector retval;
            retval << result;
            return retval;
        }
    }
    return gpuArrayHostFallback(eval, L"gammainc", nLhs, argIn);
}
//=============================================================================
// betainc(x, a, b) and betainc(x, a, b, 'lower' | 'upper')
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBetaincBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool upper = false;
    bool known = argIn.size() == 3;
    if (argIn.size() == 4
        && (argIn[3].isRowVectorCharacterArray() || argIn[3].isScalarStringArray())) {
        std::string tail = argIn[3].asUtf8String();
        known = tail == "lower" || tail == "upper";
        upper = tail == "upper";
    }
    if (known && nLhs <= 1) {
        ArrayOf owners[3];
        GpuArrayObject* operands[3] = { nullptr, nullptr, nullptr };
        bool ok = true;
        for (int k = 0; k < 3 && ok; ++k) {
            operands[k] = gpuArrayDeviceOperand(argIn[k], owners[k], false);
            ok = operands[k] != nullptr;
        }
        ArrayOf result;
        if (ok && GpuBetainc(operands[0], operands[1], operands[2], upper, result)) {
            ArrayOfVector retval;
            retval << result;
            return retval;
        }
    }
    return gpuArrayHostFallback(eval, L"betainc", nLhs, argIn);
}
//=============================================================================
#define GPU_MATH_DEFINE(NAME)                                                                      \
    ArrayOfVector Nelson::GpuEngineGateway::gpuArrayMath_##NAME(                                   \
        Evaluator* eval, int nLhs, const ArrayOfVector& argIn)                                     \
    {                                                                                              \
        return mathOrHost(eval, #NAME, nLhs, argIn);                                               \
    }
GPU_ELEMENTWISE_MATH_FUNCTIONS(GPU_MATH_DEFINE)
#undef GPU_MATH_DEFINE
//=============================================================================
GpuArrayObject*
Nelson::GpuEngineGateway::gpuArrayDeviceOperand(
    const ArrayOf& value, ArrayOf& owner, bool allowLogical)
{
    GpuArrayObject* g = AsGpuArrayObject(value);
    if (g != nullptr) {
        return g;
    }
    NelsonType c = value.dataClass();
    bool accepted = c == NLS_DOUBLE || c == NLS_SINGLE || (allowLogical && c == NLS_LOGICAL);
    if (!accepted || value.isComplex() || value.isSparse()) {
        return nullptr;
    }
    owner = CreateGpuArray(value);
    return AsGpuArrayObject(owner);
}
//=============================================================================
// __gpuArray_host__(name, args...): the host function on gathered inputs, numeric results back
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayHostCallBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.empty() || !argIn[0].isRowVectorCharacterArray()) {
        Error(L"Nelson:gpuArray:functionNameExpected", L"A function name expected.");
    }
    ArrayOfVector args;
    for (size_t k = 1; k < argIn.size(); ++k) {
        args << argIn[k];
    }
    return gpuArrayHostFallback(eval, argIn[0].asWideString(), nLhs, args);
}
//=============================================================================
