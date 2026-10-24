//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "gpuOperatorsBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
#include "GpuExprCompiler.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
notTranslatable(const std::string& reason = "")
{
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(false);
    // reason is only consulted for diagnostics; the .m ignores it when ok=false
    retval << ArrayOf::characterArray(reason);
    return retval;
}
//=============================================================================
// The class an input contributes to the fused kernel: a gpuArray reports its
// underlyingType, a host array its data class.
static std::string
effectiveClass(const ArrayOf& value)
{
    GpuArrayObject* g = AsGpuArrayObject(value);
    if (g != nullptr) {
        return g->underlyingType();
    }
    switch (value.dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX:
        return "double";
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
        return "single";
    case NLS_LOGICAL:
        return "logical";
    case NLS_INT8:
        return "int8";
    case NLS_UINT8:
        return "uint8";
    case NLS_INT16:
        return "int16";
    case NLS_UINT16:
        return "uint16";
    case NLS_INT32:
        return "int32";
    case NLS_UINT32:
        return "uint32";
    default:
        return "";
    }
}
//=============================================================================
// Small integer classes that stay exact in i32 arithmetic for the safe subset
// (products fit: int16^2 ~ 1.07e9 < 2^31). uint16/int32/uint32 are excluded and
// take the host fallback. Returns false for a non-small-int class.
static bool
smallIntegerRange(const std::string& className, long& lo, long& hi)
{
    if (className == "int8") {
        lo = -128;
        hi = 127;
        return true;
    }
    if (className == "uint8") {
        lo = 0;
        hi = 255;
        return true;
    }
    if (className == "int16") {
        lo = -32768;
        hi = 32767;
        return true;
    }
    return false;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayArrayfunBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 1024);
    nargoutcheck(nLhs, 0, 2);

    // argIn[0] must be an anonymous function handle with a translatable body
    if (!argIn[0].isFunctionHandle()) {
        return notTranslatable("not a function handle");
    }
    function_handle fh = argIn[0].asFunctionHandle();
    auto* funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
    if (funcDef == nullptr || funcDef->type() != NLS_ANONYMOUS_MACRO_FUNCTION) {
        return notTranslatable("not an anonymous function");
    }
    auto* def = static_cast<AnonymousMacroFunctionDef*>(funcDef);

    // any trailing option (UniformOutput / ErrorHandler / char args) -> fall back
    for (size_t k = 1; k < argIn.size(); ++k) {
        if (argIn[k].isRowVectorCharacterArray() || argIn[k].isScalarStringArray()) {
            return notTranslatable("option argument present");
        }
    }

    // Choose the compilation mode from the input classes: all small-integer of
    // one class (int8/uint8/int16) -> saturating integer kernel; all float-like
    // (double/single/logical) -> the f32 kernel; anything else (uint16, int32,
    // uint32, or a mix) -> host fallback.
    bool integerMode = true;
    bool floatMode = true;
    std::string integerClass;
    for (size_t k = 1; k < argIn.size(); ++k) {
        std::string cls = effectiveClass(argIn[k]);
        long lo = 0;
        long hi = 0;
        if (smallIntegerRange(cls, lo, hi)) {
            if (integerClass.empty()) {
                integerClass = cls;
            } else if (integerClass != cls) {
                integerMode = false; // mixed integer classes are not accelerated
            }
            floatMode = false;
        } else if (cls == "double" || cls == "single" || cls == "logical") {
            integerMode = false;
        } else {
            integerMode = false;
            floatMode = false;
        }
    }
    if (!integerMode && !floatMode) {
        return notTranslatable("input classes not supported by the fused kernel");
    }

    long lo = 0;
    long hi = 0;
    GpuExprCompileResult compiled;
    if (integerMode) {
        smallIntegerRange(integerClass, lo, hi);
        compiled = GpuCompileAnonymousBodyInteger(def, lo, hi);
    } else {
        compiled = GpuCompileAnonymousBody(def);
    }
    if (!compiled.ok) {
        return notTranslatable(compiled.failureReason);
    }
    // one array input per @(...) parameter
    if (argIn.size() - 1 != compiled.parameterCount) {
        return notTranslatable("input count does not match parameter count");
    }

    std::vector<GpuArrayObject*> inputs;
    std::vector<GpuArrayObject*> temporaries;
    ArrayOf result;
    try {
        for (size_t k = 1; k < argIn.size(); ++k) {
            GpuArrayObject* g = AsGpuArrayObject(argIn[k]);
            if (g == nullptr) {
                g = UploadGpuArray(argIn[k]); // host input uploaded as a temporary
                temporaries.push_back(g);
            }
            inputs.push_back(g);
        }
        // arrayfun requires all inputs to have the same size (no broadcasting).
        // On a mismatch, fall back to the host arrayfun for a matching error.
        for (size_t k = 1; k < inputs.size(); ++k) {
            if (!inputs[k]->dimensions().equals(inputs[0]->dimensions())) {
                for (auto* t : temporaries) {
                    delete t;
                }
                return notTranslatable("inputs must have the same size");
            }
        }
        if (integerMode) {
            result = GpuArrayfunInteger(
                compiled.wgslExpr, compiled.logicalResult, integerClass, inputs);
        } else {
            result = GpuArrayfun(compiled.wgslExpr, compiled.logicalResult, inputs);
        }
    } catch (...) {
        for (auto* t : temporaries) {
            delete t;
        }
        throw;
    }
    for (auto* t : temporaries) {
        delete t;
    }
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(true);
    retval << result;
    return retval;
}
//=============================================================================
