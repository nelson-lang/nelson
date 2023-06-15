//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BuiltInFunctionDef.hpp"
#include "EvaluateBuiltinCatchRuntimeException.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "NelsonGateway.hpp"
#include "ClassName.hpp"
#include "ClassToString.hpp"
#include "FunctionsInMemory.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BuiltInFunctionDef::BuiltInFunctionDef()
{
    fileName.clear();
    retCount = 0;
    argCount = 0;
    fptr = nullptr;
    builtinPrototype = (size_t)BUILTIN_PROTOTYPE::CPP_BUILTIN;
    interleavedComplex = true;
}
//=============================================================================
BuiltInFunctionDef::~BuiltInFunctionDef() = default;
//=============================================================================
ArrayOfVector
BuiltInFunctionDef::evaluateFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    lock();
    if (this->getName()[0] != OVERLOAD_SYMBOL_CHAR && eval->isOverloadAllowed()
        && inputs.size() > 0) {
        NelsonType destinationType = inputs[0].getDataClass();
        std::string destinationTypeName;
        if (destinationType >= NLS_STRUCT_ARRAY) {
            destinationTypeName = ClassName(inputs[0]);
        } else {
            destinationTypeName = ClassToString(destinationType);
        }

        FunctionDef* funcDef = nullptr;
        std::string overloadTypeName = overloadFunctionName(destinationTypeName, getName());
        if (!FunctionsInMemory::getInstance()->find(
                overloadTypeName, funcDef, FunctionsInMemory::ALL)) {
            eval->getContext()->lookupFunction(overloadTypeName, funcDef, false);
        }

        bool isSameBuiltin = false;
        if (funcDef && funcDef->type() == NLS_BUILT_IN_FUNCTION) {
            BuiltInFunctionDef* ptrBuiltin = static_cast<BuiltInFunctionDef*>(funcDef);
            BuiltInFunctionDef* ptrBuiltinThis = static_cast<BuiltInFunctionDef*>(this);
            isSameBuiltin = (ptrBuiltin->fptr == ptrBuiltinThis->fptr);
        }
        if (funcDef && !isSameBuiltin) {
            return funcDef->evaluateFunction(eval, inputs, nargout);
        }
    }
    ArrayOfVector outputs;
    eval->callstack.pushDebug(this->getName(), std::string("built-in ") + this->getName());
    size_t stackDepth = eval->callstack.size();
    uint64 tic = 0;
    try {
        tic = Profiler::getInstance()->tic();
        outputs = EvaluateBuiltinCatchRuntimeException(
            eval, fptr, inputs, nargout, builtinPrototype, interleavedComplex);
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, this->getName(), this->getFilename());
            Profiler::getInstance()->toc(tic, stack);
        }
        while (eval->callstack.size() > stackDepth) {
            eval->callstack.pop_back();
        }
        eval->callstack.popDebug();
        return outputs;
    } catch (const Exception&) {
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, this->getName(), this->getFilename());
            Profiler::getInstance()->toc(tic, stack);
        }
        while (eval->callstack.size() > stackDepth) {
            eval->callstack.pop_back();
        }
        eval->callstack.popDebug();
        throw;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
