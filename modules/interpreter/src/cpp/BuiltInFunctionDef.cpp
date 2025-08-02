//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "OverloadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BuiltInFunctionDef::BuiltInFunctionDef(bool isOverload) : FunctionDef(isOverload)
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
    if (eval->withOverload && inputs.size() > 0 && !this->isOverload()
        && this->overloadAutoMode == NLS_OVERLOAD_AUTO_ON) {
        bool wasFound = false;
        ArrayOfVector res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), nargout, inputs,
            getName(), ClassName(inputs[0]), inputs[0].getDataClass(), wasFound);
        if (wasFound) {
            return res;
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
