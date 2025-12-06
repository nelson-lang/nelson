//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "onCleanupBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OnCleanupObjectHandle.hpp"
#include "MacroFunctionDef.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::onCleanupBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    // Validate argument type
    const ArrayOf& functionArray = argIn[0];
    if (!functionArray.isFunctionHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
    }

    // Create onCleanup handle
    auto* onCleanupObj = new OnCleanupObjectHandle(functionArray);
    ArrayOf handleArray = ArrayOf::handleConstructor(onCleanupObj);

    // Register cleanup only if inside a macro function
    Context* ctx = eval->getContext();
    FunctionDef* funcDef = nullptr;

    const std::string& currentName = ctx->getCurrentScope()->getName();
    if (ctx->lookupFunction(currentName, funcDef) && funcDef->type() == NLS_MACRO_FUNCTION) {
        auto* macroFunc = static_cast<MacroFunctionDef*>(funcDef);
        macroFunc->addCleanupFunction(handleArray);
    }

    return ArrayOfVector { handleArray };
}
//=============================================================================
