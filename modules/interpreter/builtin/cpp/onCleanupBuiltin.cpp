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
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OnCleanupObjectHandle.hpp"
#include "Context.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::onCleanupBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    const ArrayOf& functionArray = argIn[0];
    if (!functionArray.isFunctionHandle()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"function handle");
    }

    auto* onCleanupObj = new OnCleanupObjectHandle(functionArray);
    ArrayOf handleArray = ArrayOf::handle(onCleanupObj);

    // The task belongs to the function call being evaluated (its own scope),
    // whatever kind of function it is: it runs when that call exits.
    Context* ctx = eval->getContext();
    Scope* scope = ctx->getCurrentScope();
    if (scope != ctx->getGlobalScope() && scope != ctx->getBaseScope()) {
        scope->addCleanupTask(handleArray);
    }

    return ArrayOfVector { handleArray };
}
//=============================================================================
