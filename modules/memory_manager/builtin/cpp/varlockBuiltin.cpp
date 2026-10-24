//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "varlockBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "LockVariable.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::varlockBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 2, 2);
    if (!argIn[0].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    std::string scopename = argIn[0].asUtf8String();
    if (!((scopename == "global") || (scopename == "base") || (scopename == "caller")
            || (scopename == "local"))) {
        Error(L"Nelson:memory_manager:invalidScope",
            L"#%d argument must contain a string: \'global\', \'base\', \'local\' or \'caller\' "
            L"expected.",
            1);
    }
    Context* context = eval->getContext();
    Scope* scope = nullptr;
    if (scopename == "global") {
        scope = context->getGlobalScope();
    }
    if (scopename == "base") {
        scope = context->getBaseScope();
    }
    if (scopename == "caller") {
        scope = context->getCallerScope();
    }
    if (scopename == "local") {
        scope = context->getCurrentScope();
    }
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    std::string varname = argIn[1].asUtf8String();
    if (!IsValidVariableName(varname)) {
        Error(L"Nelson:memory_manager:invalidVariableName",
            L"#%d argument must contain a valid variable name.", 2);
    }
    if ((scope != nullptr) && !scope->isVariable(varname)) {
        Error(L"Nelson:memory_manager:variableNotFound",
            L"#%d argument must be an existing variable name.", 2);
    }
    LockVariable(varname, scope);
    return retval;
}
//=============================================================================
