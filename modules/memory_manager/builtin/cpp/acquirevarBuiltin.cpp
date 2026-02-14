//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "acquirevarBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::acquirevarBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 3);
    if (!argIn[0].isRowVectorCharacterArray()) {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 1, NLS_STRING_ARRAY_STR);
    }
    std::string scopename = argIn[0].getContentAsCString();
    if (!((scopename == "global") || (scopename == "base") || (scopename == "caller")
            || (scopename == "local"))) {
        raiseError(L"Nelson:memory_manager:ERROR_1_ARGUMENT_MUST_CONTAIN_A_STRING_GLOBAL_BASE_"
                   L"LOCAL_OR_CALLER",
            ERROR_1_ARGUMENT_MUST_CONTAIN_A_STRING_GLOBAL_BASE_LOCAL_OR_CALLER);
    }
    if (!argIn[1].isRowVectorCharacterArray()) {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 2, NLS_STRING_ARRAY_STR);
    }
    std::string varname = argIn[1].getContentAsCString();
    if (!IsValidVariableName(varname)) {
        raiseError(L"Nelson:memory_manager:ERROR_2_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
            ERROR_2_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
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
    ArrayOf value;
    bool bFind = false;
    if (scope != nullptr) {
        bFind = scope->lookupVariable(varname, value);
    }
    if (argIn.size() == 2) {
        if (bFind) {
            retval << value;
        } else {
            raiseError(L"Nelson:memory_manager:ERROR_VARIABLE_NOT_FOUND", ERROR_VARIABLE_NOT_FOUND);
        }
    } else // argIn.size() == 3
    {
        if (bFind) {
            retval << value;
        } else {
            retval << argIn[2];
        }
    }
    return retval;
}
//=============================================================================
