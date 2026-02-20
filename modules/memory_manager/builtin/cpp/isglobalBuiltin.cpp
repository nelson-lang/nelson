//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isglobalBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::isglobalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    Context* ctxt = eval->getContext();
    Scope* globalScope = ctxt->getGlobalScope();
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    if (!argIn[0].isRowVectorCharacterArray()) {
        raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), 1, NLS_STRING_ARRAY_STR);
    }
    std::string arg = argIn[0].getContentAsCString();
    if (!IsValidVariableName(arg)) {
        raiseError(L"Nelson:memory_manager:ERROR_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
            ERROR_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
    }
    bool bIsGlobal = globalScope->isVariable(arg);
    ArrayOfVector retval;
    retval << ArrayOf::logicalConstructor(bIsGlobal);
    return retval;
}
//=============================================================================
