//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assigninBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::assigninBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3, 3);
    if (!argIn[0].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    std::string scopename = argIn[0].getContentAsCString();
    if (!((scopename == "global") || (scopename == "base") || (scopename == "caller")
            || (scopename == "local"))) {
        Error(_W("#1 Argument must contain a string: \'global\', \'base\', \'local\' or \'caller\' "
                 "expected."));
    }
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }
    std::string varname = argIn[1].getContentAsCString();
    if (!IsValidVariableName(varname)) {
        Error(_W("#2 Argument must contain a valid variable name."));
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
    if ((scope != nullptr) && scope->isLockedVariable(varname)) {
        Error(_W("Redefining permanent variable."));
    }
    if (scope != nullptr) {
        ArrayOf varValue = argIn[2];
        scope->insertVariable(varname, varValue);
    }
    return retval;
}
//=============================================================================
