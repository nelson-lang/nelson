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
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    std::string arg = argIn[0].getContentAsCString();
    if (!IsValidVariableName(arg)) {
        Error(_W("Argument must contain a valid variable name."));
    }
    bool bIsGlobal = globalScope->isVariable(arg);
    ArrayOfVector retval;
    retval << ArrayOf::logicalConstructor(bIsGlobal);
    return retval;
}
//=============================================================================
