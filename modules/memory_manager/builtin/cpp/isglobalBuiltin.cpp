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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
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
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    std::string arg = argIn[0].asUtf8String();
    if (!IsValidVariableName(arg)) {
        Error(L"Nelson:memory_manager:invalidVariableName",
            L"#%d argument must contain a valid variable name.", (1));
    }
    bool bIsGlobal = globalScope->isVariable(arg);
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(bIsGlobal);
    return retval;
}
//=============================================================================
