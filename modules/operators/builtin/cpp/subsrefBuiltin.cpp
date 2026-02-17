//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "subsrefBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::subsrefBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 1);
    if (!argIn[1].isStruct()) {
        raiseError2(L"nelson:validators:mustBeStructAtPosition", 2);
    }

    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string fname = "__subsref__";
    if (!context->lookupFunction(fname, funcDef)) {
        raiseError(L"Nelson:operators:ERROR_FUNCTION_NOT_AVAILABLE", ERROR_FUNCTION_NOT_AVAILABLE,
            utf8_to_wstring(fname));
    }
    try {
        eval->withOverload = false;
        retval = funcDef->evaluateFunction(eval, argIn, nLhs);
        eval->withOverload = true;
    } catch (const Exception&) {
        eval->withOverload = true;
        throw;
    }
    return retval;
}
//=============================================================================
