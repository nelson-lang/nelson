//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include "parsefileBuiltin.hpp"
#include "Error.hpp"
#include "ParseFile.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::parsefileBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring filename;
    if (argIn[0].isRowVectorCharacterArray()) {
        filename = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    ParserState parserState = ParseFile(eval, filename);
    switch (parserState) {
    case ScriptBlock: {
        retval << ArrayOf::characterArrayConstructor("script");
    } break;
    case FuncDef: {
        retval << ArrayOf::characterArrayConstructor("function");
    } break;
    default: {
        retval << ArrayOf::characterArrayConstructor("error");
    } break;
    }
    return retval;
}
//=============================================================================
