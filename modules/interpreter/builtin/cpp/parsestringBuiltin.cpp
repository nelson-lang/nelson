//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "parsestringBuiltin.hpp"
#include "Error.hpp"
#include "ParserInterface.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::parsestringBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string command;
    if (argIn[0].isRowVectorCharacterArray()) {
        command = argIn[0].getContentAsCString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    ParserState parserState = ParseError;
    Exception previousException(eval->getLastErrorException());
    try {
        parserState = parseString(eval->lexerContext, command + "\n");
    } catch (const Exception&) {
        parserState = ParseError;
        eval->setLastErrorException(previousException);
    }
    switch (parserState) {
    case ScriptBlock: {
        retval << ArrayOf::characterArrayConstructor("script");
    } break;
    case FuncDef: {
        MacroFunctionDef* cp = getParsedFunctionDef();
        if (cp) {
            retval << ArrayOf::characterArrayConstructor("function");
        } else {
            retval << ArrayOf::characterArrayConstructor("script");
        }
    } break;
    default: {
        retval << ArrayOf::characterArrayConstructor("error");
    } break;
    }
    return retval;
}
//=============================================================================
