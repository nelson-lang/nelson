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
#include "ErrorID.hpp"
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
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        command = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    ParserContext parserContext;
    ParserState parserState = ParseError;
    NelsonException previousException(eval->getLastErrorException());
    try {
        parserContext = parseStringResult(eval->lexerContext, command + "\n");
        parserState = parserContext.state();
        if (parserState == ParseError) {
            eval->setLastErrorException(previousException);
        }
    } catch (const NelsonException&) {
        parserState = ParseError;
        eval->setLastErrorException(previousException);
    }
    switch (parserState) {
    case ScriptBlock: {
        retval << ArrayOf::characterArray("script");
    } break;
    case FuncDef: {
        if (parserContext.macroFunctionDef != nullptr) {
            retval << ArrayOf::characterArray("function");
        } else {
            retval << ArrayOf::characterArray("script");
        }
    } break;
    case ClassDef: {
        retval << ArrayOf::characterArray("classdef");
    } break;
    default: {
        retval << ArrayOf::characterArray("error");
    } break;
    }
    return retval;
}
//=============================================================================
