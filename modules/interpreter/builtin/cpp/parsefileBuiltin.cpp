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
#include "ErrorID.hpp"
#include "ParseFile.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
        filename = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    ParserState parserState = ParseFile(eval, filename);
    switch (parserState) {
    case ScriptBlock: {
        retval << ArrayOf::characterArray("script");
    } break;
    case FuncDef: {
        retval << ArrayOf::characterArray("function");
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
