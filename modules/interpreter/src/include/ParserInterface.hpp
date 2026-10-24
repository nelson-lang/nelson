//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstdio>
#include <string>
#include <utility>
#include <vector>
#include "AbstractSyntaxTree.hpp"
#include "FunctionDef.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserState.hpp"
#include "nlsInterpreter_exports.h"
#include "LexerContext.hpp"
//=============================================================================
namespace Nelson {
struct ParserDiagnostic
{
    std::string message;
    std::string filename;
    int line = 0;
    int column = 0;
};
//=============================================================================
struct ParserContext
{
    AbstractSyntaxTreePtr ast = nullptr;
    MacroFunctionDef* macroFunctionDef = nullptr;
    ParserState parserState = FuncDef;
    ParserDiagnostic diagnostic;

    ParserState
    state() const
    {
        return parserState;
    }
};
//=============================================================================
NLSINTERPRETER_IMPEXP void
resetParser();
NLSINTERPRETER_IMPEXP AbstractSyntaxTreePtr
getParsedScriptBlock();
NLSINTERPRETER_IMPEXP MacroFunctionDef*
getParsedFunctionDef();
NLSINTERPRETER_IMPEXP ParserState
parseState();
NLSINTERPRETER_IMPEXP ParserState
parseString(LexerContext& lexerContext, const std::string& txt);
NLSINTERPRETER_IMPEXP ParserContext
parseStringResult(LexerContext& lexerContext, const std::string& txt);
NLSINTERPRETER_IMPEXP ParserState
parseFile(LexerContext& lexerContext, FILE* /*fp*/, const std::string& /*fname*/);
NLSINTERPRETER_IMPEXP ParserContext
parseFileResult(LexerContext& lexerContext, FILE* /*fp*/, const std::string& /*fname*/);
/**
 * Ranges [start, end) of the metaclass queries "?Name" / "?pkg.Name" in the source
 * (strings and comments skipped). The parser rewrites them as metaclass('Name').
 */
NLSINTERPRETER_IMPEXP std::vector<std::pair<size_t, size_t>>
findMetaclassQueryRanges(const std::string& txt);
//=============================================================================
} // namespace Nelson
//=============================================================================
