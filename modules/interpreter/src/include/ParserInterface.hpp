//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstdio>
#include "AbstractSyntaxTree.hpp"
#include "FunctionDef.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserState.hpp"
#include "nlsInterpreter_exports.h"
#include "LexerContext.hpp"
//=============================================================================
namespace Nelson {
/**
 * Reset the parser to its default state.
 */
NLSINTERPRETER_IMPEXP void
resetParser();
/**
 * Get the AST from a parse of a script or statement sequence.
 */
NLSINTERPRETER_IMPEXP AbstractSyntaxTreePtr
getParsedScriptBlock();
/**
 * Get the function definition from a parse of a function-definition.
 */
NLSINTERPRETER_IMPEXP MacroFunctionDef*
getParsedFunctionDef();
/**
 * Get the current parser state.
 */
NLSINTERPRETER_IMPEXP ParserState
parseState();
/**
 * Parse the given string.
 */
NLSINTERPRETER_IMPEXP ParserState
parseString(LexerContext& lexerContext, const std::string& txt);
/**
 * Parse the given file (with the given filename).
 */
NLSINTERPRETER_IMPEXP ParserState
parseFile(LexerContext& lexerContext, FILE* /*fp*/, const std::string& /*fname*/);
} // namespace Nelson
//=============================================================================
