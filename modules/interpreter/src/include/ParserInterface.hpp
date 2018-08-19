//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "AST.hpp"
#include "FunctionDef.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserState.hpp"
#include "nlsInterpreter_exports.h"
#include <stdio.h>
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
NLSINTERPRETER_IMPEXP ASTPtr
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
parseString(const std::string& txt);
/**
 * Parse the given file (with the given filename).
 */
NLSINTERPRETER_IMPEXP ParserState
parseFile(FILE*, const std::string&);
} // namespace Nelson
//=============================================================================
