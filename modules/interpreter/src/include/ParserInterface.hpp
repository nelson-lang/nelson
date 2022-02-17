//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
parseString(const std::string& txt);
/**
 * Parse the given file (with the given filename).
 */
NLSINTERPRETER_IMPEXP ParserState
parseFile(FILE* /*fp*/, const std::string& /*fname*/);
} // namespace Nelson
//=============================================================================
