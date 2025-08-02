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
#include <string>
#include "MacroFunctionDef.hpp"
#include "AbstractSyntaxTree.hpp"
#include "LexerContext.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
functionBody(const ParseRHS& lhsRhs, const ParseRHS& nameRhs, const ParseRHS& rhsRhs,
    const ParseRHS& codeRhs);
//=============================================================================
std::string
decodeline(const ParseRHS& val);
//=============================================================================
int
yyxpt(const std::string& xStr, const ParseRHS& val);
//=============================================================================
void
callyyparse(LexerContext& lexerContext);
//=============================================================================
void
setParsedScriptBlock(AbstractSyntaxTreePtr ast);
//=============================================================================
void
setParsedFunctionDef(MacroFunctionDef* r);
}
//=============================================================================
