//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
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
#include "nlsInterpreter_exports.h"
#include "LexerContext.hpp"
//=============================================================================
namespace Nelson {
NLSINTERPRETER_IMPEXP void
setLexBuffer(LexerContext& lexerContext, const std::wstring& buffer);
NLSINTERPRETER_IMPEXP void
setLexBuffer(LexerContext& lexerContext, const std::string& buffer);
NLSINTERPRETER_IMPEXP void
setLexFile(LexerContext& lexerContext, FILE* fp);
/**
 * Lex the file, and then check to see if more input is needed.
 */
NLSINTERPRETER_IMPEXP bool
lexCheckForMoreInput(LexerContext& lexerContext, int pcount);
NLSINTERPRETER_IMPEXP int
getContinuationCount(LexerContext& lexerContext);
/**
 * Install a process-wide resolver the lexer uses to decide command vs
 * expression syntax when the head of a statement is a variable ("x /y" is
 * division, "disp /y" is a command). It is consulted for parse contexts that do
 * not carry their own LexerContext::isVariableHook (e.g. the local contexts used
 * to parse script and function bodies). Passing nullptr disables it.
 */
NLSINTERPRETER_IMPEXP void
setLexerVariableResolver(bool (*resolver)(const std::string&));
//=============================================================================
} // namespace Nelson
//=============================================================================
