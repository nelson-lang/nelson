//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
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
#include <string>
#include "nlsInterpreter_exports.h"
#include "LexerContext.hpp"
//=============================================================================
namespace Nelson {
/**
 * Set the string buffer to be processed by the lexer.
 */
NLSINTERPRETER_IMPEXP void
setLexBuffer(LexerContext& lexerContext, const std::wstring& buffer);
NLSINTERPRETER_IMPEXP void
setLexBuffer(LexerContext& lexerContext, const std::string& buffer);

/**
 * Set the FILE pointer for the file to be processed
 * by the lexer.
 */
NLSINTERPRETER_IMPEXP void
setLexFile(LexerContext& lexerContext, FILE* fp);
/**
 * Lex the file, and then check to see if more input is needed.
 */
NLSINTERPRETER_IMPEXP bool
lexCheckForMoreInput(LexerContext& lexerContext, int pcount);
/**
 * Retrieve the contents of the continuationCount.
 */
NLSINTERPRETER_IMPEXP int
getContinuationCount(LexerContext& lexerContext);
//=============================================================================
} // namespace Nelson
//=============================================================================
