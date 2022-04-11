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
//=============================================================================
namespace Nelson {
/**
 * Set the string buffer to be processed by the lexer.
 */
void
setLexBuffer(const std::wstring& buffer);
void
setLexBuffer(const std::string& buffer);

/**
 * Set the FILE pointer for the file to be processed
 * by the lexer.
 */
void
setLexFile(FILE* fp);
/**
 * Lex the file, and then check to see if more input is needed.
 */
bool
lexCheckForMoreInput(int pcount);
/**
 * Retrieve the contents of the continuationCount.
 */
int
getContinuationCount();
//=============================================================================
} // namespace Nelson
//=============================================================================
