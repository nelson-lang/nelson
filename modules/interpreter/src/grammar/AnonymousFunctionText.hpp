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
#include "LexerContext.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Text of an anonymous function body as written, for func2str and display:
 * the source of its tokens without blanks, a separator inside brackets
 * written ',' or ';' (@(x)[x,1;2,3]).
 *
 * The lexer records the tokens delivered while a body is open (from the ')'
 * closing the argument list); the parser takes the text when it reduces the
 * handle, at which point the token following the body is already read.
 */
void
resetAnonymousFunctionText(const LexerContext& lexerContext);
//=============================================================================
void
recordAnonymousFunctionToken(const LexerContext& lexerContext, int token, const char* start,
    const char* end, bool closesArguments);
//=============================================================================
std::string
takeAnonymousFunctionText(const LexerContext& lexerContext);
//=============================================================================
} // namespace Nelson
//=============================================================================
