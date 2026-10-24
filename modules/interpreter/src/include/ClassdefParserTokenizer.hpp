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
#include <vector>
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
// Tokenizer for the native classdef front-end. It turns classdef
// source into a flat token stream with source positions, correctly handling
// character-array literals vs the transpose operator, line and block comments,
// and explicit ("...") and implicit (open-bracket) line continuations. Block
// keywords are NOT reserved words in Nelson (properties/methods/events are also
// builtin functions), so the tokenizer emits them as ordinary identifiers; it
// is the recursive-descent parser that recognizes them by text and position.
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
enum class ClassdefTokenType
{
    Identifier, // bare word (may be a contextual block keyword)
    Number,
    CharArray,
    StringArray,
    Operator, // + - * / etc., and the transpose '
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,
    At, // @ (superclass call)
    Less, // < (superclass list)
    Ampersand, // & (superclass list separator)
    Dot, // . (qualified names, get./set.)
    Newline, // logical statement break
    EndOfInput
};
//=============================================================================
struct ClassdefToken
{
    ClassdefTokenType type;
    std::string text;
    size_t line = 0; // 1-based line of the token start
    size_t offset = 0; // byte offset of the token start in the source
};
//=============================================================================
// Tokenize classdef source. On a lexical error (e.g. an unterminated char
// array) returns false and sets errorMessage; otherwise fills tokens (ending
// with an EndOfInput token) and returns true.
NLSINTERPRETER_IMPEXP bool
tokenizeClassdefSource(
    const std::string& source, std::vector<ClassdefToken>& tokens, std::string& errorMessage);
//=============================================================================
// Compact one-token-per-line rendering ("<type> <text>") used by tests and the
// __classdef_tokenize__ diagnostic builtin.
NLSINTERPRETER_IMPEXP std::string
classdefTokensToString(const std::vector<ClassdefToken>& tokens);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
