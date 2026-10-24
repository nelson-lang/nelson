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
#include <cstddef>
#include <cstdio>
#include <string>
#include <functional>
#include <unordered_set>
#include "Keywords.hpp"
#include "AbstractSyntaxTree.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
union contextOrPointer
{
    int i;
    AbstractSyntaxTreePtr p;
};
//=============================================================================
struct ParseRHS
{
    // Default-initialized so a value that is never explicitly assigned (most
    // notably the synthetic 'error' token pushed by the parser during error
    // recovery) is a harmless empty token rather than an uninitialized pointer.
    bool isToken = true;
    contextOrPointer v { 0 };
};
//=============================================================================
inline void
setTokenContextIfMissing(ParseRHS& value, int context)
{
    if (value.isToken && value.v.i == 0) {
        value.v.i = context;
    }
}
//=============================================================================
enum LexingStates
{
    Initial,
    Scanning,
    SpecScan
};
//=============================================================================
#define DEFAULT_BUFFER_SIZE_LEXER 256
//=============================================================================
struct LexerContext
{
    std::string textbufferStorage;
    char* textbuffer = nullptr;
    char* datap = nullptr;
    char* linestart = nullptr;
    int lineNumber = 0;
    int continuationCount = 0;
    int inBlock = 0;
    int inStatement = 0;
    bool inFunction = false;
    int countEndFunction = 0;
    LexingStates lexState = Initial;
    int bracketStack[DEFAULT_BUFFER_SIZE_LEXER];
    int bracketStackSize = 0;
    bool bracketIsDestructuring[DEFAULT_BUFFER_SIZE_LEXER];
    bool bracketIsFunctionCall[DEFAULT_BUFFER_SIZE_LEXER];
    bool bracketIsAnonymousFunctionArguments[DEFAULT_BUFFER_SIZE_LEXER];
    bool justClosedAnonymousFunctionArguments = false;
    int vcStack[DEFAULT_BUFFER_SIZE_LEXER];
    int vcStackSize = 0;
    int vcFlag = 0;
    int placeholderCounter = 0;
    bool pendingNamedArgumentComma = false;
    int pendingCommaContext = 0;
    int catchIdentifierState = 0;
    bool inDebugCommand = false;
    bool inMultilineComment = false;
    //=============================================================================
    // tokenType is read before the first token of a parse is scanned (a word
    // right after '.' is a field name): it must start from a known value.
    int tokenActive = 0;
    int tokenType = 0;
    ParseRHS tokenValue;
    int previousToken = 0;
    // Last non-whitespace token delivered to the parser. Used to collapse runs of
    // row separators (consecutive comment-only or blank lines) inside a matrix
    // '[' or cell '{' literal into one. Unlike previousToken it is never set to
    // the transient WS token.
    int lastSignificantToken = 0;
    //=============================================================================
    keywordStruct tSearch, *pSearch;
    //=============================================================================
    // Command-vs-expression disambiguation needs to know whether the head
    // identifier of a statement is a variable ("x /y" is division, "disp /y" is a
    // command). Two complementary sources are used:
    //  * isVariableHook consults the live workspace of the owning Evaluator; it is
    //    set when an Evaluator drives the lexer and null for pure tooling parses
    //    (formatting, code analysis) where no workspace exists.
    //  * lexicalVariableNames collects names proven to be variables by static
    //    analysis of the current parse buffer (assignment targets, loop indices),
    //    which covers scripts and function bodies parsed before their scope is
    //    live. A name counts as a variable when either source reports it.
    std::function<bool(const std::string&)> isVariableHook;
    std::unordered_set<std::string> lexicalVariableNames;
    //=============================================================================
};
//=============================================================================
inline constexpr const char* UNUSED_PLACEHOLDER_PREFIX = "__nelson_unused_output_";
inline constexpr std::size_t UNUSED_PLACEHOLDER_PREFIX_LEN = sizeof("__nelson_unused_output_") - 1;
inline bool
isPlaceholderIdentifier(const std::string& name)
{
    return name.compare(0, UNUSED_PLACEHOLDER_PREFIX_LEN, UNUSED_PLACEHOLDER_PREFIX) == 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
