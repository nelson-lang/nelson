//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "AnonymousFunctionText.hpp"
#include "NelsonParserTokens.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    struct BodyToken
    {
        int type;
        const char* start;
        const char* end;
    };
    //=============================================================================
    // Tokens of the anonymous function bodies open in the parse of 'owner'.
    // Kept outside LexerContext, which is embedded by value in Evaluator.
    struct AnonymousFunctionTrace
    {
        const LexerContext* owner = nullptr;
        std::vector<BodyToken> tokens;
        std::vector<size_t> bodyStarts;
    };
    //=============================================================================
    thread_local AnonymousFunctionTrace trace;
    //=============================================================================
}
//=============================================================================
static std::string
tokenText(const BodyToken& token);
//=============================================================================
void
resetAnonymousFunctionText(const LexerContext& lexerContext)
{
    trace.owner = &lexerContext;
    trace.tokens.clear();
    trace.bodyStarts.clear();
}
//=============================================================================
void
recordAnonymousFunctionToken(const LexerContext& lexerContext, int token, const char* start,
    const char* end, bool closesArguments)
{
    if (trace.owner != &lexerContext) {
        if (!closesArguments) {
            return;
        }
        resetAnonymousFunctionText(lexerContext);
    }
    if (!trace.bodyStarts.empty()) {
        trace.tokens.push_back({ token, start, end });
    }
    if (closesArguments) {
        trace.bodyStarts.push_back(trace.tokens.size());
    }
}
//=============================================================================
std::string
takeAnonymousFunctionText(const LexerContext& lexerContext)
{
    if (trace.owner != &lexerContext || trace.bodyStarts.empty()) {
        return "";
    }
    size_t first = trace.bodyStarts.back();
    trace.bodyStarts.pop_back();
    // the last token recorded is the one following the body
    size_t last = trace.tokens.empty() ? 0 : trace.tokens.size() - 1;
    std::string text;
    for (size_t k = first; k < last; ++k) {
        text += tokenText(trace.tokens[k]);
    }
    if (trace.bodyStarts.empty()) {
        trace.tokens.clear();
    }
    return text;
}
//=============================================================================
std::string
tokenText(const BodyToken& token)
{
    if (token.type == ',') {
        // written or virtual (blank between two elements) comma; the one the
        // lexer adds after a Name= argument has no text
        return token.start == token.end ? "" : ",";
    }
    if (token.type == ENDSTMNT || token.type == ENDQSTMNT) {
        // separator inside brackets: ',' or a row break (';', newline, comment)
        return (token.start < token.end && *token.start == ',') ? "," : ";";
    }
    // a token never spans a line: what precedes the last line break is a
    // continuation or a comment, and blanks are dropped (kept in a string)
    const char* begin = token.start;
    for (const char* p = token.start; p < token.end; ++p) {
        if (*p == '\n') {
            begin = p + 1;
        }
    }
    while (begin < token.end && (*begin == ' ' || *begin == '\t')) {
        ++begin;
    }
    // a Name= argument is also delivered as a text token, without quotes
    bool isText = (token.type == CHARACTER || token.type == STRING) && begin < token.end
        && (*begin == '\'' || *begin == '"');
    std::string text;
    for (const char* p = begin; p < token.end; ++p) {
        bool blank = *p == ' ' || *p == '\t' || *p == '\r';
        if (!blank || (isText && !text.empty())) {
            text.push_back(*p);
        }
    }
    return text;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
