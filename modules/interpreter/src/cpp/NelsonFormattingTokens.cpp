//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonFormattingTokens.hpp"
#include "NelsonException.hpp"
#include "LexerContext.hpp"
#include "ParserInterface.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include <utility>
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    thread_local std::vector<FormattingToken>* activeFormattingTokenSink = nullptr;
    //=============================================================================
    static void
    computeLineColumn(const std::string& source, size_t offset, int& line, int& column)
    {
        line = 0;
        column = 0;
        for (size_t k = 0; k < offset && k < source.size(); ++k) {
            if (source[k] == '\r') {
                if (k + 1 < offset && k + 1 < source.size() && source[k + 1] == '\n') {
                    ++k;
                }
                ++line;
                column = 0;
            } else if (source[k] == '\n') {
                ++line;
                column = 0;
            } else {
                ++column;
            }
        }
    }
    //=============================================================================
    static bool
    isFormattingTransposeQuote(const std::string& source, size_t position)
    {
        if (position == 0) {
            return false;
        }
        const unsigned char previous = static_cast<unsigned char>(source[position - 1]);
        return std::isalnum(previous) != 0 || previous == '_' || previous == ')' || previous == ']'
            || previous == '}' || previous == '.' || previous == '\'';
    }
    //=============================================================================
    static std::vector<std::pair<size_t, size_t>>
    findFormattingBlockComments(const std::string& source)
    {
        std::vector<std::pair<size_t, size_t>> ranges;
        char quote = 0;
        for (size_t pos = 0; pos < source.size(); ++pos) {
            const char ch = source[pos];
            if (quote != 0) {
                if (ch == quote) {
                    if (pos + 1 < source.size() && source[pos + 1] == quote) {
                        ++pos;
                    } else {
                        quote = 0;
                    }
                }
                continue;
            }
            if (ch == '"' || (ch == '\'' && !isFormattingTransposeQuote(source, pos))) {
                quote = ch;
                continue;
            }
            if (ch == '%' && pos + 1 < source.size() && source[pos + 1] == '{') {
                const size_t end = source.find("%}", pos + 2);
                if (end == std::string::npos) {
                    throw NelsonException(Nelson::resolveMessageTemplate(
                        "Nelson:interpreter:unterminatedBlockCommentCode",
                        "Unterminated block comment in code passed to smartindent."));
                }
                ranges.emplace_back(pos, end + 2);
                pos = end + 1;
                continue;
            }
            // Line comments and continuation trivia cannot open a block comment.
            if (ch == '%' || source.compare(pos, 3, "...") == 0) {
                const size_t end = source.find_first_of("\r\n", pos);
                pos = end == std::string::npos ? source.size() - 1 : end;
            }
        }
        return ranges;
    }
    //=============================================================================
    static std::vector<FormattingToken>
    collectFormattingBlockCommentTokens(const std::string& source)
    {
        std::vector<FormattingToken> tokens;
        for (const auto& range : findFormattingBlockComments(source)) {
            const size_t end = range.second;
            size_t lineStart = range.first;
            while (lineStart < end) {
                size_t lineEnd = lineStart;
                while (lineEnd < end && source[lineEnd] != '\r' && source[lineEnd] != '\n') {
                    ++lineEnd;
                }
                FormattingToken token;
                token.type = 0;
                token.offset = lineStart;
                token.text = source.substr(lineStart, lineEnd - lineStart);
                token.isComment = true;
                token.isBlockComment = true;
                computeLineColumn(source, token.offset, token.line, token.column);
                tokens.push_back(token);
                if (lineEnd >= end) {
                    break;
                }
                if (source[lineEnd] == '\r' && lineEnd + 1 < end && source[lineEnd + 1] == '\n') {
                    lineStart = lineEnd + 2;
                } else {
                    lineStart = lineEnd + 1;
                }
            }
        }
        return tokens;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
setFormattingTokenSink(std::vector<FormattingToken>* sink)
{
    activeFormattingTokenSink = sink;
}
//=============================================================================
std::vector<FormattingToken>*
getFormattingTokenSink()
{
    return activeFormattingTokenSink;
}
//=============================================================================
std::vector<FormattingToken>
tokenizeForFormatting(const std::string& source)
{
    std::string parseSource = maskFormattingBlockComments(source);
    if (parseSource.empty() || (parseSource.back() != '\n' && parseSource.back() != '\r')) {
        parseSource += '\n';
    }
    // The parser rewrites "?Name" as the longer "metaclass('Name')", which would shift the
    // token offsets: lex a same-length identifier instead and give the tokens back their "?".
    const std::vector<std::pair<size_t, size_t>> metaclassQueries
        = findMetaclassQueryRanges(parseSource);
    for (const auto& range : metaclassQueries) {
        parseSource[range.first] = 'Q';
    }

    LexerContext lexerContext;
    std::vector<FormattingToken> tokens = collectFormattingBlockCommentTokens(source);
    setFormattingTokenSink(&tokens);
    try {
        (void)parseStringResult(lexerContext, parseSource);
    } catch (...) {
        setFormattingTokenSink(nullptr);
        throw;
    }
    setFormattingTokenSink(nullptr);
    std::sort(tokens.begin(), tokens.end(),
        [](const FormattingToken& a, const FormattingToken& b) { return a.offset < b.offset; });
    for (FormattingToken& token : tokens) {
        // The trivia after "..." runs over the comment lines that follow it, which are
        // formatted as their own lines: keep only the physical line of the "...".
        if (token.text.compare(0, 3, "...") == 0) {
            token.text = token.text.substr(0, token.text.find_first_of("\r\n"));
        }
        const bool isMetaclassQuery = std::any_of(metaclassQueries.begin(), metaclassQueries.end(),
            [&token](const auto& range) { return range.first == token.offset; });
        if (isMetaclassQuery && !token.text.empty() && token.text[0] == 'Q') {
            token.text[0] = '?';
        }
    }
    return tokens;
}
//=============================================================================
std::string
maskFormattingBlockComments(const std::string& source)
{
    std::string masked = source;
    for (const auto& range : findFormattingBlockComments(source)) {
        for (size_t k = range.first; k < range.second; ++k) {
            if (masked[k] != '\r' && masked[k] != '\n') {
                masked[k] = ' ';
            }
        }
    }
    return masked;
}
//=============================================================================
const std::vector<const char*>&
getFormattingContextualBlockKeywords()
{
    static const std::vector<const char*> keywords
        = { "arguments", "classdef", "properties", "methods", "events", "enumeration" };
    return keywords;
}
//=============================================================================
const std::vector<const char*>&
getFormattingOperators()
{
    static const std::vector<const char*> operators = { "==", "~=", "<=", ">=", "&&", "||", ".*",
        "./", ".\\", ".^", ".'", "=", "<", ">", "+", "-", "*", "/", "\\", "^", "&", "|", "~", "'" };
    return operators;
}
//=============================================================================
bool
isFormattingUnaryOperatorCandidate(const std::string& op)
{
    return op == "+" || op == "-" || op == "~";
}
//=============================================================================
bool
isFormattingPostfixOperator(const std::string& op)
{
    return op == "'" || op == ".'";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
