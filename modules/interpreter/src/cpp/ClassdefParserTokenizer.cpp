//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserTokenizer.hpp"
#include "ClassdefParserHelpers.hpp"
#include <cctype>
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
static bool
isIdentifierStartChar(char ch)
{
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_';
}
//=============================================================================
static bool
isIdentifierBodyChar(char ch)
{
    return isIdentifierStartChar(ch) || (ch >= '0' && ch <= '9');
}
//=============================================================================
static bool
isDigitChar(char ch)
{
    return ch >= '0' && ch <= '9';
}
//=============================================================================
// Consume a char-array ('...') or string ("...") literal starting at index k
// (which is the opening quote). Doubled quotes are escapes. Returns the index
// just past the closing quote, or npos on an unterminated literal.
static size_t
scanQuotedLiteral(const std::string& source, size_t k, char delimiter)
{
    ++k;
    while (k < source.size()) {
        const char ch = source[k];
        if (ch == '\n') {
            return std::string::npos; // literals never span physical lines
        }
        if (ch == delimiter) {
            if (k + 1 < source.size() && source[k + 1] == delimiter) {
                k += 2;
                continue;
            }
            return k + 1;
        }
        ++k;
    }
    return std::string::npos;
}
//=============================================================================
bool
tokenizeClassdefSource(
    const std::string& source, std::vector<ClassdefToken>& tokens, std::string& errorMessage)
{
    tokens.clear();
    errorMessage.clear();
    size_t line = 1;
    int bracketDepth = 0;
    const size_t n = source.size();
    size_t k = 0;
    size_t lineStart = 0;
    int blockCommentDepth = 0;
    while (k < n) {
        const char ch = source[k];
        // Inside a %{ %} block comment: skip whole lines up to the closing "%}".
        if (blockCommentDepth > 0) {
            size_t lineEnd = k;
            while (lineEnd < n && source[lineEnd] != '\n') {
                ++lineEnd;
            }
            const std::string trimmed = trim(source.substr(lineStart, lineEnd - lineStart));
            if (trimmed == "%{") {
                ++blockCommentDepth;
            } else if (trimmed == "%}") {
                --blockCommentDepth;
            }
            k = (lineEnd < n) ? lineEnd + 1 : n;
            lineStart = k;
            ++line;
            continue;
        }
        // A % comment runs to the end of the line. A line holding only "%{"
        // opens a block comment.
        if (ch == '%') {
            const bool startOfLineContent = trim(source.substr(lineStart, k - lineStart)).empty();
            if (startOfLineContent) {
                size_t lineEnd = k;
                while (lineEnd < n && source[lineEnd] != '\n') {
                    ++lineEnd;
                }
                if (trim(source.substr(lineStart, lineEnd - lineStart)) == "%{") {
                    ++blockCommentDepth;
                    k = (lineEnd < n) ? lineEnd + 1 : n;
                    lineStart = k;
                    ++line;
                    continue;
                }
            }
            while (k < n && source[k] != '\n') {
                ++k;
            }
            continue;
        }
        if (ch == '.' && k + 2 < n && source[k + 1] == '.' && source[k + 2] == '.') {
            // "..." continuation: skip past the newline, emit no Newline token.
            while (k < n && source[k] != '\n') {
                ++k;
            }
            if (k < n) {
                ++k;
                lineStart = k;
                ++line;
            }
            continue;
        }
        if (ch == '\n') {
            ++k;
            lineStart = k;
            // A newline inside open brackets is an implicit continuation, not a
            // statement break.
            if (bracketDepth <= 0) {
                if (!tokens.empty() && tokens.back().type != ClassdefTokenType::Newline) {
                    tokens.push_back({ ClassdefTokenType::Newline, "\\n", line, k });
                }
            }
            ++line;
            continue;
        }
        if (ch == ' ' || ch == '\t' || ch == '\r') {
            ++k;
            continue;
        }
        const size_t startOffset = k;
        if (isIdentifierStartChar(ch)) {
            size_t e = k + 1;
            while (e < n && isIdentifierBodyChar(source[e])) {
                ++e;
            }
            tokens.push_back(
                { ClassdefTokenType::Identifier, source.substr(k, e - k), line, startOffset });
            k = e;
            continue;
        }
        // Number (integer/real, no leading sign - that is an operator)
        if (isDigitChar(ch) || (ch == '.' && k + 1 < n && isDigitChar(source[k + 1]))) {
            size_t e = k;
            while (e < n
                && (isDigitChar(source[e]) || source[e] == '.' || source[e] == 'e'
                    || source[e] == 'E'
                    || ((source[e] == '+' || source[e] == '-') && e > k
                        && (source[e - 1] == 'e' || source[e - 1] == 'E')))) {
                ++e;
            }
            tokens.push_back(
                { ClassdefTokenType::Number, source.substr(k, e - k), line, startOffset });
            k = e;
            continue;
        }
        if (ch == '\'') {
            // A single quote is the transpose operator only when it directly
            // follows a value with no intervening whitespace. Inside brackets a
            // space before a quote separates matrix elements, so the quote opens
            // a char array (e.g. "[num2str(x) ' >>']"). Decide on the character
            // physically preceding the quote, which reflects that whitespace,
            // rather than the last significant token (which skips whitespace).
            const char precedingChar = (k > 0) ? source[k - 1] : '\0';
            const bool opensCharArray = singleQuoteOpensCharArray(precedingChar);
            if (opensCharArray) {
                const size_t e = scanQuotedLiteral(source, k, '\'');
                if (e == std::string::npos) {
                    errorMessage = "Unterminated character array.";
                    return false;
                }
                tokens.push_back(
                    { ClassdefTokenType::CharArray, source.substr(k, e - k), line, startOffset });
                k = e;
                continue;
            }
            tokens.push_back({ ClassdefTokenType::Operator, "'", line, startOffset });
            ++k;
            continue;
        }
        if (ch == '"') {
            const size_t e = scanQuotedLiteral(source, k, '"');
            if (e == std::string::npos) {
                errorMessage = "Unterminated string.";
                return false;
            }
            tokens.push_back(
                { ClassdefTokenType::StringArray, source.substr(k, e - k), line, startOffset });
            k = e;
            continue;
        }
        ClassdefTokenType type = ClassdefTokenType::Operator;
        bool single = true;
        switch (ch) {
        case '(':
            type = ClassdefTokenType::LeftParen;
            ++bracketDepth;
            break;
        case ')':
            type = ClassdefTokenType::RightParen;
            --bracketDepth;
            break;
        case '[':
            type = ClassdefTokenType::LeftBracket;
            ++bracketDepth;
            break;
        case ']':
            type = ClassdefTokenType::RightBracket;
            --bracketDepth;
            break;
        case '{':
            type = ClassdefTokenType::LeftBrace;
            ++bracketDepth;
            break;
        case '}':
            type = ClassdefTokenType::RightBrace;
            --bracketDepth;
            break;
        case ',':
            type = ClassdefTokenType::Comma;
            break;
        case ';':
            type = ClassdefTokenType::Semicolon;
            break;
        case '@':
            type = ClassdefTokenType::At;
            break;
        case '<':
            type = ClassdefTokenType::Less;
            break;
        case '&':
            type = ClassdefTokenType::Ampersand;
            break;
        case '.':
            type = ClassdefTokenType::Dot;
            break;
        default:
            type = ClassdefTokenType::Operator;
            single = false;
            break;
        }
        if (single) {
            tokens.push_back({ type, std::string(1, ch), line, startOffset });
            ++k;
            continue;
        }
        size_t e = k + 1;
        while (e < n) {
            const char oc = source[e];
            if (isIdentifierStartChar(oc) || isDigitChar(oc) || oc == ' ' || oc == '\t'
                || oc == '\r' || oc == '\n' || oc == '(' || oc == ')' || oc == '[' || oc == ']'
                || oc == '{' || oc == '}' || oc == ',' || oc == ';' || oc == '@' || oc == '\''
                || oc == '"' || oc == '%') {
                break;
            }
            ++e;
        }
        tokens.push_back(
            { ClassdefTokenType::Operator, source.substr(k, e - k), line, startOffset });
        k = e;
    }
    if (!tokens.empty() && tokens.back().type == ClassdefTokenType::Newline) {
        tokens.pop_back();
    }
    tokens.push_back({ ClassdefTokenType::EndOfInput, "", line, n });
    return true;
}
//=============================================================================
static const char*
tokenTypeName(ClassdefTokenType type)
{
    switch (type) {
    case ClassdefTokenType::Identifier:
        return "ident";
    case ClassdefTokenType::Number:
        return "number";
    case ClassdefTokenType::CharArray:
        return "char";
    case ClassdefTokenType::StringArray:
        return "string";
    case ClassdefTokenType::Operator:
        return "op";
    case ClassdefTokenType::LeftParen:
        return "lparen";
    case ClassdefTokenType::RightParen:
        return "rparen";
    case ClassdefTokenType::LeftBracket:
        return "lbracket";
    case ClassdefTokenType::RightBracket:
        return "rbracket";
    case ClassdefTokenType::LeftBrace:
        return "lbrace";
    case ClassdefTokenType::RightBrace:
        return "rbrace";
    case ClassdefTokenType::Comma:
        return "comma";
    case ClassdefTokenType::Semicolon:
        return "semicolon";
    case ClassdefTokenType::At:
        return "at";
    case ClassdefTokenType::Less:
        return "less";
    case ClassdefTokenType::Ampersand:
        return "amp";
    case ClassdefTokenType::Dot:
        return "dot";
    case ClassdefTokenType::Newline:
        return "newline";
    case ClassdefTokenType::EndOfInput:
        return "eof";
    }
    return "?";
}
//=============================================================================
std::string
classdefTokensToString(const std::vector<ClassdefToken>& tokens)
{
    std::string result;
    for (const auto& token : tokens) {
        result += tokenTypeName(token.type);
        if (!token.text.empty() && token.type != ClassdefTokenType::Newline) {
            result += " ";
            result += token.text;
        }
        result += "\n";
    }
    return result;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
