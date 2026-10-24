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
#include <algorithm>
#include <cctype>
#include <cstring>
#include <string>
#include <vector>
#include "ClassdefParser.hpp"
#include "ErrorID.hpp"
#include "Keywords.hpp"
#include "NelsonException.hpp"
#include "NelsonFormattingTokens.hpp"
#include "NelsonParserTokens.hpp"
//=============================================================================
namespace Nelson::codeindent_detail {
struct SourceLine
{
    std::string text;
    std::string eol;
    size_t startOffset = 0;
};
//=============================================================================
enum class ScanState
{
    Normal,
    SingleQuotedText,
    DoubleQuotedText,
    BlockComment
};
//=============================================================================
enum class LineKind
{
    Other,
    Opener,
    ClassBlockOpener,
    Middle,
    CaseOrOtherwise,
    Closer
};
//=============================================================================
static std::vector<SourceLine>
splitLines(const std::string& source)
{
    std::vector<SourceLine> lines;
    size_t start = 0;
    while (start < source.size()) {
        size_t pos = start;
        while (pos < source.size() && source[pos] != '\r' && source[pos] != '\n') {
            ++pos;
        }
        SourceLine line;
        line.startOffset = start;
        line.text = source.substr(start, pos - start);
        if (pos < source.size()) {
            if (source[pos] == '\r' && pos + 1 < source.size() && source[pos + 1] == '\n') {
                line.eol = "\r\n";
                pos += 2;
            } else {
                line.eol.assign(1, source[pos]);
                ++pos;
            }
        }
        lines.push_back(line);
        start = pos;
    }
    if (source.empty()) {
        return lines;
    }
    if (!source.empty() && (source.back() == '\n' || source.back() == '\r')) {
        lines.push_back(SourceLine {});
    }
    return lines;
}
//=============================================================================
static std::string
joinLines(const std::vector<SourceLine>& lines)
{
    std::string result;
    for (const SourceLine& line : lines) {
        result += line.text;
        result += line.eol;
    }
    return result;
}
//=============================================================================
// Lines a text literal spans ("REF = '" ... "';"): the line it opens on may
// only be re-indented, the lines it continues on are kept byte for byte.
enum class TextSpan
{
    None,
    Opens,
    Continues
};
//=============================================================================
static std::vector<TextSpan>
collectTextSpans(const std::vector<FormattingToken>& tokens, size_t lineCount)
{
    std::vector<TextSpan> spans(lineCount, TextSpan::None);
    for (const FormattingToken& token : tokens) {
        if (token.type != CHARACTER && token.type != STRING) {
            continue;
        }
        const size_t first = static_cast<size_t>(std::max(token.line, 0));
        const size_t count
            = static_cast<size_t>(std::count(token.text.begin(), token.text.end(), '\n'));
        for (size_t k = 1; k <= count && first + k < lineCount; ++k) {
            spans[first + k] = TextSpan::Continues;
        }
        if (count > 0 && first < lineCount && spans[first] == TextSpan::None) {
            spans[first] = TextSpan::Opens;
        }
    }
    return spans;
}
//=============================================================================
static bool
isIdentifierCharacter(char ch)
{
    return std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_';
}
//=============================================================================
static bool
startsKeywordAt(const std::string& text, size_t pos, const char* keyword)
{
    const size_t length = std::strlen(keyword);
    if (pos + length > text.size() || text.compare(pos, length, keyword) != 0) {
        return false;
    }
    if (pos > 0 && isIdentifierCharacter(text[pos - 1])) {
        return false;
    }
    return pos + length >= text.size() || !isIdentifierCharacter(text[pos + length]);
}
//=============================================================================
static bool
startsAnyKeywordAt(const std::string& text, size_t pos, const std::vector<const char*>& keywords)
{
    return std::any_of(keywords.begin(), keywords.end(),
        [&](const char* keyword) { return startsKeywordAt(text, pos, keyword); });
}
//=============================================================================
static const char*
keywordText(int ordinal)
{
    for (const auto& keyword : keyWord) {
        if (keyword.ordinal == ordinal) {
            return keyword.word;
        }
    }
    return "";
}
//=============================================================================
static bool
startsAnyParserKeywordAt(const std::string& text, size_t pos, const std::vector<int>& ordinals)
{
    return std::any_of(ordinals.begin(), ordinals.end(),
        [&](int ordinal) { return startsKeywordAt(text, pos, keywordText(ordinal)); });
}
//=============================================================================
static bool
matchesAnyParserKeyword(const std::string& text, const std::vector<int>& ordinals)
{
    return std::any_of(ordinals.begin(), ordinals.end(),
        [&](int ordinal) { return text == keywordText(ordinal); });
}
//=============================================================================
static bool
isSingleQuotedTextStart(const std::string& text, size_t quotePosition)
{
    // Same rule as the lexer: a quote is a transpose only when it directly
    // follows a value; after whitespace it opens a text ("case 'a'", "[x 'a']").
    if (quotePosition == 0) {
        return true;
    }
    const char ch = text[quotePosition - 1];
    const bool followsValue = std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_'
        || ch == ')' || ch == ']' || ch == '}' || ch == '.' || ch == '\'';
    return !followsValue;
}
//=============================================================================
static std::string
stripCodeLine(const std::string& line, ScanState& state)
{
    std::string result;
    result.reserve(line.size());
    for (size_t k = 0; k < line.size();) {
        const char ch = line[k];
        if (state == ScanState::BlockComment) {
            if (ch == '%' && k + 1 < line.size() && line[k + 1] == '}') {
                state = ScanState::Normal;
                k += 2;
            } else {
                ++k;
            }
            continue;
        }
        if (state == ScanState::SingleQuotedText) {
            if (ch == '\'' && k + 1 < line.size() && line[k + 1] == '\'') {
                k += 2;
            } else {
                if (ch == '\'') {
                    state = ScanState::Normal;
                }
                ++k;
            }
            continue;
        }
        if (state == ScanState::DoubleQuotedText) {
            if (ch == '"' && k + 1 < line.size() && line[k + 1] == '"') {
                k += 2;
            } else {
                if (ch == '"') {
                    state = ScanState::Normal;
                }
                ++k;
            }
            continue;
        }
        if (ch == '%' && k + 1 < line.size() && line[k + 1] == '{') {
            state = ScanState::BlockComment;
            k += 2;
            continue;
        }
        if (ch == '%' || (ch == '/' && k + 1 < line.size() && line[k + 1] == '/')) {
            break;
        }
        if (ch == '\'' && isSingleQuotedTextStart(line, k)) {
            state = ScanState::SingleQuotedText;
            ++k;
            continue;
        }
        if (ch == '"') {
            state = ScanState::DoubleQuotedText;
            ++k;
            continue;
        }
        result += ch;
        ++k;
    }
    return result;
}
//=============================================================================
static int
leadingColumns(const std::string& line, int tabSize)
{
    int columns = 0;
    for (size_t k = 0; k < line.size() && (line[k] == ' ' || line[k] == '\t'); ++k) {
        columns += line[k] == '\t' ? tabSize : 1;
    }
    return columns;
}
//=============================================================================
static std::string
trimLeft(const std::string& text)
{
    size_t pos = 0;
    while (pos < text.size() && std::isspace(static_cast<unsigned char>(text[pos])) != 0) {
        ++pos;
    }
    return text.substr(pos);
}
//=============================================================================
static std::string
firstIdentifierText(const std::string& text)
{
    const std::string trimmed = trimLeft(text);
    size_t pos = 0;
    while (pos < trimmed.size() && isIdentifierCharacter(trimmed[pos])) {
        ++pos;
    }
    return trimmed.substr(0, pos);
}
//=============================================================================
static std::string
trimRight(const std::string& text)
{
    size_t pos = text.size();
    while (pos > 0 && (text[pos - 1] == ' ' || text[pos - 1] == '\t')) {
        --pos;
    }
    return text.substr(0, pos);
}
//=============================================================================
static void
trimRightInPlace(std::string& text)
{
    text = trimRight(text);
}
//=============================================================================
static LineKind
classifyLineFromCode(const std::string& code, bool classBody)
{
    const std::string trimmed = trimLeft(code);
    if (trimmed.empty()) {
        return LineKind::Other;
    }
    static const std::vector<int> openerOrdinals = { NLS_KEYWORD_FUNCTION, NLS_KEYWORD_IF,
        NLS_KEYWORD_FOR, NLS_KEYWORD_WHILE, NLS_KEYWORD_SWITCH, NLS_KEYWORD_TRY };
    static const std::vector<int> middleOrdinals
        = { NLS_KEYWORD_ELSE, NLS_KEYWORD_ELSEIF, NLS_KEYWORD_CATCH };
    static const std::vector<int> switchLabelOrdinals = { NLS_KEYWORD_CASE, NLS_KEYWORD_OTHERWISE };
    if (startsAnyParserKeywordAt(trimmed, 0, middleOrdinals)) {
        return LineKind::Middle;
    }
    if (startsAnyParserKeywordAt(trimmed, 0, switchLabelOrdinals)) {
        return LineKind::CaseOrOtherwise;
    }
    if (startsKeywordAt(trimmed, 0, keywordText(NLS_KEYWORD_END))) {
        return LineKind::Closer;
    }
    const std::string identifier = firstIdentifierText(trimmed);
    const std::string suffix = trimLeft(trimmed.substr(identifier.size()));
    // Class definitions are opaque to the lexer, so their member blocks use source context.
    if (identifier == "classdef"
        || (classBody && startsAnyKeywordAt(trimmed, 0, getFormattingContextualBlockKeywords()))
        || (identifier == "arguments" && (suffix.empty() || suffix.front() == '('))) {
        return LineKind::ClassBlockOpener;
    }
    if (startsAnyParserKeywordAt(trimmed, 0, openerOrdinals)) {
        return LineKind::Opener;
    }
    return LineKind::Other;
}
//=============================================================================
static bool
isContextualBlockKeyword(const std::string& text)
{
    return std::any_of(getFormattingContextualBlockKeywords().begin(),
        getFormattingContextualBlockKeywords().end(),
        [&](const char* keyword) { return text == keyword; });
}
//=============================================================================
static LineKind
classifyLineFromLexerToken(const std::string& code, const FormattingToken* token, bool classBody)
{
    if (token == nullptr) {
        return classifyLineFromCode(code, classBody);
    }
    if (token->isComment) {
        return LineKind::Other;
    }
    switch (token->type) {
    case ELSE:
    case ELSEIF:
    case CATCH:
        return LineKind::Middle;
    case CASE:
    case OTHERWISE:
        return LineKind::CaseOrOtherwise;
    case END:
    case ENDFUNCTION:
        return LineKind::Closer;
    case FUNCTION:
    case IF:
    case FOR:
    case WHILE:
    case SWITCH:
    case TRY:
    case ARGUMENTS:
        return LineKind::Opener;
    case IDENT: {
        const std::string codeFirstIdentifier = firstIdentifierText(code);
        if (!codeFirstIdentifier.empty() && codeFirstIdentifier == token->text && classBody
            && isContextualBlockKeyword(token->text)) {
            return LineKind::ClassBlockOpener;
        }
        // The lexer leaves contextual names as IDENT in ordinary executable statements.
        return LineKind::Other;
    }
    default:
        break;
    }
    return classifyLineFromCode(code, classBody);
}
//=============================================================================
struct BlockEvent
{
    LineKind kind;
    bool isSwitch = false;
    bool leading = false;
    bool isFunction = false;
};
//=============================================================================
using BlockEvents = std::vector<std::vector<BlockEvent>>;
//=============================================================================
static void
appendBlockEvent(std::vector<BlockEvent>& events, LineKind kind, bool isSwitch, bool leading,
    int& balance, bool isFunction = false)
{
    if (kind == LineKind::Other) {
        return;
    }
    events.push_back({ kind, isSwitch, leading, isFunction });
    if (kind == LineKind::Closer) {
        --balance;
    } else if (kind == LineKind::Opener || kind == LineKind::ClassBlockOpener) {
        ++balance;
    }
}
//=============================================================================
static void
collectOpaqueLineEvents(const std::string& code, bool classSource, int& balance, int& bracketDepth,
    std::vector<BlockEvent>& events)
{
    // Class definitions and arguments bodies are opaque to the parser's lexer sink.
    // Only statement starts outside brackets can introduce blocks in this fallback.
    bool statementStart = bracketDepth == 0;
    const size_t first = code.find_first_not_of(" \t");
    for (size_t pos = 0; pos < code.size();) {
        const char ch = code[pos];
        if (std::isspace(static_cast<unsigned char>(ch)) != 0) {
            ++pos;
            continue;
        }
        if (ch == '(' || ch == '[' || ch == '{') {
            ++bracketDepth;
        } else if (ch == ')' || ch == ']' || ch == '}') {
            bracketDepth = std::max(bracketDepth - 1, 0);
        } else if ((ch == ',' || ch == ';') && bracketDepth == 0) {
            statementStart = true;
            ++pos;
            continue;
        } else if (statementStart && bracketDepth == 0 && isIdentifierCharacter(ch)) {
            const std::string suffix = code.substr(pos);
            const LineKind kind = classifyLineFromCode(suffix, classSource && balance == 1);
            const bool isSwitch = startsKeywordAt(suffix, 0, keywordText(NLS_KEYWORD_SWITCH));
            appendBlockEvent(events, kind, isSwitch, pos == first, balance);
        }
        statementStart = false;
        ++pos;
    }
}
//=============================================================================
static BlockEvents
collectBlockEvents(const std::vector<SourceLine>& lines,
    const std::vector<std::vector<const FormattingToken*>>& tokensByLine, bool classSource)
{
    BlockEvents events(lines.size());
    ScanState scanState = ScanState::Normal;
    int balance = 0;
    int bracketDepth = 0;
    for (size_t line = 0; line < lines.size(); ++line) {
        const std::string code = stripCodeLine(lines[line].text, scanState);
        bool first = true;
        for (const FormattingToken* token : tokensByLine[line]) {
            if (token->isComment || token->type == ENDSTMNT || token->type == ENDQSTMNT) {
                continue;
            }
            // MAGICEND is an indexing operand, not a statement terminator.
            const LineKind kind = token->type == MAGICEND
                ? LineKind::Other
                : classifyLineFromLexerToken(first ? code : "", token, classSource && balance == 1);
            appendBlockEvent(
                events[line], kind, token->type == SWITCH, first, balance, token->type == FUNCTION);
            first = false;
        }
        if (first) {
            collectOpaqueLineEvents(code, classSource, balance, bracketDepth, events[line]);
        }
    }
    return events;
}
//=============================================================================
static void
validateBalancedBlocks(const BlockEvents& events)
{
    int level = 0;
    for (const auto& line : events) {
        for (const BlockEvent& event : line) {
            const LineKind kind = event.kind;
            if (kind == LineKind::Closer) {
                if (level == 0) {
                    throw NelsonException(
                        Nelson::resolveMessageTemplate("Nelson:interpreter:unexpectedEndCodePassed",
                            "Unexpected 'end' in code passed to smartindent."));
                }
                --level;
            } else if (kind == LineKind::Opener || kind == LineKind::ClassBlockOpener) {
                ++level;
            }
        }
    }
    if (level != 0) {
        throw NelsonException(
            Nelson::resolveMessageTemplate("Nelson:interpreter:missingEndCodePassed",
                "Missing 'end' in code passed to smartindent."));
    }
}
//=============================================================================
static int
blockLineLevel(LineKind kind, int level, const std::vector<int>& switches)
{
    if (kind == LineKind::Closer) {
        return !switches.empty() && switches.back() == level - 1 ? std::max(switches.back() - 1, 0)
                                                                 : std::max(level - 1, 0);
    }
    if (kind == LineKind::Middle) {
        return std::max(level - 1, 0);
    }
    if (kind == LineKind::CaseOrOtherwise) {
        return switches.empty() ? std::max(level - 1, 0) : switches.back();
    }
    return level;
}
//=============================================================================
static void
advanceBlockIndent(const BlockEvent& event, int& level, std::vector<int>& switches)
{
    if (event.kind == LineKind::Closer) {
        level = blockLineLevel(event.kind, level, switches);
        while (!switches.empty() && switches.back() > level) {
            switches.pop_back();
        }
    } else if (event.kind == LineKind::CaseOrOtherwise) {
        level = blockLineLevel(event.kind, level, switches) + 1;
    } else if (event.kind == LineKind::Opener || event.kind == LineKind::ClassBlockOpener) {
        if (event.isSwitch) {
            switches.push_back(level + 1);
        }
        ++level;
    }
}
//=============================================================================
enum class TokenKind
{
    None,
    Operand,
    Operator,
    ControlKeyword,
    Open,
    Close,
    Separator,
    ReferenceDot
};
//=============================================================================
static bool
isInsideBracketLiteral(const std::vector<char>& openStack)
{
    return !openStack.empty() && (openStack.back() == '[' || openStack.back() == '{');
}
//=============================================================================
static bool
hasOnlySpacesBetween(const std::string& text, size_t begin, size_t end)
{
    if (begin > end || end > text.size()) {
        return false;
    }
    for (size_t k = begin; k < end; ++k) {
        if (text[k] != ' ' && text[k] != '\t') {
            return false;
        }
    }
    return begin < end;
}
//=============================================================================
// Where "a -b" (space before the operator, none after) differs from "a - b":
// between the elements of a [] or {} literal ("[a -b]" is two elements), and
// after a statement-initial identifier, where it selects command syntax
// ("disp -abc" passes the literal '-abc').
static bool
isSpacingSensitiveOperator(const std::vector<char>& openStack, const std::string& op,
    TokenKind previous, bool afterStatementHead)
{
    const bool isSign = op == "-" || op == "+";
    if (isInsideBracketLiteral(openStack)) {
        return isSign && (previous == TokenKind::Operand || previous == TokenKind::Close);
    }
    return afterStatementHead && openStack.empty() && (isSign || op == "/" || op == "\\");
}
//=============================================================================
// Inside [] and {}, "a (1)" is two elements where "a(1)" is an indexing: an
// opening bracket after an operand keeps the space the source had there.
static bool
isSpaceSeparatedElement(const std::vector<char>& openStack, TokenKind previous, bool spaceBefore)
{
    return spaceBefore && isInsideBracketLiteral(openStack)
        && (previous == TokenKind::Operand || previous == TokenKind::Close);
}
//=============================================================================
static bool
isCompactSignedLiteralOperator(const std::string& code, const std::vector<char>& openStack,
    const std::string& op, size_t operatorPosition, TokenKind previous, bool afterStatementHead,
    bool atLineStart)
{
    if (!isSpacingSensitiveOperator(openStack, op, previous, afterStatementHead)) {
        return false;
    }
    if (operatorPosition + op.size() >= code.size()) {
        return false;
    }
    // At the start of a line, "previous" can only come from a continued line.
    const bool spaceBefore = operatorPosition == 0
        ? atLineStart
        : (code[operatorPosition - 1] == ' ' || code[operatorPosition - 1] == '\t');
    return spaceBefore
        && std::isspace(static_cast<unsigned char>(code[operatorPosition + op.size()])) == 0;
}
//=============================================================================
static bool
isUnaryOperator(const std::string& op, TokenKind previous)
{
    return isFormattingUnaryOperatorCandidate(op)
        && (previous == TokenKind::None || previous == TokenKind::Operator
            || previous == TokenKind::ControlKeyword || previous == TokenKind::Open
            || previous == TokenKind::Separator);
}
//=============================================================================
static bool
isBinaryOperator(const std::string& op, TokenKind previous)
{
    if (isFormattingPostfixOperator(op)) {
        return false;
    }
    return !isUnaryOperator(op, previous);
}
//=============================================================================
static bool
isControlKeyword(const std::string& text)
{
    static const std::vector<int> parserControlOrdinals
        = { NLS_KEYWORD_IF, NLS_KEYWORD_ELSEIF, NLS_KEYWORD_WHILE, NLS_KEYWORD_FOR,
              NLS_KEYWORD_SWITCH, NLS_KEYWORD_CASE, NLS_KEYWORD_CATCH };
    return matchesAnyParserKeyword(text, parserControlOrdinals);
}
//=============================================================================
static bool
isOperandToken(const FormattingToken& token)
{
    switch (token.type) {
    case IDENT:
    case NUMERIC:
    case CHARACTER:
    case STRING:
    case MAGICEND:
    case VARARGIN:
    case VARARGOUT:
        return true;
    default:
        return false;
    }
}
//=============================================================================
// Whitespace between two tokens; a token left on a line that "..." continued
// counts as separated ("[a ..." then "-b]" reads "[a -b]").
static bool
isSpacedFromPrevious(const std::string& line, size_t lineStart, const FormattingToken& token,
    const FormattingToken* previousToken)
{
    if (previousToken == nullptr || token.offset < lineStart) {
        return false;
    }
    const size_t previousEnd = previousToken->offset + previousToken->text.size();
    if (previousEnd < lineStart) {
        return true;
    }
    const size_t localTokenOffset = token.offset - lineStart;
    return localTokenOffset <= line.size()
        && hasOnlySpacesBetween(line, previousEnd - lineStart, localTokenOffset);
}
//=============================================================================
static bool
isCompactSignedLiteralToken(const std::string& line, size_t lineStart,
    const std::vector<char>& openStack, const FormattingToken& token,
    const FormattingToken* previousToken, const FormattingToken* nextToken, TokenKind previous,
    bool afterStatementHead)
{
    if (previousToken == nullptr || nextToken == nullptr
        || !isSpacingSensitiveOperator(openStack, token.text, previous, afterStatementHead)) {
        return false;
    }
    const size_t previousEnd = previousToken->offset + previousToken->text.size();
    const size_t tokenEnd = token.offset + token.text.size();
    if (token.offset < previousEnd || tokenEnd > nextToken->offset
        || !isSpacedFromPrevious(line, lineStart, token, previousToken)) {
        return false;
    }
    // The source parsed, so an operator glued to what follows starts an operand
    // ("-b", "-(x)", "-[1 2]", "-~b"): no space may be inserted there.
    return tokenEnd == nextToken->offset;
}
//=============================================================================
static bool
isControlKeywordToken(const FormattingToken& token)
{
    switch (token.type) {
    case IF:
    case ELSEIF:
    case WHILE:
    case FOR:
    case SWITCH:
    case CASE:
    case CATCH:
    case FUNCTION:
        return true;
    case IDENT:
        return isControlKeyword(token.text);
    default:
        return false;
    }
}
//=============================================================================
static bool
isOpenToken(const FormattingToken& token)
{
    return token.type == '(' || token.type == '[' || token.type == '{' || token.type == REFLPAREN;
}
//=============================================================================
static bool
isCloseToken(const FormattingToken& token)
{
    return token.type == ')' || token.type == ']' || token.type == '}' || token.type == REFRPAREN;
}
//=============================================================================
static bool
isSeparatorToken(const FormattingToken& token)
{
    // The lexer represents the comma after a catch identifier as ENDSTMNT.
    return token.type == ',' || token.type == ENDQSTMNT
        || (token.type == ENDSTMNT && token.text == ",");
}
//=============================================================================
static bool
isPostfixToken(const FormattingToken& token)
{
    return token.type == DOTTRANSPOSE || token.text == "'" || token.text == ".'";
}
//=============================================================================
static bool
isUnaryToken(const FormattingToken& token, TokenKind previous)
{
    if (token.type == POS || token.type == NEG || token.type == NOT) {
        return true;
    }
    return isFormattingUnaryOperatorCandidate(token.text)
        && (previous == TokenKind::None || previous == TokenKind::Operator
            || previous == TokenKind::ControlKeyword || previous == TokenKind::Open
            || previous == TokenKind::Separator);
}
//=============================================================================
static bool
isOperatorToken(const FormattingToken& token)
{
    switch (token.type) {
    case LE:
    case GE:
    case EQ:
    case DOTTIMES:
    case DOTRDIV:
    case DOTLDIV:
    case DOTPOWER:
    case DOTTRANSPOSE:
    case SOR:
    case SAND:
    case NE:
    case POS:
    case NEG:
    case NOT:
        return true;
    default:
        break;
    }
    return token.text == "=" || token.text == "<" || token.text == ">" || token.text == "+"
        || token.text == "-" || token.text == "*" || token.text == "/" || token.text == "\\"
        || token.text == "^" || token.text == "&" || token.text == "|" || token.text == "~"
        || token.text == "'";
}
//=============================================================================
static void
appendSpaceIfNeeded(std::string& result)
{
    if (!result.empty() && result.back() != ' ') {
        result += ' ';
    }
}
//=============================================================================
static bool
isCommentToken(const FormattingToken& token)
{
    if (token.isComment) {
        return true;
    }
    const std::string trimmed = trimLeft(token.text);
    return startsKeywordAt(trimmed, 0, "%") || startsKeywordAt(trimmed, 0, "//")
        || (!trimmed.empty() && trimmed[0] == '%')
        || (trimmed.size() >= 2 && trimmed[0] == '/' && trimmed[1] == '/');
}
//=============================================================================
} // namespace Nelson::codeindent_detail
//=============================================================================
