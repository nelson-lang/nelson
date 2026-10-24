//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cctype>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include "BytecodeCompiler.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserNative.hpp"
#include "Error.hpp"
#include "NelsonException.hpp"
#include "Keywords.hpp"
#include "LexerContext.hpp"
#include "MacroFunctionDef.hpp"
#include "NelsonCodeIndent.hpp"
#include "NelsonCodeIndentHelpers.hpp"
#include "NelsonCodeIndentBrackets.hpp"
#include "NelsonCodeIndentEndlessFunctions.hpp"
#include "NelsonCodeIndentFileIO.hpp"
#include "NelsonFormatSemanticCheck.hpp"
#include "NelsonFormattingTokens.hpp"
#include "NelsonParserTokens.hpp"
#include "ParserInterface.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace codeindent_detail;
//=============================================================================
namespace {
    //=============================================================================
    // Open-bracket stack entry for the "(" of "@(": it closes like "(".
    constexpr char anonymousParametersMark = '@';
    //=============================================================================
    // Spacing context carried across the code segments of a line, which quoted
    // text splits ("['abc' -b]" is two elements, as is "[a -b]"), and across a
    // "..." continuation ("[a ..." then "-b]" is two elements, "- b]" one).
    struct CodeSegmentState
    {
        TokenKind previous = TokenKind::None;
        const FormattingToken* previousToken = nullptr;
        bool afterStatementHead = false;
        // The last token closed the parameters of "@(x)": its body keeps the
        // author's space ("@(x) {x, x}").
        bool afterAnonymousParameters = false;
    };
    //=============================================================================
    static std::string
    formatLineFromTokens(const std::string& line, const std::vector<const FormattingToken*>& tokens,
        size_t lineStart, std::vector<char>& openStack, CodeSegmentState& carry)
    {
        // An arguments token spans its whole block; format its source lines separately.
        if (tokens.empty() || tokens.front()->type == ARGUMENTS) {
            return std::string();
        }

        std::string result;
        result.reserve(line.size());
        TokenKind& previous = carry.previous;
        const FormattingToken*& previousToken = carry.previousToken;
        bool& afterStatementHead = carry.afterStatementHead;
        size_t lastTokenEnd = lineStart;
        for (size_t k = 0; k < tokens.size(); ++k) {
            const FormattingToken& token = *tokens[k];
            std::string text = token.type == ENDQSTMNT ? ";" : token.text;
            if (isCommentToken(token)) {
                if (token.isBlockComment) {
                    return trimRight(token.text);
                }
                trimRightInPlace(result);
                if (!result.empty()) {
                    result += ' ';
                }
                result += trimRight(token.text);
                lastTokenEnd = std::max(lastTokenEnd, token.offset + token.text.size());
                break;
            }
            if (text.empty() || (token.type == ENDSTMNT && !isSeparatorToken(token))) {
                continue;
            }
            lastTokenEnd = std::max(lastTokenEnd, token.offset + token.text.size());
            const bool tokenAfterStatementHead = afterStatementHead;
            afterStatementHead = false;
            const bool afterAnonymousParameters = carry.afterAnonymousParameters;
            carry.afterAnonymousParameters = false;
            if (isOperandToken(token) || isControlKeywordToken(token)) {
                afterStatementHead = token.type == IDENT && openStack.empty()
                    && (previous == TokenKind::None || previous == TokenKind::Separator);
                if (previous == TokenKind::Operand || previous == TokenKind::ControlKeyword
                    || previous == TokenKind::Close) {
                    appendSpaceIfNeeded(result);
                }
                result += text;
                previous
                    = isControlKeywordToken(token) ? TokenKind::ControlKeyword : TokenKind::Operand;
                previousToken = &token;
                continue;
            }
            if (text == ".") {
                trimRightInPlace(result);
                result += text;
                previous = TokenKind::ReferenceDot;
                previousToken = &token;
                continue;
            }
            if (isOpenToken(token)) {
                const bool spaceBefore
                    = isSpacedFromPrevious(line, lineStart, token, previousToken);
                if (previous == TokenKind::ControlKeyword
                    || (afterAnonymousParameters && spaceBefore)
                    || isSpaceSeparatedElement(openStack, previous, spaceBefore)) {
                    appendSpaceIfNeeded(result);
                } else if (previous != TokenKind::Operator && previous != TokenKind::Separator) {
                    trimRightInPlace(result);
                }
                result += text;
                if (text == "[" || text == "{" || text == "(") {
                    const bool anonymousParameters
                        = text == "(" && previousToken != nullptr && previousToken->text == "@";
                    openStack.push_back(anonymousParameters ? anonymousParametersMark : text[0]);
                }
                previous = TokenKind::Open;
                previousToken = &token;
                continue;
            }
            if (isCloseToken(token)) {
                trimRightInPlace(result);
                result += text;
                carry.afterAnonymousParameters
                    = !openStack.empty() && openStack.back() == anonymousParametersMark;
                if (!openStack.empty()) {
                    openStack.pop_back();
                }
                previous = TokenKind::Close;
                previousToken = &token;
                continue;
            }
            if (isSeparatorToken(token)) {
                trimRightInPlace(result);
                result += text;
                if (k + 1 < tokens.size() && !isCloseToken(*tokens[k + 1])
                    && tokens[k + 1]->type != ENDSTMNT) {
                    result += ' ';
                }
                previous = TokenKind::Separator;
                previousToken = &token;
                continue;
            }
            if (text == ":") {
                // "h(end, :)" keeps the space after its separator.
                if (previous != TokenKind::Separator) {
                    trimRightInPlace(result);
                }
                result += text;
                previous = TokenKind::Operator;
                previousToken = &token;
                continue;
            }
            if (isOperatorToken(token)) {
                const bool isCompactSignedLiteral
                    = isCompactSignedLiteralToken(line, lineStart, openStack, token, previousToken,
                        k + 1 < tokens.size() ? tokens[k + 1] : nullptr, previous,
                        tokenAfterStatementHead);
                if (isPostfixToken(token)) {
                    trimRightInPlace(result);
                    result += text;
                } else if (isUnaryToken(token, previous) || isCompactSignedLiteral) {
                    if (previous == TokenKind::ControlKeyword) {
                        appendSpaceIfNeeded(result);
                    } else if (isCompactSignedLiteral) {
                        appendSpaceIfNeeded(result);
                    }
                    result += text;
                } else {
                    trimRightInPlace(result);
                    appendSpaceIfNeeded(result);
                    result += text;
                    result += ' ';
                }
                // A transposed operand ends an operand: "[a' -b']" is two elements.
                previous = isPostfixToken(token) ? TokenKind::Close : TokenKind::Operator;
                previousToken = &token;
                continue;
            }
            if (previous == TokenKind::Operand || previous == TokenKind::ControlKeyword
                || previous == TokenKind::Close) {
                appendSpaceIfNeeded(result);
            }
            result += text;
            // "@" prefixes its handle: "@sin", "@(x) x".
            previous = text == "@" ? TokenKind::Operator : TokenKind::Operand;
            previousToken = &token;
        }
        if (lastTokenEnd >= lineStart) {
            const size_t localEnd = std::min(lastTokenEnd - lineStart, line.size());
            const std::string trailing = line.substr(localEnd);
            const std::string trimmedTrailing = trimLeft(trailing);
            if (!trimmedTrailing.empty()
                && (trimmedTrailing[0] == '%'
                    || (trimmedTrailing.size() >= 2 && trimmedTrailing[0] == '/'
                        && trimmedTrailing[1] == '/'))) {
                trimRightInPlace(result);
                if (!result.empty()) {
                    result += ' ';
                }
                result += trimRight(trimmedTrailing);
            }
        }
        return trimRight(result);
    }
    //=============================================================================
    static std::string
    readOperator(const std::string& text, size_t pos)
    {
        for (const char* op : getFormattingOperators()) {
            const size_t length = std::strlen(op);
            if (pos + length <= text.size() && text.compare(pos, length, op) == 0) {
                return op;
            }
        }
        return std::string();
    }
    //=============================================================================
    static void
    formatCodeSegment(const std::string& code, std::string& result, CodeSegmentState& state,
        std::vector<char>& openStack)
    {
        TokenKind& previous = state.previous;
        bool& afterStatementHead = state.afterStatementHead;
        for (size_t k = 0; k < code.size();) {
            const char ch = code[k];
            if (std::isspace(static_cast<unsigned char>(ch)) != 0) {
                ++k;
                continue;
            }
            const bool charAfterStatementHead = afterStatementHead;
            afterStatementHead = false;
            const bool afterAnonymousParameters = state.afterAnonymousParameters;
            state.afterAnonymousParameters = false;
            if (std::isalpha(static_cast<unsigned char>(ch)) != 0 || ch == '_') {
                afterStatementHead = openStack.empty()
                    && (previous == TokenKind::None || previous == TokenKind::Separator);
                if (previous == TokenKind::Operand || previous == TokenKind::ControlKeyword
                    || previous == TokenKind::Close) {
                    appendSpaceIfNeeded(result);
                }
                const size_t start = k;
                while (k < code.size() && isIdentifierCharacter(code[k])) {
                    ++k;
                }
                const std::string word = code.substr(start, k - start);
                result += word;
                // "function [a, b] = f()" keeps its space like "if (x)".
                previous = isControlKeyword(word) || word == "function" ? TokenKind::ControlKeyword
                                                                        : TokenKind::Operand;
                afterStatementHead = afterStatementHead && previous == TokenKind::Operand;
                continue;
            }
            if (std::isdigit(static_cast<unsigned char>(ch)) != 0) {
                if (previous == TokenKind::Operand || previous == TokenKind::ControlKeyword
                    || previous == TokenKind::Close) {
                    appendSpaceIfNeeded(result);
                }
                const size_t start = k;
                while (k < code.size()) {
                    if (std::isalnum(static_cast<unsigned char>(code[k])) != 0 || code[k] == '_') {
                        ++k;
                        continue;
                    }
                    if (code[k] == '.' && k + 1 < code.size()
                        && std::isdigit(static_cast<unsigned char>(code[k + 1])) != 0) {
                        ++k;
                        continue;
                    }
                    // Exponent sign: "1e-6" is one number.
                    if ((code[k] == '-' || code[k] == '+')
                        && (code[k - 1] == 'e' || code[k - 1] == 'E') && k + 1 < code.size()
                        && std::isdigit(static_cast<unsigned char>(code[k + 1])) != 0) {
                        ++k;
                        continue;
                    }
                    break;
                }
                result += code.substr(start, k - start);
                previous = TokenKind::Operand;
                continue;
            }
            if (ch == '.' && readOperator(code, k).empty()) {
                trimRightInPlace(result);
                result += ch;
                previous = TokenKind::ReferenceDot;
                ++k;
                continue;
            }
            if (ch == '(' || ch == '[' || ch == '{') {
                const bool spaceBefore = k > 0
                    ? std::isspace(static_cast<unsigned char>(code[k - 1])) != 0
                    : result.empty();
                // A validator list keeps its space: "x(1, 1) double {mustBePositive}".
                const bool validatorList = ch == '{' && spaceBefore && openStack.empty()
                    && (previous == TokenKind::Operand || previous == TokenKind::Close);
                if (previous == TokenKind::ControlKeyword || validatorList
                    || (afterAnonymousParameters && spaceBefore)
                    || isSpaceSeparatedElement(openStack, previous, spaceBefore)) {
                    appendSpaceIfNeeded(result);
                } else if (previous == TokenKind::Operator && !result.empty()
                    && result.back() == ' ') {
                    // Keep the space inserted after binary operators, but not after unary
                    // operators.
                } else if (previous != TokenKind::Separator) {
                    trimRightInPlace(result);
                }
                const bool anonymousParameters
                    = ch == '(' && !result.empty() && result.back() == '@';
                result += ch;
                openStack.push_back(anonymousParameters ? anonymousParametersMark : ch);
                previous = TokenKind::Open;
                ++k;
                continue;
            }
            if (ch == ')' || ch == ']' || ch == '}') {
                trimRightInPlace(result);
                result += ch;
                state.afterAnonymousParameters
                    = !openStack.empty() && openStack.back() == anonymousParametersMark;
                if (!openStack.empty()) {
                    openStack.pop_back();
                }
                previous = TokenKind::Close;
                ++k;
                continue;
            }
            if (ch == ',' || ch == ';') {
                trimRightInPlace(result);
                result += ch;
                size_t next = k + 1;
                while (next < code.size()
                    && std::isspace(static_cast<unsigned char>(code[next])) != 0) {
                    ++next;
                }
                if (next >= code.size()
                    || (code[next] != ')' && code[next] != ']' && code[next] != '}')) {
                    result += ' ';
                }
                previous = TokenKind::Separator;
                k = next;
                continue;
            }
            if (ch == ':') {
                if (previous != TokenKind::Separator) {
                    trimRightInPlace(result);
                }
                result += ch;
                previous = TokenKind::Operator;
                ++k;
                continue;
            }
            const std::string op = readOperator(code, k);
            if (!op.empty()) {
                const bool isCompactSignedLiteral = isCompactSignedLiteralOperator(
                    code, openStack, op, k, previous, charAfterStatementHead, result.empty());
                if (isBinaryOperator(op, previous) && !isCompactSignedLiteral) {
                    trimRightInPlace(result);
                    appendSpaceIfNeeded(result);
                    result += op;
                    result += ' ';
                    previous = TokenKind::Operator;
                } else {
                    if (previous == TokenKind::ControlKeyword) {
                        appendSpaceIfNeeded(result);
                    } else if (isCompactSignedLiteral) {
                        appendSpaceIfNeeded(result);
                    }
                    result += op;
                    // A transposed operand ends an operand: "[a' -b']" is two elements.
                    previous
                        = isFormattingPostfixOperator(op) ? TokenKind::Close : TokenKind::Operator;
                }
                k += op.size();
                continue;
            }
            result += ch;
            // "@f" and the metaclass query "?Name" stay attached to what follows.
            previous = ch == '@' || ch == '?' ? TokenKind::Operator : TokenKind::Operand;
            ++k;
        }
    }
    //=============================================================================
    static std::string
    formatLineCodePreservingText(const std::string& line, ScanState& state,
        std::vector<char>& openStack, CodeSegmentState& segmentState)
    {
        std::string result;
        std::string code;
        auto flushCode = [&]() {
            if (!code.empty()) {
                formatCodeSegment(code, result, segmentState, openStack);
                code.clear();
            }
        };
        // Quoted text is an operand: keep it apart from what precedes it
        // ("case 'a'", "[x 'a']", "f(a, 'b')").
        auto startText = [&]() {
            flushCode();
            const TokenKind previous = segmentState.previous;
            if (previous == TokenKind::Operand || previous == TokenKind::Close
                || previous == TokenKind::ControlKeyword || previous == TokenKind::Separator) {
                appendSpaceIfNeeded(result);
            }
            segmentState.previous = TokenKind::Operand;
            segmentState.afterStatementHead = false;
        };
        for (size_t k = 0; k < line.size();) {
            const char ch = line[k];
            if (state == ScanState::BlockComment) {
                const size_t end = line.find("%}", k);
                if (end == std::string::npos) {
                    result += line.substr(k);
                    return trimRight(result);
                }
                result += line.substr(k, end + 2 - k);
                state = ScanState::Normal;
                k = end + 2;
                continue;
            }
            if (ch == '%' && k + 1 < line.size() && line[k + 1] == '{') {
                flushCode();
                trimRightInPlace(result);
                if (!result.empty()) {
                    result += ' ';
                }
                result += line.substr(k);
                state = ScanState::BlockComment;
                return trimRight(result);
            }
            // A "..." continuation: what follows it on the line is comment text.
            if (ch == '%' || line.compare(k, 3, "...") == 0
                || (ch == '/' && k + 1 < line.size() && line[k + 1] == '/')) {
                flushCode();
                trimRightInPlace(result);
                if (!result.empty()) {
                    result += ' ';
                }
                result += line.substr(k);
                return trimRight(result);
            }
            if (ch == '\'' && isSingleQuotedTextStart(line, k)) {
                startText();
                result += '\'';
                ++k;
                while (k < line.size()) {
                    result += line[k];
                    if (line[k] == '\'' && k + 1 < line.size() && line[k + 1] == '\'') {
                        result += line[k + 1];
                        k += 2;
                        continue;
                    }
                    if (line[k] == '\'') {
                        ++k;
                        break;
                    }
                    ++k;
                }
                continue;
            }
            if (ch == '"') {
                startText();
                result += '"';
                ++k;
                while (k < line.size()) {
                    result += line[k];
                    if (line[k] == '"' && k + 1 < line.size() && line[k + 1] == '"') {
                        result += line[k + 1];
                        k += 2;
                        continue;
                    }
                    if (line[k] == '"') {
                        ++k;
                        break;
                    }
                    ++k;
                }
                continue;
            }
            code += ch;
            ++k;
        }
        flushCode();
        return trimRight(result);
    }
    //=============================================================================
    static std::string
    formatClassBlockHeaderSpacing(const std::string& line)
    {
        std::string result = line;
        const std::string trimmed = trimLeft(result);
        for (const char* keyword : getFormattingContextualBlockKeywords()) {
            if (startsKeywordAt(trimmed, 0, keyword)) {
                const size_t leadingSpaces = result.size() - trimmed.size();
                const size_t afterKeyword = leadingSpaces + std::strlen(keyword);
                if (afterKeyword < result.size() && result[afterKeyword] == '(') {
                    result.insert(afterKeyword, 1, ' ');
                }
                break;
            }
        }
        return result;
    }
    //=============================================================================
    static void
    setLeadingSpaces(std::string& line, int spaces)
    {
        if (trimLeft(line).empty()) {
            line.clear();
            return;
        }
        size_t pos = 0;
        while (pos < line.size() && (line[pos] == ' ' || line[pos] == '\t')) {
            ++pos;
        }
        line = std::string(static_cast<size_t>(std::max(spaces, 0)), ' ') + line.substr(pos);
    }
    //=============================================================================
    static void
    validateWithParserAndBytecode(
        const std::string& source, const std::wstring& sourcePath, const BlockEvents& blockEvents)
    {
        std::string parseSource = maskFormattingBlockComments(source);
        if (parseSource.empty() || (parseSource.back() != '\n' && parseSource.back() != '\r')) {
            parseSource += '\n';
        }
        validateBalancedBlocks(blockEvents);
        LexerContext lexerContext;
        ParserContext parserContext = parseStringResult(lexerContext, parseSource);
        if (parserContext.state() == ParseError) {
            std::string message = parserContext.diagnostic.message.empty()
                ? Nelson::resolveMessageTemplate("Nelson:interpreter:unableParseCodeSmart",
                      "Unable to parse code for smart indentation.")
                : parserContext.diagnostic.message;
            throw NelsonException(message);
        }
        try {
            if (classdefLooksLikeSource(parseSource)) {
                ClassdefDefinition definition;
                std::string errorMessage;
                if (!classdef_detail::parseClassdefSourceNativeFull(
                        parseSource, definition, errorMessage)) {
                    throw NelsonException(errorMessage.empty()
                            ? Nelson::resolveMessageTemplate(
                                  "Nelson:interpreter:unableParseClassdefSmart",
                                  "Unable to parse classdef for smart indentation.")
                            : errorMessage);
                }
            }
            if (parserContext.state() == ScriptBlock) {
                if (parserContext.ast != nullptr) {
                    (void)BytecodeCompiler::compileScript(parserContext.ast, sourcePath);
                }
                MacroFunctionDef* functionDef = parserContext.macroFunctionDef;
                while (functionDef != nullptr) {
                    (void)BytecodeCompiler::compileFunction(functionDef->code,
                        functionDef->getName(), sourcePath, functionDef->arguments,
                        functionDef->returnVals, functionDef->classdefOwnerClassName);
                    functionDef = functionDef->nextFunction;
                }
                return;
            }
            if (parserContext.state() == FuncDef && parserContext.macroFunctionDef != nullptr) {
                MacroFunctionDef* functionDef = parserContext.macroFunctionDef;
                while (functionDef != nullptr) {
                    (void)BytecodeCompiler::compileFunction(functionDef->code,
                        functionDef->getName(), sourcePath, functionDef->arguments,
                        functionDef->returnVals, functionDef->classdefOwnerClassName);
                    functionDef = functionDef->nextFunction;
                }
                return;
            }
            if (parserContext.state() == ClassDef) {
                return;
            }
        } catch (const NelsonException&) {
            throw;
        } catch (...) {
            throw NelsonException(
                Nelson::resolveMessageTemplate("Nelson:interpreter:unableCompileCodeSmart",
                    "Unable to compile code for smart indentation."));
        }
    }
    //=============================================================================
    static const std::string utf8ByteOrderMark = "\xEF\xBB\xBF";
    //=============================================================================
    static bool
    startsWithUtf8ByteOrderMark(const std::string& text)
    {
        return text.compare(0, utf8ByteOrderMark.size(), utf8ByteOrderMark) == 0;
    }
    //=============================================================================
    static std::string
    withLexerLineEndings(const std::string& text)
    {
        std::string result;
        result.reserve(text.size());
        for (size_t k = 0; k < text.size(); ++k) {
            if (text[k] == '\r' && k + 1 < text.size() && text[k + 1] == '\n') {
                continue;
            }
            result += text[k];
        }
        return result;
    }
    //=============================================================================
    // Safety net: whatever the spacing rules, never hand back a text that parses
    // to something else than the source.
    static void
    ensureSameSemantics(
        const std::string& source, const std::string& formatted, const std::wstring& sourcePath)
    {
        if (formattingSemanticFingerprint(source) != formattingSemanticFingerprint(formatted)) {
            throw NelsonException(
                Nelson::resolveMessageTemplate("Nelson:interpreter:formattingChangesMeaning",
                    "Formatting would change the meaning of the code; the source is left "
                    "unchanged: ")
                + wstring_to_utf8(sourcePath));
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
smartIndentCode(
    const std::wstring& source, int tabSize, const std::wstring& sourcePath, bool fullFormat)
{
    const int indentSize = std::max(tabSize, 0);
    // Format the text as the lexer reads it (no byte order mark, LF line endings)
    // so that token offsets match line offsets; both are restored on output.
    std::string rawSource = wstring_to_utf8(source);
    const bool hasByteOrderMark = startsWithUtf8ByteOrderMark(rawSource);
    if (hasByteOrderMark) {
        rawSource.erase(0, utf8ByteOrderMark.size());
    }
    const std::vector<SourceLine> rawLines = splitLines(rawSource);
    const std::string utf8Source = withLexerLineEndings(rawSource);
    std::vector<SourceLine> lines = splitLines(utf8Source);
    std::vector<const FormattingToken*> firstTokensByLine(lines.size(), nullptr);
    std::vector<std::vector<const FormattingToken*>> tokensByLine(lines.size());
    const std::vector<FormattingToken> formattingTokens = tokenizeForFormatting(utf8Source);
    for (const FormattingToken& token : formattingTokens) {
        if (token.line >= 0 && static_cast<size_t>(token.line) < firstTokensByLine.size()
            && (token.type != ENDSTMNT || isSeparatorToken(token))) {
            tokensByLine[static_cast<size_t>(token.line)].push_back(&token);
            if (firstTokensByLine[static_cast<size_t>(token.line)] == nullptr
                && token.type != ENDSTMNT && token.type != ENDQSTMNT) {
                firstTokensByLine[static_cast<size_t>(token.line)] = &token;
            }
        }
    }

    const std::vector<TextSpan> textSpans = collectTextSpans(formattingTokens, lines.size());
    const std::vector<BracketLineState> bracketStates
        = collectBracketLineStates(formattingTokens, lines.size());
    const bool classSource = classdefLooksLikeSource(utf8Source);
    BlockEvents blockEvents = collectBlockEvents(lines, tokensByLine, classSource);
    if (!classSource) {
        closeEndlessFunctions(blockEvents);
    }
    validateWithParserAndBytecode(utf8Source, sourcePath, blockEvents);
    int level = 0;
    std::vector<int> switchLevels;
    ScanState scanState = ScanState::Normal;
    ScanState formatState = ScanState::Normal;
    // Brackets stay open across lines ("[1 2 ...", "3 -4]"), the last token only
    // across a "..." continuation.
    std::vector<char> openStack;
    CodeSegmentState carry;
    bool continued = false;
    // A continuation line ("..." above it, or starting inside an open bracket)
    // keeps its extra indentation relative to the first line of its statement,
    // or gets one level when it has none. The line closing the outermost
    // bracket ("};") goes back to the statement indentation.
    int statementSourceIndent = 0;
    int statementIndent = 0;
    size_t lineIndex = 0;
    for (SourceLine& line : lines) {
        const std::string code = stripCodeLine(line.text, scanState);
        const BracketLineState bracketState
            = lineIndex < bracketStates.size() ? bracketStates[lineIndex] : BracketLineState {};
        const bool continuationLine = continued || bracketState.insideBracket;
        if (!continued) {
            carry = CodeSegmentState {};
        }
        continued = code.find("...") != std::string::npos;
        const int sourceIndent = leadingColumns(line.text, indentSize);
        const FormattingToken* firstToken
            = lineIndex < firstTokensByLine.size() ? firstTokensByLine[lineIndex] : nullptr;
        const auto& events = blockEvents[lineIndex];
        const LineKind kind
            = !events.empty() && events.front().leading ? events.front().kind : LineKind::Other;
        const int lineLevel = blockLineLevel(kind, level, switchLevels);
        const TextSpan span = lineIndex < textSpans.size() ? textSpans[lineIndex] : TextSpan::None;
        if (span != TextSpan::None) {
            // Part of the text is on this line: any change to it changes the value.
        } else if (fullFormat) {
            std::string formattedLineFromTokens = lineIndex < tokensByLine.size()
                ? formatLineFromTokens(
                      line.text, tokensByLine[lineIndex], line.startOffset, openStack, carry)
                : std::string();
            line.text = formattedLineFromTokens.empty()
                ? formatLineCodePreservingText(line.text, formatState, openStack, carry)
                : formattedLineFromTokens;
            if (kind == LineKind::ClassBlockOpener
                || (firstToken != nullptr && firstToken->type == ARGUMENTS)) {
                line.text = formatClassBlockHeaderSpacing(line.text);
            }
        } else {
            line.text = trimRight(line.text);
        }
        if (!continuationLine) {
            statementSourceIndent = sourceIndent;
            statementIndent = lineLevel * indentSize;
        }
        const int extraIndent = !continuationLine || bracketState.closesOutermost ? 0
            : sourceIndent > statementSourceIndent ? sourceIndent - statementSourceIndent
                                                   : indentSize;
        if (span != TextSpan::Continues) {
            setLeadingSpaces(line.text, statementIndent + extraIndent);
        }
        for (const BlockEvent& event : events) {
            advanceBlockIndent(event, level, switchLevels);
        }
        ++lineIndex;
    }
    ensureSameSemantics(utf8Source, joinLines(lines), sourcePath);
    for (size_t k = 0; k < lines.size() && k < rawLines.size(); ++k) {
        lines[k].eol = rawLines[k].eol;
    }
    return utf8_to_wstring(
        (hasByteOrderMark ? utf8ByteOrderMark : std::string()) + joinLines(lines));
}
//=============================================================================
void
smartIndentFile(const std::wstring& filename, int tabSize, bool doBackup, bool fullFormat)
{
    std::filesystem::path path(filename);
    std::string content;
    if (!readTextFile(path, content)) {
        throw NelsonException(Nelson::resolveMessageTemplate(
            "Nelson:interpreter:cannotReadFilePassed", "Cannot read file passed to smartindent."));
    }
    content = normalizeTextLineEndings(content);
    std::wstring source;
    if (!utf8_to_wstring(content, source)) {
        throw NelsonException(
            Nelson::resolveMessageTemplate("Nelson:interpreter:cannotDecodeUtfFile",
                "Cannot decode UTF-8 file passed to smartindent."));
    }
    const std::wstring formattedSource = smartIndentCode(source, tabSize, filename, fullFormat);
    std::string formattedContent;
    if (!wstring_to_utf8(formattedSource, formattedContent)) {
        throw NelsonException(
            Nelson::resolveMessageTemplate("Nelson:interpreter:cannotEncodeUtfFile",
                "Cannot encode UTF-8 file passed to smartindent."));
    }
    if (doBackup) {
        std::filesystem::path backupPath = path;
        backupPath += ".bak";
        std::error_code errorCode;
        std::filesystem::copy_file(
            path, backupPath, std::filesystem::copy_options::overwrite_existing, errorCode);
        if (errorCode) {
            throw NelsonException(
                Nelson::resolveMessageTemplate("Nelson:interpreter:cannotCreateBackupFile",
                    "Cannot create backup file passed to smartindent."));
        }
    }
    if (!writeTextFile(path, formattedContent)) {
        throw NelsonException(
            Nelson::resolveMessageTemplate("Nelson:interpreter:cannotWriteFilePassed",
                "Cannot write file passed to smartindent."));
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
