//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Hand-written lexer implementation extracted from the flex grammar.
//
// This file holds the constant tokenizing logic that used to be embedded in
// the %{ ... %} block of NelsonLexer.lxx and copied verbatim into the
// generated NelsonLexer.cpp. It is compiled directly, so the grammar source
// stays minimal and the generated scanner no longer duplicates this code.
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include "StringHelpers.hpp"
#include <cwctype>
#include <cctype>
#include <cstdio>
#include <string>
#include <sys/stat.h>
#include <sys/types.h>
#include <vector>
#include <cstring>
#include <cstdint>
#include <algorithm>
//=============================================================================
#define WS 999
#define YYSTYPE Nelson::ParseRHS
//=============================================================================
#include "AbstractSyntaxTree.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "NelsonException.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FileParser.hpp"
#include "Keywords.hpp"
#include "LexerInterface.hpp"
#include "NelsonFormattingTokens.hpp"
#include "NelsonParserHelpers.hpp"
#include "NelsonParserTokens.hpp"
#include "LexerContext.hpp"
#include "NelsonLexerHelpers.hpp"
#include "AnonymousFunctionText.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Process-wide fallback resolver for the command-vs-expression variable check,
// used when a parse context has no LexerContext::isVariableHook of its own (the
// local contexts that parse script and function bodies). Installed by the
// Evaluator; see setLexerVariableResolver.
static bool (*g_lexerVariableResolver)(const std::string&) = nullptr;
//=============================================================================
namespace Nelson {
void
setLexerVariableResolver(bool (*resolver)(const std::string&))
{
    g_lexerVariableResolver = resolver;
}
} // namespace Nelson
//=============================================================================
void
clearTextBufferLexer(Nelson::LexerContext& lexerContext)
{
    lexerContext.textbufferStorage.clear();
    lexerContext.textbuffer = nullptr;
    lexerContext.datap = nullptr;
    lexerContext.linestart = nullptr;
}
//=============================================================================
indexType
ContextInt(LexerContext& lexerContext)
{
    if (lexerContext.datap == lexerContext.linestart) {
        return (1 << 16 | lexerContext.lineNumber);
    }
    return ((lexerContext.datap - lexerContext.linestart + 1) << 16)
        | (lexerContext.lineNumber + 1);
}
//=============================================================================
void
NextLine(LexerContext& lexerContext)
{
    lexerContext.lineNumber++;
    lexerContext.linestart = lexerContext.datap;
}
//=============================================================================
static void
LexerException(LexerContext& lexerContext, const std::string& msg)
{
    std::string error_message;

    if (!getParserFilenameU().empty() && !msg.empty()) {
        error_message = formatI18nMessage(resolveMessageTemplate("Nelson:Lexer:atLineOfFile",
                                              "Lexical error '%s' at line %d of file %s"),
            msg, lexerContext.lineNumber + 1, getParserFilenameU());
    } else {
        if (!msg.empty()) {
            error_message = formatI18nMessage(
                resolveMessageTemplate("Nelson:Lexer:withToken", "Lexical error '%s'"), msg);
        } else {
            error_message = resolveMessageTemplate("Nelson:Lexer", "Lexical error");
        }
    }

    setParserDiagnostic(error_message, lexerContext.lineNumber + 1,
        static_cast<int>(lexerContext.datap - lexerContext.linestart + 1));
    raiseParserError(error_message, "Nelson:Lexer");
}
//=============================================================================
inline void
pushBracket(LexerContext& lexerContext, char t, bool isDestructuring, bool isFunctionCall,
    bool isAnonymousFunctionArguments)
{
    if (lexerContext.bracketStackSize >= DEFAULT_BUFFER_SIZE_LEXER) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate(
                "Nelson:interpreter:nestingTooDeep", "nesting too deep"));
    }
    lexerContext.bracketStack[lexerContext.bracketStackSize] = t;
    lexerContext.bracketIsDestructuring[lexerContext.bracketStackSize] = isDestructuring;
    lexerContext.bracketIsFunctionCall[lexerContext.bracketStackSize] = isFunctionCall;
    lexerContext.bracketIsAnonymousFunctionArguments[lexerContext.bracketStackSize]
        = isAnonymousFunctionArguments;
    lexerContext.bracketStackSize++;
}
//=============================================================================
inline bool
isTopBracketAnonymousFunctionArguments(const LexerContext& lexerContext)
{
    return lexerContext.bracketStackSize > 0
        && lexerContext.bracketIsAnonymousFunctionArguments[lexerContext.bracketStackSize - 1];
}
//=============================================================================
inline void
popBracket(LexerContext& lexerContext, char t)
{
    if (lexerContext.bracketStackSize <= 0) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate(
                "Nelson:interpreter:mismatchedParenthesis", "mismatched parenthesis"));
    }
    if (lexerContext.bracketStack[--lexerContext.bracketStackSize] != t) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate(
                "Nelson:interpreter:mismatchedParenthesis", "mismatched parenthesis"));
    }
    lexerContext.bracketIsDestructuring[lexerContext.bracketStackSize] = false;
    lexerContext.bracketIsFunctionCall[lexerContext.bracketStackSize] = false;
    lexerContext.bracketIsAnonymousFunctionArguments[lexerContext.bracketStackSize] = false;
}
//=============================================================================
inline void
pushVCState(LexerContext& lexerContext)
{
    if (lexerContext.vcStackSize >= DEFAULT_BUFFER_SIZE_LEXER) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate(
                "Nelson:interpreter:nestingTooDeep", "nesting too deep"));
    }
    lexerContext.vcStack[lexerContext.vcStackSize++] = lexerContext.vcFlag;
}
//=============================================================================
inline void
popVCState(LexerContext& lexerContext)
{
    lexerContext.vcFlag = lexerContext.vcStack[--lexerContext.vcStackSize];
}
//=============================================================================
inline char
peekPreviousMeaningfulChar(LexerContext& lexerContext)
{
    if (lexerContext.datap == lexerContext.textbuffer) {
        return '\0';
    }
    const char* begin = lexerContext.textbuffer;
    const char* cursor = lexerContext.datap - 1;
    while (cursor >= begin) {
        const unsigned char current = static_cast<unsigned char>(*cursor);
        if (isspace(current)) {
            cursor--;
            continue;
        }
        if (*cursor == '%') {
            while (cursor >= begin && *cursor != '\n') {
                cursor--;
            }
            continue;
        }
        if ((*cursor == '.') && (cursor - begin >= 2) && (cursor[-1] == '.')
            && (cursor[-2] == '.')) {
            cursor -= 3;
            continue;
        }
        return static_cast<char>(current);
    }
    return '\0';
}
//=============================================================================
inline char
peekNextMeaningfulChar(LexerContext& lexerContext)
{
    const char* cursor = lexerContext.datap + 1;
    while (*cursor != '\0') {
        if (*cursor == '%') {
            while (*cursor != '\0' && *cursor != '\n') {
                cursor++;
            }
            continue;
        }
        if ((*cursor == '.') && (cursor[1] != '\0') && (cursor[2] != '\0') && (cursor[1] == '.')
            && (cursor[2] == '.')) {
            cursor += 3;
            while (*cursor != '\0' && *cursor != '\n') {
                cursor++;
            }
            continue;
        }
        if (isspace(static_cast<unsigned char>(*cursor))) {
            cursor++;
            continue;
        }
        return *cursor;
    }
    return '\0';
}
//=============================================================================
inline const char*
skipWhitespaceCommentsAndContinuations(const char* cursor)
{
    while (*cursor != '\0') {
        if ((*cursor == ' ') || (*cursor == '\t') || (*cursor == '\r') || (*cursor == '\n')) {
            cursor++;
            continue;
        }
        if (*cursor == '%') {
            while ((*cursor != '\0') && (*cursor != '\n')) {
                cursor++;
            }
            continue;
        }
        if ((*cursor == '.') && (cursor[1] != '\0') && (cursor[2] != '\0') && (cursor[1] == '.')
            && (cursor[2] == '.')) {
            cursor += 3;
            while ((*cursor != '\0') && (*cursor != '\n')) {
                cursor++;
            }
            continue;
        }
        break;
    }
    return cursor;
}
//=============================================================================
inline const char*
skipStringLiteral(const char* cursor)
{
    const char delimiter = *cursor;
    cursor++;
    while (*cursor != '\0') {
        if ((*cursor == delimiter) && (cursor[1] == delimiter)) {
            cursor += 2;
            continue;
        }
        if (*cursor == delimiter) {
            cursor++;
            break;
        }
        cursor++;
    }
    return cursor;
}
//=============================================================================
inline bool
bracketFollowedByAssignment(LexerContext& lexerContext)
{
    const char* cursor = lexerContext.datap + 1;
    int depth = 1;
    while (*cursor != '\0' && depth > 0) {
        if (*cursor == '%') {
            while ((*cursor != '\0') && (*cursor != '\n')) {
                cursor++;
            }
            continue;
        }
        if ((*cursor == '.') && (cursor[1] != '\0') && (cursor[2] != '\0') && (cursor[1] == '.')
            && (cursor[2] == '.')) {
            cursor += 3;
            while ((*cursor != '\0') && (*cursor != '\n')) {
                cursor++;
            }
            continue;
        }
        if ((*cursor == '\'') || (*cursor == '"')) {
            cursor = skipStringLiteral(cursor);
            continue;
        }
        if (*cursor == '[') {
            depth++;
        } else if (*cursor == ']') {
            depth--;
            cursor++;
            if (depth == 0) {
                break;
            }
            continue;
        }
        cursor++;
    }
    if (depth != 0) {
        return false;
    }
    cursor = skipWhitespaceCommentsAndContinuations(cursor);
    return (*cursor == '=');
}
//=============================================================================
inline bool
isFunctionCallOpening(const LexerContext& lexerContext)
{
    return (lexerContext.previousToken == IDENT);
}
//=============================================================================
inline bool
isInsideFunctionCallArguments(const LexerContext& lexerContext)
{
    if (lexerContext.bracketStackSize <= 0) {
        return false;
    }
    const int idx = lexerContext.bracketStackSize - 1;
    return (lexerContext.bracketStack[idx] == '(') && lexerContext.bracketIsFunctionCall[idx];
}
//=============================================================================
inline bool
isTildeSlotPattern(LexerContext& lexerContext)
{
    if (lexerContext.bracketStackSize <= 0) {
        return false;
    }
    const char enclosing
        = static_cast<char>(lexerContext.bracketStack[lexerContext.bracketStackSize - 1]);
    if ((enclosing != '[') && (enclosing != '{')) {
        return false;
    }
    const char previous = peekPreviousMeaningfulChar(lexerContext);
    if ((previous != enclosing) && (previous != ',')) {
        return false;
    }
    const char next = peekNextMeaningfulChar(lexerContext);
    if (enclosing == '[') {
        return (next == ',' || next == ']');
    }
    return (next == ',' || next == '}');
}
//=============================================================================
inline bool
isTildePlaceholder(LexerContext& lexerContext)
{
    if (!isTildeSlotPattern(lexerContext)) {
        return false;
    }
    const char enclosing
        = static_cast<char>(lexerContext.bracketStack[lexerContext.bracketStackSize - 1]);
    if (enclosing != '[') {
        return false;
    }
    return lexerContext.bracketIsDestructuring[lexerContext.bracketStackSize - 1];
}
//=============================================================================
inline bool
isIllegalTildeSlotUsage(LexerContext& lexerContext)
{
    if (!isTildeSlotPattern(lexerContext)) {
        return false;
    }
    return !isTildePlaceholder(lexerContext);
}
//=============================================================================
inline std::string
generatePlaceholderIdentifier(LexerContext& lexerContext)
{
    lexerContext.placeholderCounter++;
    return fmt::format("{}{}", UNUSED_PLACEHOLDER_PREFIX, lexerContext.placeholderCounter);
}
//=============================================================================
inline bool
isPathCommandShortCut(const std::wstring& wcommand, const std::wstring& wline)
{
    std::wstring trimmedLine = StringHelpers::trim_copy(wline);
    if (StringHelpers::ends_with(trimmedLine, L"\n")) {
        trimmedLine.pop_back();
    }
    if (trimmedLine == wcommand) {
        return true;
    }
    if (StringHelpers::starts_with(trimmedLine, wcommand + L" ")) {
        StringHelpers::replace_first(trimmedLine, wcommand + L" ", L"");
        StringHelpers::trim(trimmedLine);

        // A leading '=' means an assignment ("dir = expr") or a comparison
        // ("dir == expr"): the word is used as a variable, not a path command.
        if (StringHelpers::starts_with(trimmedLine, L"=")) {
            return false;
        }

        // Strip any comment from the end (% or %{)
        size_t commentPos = trimmedLine.find(L'%');
        if (commentPos != std::wstring::npos) {
            // Everything before % is the potential path
            trimmedLine = trimmedLine.substr(0, commentPos);
            StringHelpers::trim(trimmedLine);
            // If nothing left after removing comment, not a path command
            if (trimmedLine.empty()) {
                return false;
            }
        }

        bool haveSimpleQuotes = StringHelpers::starts_with(trimmedLine, L"'")
            && StringHelpers::ends_with(trimmedLine, L"'");
        return (haveSimpleQuotes || !StringHelpers::starts_with(trimmedLine, L"("));
    }
    return false;
}
//=============================================================================
inline bool
isNoArgumentPathCommandShortCut(const std::wstring& wline)
{
    std::wstring trimmedLine = StringHelpers::trim_copy(wline);
    if (StringHelpers::ends_with(trimmedLine, L"\n")) {
        trimmedLine.pop_back();
    }
    static const std::wstring commands[] = { L"ls", L"cd", L"dir" };
    for (const auto& command : commands) {
        if (trimmedLine == command) {
            return true;
        }
    }
    return false;
}
//=============================================================================
inline bool
isDebugCommandShortCut(
    LexerContext& lexerContext, const std::wstring& wcommand, const std::wstring& wline)
{
    std::wstring trimmedLine = StringHelpers::trim_copy(wline);
    if (StringHelpers::ends_with(trimmedLine, L"\n")) {
        trimmedLine.pop_back();
    }
    if (StringHelpers::starts_with(trimmedLine, wcommand + L" ")) {
        StringHelpers::replace_first(trimmedLine, wcommand + L" ", L"");
        StringHelpers::trim(trimmedLine);
        if (!StringHelpers::starts_with(trimmedLine, L"(")) {
            lexerContext.inDebugCommand = true;
            return true;
        }
    }
    return false;
}
//=============================================================================
inline bool
testSpecialFuncs(LexerContext& lexerContext)
{
    const std::wstring wline = utf8_to_wstring(std::string(lexerContext.datap));

    // Early return if the first character is not alphabetic
    if (wline.empty() || !iswalpha(wline[0])) {
        return false;
    }

    // Check for debug commands (dbstop, dbclear)
    static const std::wstring debugCommands[] = { L"dbstop", L"dbclear" };
    for (const auto& command : debugCommands) {
        if (isDebugCommandShortCut(lexerContext, command, wline)) {
            return true;
        }
    }

    // Check for hardcoded shortcuts
    static const std::wstring commands[] = { L"ls", L"cd", L"dir", L"addpath", L"rmpath" };
    for (const auto& command : commands) {
        if (isPathCommandShortCut(command, wline)) {
            return true;
        }
    }

    // Check for keyword identifier length
    std::wstring wkeyword;
    size_t i = 0;
    wkeyword.reserve(IDENTIFIER_LENGTH_MAX);

    while (i < wline.size() && iswalnum(wline[i]) && i < (IDENTIFIER_LENGTH_MAX + 1)) {
        wkeyword.push_back(wline[i]);
        ++i;
    }

    const std::string keyword = wstring_to_utf8(wkeyword);

    // Error handling if the keyword length exceeds the max length
    if (keyword.length() > IDENTIFIER_LENGTH_MAX) {
        Error("Nelson:namelengthmaxexceeded", "Maximum name length exceeded.");
    }
    return false;
}
//=============================================================================
inline void
setTokenType(LexerContext& lexerContext, int type)
{
    lexerContext.tokenType = type;
    lexerContext.tokenActive = 1;
    lexerContext.tokenValue.isToken = true;
    lexerContext.tokenValue.v.p = nullptr;
}
//=============================================================================
static void
setIdentifierToken(LexerContext& lexerContext, const char* ident, int context)
{
    setTokenType(lexerContext, IDENT);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(id_node, ident, context);
}
//=============================================================================
static void
rejectInvalidDeclarationCommandOperand(LexerContext& lexerContext, const char* ident)
{
    if (strcmp(ident, "global") != 0 && strcmp(ident, "persistent") != 0) {
        return;
    }
    const char* p = lexerContext.datap;
    while (*p == ' ' || *p == '\t') {
        ++p;
    }
    if (*p == '(') {
        return;
    }
    bool expectOperandStart = true;
    bool inOperand = false;
    while (*p != '\0' && *p != '\n' && *p != '\r' && *p != ';' && *p != ',') {
        if (*p == '%') {
            return;
        }
        if (*p == ' ' || *p == '\t') {
            if (inOperand) {
                expectOperandStart = true;
                inOperand = false;
            }
            ++p;
            continue;
        }
        if (expectOperandStart
            && (*p == '\'' || *p == '"' || std::isdigit(static_cast<unsigned char>(*p)))) {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate("Nelson:interpreter:containValidVariableName",
                    "Argument must contain a valid variable name."));
        }
        expectOperandStart = false;
        inOperand = true;
        ++p;
    }
}
//=============================================================================
static void
setSpecialCallToken(LexerContext& lexerContext)
{
    lexerContext.tokenType = SPECIALCALL;
}
//=============================================================================
static bool
isStatementStartPreviousToken(const LexerContext& lexerContext)
{
    return lexerContext.previousToken == 0 || lexerContext.previousToken == ENDSTMNT
        || lexerContext.previousToken == ENDQSTMNT || lexerContext.previousToken == ',';
}
//=============================================================================
static bool
isDottedOperatorAhead(const char* p)
{
    if (p == nullptr || p[0] != '.') {
        return false;
    }
    return p[1] == '*' || p[1] == '/' || p[1] == '\\' || p[1] == '^' || p[1] == '\'';
}
//=============================================================================
static bool
hasTopLevelAssignmentAhead(const char* p)
{
    int depth = 0;
    bool inSingleQuote = false;
    bool inDoubleQuote = false;
    for (const char* current = p; *current != '\0'; ++current) {
        const char ch = *current;
        if (inSingleQuote) {
            if (ch == '\'' && current[1] == '\'') {
                ++current;
            } else if (ch == '\'') {
                inSingleQuote = false;
            }
            continue;
        }
        if (inDoubleQuote) {
            if (ch == '"' && current[1] == '"') {
                ++current;
            } else if (ch == '"') {
                inDoubleQuote = false;
            }
            continue;
        }
        if (ch == '\'' || ch == '"') {
            inSingleQuote = (ch == '\'');
            inDoubleQuote = (ch == '"');
            continue;
        }
        if (ch == '\n' || ch == '\r' || ch == ';' || ch == ',') {
            return false;
        }
        if (ch == '(' || ch == '[' || ch == '{') {
            ++depth;
            continue;
        }
        if (ch == ')' || ch == ']' || ch == '}') {
            if (depth > 0) {
                --depth;
            }
            continue;
        }
        if (depth == 0 && ch == '=' && current[-1] != '=' && current[-1] != '<'
            && current[-1] != '>' && current[-1] != '~' && current[1] != '=') {
            return true;
        }
    }
    return false;
}
//=============================================================================
static int
getFunctionDeclarationState(const LexerContext& lexerContext)
{
    return (lexerContext.pendingCommaContext < 0 && lexerContext.pendingCommaContext > -10)
        ? -lexerContext.pendingCommaContext
        : 0;
}
//=============================================================================
static void
setFunctionDeclarationState(LexerContext& lexerContext, int state)
{
    lexerContext.pendingCommaContext = state > 0 ? -state : 0;
}
//=============================================================================
static bool
isAfterFunctionTerminator(const LexerContext& lexerContext)
{
    return lexerContext.pendingCommaContext == -10;
}
//=============================================================================
static void
setAfterFunctionTerminator(LexerContext& lexerContext, bool value)
{
    lexerContext.pendingCommaContext = value ? -10 : 0;
}
//=============================================================================
static void
updateCatchIdentifierState(LexerContext& lexerContext, int tokenType)
{
    if (tokenType == WS) {
        return;
    }
    if (lexerContext.catchIdentifierState == 0) {
        lexerContext.catchIdentifierState = (tokenType == CATCH) ? 1 : 0;
        return;
    }
    if (lexerContext.catchIdentifierState == 1) {
        lexerContext.catchIdentifierState = (tokenType == IDENT) ? 2 : 0;
        return;
    }
    lexerContext.catchIdentifierState = 0;
}
//=============================================================================
inline int
match(LexerContext& lexerContext, const char* str)
{
    if (strncmp(str, lexerContext.datap, strlen(str)) == 0) {
        lexerContext.datap += strlen(str);
        return 1;
    }
    return 0;
}
//=============================================================================
inline int
isE(char p)
{
    return static_cast<int>((p == 'e') || (p == 'E') || (p == 'd') || (p == 'D'));
}
//=============================================================================
inline int
isWhitespace(LexerContext& lexerContext)
{
    return static_cast<int>((match(lexerContext, " ") != 0) || (match(lexerContext, "\t") != 0));
}
//=============================================================================
inline int
isNewline(LexerContext& lexerContext)
{
    return static_cast<int>((match(lexerContext, "\n") != 0) || (match(lexerContext, "\r\n") != 0));
}
//=============================================================================
inline int
testAlphaChar(LexerContext& lexerContext)
{
    int c = static_cast<int>(lexerContext.datap[0]);
    if (c < 0) {
        return 0;
    }
    return (isalpha(c));
}
//=============================================================================
inline int
testAlphaNumChar(LexerContext& lexerContext)
{
    int c = static_cast<int>(lexerContext.datap[0]);
    if (c < 0) {
        return 0;
    }
    return static_cast<int>((isalnum(c) != 0) || (c == '_'));
}
//=============================================================================
inline int
_isDigit(char c)
{
    return static_cast<int>(c >= 48 && c <= 57);
}
//=============================================================================
inline int
testDigit(LexerContext& lexerContext)
{
    int c = static_cast<int>(lexerContext.datap[0]);
    return (_isDigit(c));
}
//=============================================================================
inline int
testNewline(LexerContext& lexerContext)
{
    return static_cast<int>((lexerContext.datap[0] == 0) || (lexerContext.datap[0] == '\n')
        || ((lexerContext.datap[0] == '\r') && (lexerContext.datap[1] == '\n')));
}
//=============================================================================
inline int
previousChar(LexerContext& lexerContext)
{
    if (lexerContext.datap == lexerContext.textbuffer) {
        return 0;
    }
    return lexerContext.datap[-1];
}
//=============================================================================
inline int
currentChar(LexerContext& lexerContext)
{
    return lexerContext.datap[0];
}
//=============================================================================
inline void
discardChar(LexerContext& lexerContext)
{
    lexerContext.datap++;
}
//=============================================================================
static const char*
skipContinuationLine(const char* current)
{
    // Everything after a '...' up to the end of the physical line is dropped as
    // continuation trivia. A '%{' appearing here is ordinary comment text, not a
    // block-comment opener (a block comment only opens when '%{' is alone on its
    // line), so it must not consume past the newline.
    while (*current != '\0' && *current != '\n' && *current != '\r') {
        ++current;
    }
    return current;
}
//=============================================================================
static const char*
skipContinuationTrivia(const char*, const char* ptr)
{
    const char* current = ptr;
    if (strncmp(current, "...", 3) != 0) {
        return current;
    }
    current += 3;
    current = skipContinuationLine(current);
    for (;;) {
        while (*current == ' ' || *current == '\t') {
            ++current;
        }
        if (*current == '%') {
            current = skipContinuationLine(current);
        }
        if (*current == '\r') {
            ++current;
            if (*current == '\n') {
                ++current;
            }
            continue;
        }
        if (*current == '\n') {
            ++current;
            continue;
        }
        if (strncmp(current, "...", 3) == 0) {
            current += 3;
            while (*current != '\0' && *current != '\n' && *current != '\r') {
                ++current;
            }
            continue;
        }
        break;
    }
    return current;
}
//=============================================================================
static void
fetchMultilineComment(LexerContext& lexerContext);
//=============================================================================
static void
skipContinuationLine(LexerContext& lexerContext)
{
    // Drop the remainder of the physical line as continuation trivia. A mid-line
    // '%{' here is comment text, not a block-comment opener, so it must not be
    // routed to fetchMultilineComment (that would swallow the following lines).
    while (lexerContext.datap[0] != '\0' && lexerContext.datap[0] != '\n'
        && lexerContext.datap[0] != '\r') {
        discardChar(lexerContext);
    }
}
//=============================================================================
static void
skipContinuationTrivia(LexerContext& lexerContext)
{
    skipContinuationLine(lexerContext);
    bool keepScanning = true;
    while (keepScanning) {
        keepScanning = false;
        while (lexerContext.datap[0] == ' ' || lexerContext.datap[0] == '\t') {
            discardChar(lexerContext);
        }
        if (lexerContext.datap[0] == '%' && lexerContext.datap[1] == '{') {
            fetchMultilineComment(lexerContext);
            keepScanning = true;
            continue;
        }
        if (lexerContext.datap[0] == '%') {
            skipContinuationLine(lexerContext);
            keepScanning = true;
        }
        if (match(lexerContext, "\r\n") != 0 || match(lexerContext, "\n") != 0) {
            NextLine(lexerContext);
            keepScanning = true;
        }
        if (match(lexerContext, "...") != 0) {
            lexerContext.continuationCount++;
            while (lexerContext.datap[0] != '\0' && lexerContext.datap[0] != '\n'
                && lexerContext.datap[0] != '\r') {
                discardChar(lexerContext);
            }
            keepScanning = true;
        }
    }
}
//=============================================================================
inline int
testCharacterArrayTerm(LexerContext& lexerContext)
{
    return static_cast<int>((lexerContext.datap[0] == '\n') || (lexerContext.datap[0] == '\r')
        || (lexerContext.datap[0] == ';') || (lexerContext.datap[0] == ',')
        || (lexerContext.datap[0] == ' '));
}
//=============================================================================
inline bool
hasNamedArgumentAssignment(const LexerContext& lexerContext)
{
    if (!isInsideFunctionCallArguments(lexerContext)) {
        return false;
    }
    const char* cursor = lexerContext.datap;
    while ((*cursor == ' ') || (*cursor == '\t')) {
        cursor++;
    }
    if (*cursor != '=') {
        return false;
    }
    return (cursor[1] != '=');
}
//=============================================================================
// Convert "name = value" sequences inside function call parentheses into
// quoted positional arguments so the parser sees ""name", value".
static bool
convertIdentifierToNamedArgument(LexerContext& lexerContext, const char* ident, int context)
{
    if (!hasNamedArgumentAssignment(lexerContext)) {
        return false;
    }
    setTokenType(lexerContext, STRING);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(const_string_node, ident, context);

    while ((lexerContext.datap[0] == ' ') || (lexerContext.datap[0] == '\t')) {
        discardChar(lexerContext);
    }
    if (currentChar(lexerContext) == '=') {
        discardChar(lexerContext);
    }
    while ((lexerContext.datap[0] == ' ') || (lexerContext.datap[0] == '\t')) {
        discardChar(lexerContext);
    }

    lexerContext.pendingNamedArgumentComma = true;
    lexerContext.pendingCommaContext = context;
    return true;
}
//=============================================================================
void
lexUntermCharacterArray(LexerContext& lexerContext)
{
    std::string stringval;
    while (isWhitespace(lexerContext) != 0) {
        ;
    }
    // A '...' at an argument boundary is a line continuation joining the next
    // physical line to this command (for example a 'global' or 'persistent'
    // declaration split over several lines), so consume it and the continuation
    // trivia instead of taking '...' as a literal argument.
    while (match(lexerContext, "...") != 0) {
        lexerContext.continuationCount++;
        skipContinuationTrivia(lexerContext);
        while (isWhitespace(lexerContext) != 0) {
            ;
        }
    }
    // Command syntax: every space-separated argument on the line is a literal
    // character array, so an argument such as Component:some-id (with a colon or
    // a hyphen) is taken as a single string instead of being lexed as separate
    // operator tokens. The command ends at a newline, ';', ',' or the start of a
    // comment '%', which are left for the scanning state to handle.
    if (testNewline(lexerContext) != 0 || currentChar(lexerContext) == ';'
        || currentChar(lexerContext) == ',' || currentChar(lexerContext) == '%') {
        lexerContext.lexState = Scanning;
        return;
    }
    // Track bracket nesting so that separators (spaces, commas, semicolons)
    // appearing inside a parenthesised argument such as f(x, y) do not split the
    // argument: the whole f(x, y) is kept as a single literal character array.
    // At bracket depth zero the separators still terminate the argument, so
    // 'a b c' remains three separate arguments.
    int bracketDepth = 0;
    while (true) {
        const char currentValue = static_cast<char>(currentChar(lexerContext));
        if (currentValue == '\0' || currentValue == '\n' || currentValue == '\r') {
            break;
        }
        if (bracketDepth == 0
            && (currentValue == ' ' || currentValue == ',' || currentValue == ';'
                || currentValue == '%')) {
            // '%' starts a comment (e.g. hold on% comment), ending the command
            // even without a preceding space; leave it for the scanning state.
            break;
        }
        if (currentValue == '(' || currentValue == '[' || currentValue == '{') {
            ++bracketDepth;
        } else if (currentValue == ')' || currentValue == ']' || currentValue == '}') {
            if (bracketDepth > 0) {
                --bracketDepth;
            }
        }
        stringval.push_back(currentValue);
        discardChar(lexerContext);
    }
    setTokenType(lexerContext, CHARACTER);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_character_array_node, stringval, static_cast<int>(ContextInt(lexerContext)));
#ifdef LEXDEBUG
    printf("Untermed string %s\r\n", stringval);
#endif
    lexerContext.lexState = SpecScan;
}
//=============================================================================
void
lexString(LexerContext& lexerContext)
{
    std::string stringval;
    discardChar(lexerContext);
    int curchar = currentChar(lexerContext);
    char ch = lexerContext.datap[1];
    while (
        (curchar != '"') || ((curchar == '"') && (ch == '"')) && (testNewline(lexerContext) == 0)) {
        if ((currentChar(lexerContext) == '"') && (ch == '"')) {
            discardChar(lexerContext);
        }
        stringval.push_back(static_cast<char>(curchar));
        discardChar(lexerContext);
        curchar = currentChar(lexerContext);
        if (strlen(lexerContext.datap) > 1) {
            ch = lexerContext.datap[1];
        } else {
            break;
        }
    }
    if (testNewline(lexerContext) != 0) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate(
                "Nelson:interpreter:unterminatedString", "unterminated string"));
    }
    discardChar(lexerContext);
    setTokenType(lexerContext, STRING);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_string_node, stringval, static_cast<int>(ContextInt(lexerContext)));
}
//=============================================================================
void
lexCharacterArray(LexerContext& lexerContext)
{
    std::string stringval;
    discardChar(lexerContext);
    int curchar = currentChar(lexerContext);
    char ch = lexerContext.datap[1];
    while ((curchar != '\'')
        || ((curchar == '\'') && (ch == '\'')) && (testNewline(lexerContext) == 0)) {
        if ((currentChar(lexerContext) == '\'') && (ch == '\'')) {
            discardChar(lexerContext);
        }
        stringval.push_back(static_cast<char>(curchar));
        discardChar(lexerContext);
        curchar = currentChar(lexerContext);
        if (strlen(lexerContext.datap) > 1) {
            ch = lexerContext.datap[1];
        } else {
            break;
        }
    }
    if (testNewline(lexerContext) != 0) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate(
                "Nelson:interpreter:unterminatedCharacterArray", "unterminated character array"));
    }
    discardChar(lexerContext);
    setTokenType(lexerContext, CHARACTER);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_character_array_node, stringval, static_cast<int>(ContextInt(lexerContext)));
}
//=============================================================================
// Contextual "arguments" validation block support (native front-end). Defined
// later, alongside the shared string helpers they rely on.
static bool
argumentsHeaderLineMatches(const char* restOfLine);
static void
lexArgumentsBlock(LexerContext& lexerContext, int context);
//=============================================================================
// Add every [A-Za-z_][A-Za-z0-9_]* run in [begin, end) to the set of names
// statically known to be variables (used by the command-vs-expression rule).
static void
addIdentifiersAsLexicalVariables(LexerContext& lexerContext, const char* begin, const char* end)
{
    const char* p = begin;
    while (p < end) {
        const unsigned char ch = static_cast<unsigned char>(*p);
        if ((isalpha(ch) != 0) || (ch == '_')) {
            const char* start = p;
            while (p < end && ((isalnum(static_cast<unsigned char>(*p)) != 0) || (*p == '_'))) {
                ++p;
            }
            lexerContext.lexicalVariableNames.emplace(start, p);
        } else {
            ++p;
        }
    }
}
//=============================================================================
// Record a function's output and input parameter names as variables, so a
// command-position use of a parameter parses as an expression
// ("function f(x); x -1; end" is x-1, not the command x('-1')). Called once per
// "function" keyword with the cursor just past it; the scan is bounded to the
// signature (outputs before '=', inputs inside the '(...)') and never reads into
// the body. The function name itself is deliberately skipped.
static void
recordFunctionSignatureVariables(LexerContext& lexerContext, const char* afterKeyword)
{
    const char* lineEnd = afterKeyword;
    while ((*lineEnd != '\0') && (*lineEnd != '\n') && (*lineEnd != '\r')) {
        ++lineEnd;
    }
    // A top-level '=' before the first '(' separates the output list from the name.
    const char* eq = nullptr;
    for (const char* p = afterKeyword; p < lineEnd; ++p) {
        if ((*p == '(') || (*p == '%')) {
            if (*p == '%') {
                lineEnd = p;
            }
            break;
        }
        if ((*p == '=') && (p[1] != '=') && ((p == afterKeyword) || (p[-1] != '='))) {
            eq = p;
            break;
        }
    }
    if (eq != nullptr) {
        addIdentifiersAsLexicalVariables(lexerContext, afterKeyword, eq);
    }
    // Skip whitespace to the function name and step over it.
    const char* p = (eq != nullptr) ? (eq + 1) : afterKeyword;
    while ((p < lineEnd) && ((*p == ' ') || (*p == '\t'))) {
        ++p;
    }
    while ((p < lineEnd) && ((isalnum(static_cast<unsigned char>(*p)) != 0) || (*p == '_'))) {
        ++p;
    }
    // Inputs: identifiers inside the balanced '(...)'.
    while ((p < lineEnd) && (*p != '(') && (*p != '%')) {
        ++p;
    }
    if ((p < lineEnd) && (*p == '(')) {
        const char* open = p + 1;
        const char* q = open;
        int depth = 1;
        while ((q < lineEnd) && (depth > 0)) {
            if (*q == '(') {
                ++depth;
            } else if (*q == ')') {
                --depth;
                if (depth == 0) {
                    break;
                }
            }
            ++q;
        }
        addIdentifiersAsLexicalVariables(lexerContext, open, q);
    }
}
//=============================================================================
void
lexIdentifier(LexerContext& lexerContext)
{
    int i = 0;
    char ident[IDENTIFIER_LENGTH_MAX + 1];
    while (testAlphaNumChar(lexerContext) != 0) {
        if (i >= IDENTIFIER_LENGTH_MAX) {
            std::string msg = formatI18nMessage(
                Nelson::resolveMessageTemplate("Nelson:interpreter:exceedsNelsonMaximumName",
                    "exceeds the Nelson maximum name length of %d characters."),
                IDENTIFIER_LENGTH_MAX);
            LexerException(lexerContext, msg);
        }
        ident[i++] = currentChar(lexerContext);
        discardChar(lexerContext);
    }
    // In debug command mode, include > followed by alphanumeric as part of identifier
    // This allows dbstop in test>myfunc syntax without quotes
    if (lexerContext.inDebugCommand && currentChar(lexerContext) == '>') {
        // Check if next char starts a valid identifier
        if (lexerContext.datap[1] != '\0'
            && (isalpha(static_cast<unsigned char>(lexerContext.datap[1]))
                || lexerContext.datap[1] == '_')) {
            if (i >= IDENTIFIER_LENGTH_MAX) {
                std::string msg = formatI18nMessage(
                    Nelson::resolveMessageTemplate("Nelson:interpreter:exceedsNelsonMaximumName",
                        "exceeds the Nelson maximum name length of %d characters."),
                    IDENTIFIER_LENGTH_MAX);
                LexerException(lexerContext, msg);
            }
            ident[i++] = currentChar(lexerContext); // include >
            discardChar(lexerContext);
            // Continue reading the subfunction name
            while (testAlphaNumChar(lexerContext) != 0) {
                if (i >= IDENTIFIER_LENGTH_MAX) {
                    std::string msg = formatI18nMessage(
                        Nelson::resolveMessageTemplate(
                            "Nelson:interpreter:exceedsNelsonMaximumName",
                            "exceeds the Nelson maximum name length of %d characters."),
                        IDENTIFIER_LENGTH_MAX);
                    LexerException(lexerContext, msg);
                }
                ident[i++] = currentChar(lexerContext);
                discardChar(lexerContext);
            }
        }
    }
    ident[i] = '\0';
    if (strcmp(ident, "endfunction") == 0) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate("Nelson:interpreter:endfunctionKeywordSupportedUse",
                "The 'endfunction' keyword is not supported. Use 'end'."));
    }
    // Skip the global/persistent declaration-operand check for a field access
    // (e.g. s.global = 1): after a '.' the identifier is an ordinary field name,
    // which the field-access guard below routes to setIdentifierToken().
    if (lexerContext.tokenType != '.' && lexerContext.previousToken != '.') {
        rejectInvalidDeclarationCommandOperand(lexerContext, ident);
    }
    strncpy(lexerContext.tSearch.word, ident, IDENTIFIER_LENGTH_MAX - 1);
    lexerContext.tSearch.word[IDENTIFIER_LENGTH_MAX - 1] = '\0';
    lexerContext.pSearch = static_cast<keywordStruct*>(bsearch(
        &lexerContext.tSearch, keyWord, KEYWORDCOUNT, sizeof(keywordStruct), compareKeyword));
    const int context = static_cast<int>(ContextInt(lexerContext));
    if (lexerContext.pSearch == nullptr) {
        // Contextual keyword: "arguments" starts a validation block only when it
        // is the first token of a statement at the top level of a function body
        // and the rest of the line is empty or a well-formed attribute list. In
        // every other position (arguments = 3, arguments(x), x = arguments,
        // arguments{1}, arguments.f) it stays an ordinary identifier.
        if (strcmp(ident, "arguments") == 0 && lexerContext.inFunction
            && lexerContext.inStatement == 0 && lexerContext.bracketStackSize == 0
            && getFunctionDeclarationState(lexerContext) == 0
            && (lexerContext.previousToken == 0 || lexerContext.previousToken == ENDSTMNT
                || lexerContext.previousToken == ENDQSTMNT)
            && argumentsHeaderLineMatches(lexerContext.datap)) {
            lexArgumentsBlock(lexerContext, context);
            return;
        }
        if (convertIdentifierToNamedArgument(lexerContext, ident, context)) {
            return;
        }
        setIdentifierToken(lexerContext, ident, context);
        return;
    }
    if (lexerContext.tokenType == '.' || lexerContext.previousToken == '.') {
        setIdentifierToken(lexerContext, ident, context);
        return;
    }
    const int functionDeclarationState = getFunctionDeclarationState(lexerContext);
    if ((functionDeclarationState == 1 || functionDeclarationState == 3)
        && lexerContext.pSearch->token == END) {
        setIdentifierToken(lexerContext, ident, context);
        return;
    }
    switch (lexerContext.pSearch->token) {
    case FUNCTION: {
        if (!lexerContext.inFunction && lexerContext.inStatement > 0) {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate(
                    "Nelson:interpreter:functionDefinitionsAllowedStatement",
                    "Function definitions are not allowed in this statement context."));
        }
        lexerContext.countEndFunction = 0;
        lexerContext.inFunction = true;
        recordFunctionSignatureVariables(lexerContext, lexerContext.datap);
        setTokenType(lexerContext, lexerContext.pSearch->token);
    } break;
    case END: {
        if (lexerContext.bracketStackSize == 0) {
            bool asEndfunction = false;
            if (lexerContext.inFunction && lexerContext.inStatement == 0) {
                asEndfunction = true;
            }
            if (asEndfunction) {
                if (lexerContext.countEndFunction == 0) {
                    setTokenType(lexerContext, ENDFUNCTION);
                    lexerContext.countEndFunction++;
                } else {
                    LexerException(lexerContext,
                        Nelson::resolveMessageTemplate(
                            "Nelson:interpreter:statementInsideAnyFunction",
                            "This statement is not inside any function."));
                }
            } else {
                setTokenType(lexerContext, END);
                lexerContext.inBlock--;
                lexerContext.inStatement--;
            }
        } else {
            setTokenType(lexerContext, MAGICEND);
        }
    } break;
    case TRY: {
        setTokenType(lexerContext, lexerContext.pSearch->token);
        lexerContext.inStatement++;
    } break;
    case SWITCH: {
        setTokenType(lexerContext, lexerContext.pSearch->token);
        lexerContext.inStatement++;
    } break;
    default: {
        setTokenType(lexerContext, lexerContext.pSearch->token);
    } break;
        // The lexer no longer _has_ to keep track of the "end" keywords
        // to match them up.  But we need this information to determine
        // if more text is needed...
    case FOR: {
        lexerContext.vcFlag = 1;
        lexerContext.inBlock++;
        lexerContext.inStatement++;
        setTokenType(lexerContext, lexerContext.pSearch->token);
    } break;
    case WHILE: {
        lexerContext.vcFlag = 1;
        lexerContext.inBlock++;
        lexerContext.inStatement++;
        setTokenType(lexerContext, lexerContext.pSearch->token);
    } break;
    case IF: {
        lexerContext.vcFlag = 1;
        lexerContext.inBlock++;
        lexerContext.inStatement++;
        setTokenType(lexerContext, lexerContext.pSearch->token);
    } break;
    case ELSEIF:
    case CASE: {
        lexerContext.vcFlag = 1;
        lexerContext.inBlock++;
        setTokenType(lexerContext, lexerContext.pSearch->token);
    } break;
    }
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p
        = AbstractSyntaxTree::createNode(reserved_node, lexerContext.pSearch->ordinal, context);
}
//=============================================================================
static void
raiseInvalidRadixDigit(LexerContext& lexerContext, bool isHexadecimal)
{
    if (isHexadecimal) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate("Nelson:interpreter:invalidDigitHexadecimalLiteral",
                "Invalid digit in hexadecimal literal. Supported hexadecimal digits are 0-9 and "
                "A-F. Supported type suffixes are u8, u16, u32, u64, and s8, s16, s32, s64."));
    } else {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate("Nelson:interpreter:invalidDigitBinaryLiteral",
                "Invalid digit in binary literal. Supported binary digits are 0 and 1. Supported "
                "type suffixes are u8, u16, u32, u64, and s8, s16, s32, s64."));
    }
}
//=============================================================================
static void
raiseRadixTooManyDigits(LexerContext& lexerContext, bool isHexadecimal)
{
    if (isHexadecimal) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate("Nelson:interpreter:hexadecimalLiteralHasToo",
                "Hexadecimal literal has too many digits for specified or implied type suffix."));
    } else {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate("Nelson:interpreter:binaryLiteralHasToo",
                "Binary literal has too many digits for specified or implied type suffix."));
    }
}
//=============================================================================
// Scan a hexadecimal (0x/0X) or binary (0b/0B) integer literal. The number of
// digits fixes the bit width (four bits per hex digit, one per binary digit);
// without a type suffix the result is the smallest unsigned integer type whose
// width covers those digits (uint8/uint16/uint32/uint64). An optional suffix
// uN / sN with N in {8, 16, 32, 64} selects the exact type, and for the signed
// forms the digit bit pattern is reinterpreted as two's complement. The value is
// emitted as a decimal constant node so the shared numeric conversion applies.
static int
lexIntegerRadixConstant(LexerContext& lexerContext, bool isHexadecimal)
{
    const char* digitsStart = lexerContext.datap + 2;
    const char* cursor = digitsStart;
    if (isHexadecimal) {
        while (isxdigit(static_cast<unsigned char>(*cursor)) != 0) {
            cursor++;
        }
    } else {
        while ((*cursor == '0') || (*cursor == '1')) {
            cursor++;
        }
    }
    const size_t digitCount = static_cast<size_t>(cursor - digitsStart);
    if (digitCount == 0) {
        raiseInvalidRadixDigit(lexerContext, isHexadecimal);
        return 1;
    }
    bool suffixSigned = false;
    int suffixWidth = 0;
    const char suffixChar = *cursor;
    if ((suffixChar == 'u') || (suffixChar == 'U') || (suffixChar == 's') || (suffixChar == 'S')) {
        suffixSigned = ((suffixChar == 's') || (suffixChar == 'S'));
        const char* widthStart = cursor + 1;
        const char* widthCursor = widthStart;
        while ((*widthCursor >= '0') && (*widthCursor <= '9')) {
            widthCursor++;
        }
        const std::string widthText(widthStart, widthCursor);
        if (widthText == "8") {
            suffixWidth = 8;
        } else if (widthText == "16") {
            suffixWidth = 16;
        } else if (widthText == "32") {
            suffixWidth = 32;
        } else if (widthText == "64") {
            suffixWidth = 64;
        } else {
            raiseInvalidRadixDigit(lexerContext, isHexadecimal);
            return 1;
        }
        cursor = widthCursor;
    } else if ((isalnum(static_cast<unsigned char>(suffixChar)) != 0) || (suffixChar == '.')) {
        raiseInvalidRadixDigit(lexerContext, isHexadecimal);
        return 1;
    }
    const int bitsPerDigit = isHexadecimal ? 4 : 1;
    const size_t bitsNeeded = digitCount * static_cast<size_t>(bitsPerDigit);
    int width = suffixWidth;
    if (suffixWidth == 0) {
        if (bitsNeeded <= 8) {
            width = 8;
        } else if (bitsNeeded <= 16) {
            width = 16;
        } else if (bitsNeeded <= 32) {
            width = 32;
        } else if (bitsNeeded <= 64) {
            width = 64;
        } else {
            raiseRadixTooManyDigits(lexerContext, isHexadecimal);
            return 1;
        }
    } else if (bitsNeeded > static_cast<size_t>(suffixWidth)) {
        raiseRadixTooManyDigits(lexerContext, isHexadecimal);
        return 1;
    }
    uint64_t raw = 0;
    for (size_t i = 0; i < digitCount; i++) {
        const char c = digitsStart[i];
        uint64_t digitValue = 0;
        if ((c >= '0') && (c <= '9')) {
            digitValue = static_cast<uint64_t>(c - '0');
        } else if ((c >= 'a') && (c <= 'f')) {
            digitValue = static_cast<uint64_t>(c - 'a' + 10);
        } else {
            digitValue = static_cast<uint64_t>(c - 'A' + 10);
        }
        raw = (raw << bitsPerDigit) | digitValue;
    }
    NODE_TYPE nodeType = null_node;
    std::string content;
    if (suffixSigned) {
        int64_t value = 0;
        if (width >= 64) {
            value = static_cast<int64_t>(raw);
        } else {
            const uint64_t signBit = static_cast<uint64_t>(1) << (width - 1);
            if ((raw & signBit) != 0) {
                value = static_cast<int64_t>(raw) - (static_cast<int64_t>(1) << width);
            } else {
                value = static_cast<int64_t>(raw);
            }
        }
        content = std::to_string(value);
        switch (width) {
        case 8:
            nodeType = const_int8_node;
            break;
        case 16:
            nodeType = const_int16_node;
            break;
        case 32:
            nodeType = const_int32_node;
            break;
        default:
            nodeType = const_int64_node;
            break;
        }
    } else {
        content = std::to_string(raw);
        switch (width) {
        case 8:
            nodeType = const_uint8_node;
            break;
        case 16:
            nodeType = const_uint16_node;
            break;
        case 32:
            nodeType = const_uint32_node;
            break;
        default:
            nodeType = const_uint64_node;
            break;
        }
    }
    const size_t consumed = static_cast<size_t>(cursor - lexerContext.datap);
    for (size_t i = 0; i < consumed; i++) {
        discardChar(lexerContext);
    }
    setTokenType(lexerContext, NUMERIC);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
        nodeType, content, static_cast<int>(ContextInt(lexerContext)));
    return 1;
}
//=============================================================================
int
lexNumber(LexerContext& lexerContext)
{
    if (lexerContext.datap[0] == '0') {
        const char radixMarker = lexerContext.datap[1];
        if ((radixMarker == 'x') || (radixMarker == 'X')) {
            return lexIntegerRadixConstant(lexerContext, true);
        }
        if ((radixMarker == 'b') || (radixMarker == 'B')) {
            return lexIntegerRadixConstant(lexerContext, false);
        }
    }
    bool isNegative = (lexerContext.tokenType == '-');
    int state = 0;
    indexType cp = 0;
    char buffer[DEFAULT_BUFFER_SIZE_LEXER];
    int intonly = 1;
    // Initialize the state...
    state = 0;
    while (state != 7) {
        switch (state) {
        case 0:
            if (lexerContext.datap[cp] == '.') {
                cp++;
                state = 3;
                intonly = 0;
            } else if (_isDigit(lexerContext.datap[cp]) != 0) {
                while (_isDigit(lexerContext.datap[cp]) != 0) {
                    cp++;
                }
                state = 1;
                break;
            } else {
                return 0;
            }
            break;
        case 1:
            if (lexerContext.datap[cp] == '.') {
                intonly = 0;
                cp++;
                state = 5;
                break;
            } else if (isE(lexerContext.datap[cp]) != 0) {
                intonly = 0;
                cp++;
                state = 2;
                break;
            } else {
                state = 7;
            }
            break;
        case 2:
            if ((lexerContext.datap[cp] == '+') || (lexerContext.datap[cp] == '-')) {
                cp++;
                state = 6;
            } else if (_isDigit(lexerContext.datap[cp]) != 0) {
                state = 6;
            } else {
                LexerException(lexerContext,
                    Nelson::resolveMessageTemplate(
                        "Nelson:interpreter:malformedFloatingPointConstant",
                        "malformed floating point constant"));
                // Without an evaluator (formatter, language server) the exception
                // returns: end the constant rather than spin on this character.
                state = 7;
            }
            break;
        case 3:
            if (_isDigit(lexerContext.datap[cp]) != 0) {
                while (_isDigit(lexerContext.datap[cp]) != 0) {
                    cp++;
                }
            } else {
                return 0;
            }
            state = 4;
            break;
        case 4:
            if (isE(lexerContext.datap[cp]) != 0) {
                intonly = 0;
                cp++;
                state = 2;
                break;
            } else {
                state = 7;
            }
            break;
        case 5:
            if (isE(lexerContext.datap[cp]) != 0) {
                intonly = 0;
                cp++;
                state = 2;
                break;
            } else if (_isDigit(lexerContext.datap[cp]) != 0) {
                while (_isDigit(lexerContext.datap[cp]) != 0) {
                    cp++;
                }
                state = 4;
                break;
            } else if ((cp > 0) && (lexerContext.datap[cp - 1] == '.')
                && ((lexerContext.datap[cp] == '*') || (lexerContext.datap[cp] == '/')
                    || (lexerContext.datap[cp] == '\\') || (lexerContext.datap[cp] == '^')
                    || (lexerContext.datap[cp] == '\''))) {
                cp--;
                state = 7;
            } else {
                state = 7;
            }
            break;
        case 6:
            if (_isDigit(lexerContext.datap[cp]) != 0) {
                while (_isDigit(lexerContext.datap[cp]) != 0) {
                    cp++;
                }
                state = 7;
            } else {
                LexerException(lexerContext,
                    Nelson::resolveMessageTemplate(
                        "Nelson:interpreter:malformedFloatingPointConstant",
                        "malformed floating point constant"));
                // Without an evaluator (formatter, language server) the exception
                // returns: end the constant rather than spin on this character.
                state = 7;
            }
        }
    }

    NODE_TYPE nodeType = null_node;
    if (lexerContext.datap[cp] == 'f') {
        // f32 --> single
        if ((lexerContext.datap[cp + 1] == '3') && (lexerContext.datap[cp + 2] == '2')) {
            cp = cp + 3;
            nodeType = const_float_node;
        }
        // f64 --> double
        else if ((lexerContext.datap[cp + 1] == '6') && (lexerContext.datap[cp + 2] == '4')) {
            cp = cp + 3;
            nodeType = const_double_node;
        } else {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate("Nelson:interpreter:malformedFloatingPointConstant",
                    "malformed floating point constant"));
        }
    } else if (lexerContext.datap[cp] == 'i') {
        // i8 --> int8
        if (lexerContext.datap[cp + 1] == '8') {
            cp = cp + 2;
            nodeType = const_int8_node;
        }
        // i16 --> int16
        else if ((lexerContext.datap[cp + 1] == '1') && (lexerContext.datap[cp + 2] == '6')) {
            cp = cp + 3;
            nodeType = const_int16_node;
        }
        // i32 --> int32
        else if ((lexerContext.datap[cp + 1] == '3') && (lexerContext.datap[cp + 2] == '2')) {
            cp = cp + 3;
            nodeType = const_int32_node;
        }
        // i64 --> int32
        else if ((lexerContext.datap[cp + 1] == '6') && (lexerContext.datap[cp + 2] == '4')) {
            cp = cp + 3;
            nodeType = const_int64_node;
        }
    } else if (lexerContext.datap[cp] == 'u') {
        if (intonly == 0) {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate(
                    "Nelson:interpreter:malformedUnsignedIntegerConstant",
                    "Malformed unsigned integer constant."));
        }
        // u8 --> uint8
        if (lexerContext.datap[cp + 1] == '8') {
            cp = cp + 2;
            nodeType = const_uint8_node;
        }
        // u16 --> uint16
        else if ((lexerContext.datap[cp + 1] == '1') && (lexerContext.datap[cp + 2] == '6')) {
            cp = cp + 3;
            nodeType = const_uint16_node;
        }
        // u32 --> uint32
        else if ((lexerContext.datap[cp + 1] == '3') && (lexerContext.datap[cp + 2] == '2')) {
            cp = cp + 3;
            nodeType = const_uint32_node;
        }
        // u64 --> uint64
        else if ((lexerContext.datap[cp + 1] == '6') && (lexerContext.datap[cp + 2] == '4')) {
            cp = cp + 3;
            nodeType = const_uint64_node;
        } else {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate(
                    "Nelson:interpreter:malformedUnsignedIntegerConstant",
                    "Malformed unsigned integer constant."));
        }
    } else if (intonly) {
        nodeType = const_int_node;
    } else {
        nodeType = const_double_node;
    }

    if (cp >= DEFAULT_BUFFER_SIZE_LEXER) {
        LexerException(lexerContext,
            Nelson::resolveMessageTemplate(
                "Nelson:interpreter:numericConstantTooLong", "numeric constant too long"));
    }
    for (indexType i = 0; i < cp; i++) {
        buffer[i] = lexerContext.datap[i];
    }
    for (indexType i = 0; i < cp; i++) {
        discardChar(lexerContext);
    }
    buffer[cp] = '\0';
    std::string content = std::string(buffer);
    setTokenType(lexerContext, NUMERIC);

    switch (nodeType) {
    case const_int_node: {
        lexerContext.tokenValue.isToken = false;
        if (currentChar(lexerContext) == 'i' || currentChar(lexerContext) == 'j') {
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_dcomplex_node, content, static_cast<int>(ContextInt(lexerContext)));
            discardChar(lexerContext);
        } else {
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_double_node, content, static_cast<int>(ContextInt(lexerContext)));
        }
    } break;
    case const_double_node: {
        lexerContext.tokenValue.isToken = false;
        if (currentChar(lexerContext) == 'i' || currentChar(lexerContext) == 'j') {
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_dcomplex_node, content, static_cast<int>(ContextInt(lexerContext)));
            discardChar(lexerContext);
        } else {
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_double_node, content, static_cast<int>(ContextInt(lexerContext)));
        }
    } break;
    case const_float_node: {
        lexerContext.tokenValue.isToken = false;
        if (currentChar(lexerContext) == 'i' || currentChar(lexerContext) == 'j') {
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_complex_node, content, static_cast<int>(ContextInt(lexerContext)));
            discardChar(lexerContext);
        } else {
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_float_node, content, static_cast<int>(ContextInt(lexerContext)));
        }
    } break;
    case const_int8_node:
    case const_int16_node:
    case const_int32_node:
    case const_int64_node: {
        lexerContext.tokenValue.isToken = false;
        // The sign is not folded into the literal here: a leading '-' is a
        // separate unary/binary operator token that the parser already applies.
        // Folding it here would double-negate (e.g. "10-3i64" -> 10-(-3)). The
        // signed minimum (e.g. -9223372036854775808i64) is preserved by folding
        // OP_UMINUS over an integer constant in the bytecode compiler, which
        // converts the negated magnitude text directly.
        lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
            nodeType, content, static_cast<int>(ContextInt(lexerContext)));

    } break;
    case const_uint8_node:
    case const_uint16_node:
    case const_uint32_node:
    case const_uint64_node: {
        if (isNegative) {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate(
                    "Nelson:interpreter:malformedUnsignedIntegerConstantUnaryMinus",
                    "Malformed unsigned integer constant with unary operator '-'."));
        }
        lexerContext.tokenValue.isToken = false;
        lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
            nodeType, content, static_cast<int>(ContextInt(lexerContext)));
    } break;
    default: {
        lexerContext.tokenValue.isToken = false;
        if (currentChar(lexerContext) == 'i' || currentChar(lexerContext) == 'j') {
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_dcomplex_node, content, static_cast<int>(ContextInt(lexerContext)));
            discardChar(lexerContext);
        } else {
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_double_node, content, static_cast<int>(ContextInt(lexerContext)));
        }
    } break;
    }
    return 1;
}
//=============================================================================
static void
fetchComment(LexerContext& lexerContext)
{
    while (isNewline(lexerContext) == 0) {
        discardChar(lexerContext);
    }
    NextLine(lexerContext);
}
//=============================================================================
static void
fetchMultilineComment(LexerContext& lexerContext)
{
    // Skip the %{
    lexerContext.datap += 2;
    lexerContext.inMultilineComment = true;

    // Look for %}
    while (lexerContext.datap[0] != '\0') {
        if ((lexerContext.datap[0] == '%') && (lexerContext.datap[1] == '}')) {
            // Found the end, skip %}
            lexerContext.datap += 2;
            lexerContext.inMultilineComment = false;
            return;
        }
        if (isNewline(lexerContext)) {
            if (match(lexerContext, "\r\n") != 0) {
                // already consumed by isNewline
            } else if (match(lexerContext, "\n") != 0) {
                // already consumed
            }
            NextLine(lexerContext);
        } else {
            discardChar(lexerContext);
        }
    }

    // If we get here, we reached end of buffer without closing %}
    // In interactive mode, this is expected and we need more input
    // The inMultilineComment flag will signal this to lexCheckForMoreInput
}
//=============================================================================
static std::string
trimLexerString(const std::string& text)
{
    size_t first = 0;
    while (first < text.size() && std::isspace(static_cast<unsigned char>(text[first]))) {
        first++;
    }
    size_t last = text.size();
    while (last > first && std::isspace(static_cast<unsigned char>(text[last - 1]))) {
        last--;
    }
    return text.substr(first, last - first);
}
//=============================================================================
static size_t
findTopLevelChar(const std::string& text, char needle)
{
    int parenDepth = 0;
    int braceDepth = 0;
    int bracketDepth = 0;
    char stringDelimiter = '\0';
    size_t k = 0;
    while (k < text.size()) {
        const char ch = text[k];
        if (stringDelimiter != '\0') {
            if (ch == stringDelimiter) {
                if ((k + 1) < text.size() && text[k + 1] == stringDelimiter) {
                    k += 2; // escaped delimiter ("" or '')
                    continue;
                } else {
                    stringDelimiter = '\0';
                    k++;
                    continue;
                }
            }
            k++;
            continue;
        }
        if (ch == '\'' || ch == '"') {
            stringDelimiter = ch;
            k++;
            continue;
        }
        if (ch == '(') {
            parenDepth++;
        } else if (ch == ')' && parenDepth > 0) {
            parenDepth--;
        } else if (ch == '{') {
            braceDepth++;
        } else if (ch == '}' && braceDepth > 0) {
            braceDepth--;
        } else if (ch == '[') {
            bracketDepth++;
        } else if (ch == ']' && bracketDepth > 0) {
            bracketDepth--;
        } else if (ch == needle && parenDepth == 0 && braceDepth == 0 && bracketDepth == 0) {
            return k;
        }
        k++;
    }
    return std::string::npos;
}
//=============================================================================
static std::vector<std::string>
splitTopLevelCommaList(const std::string& text)
{
    std::vector<std::string> parts;
    size_t start = 0;
    for (;;) {
        const std::string remaining = text.substr(start);
        const size_t comma = findTopLevelChar(remaining, ',');
        if (comma == std::string::npos) {
            std::string part = trimLexerString(remaining);
            if (!part.empty()) {
                parts.push_back(part);
            }
            break;
        }
        std::string part = trimLexerString(remaining.substr(0, comma));
        if (!part.empty()) {
            parts.push_back(part);
        }
        start += comma + 1;
    }
    return parts;
}
//=============================================================================
// A single quote opens a character array unless it directly follows a value
// (an identifier char, a closing bracket, a dot or another quote), in which
// case it is the transpose operator. Needed so a comment after a transpose
// (e.g. "[1 2 3]' % note") is recognized rather than swallowed into a string.
static bool
lexerSingleQuoteOpensCharArray(char previousChar)
{
    const bool followsValue = (previousChar >= 'a' && previousChar <= 'z')
        || (previousChar >= 'A' && previousChar <= 'Z')
        || (previousChar >= '0' && previousChar <= '9') || previousChar == '_'
        || previousChar == ')' || previousChar == ']' || previousChar == '}' || previousChar == '\''
        || previousChar == '.';
    return !followsValue;
}
//=============================================================================
static std::string
stripLineComment(const std::string& line)
{
    char stringDelimiter = '\0';
    size_t k = 0;
    while (k < line.size()) {
        const char ch = line[k];
        if (stringDelimiter != '\0') {
            if (ch == stringDelimiter) {
                if ((k + 1) < line.size() && line[k + 1] == stringDelimiter) {
                    k += 2; // escaped delimiter
                    continue;
                } else {
                    stringDelimiter = '\0';
                    k++;
                    continue;
                }
            }
            k++;
            continue;
        }
        if (ch == '"') {
            stringDelimiter = ch;
            k++;
            continue;
        }
        if (ch == '\'') {
            if (lexerSingleQuoteOpensCharArray(k > 0 ? line[k - 1] : '\0')) {
                stringDelimiter = ch;
            }
            k++;
            continue;
        }
        if (ch == '%') {
            return line.substr(0, k);
        }
        k++;
    }
    return line;
}
//=============================================================================
static std::string
stripUtf8BOM(const std::string& buffer)
{
    if (buffer.size() >= 3 && static_cast<unsigned char>(buffer[0]) == 0xEF
        && static_cast<unsigned char>(buffer[1]) == 0xBB
        && static_cast<unsigned char>(buffer[2]) == 0xBF) {
        return buffer.substr(3);
    }
    return buffer;
}
//=============================================================================
// Normalize a source buffer for the lexer: strip a UTF-8 BOM, convert CRLF to
// LF and ensure a trailing newline. This reproduces the line handling that the
// former arguments-block text rewrite performed for every buffer (the lexer
// relied on it); the arguments blocks themselves are now handled natively.
static std::string
normalizeLexerBuffer(const std::string& buffer)
{
    const std::string stripped = stripUtf8BOM(buffer);
    std::string output;
    output.reserve(stripped.size() + 1);
    size_t start = 0;
    while (start < stripped.size()) {
        size_t end = stripped.find('\n', start);
        std::string line;
        if (end == std::string::npos) {
            line = stripped.substr(start);
            start = stripped.size();
        } else {
            line = stripped.substr(start, end - start);
            start = end + 1;
        }
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        output += line;
        output += '\n';
    }
    return output;
}
//=============================================================================
// True when the remainder of the physical line after the "arguments" keyword is
// empty or a well-formed block attribute list "(Input|Output|Repeating, ...)".
// This is the whole-line-shape lookahead that distinguishes an arguments
// validation block from an ordinary use of the identifier "arguments".
static bool
argumentsHeaderLineMatches(const char* restOfLine)
{
    std::string rest;
    for (const char* p = restOfLine; *p != '\0' && *p != '\n'; ++p) {
        if (*p == '\r' && p[1] == '\n') {
            break;
        }
        rest.push_back(*p);
    }
    rest = trimLexerString(stripLineComment(rest));
    if (rest.empty()) {
        return true;
    }
    if (rest.front() != '(' || rest.back() != ')') {
        return false;
    }
    const std::string inner = trimLexerString(rest.substr(1, rest.size() - 2));
    for (const auto& token : splitTopLevelCommaList(inner)) {
        const std::string attribute = trimLexerString(token);
        if (attribute != "Input" && attribute != "Output" && attribute != "Repeating") {
            return false;
        }
    }
    return true;
}
//=============================================================================
// Capture a whole "arguments ... end" validation block as a single opaque token
// carrying the raw block source. Entry internals are never handed to the parser
// (they would breach the grammar conflict invariant); the block is lowered to
// validation statements at compile time. The block terminator is the first
// physical line whose comment-stripped, trimmed content is exactly "end", which
// matches the historical text-rewrite block boundary exactly. datap is left on
// the newline that follows that "end" line so the normal lexer emits the
// statement terminator.
static void
lexArgumentsBlock(LexerContext& lexerContext, int context)
{
    std::string blockText = "arguments";
    for (;;) {
        std::string physLine;
        while (lexerContext.datap[0] != '\0' && lexerContext.datap[0] != '\n'
            && !(lexerContext.datap[0] == '\r' && lexerContext.datap[1] == '\n')) {
            physLine.push_back(lexerContext.datap[0]);
            discardChar(lexerContext);
        }
        blockText += physLine;
        const bool isBlockEndLine = (trimLexerString(stripLineComment(physLine)) == "end");
        if (isBlockEndLine || lexerContext.datap[0] == '\0') {
            break;
        }
        if (lexerContext.datap[0] == '\r') {
            blockText.push_back('\r');
            discardChar(lexerContext);
        }
        blockText.push_back('\n');
        discardChar(lexerContext); // consume the '\n'
        NextLine(lexerContext);
    }
    setTokenType(lexerContext, ARGUMENTS);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p
        = AbstractSyntaxTree::createNode(const_character_array_node, blockText, context);
}
//=============================================================================
inline void
completeContinuation(LexerContext& lexerContext)
{
    if (lexerContext.continuationCount > 0) {
        lexerContext.continuationCount--;
    }
}
//=============================================================================
struct OperatorMatch
{
    const char* pattern;
    int tokenType;
};
//=============================================================================
// Static array of operators to match
static const OperatorMatch operators[] = { { ".*", DOTTIMES }, { "./", DOTRDIV },
    { ".\\", DOTLDIV }, { ".^", DOTPOWER }, { ".'", DOTTRANSPOSE }, { "!=", NE }, { "<>", NE },
    { "~=", NE }, { "<=", LE }, { ">=", GE }, { "==", EQ }, { "||", SOR }, { "&&", SAND } };
//=============================================================================
// Helper function to check for multi-character operators
inline bool
checkMultiCharOperators(LexerContext& lexerContext)
{
    constexpr size_t numOperators = sizeof(operators) / sizeof(operators[0]);
    // Check each operator pattern against the current lexer context
    for (size_t i = 0; i < numOperators; ++i) {
        if (match(lexerContext, operators[i].pattern) != 0) {
            setTokenType(lexerContext, operators[i].tokenType);
            return true;
        }
    }
    return false;
}
//=============================================================================
// True when the head identifier of a would-be command statement is known to be
// a variable, in which case it always parses as an expression ("x /y"
// is division, not the command x('/y')). Both the static names gathered from the
// current parse buffer and the live-workspace hook are consulted.
static bool
lexerHeadIsVariable(LexerContext& lexerContext, const std::string& name)
{
    if (name.empty()) {
        return false;
    }
    if (lexerContext.lexicalVariableNames.count(name) != 0) {
        return true;
    }
    if (static_cast<bool>(lexerContext.isVariableHook)) {
        return lexerContext.isVariableHook(name);
    }
    return (g_lexerVariableResolver != nullptr) && g_lexerVariableResolver(name);
}
//=============================================================================
// Characters that, following the "space before / no space after"
// pattern, may begin a command-syntax argument (an option flag or a path)
// rather than a binary operator.
static bool
isCommandArgumentOperatorStart(char c)
{
    return (c == '/') || (c == '\\') || (c == '-') || (c == '+');
}
//=============================================================================
// True when the text starting at p (positioned just after a statement-initial
// identifier and its trailing whitespace) is an assignment to that identifier:
// an optional chain of "(...)"/"{...}" index selectors and ".field" selectors
// followed by a single '='. Recording such a head as a variable makes a later
// command-position use parse as an expression ("x = 6; x /y" -> division).
static bool
runIsAssignmentSuffix(const char* p)
{
    while (*p != '\0') {
        if ((*p == '(') || (*p == '{')) {
            const char open = *p;
            const char close = (open == '(') ? ')' : '}';
            int depth = 0;
            while (*p != '\0') {
                if (*p == open) {
                    depth++;
                } else if (*p == close) {
                    depth--;
                    if (depth == 0) {
                        p++;
                        break;
                    }
                }
                p++;
            }
            continue;
        }
        if (*p == '.') {
            p++;
            if ((isalpha(static_cast<unsigned char>(*p)) != 0) || (*p == '_')) {
                while ((isalnum(static_cast<unsigned char>(*p)) != 0) || (*p == '_')) {
                    p++;
                }
                continue;
            }
            return false;
        }
        break;
    }
    while ((*p == ' ') || (*p == '\t')) {
        p++;
    }
    return (*p == '=') && (p[1] != '=');
}
//=============================================================================
/*
 * String detection is a bit tricky, I suppose....  A quote character
 * immediately following (without whitespace) a bracket or a alphanumeric
 * is a transpose.  Otherwise, a quote character marks the beginning of
 * a string.  This means that we need to look at the _previous_ token.
 */
void
lexScanningState(LexerContext& lexerContext)
{
    if (match(lexerContext, "...") != 0) {
        setTokenType(lexerContext, WS);
        lexerContext.continuationCount++;
        skipContinuationTrivia(lexerContext);
        return;
    }
    // comments supported
    if (currentChar(lexerContext) == '%') {
        // Check for multiline comment %{
        if (lexerContext.datap[1] == '{') {
            fetchMultilineComment(lexerContext);
            setTokenType(lexerContext, WS);
            return;
        }
        // Single-line comment
        fetchComment(lexerContext);
        setTokenType(lexerContext, ENDSTMNT);
        return;
    }
    if (currentChar(lexerContext) == '\"') {
        lexString(lexerContext);
        return;
    }
    if (currentChar(lexerContext) == '\'') {
        if (!lexerSingleQuoteOpensCharArray(static_cast<char>(previousChar(lexerContext)))) {
            /* Not a string... */
            setTokenType(lexerContext, static_cast<int>('\''));
            discardChar(lexerContext);
            return;
        }
        lexCharacterArray(lexerContext);
        return;
    }
    if (isWhitespace(lexerContext) != 0) {
        while (isWhitespace(lexerContext) != 0) {
            ;
        }
        setTokenType(lexerContext, WS);
        return;
    }
    if ((match(lexerContext, ";\n") != 0) || (match(lexerContext, ";\r\n") != 0)) {
        setTokenType(lexerContext, ENDQSTMNT);
        lexerContext.tokenValue.isToken = true;
        lexerContext.tokenValue.v.i = static_cast<int>(ContextInt(lexerContext));
        NextLine(lexerContext);
        lexerContext.lexState = Initial;
        if (lexerContext.bracketStackSize == 0) {
            lexerContext.vcFlag = 0;
        }
        lexerContext.inDebugCommand = false;
        completeContinuation(lexerContext);
        return;
    }
    if (match(lexerContext, ";") != 0) {
        setTokenType(lexerContext, ENDQSTMNT);
        if (lexerContext.bracketStackSize == 0) {
            lexerContext.vcFlag = 0;
        }
        lexerContext.lexState = Initial;
        lexerContext.inDebugCommand = false;
        return;
    }
    if ((match(lexerContext, "\r\n") != 0) || (match(lexerContext, "\n") != 0)) {
        NextLine(lexerContext);
        setTokenType(lexerContext, ENDSTMNT);
        lexerContext.lexState = Initial;
        if (lexerContext.bracketStackSize == 0) {
            lexerContext.vcFlag = 0;
        }
        lexerContext.inDebugCommand = false;
        completeContinuation(lexerContext);
        return;
    }

    if (checkMultiCharOperators(lexerContext)) {
        return;
    }

    if (currentChar(lexerContext) == '~') {
        if (isTildePlaceholder(lexerContext)) {
            const std::string placeholderName = generatePlaceholderIdentifier(lexerContext);
            setTokenType(lexerContext, IDENT);
            discardChar(lexerContext);
            lexerContext.tokenValue.isToken = false;
            lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
                id_node, placeholderName, static_cast<int>(ContextInt(lexerContext)));
            return;
        }
        if (isIllegalTildeSlotUsage(lexerContext)) {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate(
                    "Nelson:interpreter:incorrectUseTilde", "Incorrect use of tilde."));
            return;
        }
        setTokenType(lexerContext, static_cast<int>('~'));
        discardChar(lexerContext);
        return;
    }

    if ((testAlphaChar(lexerContext) != 0) || currentChar(lexerContext) == '_') {
        const bool atStatementStart = isStatementStartPreviousToken(lexerContext);
        const bool afterFor = (lexerContext.previousToken == FOR);
        lexIdentifier(lexerContext);
        // Are we inside a bracket? If so, leave well enough alone
        if ((lexerContext.tokenType != IDENT) || (lexerContext.bracketStackSize != 0)) {
            return;
        }
        // Name of the identifier just read, used for the variable-vs-command
        // decision below and for static variable tracking.
        std::string headIdentifier;
        if (lexerContext.tokenValue.v.p != nullptr) {
            headIdentifier = lexerContext.tokenValue.v.p->text;
        }
        // A "for" loop index is a variable for the rest of the parse buffer.
        if (afterFor && !headIdentifier.empty()) {
            lexerContext.lexicalVariableNames.insert(headIdentifier);
        }
        // No, so... munch the whitespace
        bool hadWhitespace = false;
        while (isWhitespace(lexerContext) != 0) {
            hadWhitespace = true;
        }
        // Record an assignment target ("x = ...", "x(i) = ...", "x.f = ...") so a
        // later command-position use of the same name parses as an expression.
        if (atStatementStart && !headIdentifier.empty()
            && runIsAssignmentSuffix(lexerContext.datap)) {
            lexerContext.lexicalVariableNames.insert(headIdentifier);
        }
        if (atStatementStart && hadWhitespace && currentChar(lexerContext) == '.'
            && strncmp(lexerContext.datap, "...", 3) != 0
            && !isDottedOperatorAhead(lexerContext.datap)
            && !hasTopLevelAssignmentAhead(lexerContext.datap)) {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate("Nelson:interpreter:unexpectedFieldAccessAfter",
                    "Unexpected field access after command-style spacing."));
            return;
        }
        // Command syntax vs expression for "ident <ws> <arg>" (the reason this
        // lexer exists): when the head identifier is a variable
        // the statement is an expression ("x /y" is division); otherwise a
        // letter-initial argument, or an operator-initial argument that follows
        // the "space before, no space after" pattern, selects command syntax
        // ("disp hello", "disp /tmp/foo", "save -append").
        const char next = static_cast<char>(currentChar(lexerContext));
        const bool headIsVariable = lexerHeadIsVariable(lexerContext, headIdentifier);
        bool enterCommandSyntax = false;
        if (hadWhitespace && !headIsVariable) {
            // A "~/" home-directory path is command syntax too: there is no
            // binary '~' operator, so "~/x" can never be a valid expression.
            const bool tildeHomePath = (next == '~') && (lexerContext.datap[1] == '/');
            if ((testAlphaChar(lexerContext) != 0) || (next == '_')) {
                enterCommandSyntax = true;
            } else if (isCommandArgumentOperatorStart(next) || tildeHomePath) {
                const char after = lexerContext.datap[1];
                const bool noSpaceAfter = (after != ' ') && (after != '\t') && (after != '\0')
                    && (after != '\n') && (after != '\r');
                // A non-variable head plus the spacing pattern is a command, at
                // statement position only: an operator on the right-hand side
                // of an assignment stays arithmetic. Without a workspace (tooling
                // parse) the variables are the ones the source itself defines.
                enterCommandSyntax = noSpaceAfter && atStatementStart;
            }
        }
        if (enterCommandSyntax) {
            lexerContext.lexState = SpecScan;
        }
        return;
    }
    if ((testDigit(lexerContext) != 0) || currentChar(lexerContext) == '.') {
        if (lexNumber(lexerContext) != 0) {
            return;
        }
    }
    if (currentChar(lexerContext) == '[') {
        const bool isDestructuring = bracketFollowedByAssignment(lexerContext);
        pushBracket(lexerContext, currentChar(lexerContext), isDestructuring, false, false);
        pushVCState(lexerContext);
        lexerContext.vcFlag = 1;
    } else if (currentChar(lexerContext) == '{') {
        pushBracket(lexerContext, currentChar(lexerContext), false, false, false);
        pushVCState(lexerContext);
        lexerContext.vcFlag = 1;
    }
    if (currentChar(lexerContext) == '(') {
        const bool functionCallContext = isFunctionCallOpening(lexerContext);
        const bool anonymousFunctionArguments = lexerContext.previousToken == '@';
        pushBracket(lexerContext, currentChar(lexerContext), false, functionCallContext,
            anonymousFunctionArguments);
        pushVCState(lexerContext);
        lexerContext.vcFlag = 0;
    }
    if (currentChar(lexerContext) == ')') {
        const bool closesAnonymousFunctionArguments
            = isTopBracketAnonymousFunctionArguments(lexerContext);
        popVCState(lexerContext);
        popBracket(lexerContext, '(');
        if (closesAnonymousFunctionArguments) {
            lexerContext.justClosedAnonymousFunctionArguments = true;
        }
    }
    if (currentChar(lexerContext) == ']') {
        popVCState(lexerContext);
        popBracket(lexerContext, '[');
    }
    if (currentChar(lexerContext) == '}') {
        popVCState(lexerContext);
        popBracket(lexerContext, '{');
    }
    if (currentChar(lexerContext) == ',') {
        if ((lexerContext.catchIdentifierState == 2) && (lexerContext.bracketStackSize == 0)) {
            setTokenType(lexerContext, ENDSTMNT);
            discardChar(lexerContext);
            lexerContext.vcFlag = 0;
            lexerContext.lexState = Initial;
            lexerContext.inDebugCommand = false;
            return;
        }
        if (lexerContext.bracketStackSize == 0) {
            lexerContext.vcFlag = 0;
        }
    }
    if (currentChar(lexerContext) < 0) {
        LexerException(lexerContext, lexerContext.datap);
    }
    setTokenType(lexerContext, currentChar(lexerContext));
    discardChar(lexerContext);
}
//=============================================================================
void
lexInitialState(LexerContext& lexerContext)
{
    if (isNewline(lexerContext) != 0) {
        NextLine(lexerContext);
    } else if (isWhitespace(lexerContext) != 0) { // nothing
    } else if (match(lexerContext, ";") != 0) {
        // nothing
    } else if (currentChar(lexerContext) == '%') {
        // Check for multiline comment %{
        if (lexerContext.datap[1] == '{') {
            fetchMultilineComment(lexerContext);
        } else {
            // Single-line comment
            fetchComment(lexerContext);
        }
    } else if (isNoArgumentPathCommandShortCut(utf8_to_wstring(std::string(lexerContext.datap)))) {
        lexIdentifier(lexerContext);
        setSpecialCallToken(lexerContext);
        lexerContext.lexState = Scanning;
    } else if (testSpecialFuncs(lexerContext)) {
        lexIdentifier(lexerContext);
        lexerContext.lexState = SpecScan;
    } else {
        lexerContext.lexState = Scanning;
    }
}
//=============================================================================
static void
updateFunctionDeclarationState(LexerContext& lexerContext, int tokenType)
{
    if (tokenType == ENDFUNCTION) {
        setAfterFunctionTerminator(lexerContext, true);
        return;
    }
    switch (getFunctionDeclarationState(lexerContext)) {
    case 0:
        if (tokenType == FUNCTION) {
            setFunctionDeclarationState(lexerContext, 1);
        }
        break;
    case 1:
        if (tokenType == IDENT || tokenType == VARARGOUT || tokenType == '[') {
            setFunctionDeclarationState(lexerContext, 2);
        } else if (tokenType == ENDSTMNT || tokenType == ENDQSTMNT) {
            setFunctionDeclarationState(lexerContext, 0);
        }
        break;
    case 2:
        if (tokenType == '=') {
            setFunctionDeclarationState(lexerContext, 3);
        } else if (tokenType == '(' || tokenType == ENDSTMNT || tokenType == ENDQSTMNT) {
            setFunctionDeclarationState(lexerContext, 0);
        }
        break;
    case 3:
        if (tokenType == IDENT) {
            setFunctionDeclarationState(lexerContext, 4);
        } else if (tokenType == ENDSTMNT || tokenType == ENDQSTMNT) {
            setFunctionDeclarationState(lexerContext, 0);
        }
        break;
    case 4:
        if (tokenType == ENDSTMNT || tokenType == ENDQSTMNT) {
            setFunctionDeclarationState(lexerContext, 0);
        }
        break;
    default:
        setFunctionDeclarationState(lexerContext, 0);
        break;
    }
}
//=============================================================================
void
yylexDoLex(LexerContext& lexerContext)
{
    switch (lexerContext.lexState) {
    case Initial:
        lexInitialState(lexerContext);
        break;
    case Scanning:
        lexScanningState(lexerContext);
        break;
    case SpecScan:
        lexUntermCharacterArray(lexerContext);
        break;
    }
}
//=============================================================================
static bool
isFormattingTriviaCharacter(char ch)
{
    return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n';
}
//=============================================================================
static size_t
skipFormattingTrivia(const std::string& text, size_t start, size_t end)
{
    size_t pos = start;
    while (pos < end) {
        while (pos < end && isFormattingTriviaCharacter(text[pos])) {
            ++pos;
        }
        if (pos < end && text[pos] == '%') {
            while (pos < end && text[pos] != '\r' && text[pos] != '\n') {
                ++pos;
            }
            continue;
        }
        break;
    }
    return pos;
}
//=============================================================================
static size_t
trimFormattingTokenEnd(const std::string& text, size_t start, size_t end)
{
    while (end > start && isFormattingTriviaCharacter(text[end - 1])) {
        --end;
    }
    return end;
}
//=============================================================================
static void
computeFormattingLineColumn(const std::string& source, size_t offset, int& line, int& column)
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
static void
appendFormattingToken(
    LexerContext& lexerContext, int tokenType, const char* startPtr, const char* endPtr)
{
    std::vector<FormattingToken>* formattingTokenSink = getFormattingTokenSink();
    if (formattingTokenSink == nullptr || startPtr == nullptr || endPtr == nullptr
        || lexerContext.textbuffer == nullptr || endPtr < lexerContext.textbuffer) {
        return;
    }
    size_t start = static_cast<size_t>(startPtr - lexerContext.textbuffer);
    size_t end = static_cast<size_t>(endPtr - lexerContext.textbuffer);
    const size_t bufferSize = lexerContext.textbufferStorage.size();
    start = std::min(start, bufferSize);
    end = std::min(end, bufferSize);
    if (end < start) {
        return;
    }
    const std::string& text = lexerContext.textbufferStorage;
    start = skipFormattingTrivia(text, start, end);
    // An empty statement (";" alone on a line, ";;") is scanned together with the
    // next token: record its separator on its own instead of inside that token.
    const bool isSeparator
        = tokenType == ';' || tokenType == ',' || tokenType == ENDSTMNT || tokenType == ENDQSTMNT;
    while (!isSeparator && start < end && (text[start] == ';' || text[start] == ',')) {
        FormattingToken separator;
        separator.type = text[start] == ';' ? ENDQSTMNT : ',';
        separator.offset = start;
        separator.text = text.substr(start, 1);
        computeFormattingLineColumn(text, separator.offset, separator.line, separator.column);
        formattingTokenSink->push_back(separator);
        start = skipFormattingTrivia(text, start + 1, end);
    }
    end = trimFormattingTokenEnd(text, start, end);
    if (start >= end) {
        return;
    }
    FormattingToken token;
    token.type = tokenType;
    token.offset = start;
    token.text = lexerContext.textbufferStorage.substr(start, end - start);
    computeFormattingLineColumn(
        lexerContext.textbufferStorage, token.offset, token.line, token.column);
    formattingTokenSink->push_back(token);
}
//=============================================================================
static int
yylexScreen(LexerContext& lexerContext, const char*& tokenStart)
{
    tokenStart = lexerContext.datap;
    if (lexerContext.pendingNamedArgumentComma) {
        lexerContext.pendingNamedArgumentComma = false;
        lexerContext.tokenActive = 1;
        lexerContext.tokenType = ',';
        lexerContext.tokenValue.isToken = true;
        lexerContext.tokenValue.v.i = (lexerContext.pendingCommaContext != 0)
            ? lexerContext.pendingCommaContext
            : static_cast<int>(ContextInt(lexerContext));
        lexerContext.pendingCommaContext = 0;
    } else {
        lexerContext.tokenActive = 0;
        while (lexerContext.tokenActive == 0) {
            yylexDoLex(lexerContext);
        }
        if ((lexerContext.tokenType == WS) && (lexerContext.vcFlag != 0)) {
            /* Check for virtual commas... */
            const bool suppressAnonymousFunctionBodySeparator
                = lexerContext.justClosedAnonymousFunctionArguments
                && lexerContext.previousToken == ')';
            if (!suppressAnonymousFunctionBodySeparator
                && ((lexerContext.previousToken == ')') || (lexerContext.previousToken == '\'')
                    || (lexerContext.previousToken == DOTTRANSPOSE)
                    || (lexerContext.previousToken == NUMERIC)
                    || (lexerContext.previousToken == CHARACTER)
                    || (lexerContext.previousToken == STRING) || (lexerContext.previousToken == ']')
                    || (lexerContext.previousToken == '}') || (lexerContext.previousToken == IDENT)
                    || (lexerContext.previousToken == MAGICEND))) {
                /* Test if next character indicates the start of an expression */
                const char* nextPtr = lexerContext.datap;
                bool hasContinuation = false;
                if (strncmp(nextPtr, "...", 3) == 0) {
                    hasContinuation = true;
                    nextPtr = skipContinuationTrivia(lexerContext.textbuffer, nextPtr);
                }
                char nextChar = *nextPtr;
                bool nextStartsExpression = false;
                if ((nextChar != '\0') && (nextChar != ']') && (nextChar != '}')
                    && (nextChar != ')') && (nextChar != ';') && (nextChar != ',')) {
                    if ((nextChar == '(') || (nextChar == '+') || (nextChar == '-')
                        || ((nextChar == '~') && (nextPtr[1] != '=')) || (nextChar == '[')
                        || (nextChar == '{') || (nextChar == '\'') || (nextChar == '"')
                        || (nextChar == '@') || ((isalnum(nextChar)) != 0)
                        || ((nextChar == '.') && ((_isDigit(nextPtr[1])) != 0))) {
                        nextStartsExpression = true;
                    } else if (!hasContinuation && (strncmp(nextPtr, "...", 3) == 0)) {
                        nextStartsExpression = true;
                    }
                }
                if (nextStartsExpression) {
                    /*
                       OK - now we have to decide if the "+/-" are infix or prefix operators...
                       In fact, this decision alone is the reason for this whole lexer.
                    */
                    if ((nextChar == '+') || (nextChar == '-')) {
                        /* If we are inside a parenthetical, we never insert virtual commas */
                        if ((lexerContext.bracketStackSize == 0)
                            || (lexerContext.bracketStack[lexerContext.bracketStackSize - 1]
                                != '(')) {
                            /*
                              OK - we are not inside a parenthetical.  Insert a virtual comma
                              if the next character is anything other than a whitespace
                            */
                            if ((nextPtr[1] != ' ') && (nextPtr[1] != '\t')) {
                                lexerContext.tokenType = ',';
                            }
                        }
                    } else {
                        lexerContext.tokenType = ',';
                    }
                }
                // Consolidated duplicate checks for virtual commas between strings.
                if (((!hasContinuation && currentChar(lexerContext) == '"')
                        || (hasContinuation && nextChar == '"'))
                    && lexerContext.previousToken == STRING) {
                    lexerContext.tokenType = ',';
                }
            }
        }
    }
    if (lexerContext.tokenType != WS && lexerContext.tokenType != ')') {
        lexerContext.justClosedAnonymousFunctionArguments = false;
    }
    appendFormattingToken(lexerContext, lexerContext.tokenType, tokenStart, lexerContext.datap);
    yylval = lexerContext.tokenValue;
    updateCatchIdentifierState(lexerContext, lexerContext.tokenType);
    lexerContext.previousToken = lexerContext.tokenType;
    return lexerContext.tokenType;
}
//=============================================================================
int
nelsonManualLex(LexerContext& lexerContext)
{
    int retval;
    const char* tokenStart = nullptr;
    yylval.v.i = 0;
    retval = yylexScreen(lexerContext, tokenStart);
    while (retval == WS) {
        retval = yylexScreen(lexerContext, tokenStart);
    }
    // Collapse a run of row separators inside a matrix '[' or cell '{' literal into
    // a single separator so that two or more consecutive comment-only (or blank)
    // lines act as one row break. Each such line emits its own
    // ENDSTMNT; the grammar accepts only a single leading/trailing/internal
    // rowSeparator, so without this a second consecutive ENDSTMNT/ENDQSTMNT has no
    // production and raises a spurious "matrix/cell definition" parse error. The
    // first separator after the opening bracket (lastSignificantToken == '[' / '{')
    // is preserved; only immediate repeats are dropped.
    while ((retval == ENDSTMNT || retval == ENDQSTMNT) && lexerContext.bracketStackSize > 0
        && ((lexerContext.bracketStack[lexerContext.bracketStackSize - 1] == '[')
            || (lexerContext.bracketStack[lexerContext.bracketStackSize - 1] == '{'))
        && ((lexerContext.lastSignificantToken == ENDSTMNT)
            || (lexerContext.lastSignificantToken == ENDQSTMNT))) {
        retval = yylexScreen(lexerContext, tokenStart);
        while (retval == WS) {
            retval = yylexScreen(lexerContext, tokenStart);
        }
    }
    lexerContext.lastSignificantToken = retval;
    recordAnonymousFunctionToken(lexerContext, retval, tokenStart, lexerContext.datap,
        retval == ')' && lexerContext.justClosedAnonymousFunctionArguments);
    // Stamped before the function-terminator checks below return early: the
    // separator that closes a function body is a token like any other, and the
    // statement node built from it reports the position of the implicit return.
    setTokenContextIfMissing(yylval, static_cast<int>(ContextInt(lexerContext)));
    if (isAfterFunctionTerminator(lexerContext)) {
        if (retval <= 0) {
            return retval;
        }
        if (retval == ENDSTMNT || retval == ENDQSTMNT) {
            return retval;
        }
        if (retval == FUNCTION) {
            setAfterFunctionTerminator(lexerContext, false);
        } else {
            LexerException(lexerContext,
                Nelson::resolveMessageTemplate("Nelson:interpreter:statementAfterFunctionEnd",
                    "Statement after function end."));
        }
    }
    updateFunctionDeclarationState(lexerContext, retval);
    return retval;
}
//=============================================================================
namespace Nelson {
void
setLexBuffer(LexerContext& lexerContext, const std::string& buffer)
{
    // Arguments validation blocks are now handled natively (contextual ARGUMENTS
    // token + grammar + compile-time lowering); the pre-lexer text rewrite is no
    // longer applied. The buffer is still normalized (BOM/CRLF/trailing newline).
    const std::string rewrittenBuffer = normalizeLexerBuffer(buffer);
    lexerContext.continuationCount = 0;
    lexerContext.bracketStackSize = 0;
    lexerContext.inBlock = 0;
    lexerContext.inStatement = 0;
    lexerContext.inFunction = false;
    lexerContext.lexState = Initial;
    lexerContext.vcStackSize = 0;
    lexerContext.placeholderCounter = 0;
    lexerContext.pendingNamedArgumentComma = false;
    lexerContext.pendingCommaContext = 0;
    lexerContext.catchIdentifierState = 0;
    lexerContext.previousToken = 0;
    lexerContext.tokenType = 0;
    lexerContext.tokenActive = 0;
    lexerContext.vcFlag = 0;
    lexerContext.lastSignificantToken = 0;
    lexerContext.justClosedAnonymousFunctionArguments = false;
    lexerContext.inDebugCommand = false;
    lexerContext.inMultilineComment = false;
    lexerContext.lexicalVariableNames.clear();
    resetAnonymousFunctionText(lexerContext);
    clearTextBufferLexer(lexerContext);
    lexerContext.textbufferStorage = rewrittenBuffer;
    lexerContext.textbuffer = const_cast<char*>(lexerContext.textbufferStorage.c_str());
    lexerContext.datap = lexerContext.textbuffer;
    lexerContext.linestart = lexerContext.datap;
    lexerContext.lineNumber = 0;
}
//=============================================================================
void
setLexBuffer(LexerContext& lexerContext, const std::wstring& buffer)
{
    setLexBuffer(lexerContext, wstring_to_utf8(buffer));
}
//=============================================================================
void
setLexFile(LexerContext& lexerContext, FILE* fp)
{
    lexerContext.inBlock = 0;
    lexerContext.inStatement = 0;
    lexerContext.inFunction = false;
    lexerContext.placeholderCounter = 0;
    lexerContext.inDebugCommand = false;
    lexerContext.inMultilineComment = false;
    struct stat st;
    clearerr(fp);
#ifdef _MSC_VER
    fstat(_fileno(fp), &st);
#else
    fstat(fileno(fp), &st);
#endif
    lexerContext.bracketStackSize = 0;
    lexerContext.lexState = Initial;
    lexerContext.vcStackSize = 0;
    lexerContext.lineNumber = 0;
    lexerContext.pendingNamedArgumentComma = false;
    lexerContext.pendingCommaContext = 0;
    lexerContext.catchIdentifierState = 0;
    lexerContext.previousToken = 0;
    lexerContext.tokenType = 0;
    lexerContext.tokenActive = 0;
    lexerContext.vcFlag = 0;
    lexerContext.lastSignificantToken = 0;
    lexerContext.justClosedAnonymousFunctionArguments = false;
    lexerContext.lexicalVariableNames.clear();
    resetAnonymousFunctionText(lexerContext);
    size_t cpos = (size_t)st.st_size;
    clearTextBufferLexer(lexerContext);
    std::string fileContent;
    fileContent.resize(cpos);
    size_t n = cpos == 0 ? 0 : fread(&fileContent[0], sizeof(char), cpos, fp);
    fileContent.resize(n);
    fileContent.push_back('\n');
    // Arguments validation blocks are handled natively; no pre-lexer rewrite.
    const std::string rewrittenBuffer = normalizeLexerBuffer(fileContent);
    lexerContext.textbufferStorage = rewrittenBuffer;
    lexerContext.textbuffer = const_cast<char*>(lexerContext.textbufferStorage.c_str());
    lexerContext.datap = lexerContext.textbuffer;
    lexerContext.linestart = lexerContext.datap;
}
//=============================================================================
bool
lexCheckForMoreInput(LexerContext& lexerContext, int ccount)
{
    try {
        while (yylex(lexerContext) > 0) {
            ;
        }
        return ((lexerContext.continuationCount > ccount)
            || ((lexerContext.bracketStackSize > 0)
                && ((lexerContext.bracketStack[lexerContext.bracketStackSize - 1] == '[')
                    || (lexerContext.bracketStack[lexerContext.bracketStackSize - 1] == '{')))
            || (lexerContext.inBlock != 0) || lexerContext.inMultilineComment);
    } catch (NelsonException&) {
        lexerContext.continuationCount = 0;
        return false;
    }
}
//=============================================================================
int
getContinuationCount(LexerContext& lexerContext)
{
    return lexerContext.continuationCount;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
