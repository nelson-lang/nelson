//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
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
#include <sys/stat.h>
#include <sys/types.h>
//=============================================================================
#define WS 999
#define YYSTYPE Nelson::ParseRHS
//=============================================================================
#include "AbstractSyntaxTree.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "FileParser.hpp"
#include "Keywords.hpp"
#include "LexerInterface.hpp"
#include "NelSonParser.h"
#include "LexerContext.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
void
clearTextBufferLexer(Nelson::LexerContext& lexerContext)
{
    if (lexerContext.textbuffer != nullptr) {
        free(lexerContext.textbuffer);
        lexerContext.textbuffer = nullptr;
    }
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
        error_message = fmt::format(_("Lexical error '{}' at line {} of file {}"), msg,
            lexerContext.lineNumber + 1, getParserFilenameU());
    } else {
        if (!msg.empty()) {
            error_message = fmt::format(_("Lexical error '{}'"), msg);
        } else {
            error_message = _("Lexical error");
        }
    }

    Error(error_message, "Nelson:Lexer");
}
//=============================================================================
inline void
pushBracket(LexerContext& lexerContext, char t, bool isDestructuring, bool isFunctionCall)
{
    lexerContext.bracketStack[lexerContext.bracketStackSize] = t;
    lexerContext.bracketIsDestructuring[lexerContext.bracketStackSize] = isDestructuring;
    lexerContext.bracketIsFunctionCall[lexerContext.bracketStackSize] = isFunctionCall;
    lexerContext.bracketStackSize++;
}
//=============================================================================
inline void
popBracket(LexerContext& lexerContext, char t)
{
    if (lexerContext.bracketStackSize <= 0) {
        LexerException(lexerContext, _("mismatched parenthesis"));
    }
    if (lexerContext.bracketStack[--lexerContext.bracketStackSize] != t) {
        LexerException(lexerContext, _("mismatched parenthesis"));
    }
    lexerContext.bracketIsDestructuring[lexerContext.bracketStackSize] = false;
    lexerContext.bracketIsFunctionCall[lexerContext.bracketStackSize] = false;
}
//=============================================================================
inline void
pushVCState(LexerContext& lexerContext)
{
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
    if (StringHelpers::starts_with(trimmedLine, wcommand + L" ")) {
        StringHelpers::replace_first(trimmedLine, wcommand + L" ", L"");
        StringHelpers::trim(trimmedLine);
        bool haveSimpleQuotes = StringHelpers::starts_with(trimmedLine, L"'")
            && StringHelpers::ends_with(trimmedLine, L"'");
        return (haveSimpleQuotes || !StringHelpers::starts_with(trimmedLine, L"("));
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

    // Check for hardcoded shortcuts
    static const std::wstring commands[] = { L"ls", L"cd", L"dir" };
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
        Error(_("Maximum name length exceeded."), "Nelson:namelengthmaxexceeded");
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
skipContinuationTrivia(const char* ptr)
{
    const char* current = ptr;
    if (strncmp(current, "...", 3) != 0) {
        return current;
    }
    current += 3;
    for (;;) {
        while (*current == ' ' || *current == '\t') {
            ++current;
        }
        if (*current == '%') {
            while (*current != '\0' && *current != '\n' && *current != '\r') {
                ++current;
            }
            continue;
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
            continue;
        }
        break;
    }
    return current;
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
    char stringval[IDENTIFIER_LENGTH_MAX + 1];
    char* strptr;
    strptr = stringval;
    while (isWhitespace(lexerContext) != 0) {
        ;
    }
    if (testNewline(lexerContext) != 0) {
        lexerContext.lexState = Scanning;
        return;
    }
    while (testCharacterArrayTerm(lexerContext) == 0) {
        *strptr++ = currentChar(lexerContext);
        discardChar(lexerContext);
    }
    *strptr++ = '\0';
    setTokenType(lexerContext, CHARACTER);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_character_array_node, stringval, static_cast<int>(ContextInt(lexerContext)));
#ifdef LEXDEBUG
    printf("Untermed string %s\r\n", stringval);
#endif
    lexerContext.lexState = Scanning;
}
//=============================================================================
void
lexString(LexerContext& lexerContext)
{
    char stringval[IDENTIFIER_LENGTH_MAX + 1];
    memset(stringval, 0, IDENTIFIER_LENGTH_MAX + 1);
    char* strptr = stringval;
    discardChar(lexerContext);
    int curchar = currentChar(lexerContext);
    char ch = lexerContext.datap[1];
    while (
        (curchar != '"') || ((curchar == '"') && (ch == '"')) && (testNewline(lexerContext) == 0)) {
        if ((currentChar(lexerContext) == '"') && (ch == '"')) {
            discardChar(lexerContext);
        }
        *strptr++ = curchar;
        discardChar(lexerContext);
        curchar = currentChar(lexerContext);
        if (strlen(lexerContext.datap) > 1) {
            ch = lexerContext.datap[1];
        } else {
            break;
        }
    }
    if (testNewline(lexerContext) != 0) {
        LexerException(lexerContext, _("unterminated string"));
    }
    discardChar(lexerContext);
    *strptr++ = '\0';
    setTokenType(lexerContext, STRING);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_string_node, stringval, static_cast<int>(ContextInt(lexerContext)));
}
//=============================================================================
void
lexCharacterArray(LexerContext& lexerContext)
{
    char stringval[IDENTIFIER_LENGTH_MAX + 1];
    memset(stringval, 0, IDENTIFIER_LENGTH_MAX + 1);
    char* strptr = stringval;
    discardChar(lexerContext);
    int curchar = currentChar(lexerContext);
    char ch = lexerContext.datap[1];
    while ((curchar != '\'')
        || ((curchar == '\'') && (ch == '\'')) && (testNewline(lexerContext) == 0)) {
        if ((currentChar(lexerContext) == '\'') && (ch == '\'')) {
            discardChar(lexerContext);
        }
        *strptr++ = curchar;
        discardChar(lexerContext);
        curchar = currentChar(lexerContext);
        if (strlen(lexerContext.datap) > 1) {
            ch = lexerContext.datap[1];
        } else {
            break;
        }
    }
    if (testNewline(lexerContext) != 0) {
        LexerException(lexerContext, _("unterminated character array"));
    }
    discardChar(lexerContext);
    *strptr++ = '\0';
    setTokenType(lexerContext, CHARACTER);
    lexerContext.tokenValue.isToken = false;
    lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_character_array_node, stringval, static_cast<int>(ContextInt(lexerContext)));
}
//=============================================================================
void
lexIdentifier(LexerContext& lexerContext)
{
    int i = 0;
    char ident[IDENTIFIER_LENGTH_MAX + 1];
    while (testAlphaNumChar(lexerContext) != 0) {
        ident[i++] = currentChar(lexerContext);
        if (i > IDENTIFIER_LENGTH_MAX) {
            std::string msg
                = fmt::format(_("exceeds the Nelson maximum name length of {} characters."),
                    IDENTIFIER_LENGTH_MAX);
            LexerException(lexerContext, msg);
        }
        discardChar(lexerContext);
    }
    ident[i] = '\0';
    strncpy(lexerContext.tSearch.word, ident, IDENTIFIER_LENGTH_MAX);
    lexerContext.tSearch.word[IDENTIFIER_LENGTH_MAX] = '\0';
    lexerContext.pSearch = static_cast<keywordStruct*>(bsearch(
        &lexerContext.tSearch, keyWord, KEYWORDCOUNT, sizeof(keywordStruct), compareKeyword));
    const int context = static_cast<int>(ContextInt(lexerContext));
    if (lexerContext.pSearch == nullptr) {
        if (convertIdentifierToNamedArgument(lexerContext, ident, context)) {
            return;
        }
        setTokenType(lexerContext, IDENT);
        lexerContext.tokenValue.isToken = false;
        lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(id_node, ident, context);
        return;
    }
    switch (lexerContext.pSearch->token) {
    case FUNCTION: {
        lexerContext.countEndFunction = 0;
        lexerContext.inFunction = true;
        setTokenType(lexerContext, lexerContext.pSearch->token);
    } break;
    case ENDFUNCTION: {
        if (lexerContext.countEndFunction == 0) {
            lexerContext.inFunction = false;
            setTokenType(lexerContext, lexerContext.pSearch->token);
            lexerContext.countEndFunction++;
        } else {
            LexerException(lexerContext, _("This statement is not inside any function."));
        }
    } break;
    case END: {
        if (lexerContext.bracketStackSize == 0) {
            bool asEndfunction = false;
            if (lexerContext.inFunction && lexerContext.inStatement == 0) {
                asEndfunction = true;
            }
            if (asEndfunction) {
                if (lexerContext.countEndFunction == 0) {
                    strncpy(ident, "endfunction", IDENTIFIER_LENGTH_MAX);
                    ident[IDENTIFIER_LENGTH_MAX] = '\0';
                    strncpy(lexerContext.tSearch.word, ident, IDENTIFIER_LENGTH_MAX);
                    lexerContext.tSearch.word[IDENTIFIER_LENGTH_MAX] = '\0';
                    lexerContext.pSearch
                        = static_cast<keywordStruct*>(bsearch(&lexerContext.tSearch, keyWord,
                            KEYWORDCOUNT, sizeof(keywordStruct), compareKeyword));
                    setTokenType(lexerContext, ENDFUNCTION);
                    lexerContext.countEndFunction++;
                } else {
                    LexerException(lexerContext, _("This statement is not inside any function."));
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
int
lexNumber(LexerContext& lexerContext)
{
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
                LexerException(lexerContext, _("malformed floating point constant"));
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
                LexerException(lexerContext, _("malformed floating point constant"));
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
            LexerException(lexerContext, _("Malformed floating point constant."));
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
            LexerException(lexerContext, _("Malformed unsigned integer constant."));
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
            LexerException(lexerContext, _("Malformed unsigned integer constant."));
        }
    } else if (intonly) {
        nodeType = const_int_node;
    } else {
        nodeType = const_double_node;
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
        if (isNegative) {
            content = "-" + content;
        }
        lexerContext.tokenValue.v.p = AbstractSyntaxTree::createNode(
            nodeType, content, static_cast<int>(ContextInt(lexerContext)));

    } break;
    case const_uint8_node:
    case const_uint16_node:
    case const_uint32_node:
    case const_uint64_node: {
        if (isNegative) {
            LexerException(
                lexerContext, _("Malformed unsigned integer constant with unary operator '-'."));
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
        while (isNewline(lexerContext) == 0) {
            discardChar(lexerContext);
        }
        setTokenType(lexerContext, WS);
        NextLine(lexerContext);
        lexerContext.continuationCount++;
        return;
    }
    // comments suppported
    if (currentChar(lexerContext) == '%') {
        fetchComment(lexerContext);
        setTokenType(lexerContext, ENDSTMNT);
        return;
    }
    if (currentChar(lexerContext) == '\"') {
        lexString(lexerContext);
        return;
    }
    if (currentChar(lexerContext) == '\'') {
        if ((previousChar(lexerContext) == ')') || (previousChar(lexerContext) == ']')
            || (previousChar(lexerContext) == '}') || (previousChar(lexerContext) == '.')
            || ((isalnum(previousChar(lexerContext))) != 0)) {
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
        completeContinuation(lexerContext);
        return;
    }
    if (match(lexerContext, ";") != 0) {
        setTokenType(lexerContext, ENDQSTMNT);
        if (lexerContext.bracketStackSize == 0) {
            lexerContext.vcFlag = 0;
        }
        lexerContext.lexState = Initial;
        return;
    }
    if ((match(lexerContext, "\r\n") != 0) || (match(lexerContext, "\n") != 0)) {
        NextLine(lexerContext);
        setTokenType(lexerContext, ENDSTMNT);
        lexerContext.lexState = Initial;
        if (lexerContext.bracketStackSize == 0) {
            lexerContext.vcFlag = 0;
        }
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
            LexerException(lexerContext, _("Incorrect use of tilde."));
            return;
        }
        setTokenType(lexerContext, static_cast<int>('~'));
        discardChar(lexerContext);
        return;
    }

    if ((testAlphaChar(lexerContext) != 0) || currentChar(lexerContext) == '_') {
        lexIdentifier(lexerContext);
        // Are we inside a bracket? If so, leave well enough alone
        if ((lexerContext.tokenType != IDENT) || (lexerContext.bracketStackSize != 0)) {
            return;
        }
        // No, so... munch the whitespace
        while (isWhitespace(lexerContext) != 0) {
            ;
        }
        // How do you know ident /ident is not ident/ident and is ident('/ident')?
        if (testAlphaChar(lexerContext) != 0) {
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
        pushBracket(lexerContext, currentChar(lexerContext), isDestructuring, false);
        pushVCState(lexerContext);
        lexerContext.vcFlag = 1;
    } else if (currentChar(lexerContext) == '{') {
        pushBracket(lexerContext, currentChar(lexerContext), false, false);
        pushVCState(lexerContext);
        lexerContext.vcFlag = 1;
    }
    if (currentChar(lexerContext) == '(') {
        const bool functionCallContext = isFunctionCallOpening(lexerContext);
        pushBracket(lexerContext, currentChar(lexerContext), false, functionCallContext);
        pushVCState(lexerContext);
        lexerContext.vcFlag = 0;
    }
    if (currentChar(lexerContext) == ')') {
        popVCState(lexerContext);
        popBracket(lexerContext, '(');
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
        fetchComment(lexerContext);
    } else if (testSpecialFuncs(lexerContext)) {
        lexIdentifier(lexerContext);
        lexerContext.lexState = SpecScan;
    } else {
        lexerContext.lexState = Scanning;
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
static int
yylexScreen(LexerContext& lexerContext)
{
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
            if ((lexerContext.previousToken == ')') || (lexerContext.previousToken == '\'')
                || (lexerContext.previousToken == NUMERIC)
                || (lexerContext.previousToken == CHARACTER)
                || (lexerContext.previousToken == STRING) || (lexerContext.previousToken == ']')
                || (lexerContext.previousToken == '}') || (lexerContext.previousToken == IDENT)
                || (lexerContext.previousToken == MAGICEND)) {
                /* Test if next character indicates the start of an expression */
                const char* nextPtr = lexerContext.datap;
                bool hasContinuation = false;
                if (strncmp(nextPtr, "...", 3) == 0) {
                    hasContinuation = true;
                    nextPtr = skipContinuationTrivia(nextPtr);
                }
                char nextChar = *nextPtr;
                bool nextStartsExpression = false;
                if ((nextChar != '\0') && (nextChar != ']') && (nextChar != '}')
                    && (nextChar != ')') && (nextChar != ';') && (nextChar != ',')) {
                    if ((nextChar == '(') || (nextChar == '+') || (nextChar == '-')
                        || (nextChar == '~') || (nextChar == '[') || (nextChar == '{')
                        || (nextChar == '\'') || ((isalnum(nextChar)) != 0)
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
    yylval = lexerContext.tokenValue;
    lexerContext.previousToken = lexerContext.tokenType;
    return lexerContext.tokenType;
}
//=============================================================================
int
yylex(LexerContext& lexerContext)
{
    int retval;
    yylval.v.i = 0;
    retval = yylexScreen(lexerContext);
    while (retval == WS) {
        retval = yylexScreen(lexerContext);
    }
    if (yylval.v.i == 0) {
        yylval.isToken = true;
        yylval.v.i = static_cast<int>(ContextInt(lexerContext));
    }
    return retval;
}
//=============================================================================
namespace Nelson {
void
setLexBuffer(LexerContext& lexerContext, const std::string& buffer)
{
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
    lexerContext.previousToken = 0;
    clearTextBufferLexer(lexerContext);
    lexerContext.textbuffer = static_cast<char*>(calloc(buffer.length() + 1, sizeof(char)));
    lexerContext.datap = lexerContext.textbuffer;
    if (lexerContext.textbuffer != nullptr) {
        strcpy(lexerContext.textbuffer, buffer.c_str());
    }
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
    lexerContext.previousToken = 0;
    size_t cpos = (size_t)st.st_size;
    clearTextBufferLexer(lexerContext);
    // Allocate enough for the text, an extra newline, and null
    lexerContext.textbuffer = static_cast<char*>(calloc((size_t)(cpos + 2), sizeof(char)));
    if (lexerContext.textbuffer != nullptr) {
        lexerContext.datap = lexerContext.textbuffer;
        size_t n = fread(lexerContext.textbuffer, sizeof(char), cpos, fp);
        lexerContext.textbuffer[n] = '\n';
        lexerContext.textbuffer[n + 1] = 0;
        lexerContext.linestart = lexerContext.datap;
    }
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
            || (lexerContext.inBlock != 0));
    } catch (Exception&) {
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
