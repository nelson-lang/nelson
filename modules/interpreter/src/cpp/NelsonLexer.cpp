//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif
//=============================================================================
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
#include "ParseRHS.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileParser.hpp"
#include "Keywords.hpp"
#include "LexerInterface.hpp"
#include "NelSonParser.h"
#include "LexerInterface.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static char* textbuffer = nullptr;
static char* datap = nullptr;
static char* linestart = nullptr;
static int lineNumber = 0;
static int continuationCount = 0;
static int inBlock = 0;
static int inStatement = 0;
static bool inFunction = false;
static int countEndFunction = 0;
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
LexingStates lexState;
int bracketStack[DEFAULT_BUFFER_SIZE_LEXER];
int bracketStackSize;
int vcStack[DEFAULT_BUFFER_SIZE_LEXER];
int vcStackSize;
int vcFlag;
//=============================================================================
/*
 * These variables capture the token information
 */
int tokenActive;
int tokenType;
ParseRHS tokenValue;
//=============================================================================
keywordStruct tSearch, *pSearch;
//=============================================================================
void
clearTextBufferLexer()
{
    if (textbuffer != nullptr) {
        free(textbuffer);
        textbuffer = nullptr;
    }
}
//=============================================================================
indexType
ContextInt()
{
    if (datap == linestart) {
        return (1 << 16 | lineNumber);
    }
    return ((datap - linestart + 1) << 16) | (lineNumber + 1);
}
//=============================================================================
void
NextLine()
{
    lineNumber++;
    linestart = datap;
}
//=============================================================================
static void
LexerException(const std::string& msg)
{
    char buffer[4906];
    if ((!getParserFilenameU().empty()) && !msg.empty()) {
        sprintf(buffer, _("Lexical error '%s'\n\tat line %d of file %s").c_str(), msg.c_str(),
            lineNumber + 1, getParserFilenameU().c_str());
    } else {
        if (!msg.empty()) {
            sprintf(buffer, _("Lexical error '%s'").c_str(), msg.c_str());
        } else {
            sprintf(buffer, "%s", _("Lexical error").c_str());
        }
    }
    Error(buffer);
}
//=============================================================================
inline void
pushBracket(char t)
{
    bracketStack[bracketStackSize++] = t;
}
//=============================================================================
inline void
popBracket(char t)
{
    if (bracketStackSize <= 0) {
        LexerException(_("mismatched parenthesis"));
    }
    if (bracketStack[--bracketStackSize] != t) {
        LexerException(_("mismatched parenthesis"));
    }
}
//=============================================================================
inline void
pushVCState()
{
    vcStack[vcStackSize++] = vcFlag;
}
//=============================================================================
inline void
popVCState()
{
    vcFlag = vcStack[--vcStackSize];
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
testSpecialFuncs()
{
    std::wstring wline = utf8_to_wstring(std::string(datap));
    if (!iswalpha(wline[0])) {
        return false;
    }
    bool isHardcodedShorcut = isPathCommandShortCut(L"ls", wline)
        || isPathCommandShortCut(L"display", wline) || isPathCommandShortCut(L"cd", wline)
        || isPathCommandShortCut(L"dir", wline);

    if (isHardcodedShorcut) {
        return true;
    }
    // Check for non-keyword identifier followed by whitespace followed by alphanum
    size_t i = 0;
    std::wstring wkeyword;
    wkeyword.reserve(IDENTIFIER_LENGTH_MAX);
    while (iswalnum(wline[i]) && i < wline.size()) { //-V781
        wkeyword.push_back(wline[i]);
        i++;
    }
    std::string keyword = wstring_to_utf8(wkeyword);
    if (keyword.length() > IDENTIFIER_LENGTH_MAX) {
        Error(_("Maximum name length exceeded."));
    }
    tSearch.word = keyword.c_str(); //-V506
    pSearch = static_cast<keywordStruct*>(
        bsearch(&tSearch, keyWord, KEYWORDCOUNT, sizeof(keywordStruct), compareKeyword));
    if (pSearch != nullptr) {
        return false;
    }
    while ((iswspace(wline[i]) || wline[i] == L'\t') && i < wline.size()) { //-V781
        i++;
    }
    return (iswalpha(wline[i]) || iswdigit(wline[i]));
}
//=============================================================================
inline void
setTokenType(int type)
{
    tokenType = type;
    tokenActive = 1;
    tokenValue.isToken = true;
    tokenValue.v.p = nullptr;
}
//=============================================================================
inline int
match(const char* str)
{
    if (strncmp(str, datap, strlen(str)) == 0) {
        datap += strlen(str);
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
isWhitespace()
{
    return static_cast<int>((match(" ") != 0) || (match("\t") != 0));
}
//=============================================================================
inline int
isNewline()
{
    return static_cast<int>((match("\n") != 0) || (match("\r\n") != 0));
}
//=============================================================================
inline int
testAlphaChar()
{
    int c = static_cast<int>(datap[0]);
    if (c < 0) {
        return 0;
    }
    return (isalpha(c));
}
//=============================================================================
inline int
testAlphaNumChar()
{
    int c = static_cast<int>(datap[0]);
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
testDigit()
{
    int c = static_cast<int>(datap[0]);
    return (_isDigit(c));
}
//=============================================================================
inline int
testNewline()
{
    return static_cast<int>(
        (datap[0] == 0) || (datap[0] == '\n') || ((datap[0] == '\r') && (datap[1] == '\n')));
}
//=============================================================================
inline int
previousChar()
{
    if (datap == textbuffer) {
        return 0;
    }
    return datap[-1];
}
//=============================================================================
inline int
currentChar()
{
    return datap[0];
}
//=============================================================================
inline void
discardChar()
{
    datap++;
}
//=============================================================================
inline int
testCharacterArrayTerm()
{
    return static_cast<int>((datap[0] == '\n') || (datap[0] == '\r') || (datap[0] == ';')
        || (datap[0] == ',') || (datap[0] == ' '));
}
//=============================================================================
void
lexUntermCharacterArray()
{
    char stringval[IDENTIFIER_LENGTH_MAX + 1];
    char* strptr;
    strptr = stringval;
    while (isWhitespace() != 0) {
        ;
    }
    if (testNewline() != 0) {
        lexState = Scanning;
        return;
    }
    while (testCharacterArrayTerm() == 0) {
        *strptr++ = currentChar();
        discardChar();
    }
    *strptr++ = '\0';
    setTokenType(CHARACTER);
    tokenValue.isToken = false;
    tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_character_array_node, stringval, static_cast<int>(ContextInt()));
#ifdef LEXDEBUG
    printf("Untermed string %s\r\n", stringval);
#endif
    //   if ((datap[0] == ';') || (datap[0] == '\r') || (datap[0] == ',') || (datap[0] == '\n'))
    //     lexState = Scanning;
    lexState = Scanning;
}
//=============================================================================
void
lexString()
{
    char stringval[IDENTIFIER_LENGTH_MAX + 1];
    memset(stringval, 0, IDENTIFIER_LENGTH_MAX + 1);
    char* strptr = stringval;
    discardChar();
    int curchar = currentChar();
    char ch = datap[1];
    while ((curchar != '"') || ((curchar == '"') && (ch == '"')) && (testNewline() == 0)) {
        if ((currentChar() == '"') && (ch == '"')) {
            discardChar();
        }
        *strptr++ = curchar;
        discardChar();
        curchar = currentChar();
        if (strlen(datap) > 1) {
            ch = datap[1];
        } else {
            break;
        }
    }
    if (testNewline() != 0) {
        LexerException(_("unterminated string"));
    }
    discardChar();
    *strptr++ = '\0';
    setTokenType(STRING);
    tokenValue.isToken = false;
    tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_string_node, stringval, static_cast<int>(ContextInt()));
}
//=============================================================================
void
lexCharacterArray()
{
    char stringval[IDENTIFIER_LENGTH_MAX + 1];
    memset(stringval, 0, IDENTIFIER_LENGTH_MAX + 1);
    char* strptr = stringval;
    discardChar();
    int curchar = currentChar();
    char ch = datap[1];
    while ((curchar != '\'') || ((curchar == '\'') && (ch == '\'')) && (testNewline() == 0)) {
        if ((currentChar() == '\'') && (ch == '\'')) {
            discardChar();
        }
        *strptr++ = curchar;
        discardChar();
        curchar = currentChar();
        if (strlen(datap) > 1) {
            ch = datap[1];
        } else {
            break;
        }
    }
    if (testNewline() != 0) {
        LexerException(_("unterminated character array"));
    }
    discardChar();
    *strptr++ = '\0';
    setTokenType(CHARACTER);
    tokenValue.isToken = false;
    tokenValue.v.p = AbstractSyntaxTree::createNode(
        const_character_array_node, stringval, static_cast<int>(ContextInt()));
}
//=============================================================================
void
lexIdentifier()
{
    int i = 0;
    char ident[IDENTIFIER_LENGTH_MAX + 1];
    while (testAlphaNumChar() != 0) {
        ident[i++] = currentChar();
        if (i > IDENTIFIER_LENGTH_MAX) {
            char msg[DEFAULT_BUFFER_SIZE_LEXER];
            sprintf(msg, _("exceeds the Nelson maximum name length of %d characters.").c_str(),
                IDENTIFIER_LENGTH_MAX);
            LexerException(msg);
        }
        discardChar();
    }
    ident[i] = '\0';
    tSearch.word = ident;
    pSearch = static_cast<keywordStruct*>(
        bsearch(&tSearch, keyWord, KEYWORDCOUNT, sizeof(keywordStruct), compareKeyword));
    if (pSearch == nullptr) {
        setTokenType(IDENT);
        tokenValue.isToken = false;
        tokenValue.v.p
            = AbstractSyntaxTree::createNode(id_node, ident, static_cast<int>(ContextInt()));
        return;
    }
    switch (pSearch->token) {
    case FUNCTION: {
        countEndFunction = 0;
        inFunction = true;
        setTokenType(pSearch->token);
    } break;
    case ENDFUNCTION: {
        if (countEndFunction == 0) {
            inFunction = false;
            setTokenType(pSearch->token);
            countEndFunction++;
        } else {
            LexerException(_("This statement is not inside any function."));
        }
    } break;
    case END: {
        if (bracketStackSize == 0) {
            bool asEndfunction = false;
            if (inFunction && inStatement == 0) {
                asEndfunction = true;
            }
            if (asEndfunction) {
                if (countEndFunction == 0) {
                    strcpy(ident, "endfunction");
                    tSearch.word = ident;
                    pSearch = static_cast<keywordStruct*>(bsearch(
                        &tSearch, keyWord, KEYWORDCOUNT, sizeof(keywordStruct), compareKeyword));
                    setTokenType(ENDFUNCTION);
                    countEndFunction++;
                } else {
                    LexerException(_("This statement is not inside any function."));
                }
            } else {
                setTokenType(END);
                inBlock--;
                inStatement--;
            }
        } else {
            setTokenType(MAGICEND);
        }
    } break;
    case TRY: {
        setTokenType(pSearch->token);
        inStatement++;
    } break;
    case SWITCH: {
        setTokenType(pSearch->token);
        inStatement++;
    } break;
    default: {
        setTokenType(pSearch->token);
    } break;
        // The lexer no longer _has_ to keep track of the "end" keywords
        // to match them up.  But we need this information to determine
        // if more text is needed...
    case FOR: {
        vcFlag = 1;
        inBlock++;
        inStatement++;
        setTokenType(pSearch->token);
    } break;
    case WHILE: {
        vcFlag = 1;
        inBlock++;
        inStatement++;
        setTokenType(pSearch->token);
    } break;
    case IF: {
        vcFlag = 1;
        inBlock++;
        inStatement++;
        setTokenType(pSearch->token);
    } break;
    case ELSEIF:
    case CASE: {
        vcFlag = 1;
        inBlock++;
        setTokenType(pSearch->token);
    } break;
    }
    tokenValue.isToken = false;
    tokenValue.v.p = AbstractSyntaxTree::createNode(
        reserved_node, pSearch->ordinal, static_cast<int>(ContextInt()));
}
//=============================================================================
int
lexNumber()
{
    bool isNegative = (tokenType == '-');
    int state = 0;
    indexType cp = 0;
    char buffer[DEFAULT_BUFFER_SIZE_LEXER];
    int intonly = 1;
    // Initialize the state...
    state = 0;
    while (state != 7) {
        switch (state) {
        case 0:
            if (datap[cp] == '.') {
                cp++;
                state = 3;
                intonly = 0;
            } else if (_isDigit(datap[cp]) != 0) {
                while (_isDigit(datap[cp]) != 0) {
                    cp++;
                }
                state = 1;
                break;
            } else {
                return 0;
            }
            break;
        case 1:
            if (datap[cp] == '.') {
                intonly = 0;
                cp++;
                state = 5;
                break;
            } else if (isE(datap[cp]) != 0) {
                intonly = 0;
                cp++;
                state = 2;
                break;
            } else {
                state = 7;
            }
            break;
        case 2:
            if ((datap[cp] == '+') || (datap[cp] == '-')) {
                cp++;
                state = 6;
            } else if (_isDigit(datap[cp]) != 0) {
                state = 6;
            } else {
                LexerException(_("malformed floating point constant"));
            }
            break;
        case 3:
            if (_isDigit(datap[cp]) != 0) {
                while (_isDigit(datap[cp]) != 0) {
                    cp++;
                }
            } else {
                return 0;
            }
            state = 4; //-V112
            break;
        case 4:
            if (isE(datap[cp]) != 0) {
                intonly = 0;
                cp++;
                state = 2;
                break;
            } else {
                state = 7;
            }
            break;
        case 5:
            if (isE(datap[cp]) != 0) {
                intonly = 0;
                cp++;
                state = 2;
                break;
            } else if (_isDigit(datap[cp]) != 0) {
                while (_isDigit(datap[cp]) != 0) {
                    cp++;
                }
                state = 4; //-V112
                break;
            } else {
                state = 7;
            }
            break;
        case 6:
            if (_isDigit(datap[cp]) != 0) {
                while (_isDigit(datap[cp]) != 0) {
                    cp++;
                }
                state = 7;
            } else {
                LexerException(_("malformed floating point constant"));
            }
        }
    }

    NODE_TYPE nodeType = null_node;
    if (datap[cp] == 'f') {
        // f32 --> single
        if ((datap[cp + 1] == '3') && (datap[cp + 2] == '2')) {
            cp = cp + 3;
            nodeType = const_float_node;
        }
        // f64 --> double
        else if ((datap[cp + 1] == '6') && (datap[cp + 2] == '4')) {
            cp = cp + 3;
            nodeType = const_double_node;
        } else {
            LexerException(_("Malformed floating point constant."));
        }
    } else if (datap[cp] == 'i') {
        // i8 --> int8
        if (datap[cp + 1] == '8') {
            cp = cp + 2;
            nodeType = const_int8_node;
        }
        // i16 --> int16
        else if ((datap[cp + 1] == '1') && (datap[cp + 2] == '6')) {
            cp = cp + 3;
            nodeType = const_int16_node;
        }
        // i32 --> int32
        else if ((datap[cp + 1] == '3') && (datap[cp + 2] == '2')) {
            cp = cp + 3;
            nodeType = const_int32_node;
        }
        // i64 --> int32
        else if ((datap[cp + 1] == '6') && (datap[cp + 2] == '4')) {
            cp = cp + 3;
            nodeType = const_int64_node;
        }
    } else if (datap[cp] == 'u') {
        if (intonly == 0) {
            LexerException(_("Malformed unsigned integer constant."));
        }
        // u8 --> uint8
        if (datap[cp + 1] == '8') {
            cp = cp + 2;
            nodeType = const_uint8_node;
        }
        // u16 --> uint16
        else if ((datap[cp + 1] == '1') && (datap[cp + 2] == '6')) {
            cp = cp + 3;
            nodeType = const_uint16_node;
        }
        // u32 --> uint32
        else if ((datap[cp + 1] == '3') && (datap[cp + 2] == '2')) {
            cp = cp + 3;
            nodeType = const_uint32_node;
        }
        // u64 --> uint64
        else if ((datap[cp + 1] == '6') && (datap[cp + 2] == '4')) {
            cp = cp + 3;
            nodeType = const_uint64_node;
        } else {
            LexerException(_("Malformed unsigned integer constant."));
        }
    } else if (intonly) {
        nodeType = const_int_node;
    } else {
        nodeType = const_double_node;
    }

    for (indexType i = 0; i < cp; i++) {
        buffer[i] = datap[i];
    }
    for (indexType i = 0; i < cp; i++) {
        discardChar();
    }
    buffer[cp] = '\0';
    std::string content = std::string(buffer);
    setTokenType(NUMERIC);

    switch (nodeType) {
    case const_int_node: {
        tokenValue.isToken = false;
        if (currentChar() == 'i') {
            tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_dcomplex_node, content, static_cast<int>(ContextInt()));
            discardChar();
        } else {
            tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_double_node, content, static_cast<int>(ContextInt()));
        }
    } break;
    case const_double_node: {
        tokenValue.isToken = false;
        if (currentChar() == 'i') {
            tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_dcomplex_node, content, static_cast<int>(ContextInt()));
            discardChar();
        } else {
            tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_double_node, content, static_cast<int>(ContextInt()));
        }
    } break;
    case const_float_node: {
        tokenValue.isToken = false;
        if (currentChar() == 'i') {
            tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_complex_node, content, static_cast<int>(ContextInt()));
            discardChar();
        } else {
            tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_float_node, content, static_cast<int>(ContextInt()));
        }
    } break;
    case const_int8_node:
    case const_int16_node:
    case const_int32_node:
    case const_int64_node: {
        tokenValue.isToken = false;
        if (isNegative) {
            content = "-" + content;
        }
        tokenValue.v.p
            = AbstractSyntaxTree::createNode(nodeType, content, static_cast<int>(ContextInt()));

    } break;
    case const_uint8_node:
    case const_uint16_node:
    case const_uint32_node:
    case const_uint64_node: {
        if (isNegative) {
            LexerException(_("Malformed unsigned integer constant with unary operator '-'."));
        }
        tokenValue.isToken = false;
        tokenValue.v.p
            = AbstractSyntaxTree::createNode(nodeType, content, static_cast<int>(ContextInt()));
    } break;
    default: {
        tokenValue.isToken = false;
        if (currentChar() == 'i') {
            tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_dcomplex_node, content, static_cast<int>(ContextInt()));
            discardChar();
        } else {
            tokenValue.v.p = AbstractSyntaxTree::createNode(
                const_double_node, content, static_cast<int>(ContextInt()));
        }
    } break;
    }
    return 1;
}
//=============================================================================
static void
fetchComment()
{
    while (isNewline() == 0) {
        discardChar();
    }
    NextLine();
}
//=============================================================================
/*
 * String detection is a bit tricky, I suppose....  A quote character
 * immediately following (without whitespace) a bracket or a alphanumeric
 * is a transpose.  Otherwise, a quote character marks the beginning of
 * a string.  This means that we need to look at the _previous_ token.
 */
void
lexScanningState()
{
    if (match("...") != 0) {
        while (isNewline() == 0) {
            discardChar();
        }
        NextLine();
        continuationCount++;
    }
    // comments suppported
    if (currentChar() == '%') {
        fetchComment();
        setTokenType(ENDSTMNT);
        return;
    }
    if (currentChar() == '\"') {
        lexString();
        return;
    }
    if (currentChar() == '\'') {
        if ((previousChar() == ')') || (previousChar() == ']') || (previousChar() == '}')
            || ((isalnum(previousChar())) != 0)) {
            /* Not a string... */
            setTokenType(static_cast<int>('\''));
            discardChar();
            return;
        }
        lexCharacterArray();
        return;
    }
    if (isWhitespace() != 0) {
        while (isWhitespace() != 0) {
            ;
        }
        setTokenType(WS);
        return;
    }
    if ((match(";\n") != 0) || (match(";\r\n") != 0)) {
        setTokenType(ENDQSTMNT);
        tokenValue.isToken = true;
        tokenValue.v.i = static_cast<int>(ContextInt());
        NextLine();
        lexState = Initial;
        if (bracketStackSize == 0) {
            vcFlag = 0;
        }
        return;
    }
    if (match(";") != 0) {
        setTokenType(ENDQSTMNT);
        if (bracketStackSize == 0) {
            vcFlag = 0;
        }
        lexState = Initial;
        return;
    }
    if ((match("\r\n") != 0) || (match("\n") != 0)) {
        NextLine();
        setTokenType(ENDSTMNT);
        lexState = Initial;
        if (bracketStackSize == 0) {
            vcFlag = 0;
        }
        return;
    }
    if (match(".*") != 0) {
        setTokenType(DOTTIMES);
        return;
    }
    if (match("./") != 0) {
        setTokenType(DOTRDIV);
        return;
    }
    if (match(".\\") != 0) {
        setTokenType(DOTLDIV);
        return;
    }
    if (match(".^") != 0) {
        setTokenType(DOTPOWER);
        return;
    }
    if (match(".'") != 0) {
        setTokenType(DOTTRANSPOSE);
        return;
    }
    if (match("!=") != 0) {
        setTokenType(NE);
        return;
    }
    if (match("<>") != 0) {
        setTokenType(NE);
        return;
    }
    if (match("~=") != 0) {
        setTokenType(NE);
        return;
    }
    if (match("<=") != 0) {
        setTokenType(LE);
        return;
    }
    if (match(">=") != 0) {
        setTokenType(GE);
        return;
    }
    if (match("==") != 0) {
        setTokenType(EQ);
        return;
    }
    if (match("||") != 0) {
        setTokenType(SOR);
        return;
    }
    if (match("&&") != 0) {
        setTokenType(SAND);
        return;
    }
    if ((testAlphaChar() != 0) || currentChar() == '_') {
        lexIdentifier();
        // Are we inside a bracket? If so, leave well enough alone
        if ((tokenType != IDENT) || (bracketStackSize != 0)) {
            return;
        }
        // No, so... munch the whitespace
        while (isWhitespace() != 0) {
            ;
        }
        // How do you know ident /ident is not ident/ident and is ident('/ident')?
        if (testAlphaChar() != 0) {
            lexState = SpecScan;
        }
        return;
    }
    if ((testDigit() != 0) || currentChar() == '.') {
        if (lexNumber() != 0) {
            return;
        }
    }
    if ((currentChar() == '[') || (currentChar() == '{')) {
        pushBracket(currentChar());
        pushVCState();
        vcFlag = 1;
    }
    if (currentChar() == '(') {
        pushBracket(currentChar());
        pushVCState();
        vcFlag = 0;
    }
    if (currentChar() == ')') {
        popVCState();
        popBracket('(');
    }
    if (currentChar() == ']') {
        popVCState();
        popBracket('[');
    }
    if (currentChar() == '}') {
        popVCState();
        popBracket('{');
    }
    if (currentChar() == ',') {
        if (bracketStackSize == 0) {
            vcFlag = 0;
        }
    }
    if (currentChar() < 0) {
        LexerException(datap);
    }
    setTokenType(currentChar());
    discardChar();
}
//=============================================================================
void
lexInitialState()
{
    if (isNewline() != 0) {
        NextLine();
    } else if (isWhitespace() != 0) { // nothing
    } else if (match(";") != 0) {
        // nothing
    } else if (currentChar() == '%') {
        fetchComment();
    } else if (testSpecialFuncs()) {
        lexIdentifier();
        lexState = SpecScan;
    } else {
        lexState = Scanning;
    }
}
//=============================================================================
void
yylexDoLex()
{
    switch (lexState) {
    case Initial:
        lexInitialState();
        break;
    case Scanning:
        lexScanningState();
        break;
    case SpecScan:
        lexUntermCharacterArray();
        break;
    }
}
//=============================================================================
int
yylexScreen()
{
    static int previousToken = 0;
    tokenActive = 0;
    while (tokenActive == 0) {
        yylexDoLex();
    }
    if ((tokenType == WS) && (vcFlag != 0)) {
        /* Check for virtual commas... */
        if ((previousToken == ')') || (previousToken == '\'') || (previousToken == NUMERIC)
            || (previousToken == CHARACTER) || (previousToken == STRING) || (previousToken == ']')
            || (previousToken == '}') || (previousToken == IDENT) || (previousToken == MAGICEND)) {
            /* Test if next character indicates the start of an expression */
            if ((currentChar() == '(') || (currentChar() == '+') || (currentChar() == '-')
                || (currentChar() == '~') || (currentChar() == '[') || (currentChar() == '{')
                || (currentChar() == '\'') || ((isalnum(currentChar())) != 0)
                || ((currentChar() == '.') && ((_isDigit(datap[1])) != 0))
                || (strncmp(datap, "...", 3) == 0)) {
                /*
                   OK - now we have to decide if the "+/-" are infix or prefix operators...
                   In fact, this decision alone is the reason for this whole lexer.
                */
                if ((currentChar() == '+') || (currentChar() == '-')) {
                    /* If we are inside a parenthetical, we never insert virtual commas */
                    if ((bracketStackSize == 0) || (bracketStack[bracketStackSize - 1] != '(')) {
                        /*
                          OK - we are not inside a parenthetical.  Insert a virtual comma
                          if the next character is anything other than a whitespace
                        */
                        if ((datap[1] != ' ') && (datap[1] != '\t')) {
                            tokenType = ',';
                        }
                    }
                } else {
                    tokenType = ',';
                }
            }
            if (currentChar() == '"' && previousToken == STRING) {
                tokenType = ',';
            }
        }
    }
    yylval = tokenValue;
    previousToken = tokenType;
    return tokenType;
}
//=============================================================================
int
yylex()
{
    int retval;
    yylval.v.i = 0;
    retval = yylexScreen();
    while (retval == WS) {
        retval = yylexScreen();
    }
    if (yylval.v.i == 0) {
        yylval.isToken = true;
        yylval.v.i = static_cast<int>(ContextInt());
    }
    return retval;
}
//=============================================================================
namespace Nelson {
void
setLexBuffer(const std::string& buffer)
{
    continuationCount = 0;
    bracketStackSize = 0;
    inBlock = 0;
    inStatement = 0;
    inFunction = false;
    lexState = Initial;
    vcStackSize = 0;
    clearTextBufferLexer();
    textbuffer = static_cast<char*>(calloc(buffer.length() + 1, sizeof(char)));
    datap = textbuffer;
    if (textbuffer != nullptr) {
        strcpy(textbuffer, buffer.c_str());
    }
    linestart = datap;
    lineNumber = 0;
}
//=============================================================================
void
setLexBuffer(const std::wstring& buffer)
{
    setLexBuffer(wstring_to_utf8(buffer));
}
//=============================================================================
void
setLexFile(FILE* fp)
{
    inBlock = 0;
    inStatement = 0;
    inFunction = false;
    struct stat st;
    clearerr(fp);
#ifdef _MSC_VER
    fstat(_fileno(fp), &st);
#else
    fstat(fileno(fp), &st);
#endif
    bracketStackSize = 0;
    lexState = Initial;
    vcStackSize = 0;
    lineNumber = 0;
    size_t cpos = (size_t)st.st_size;
    clearTextBufferLexer();
    // Allocate enough for the text, an extra newline, and null
    textbuffer = static_cast<char*>(calloc((size_t)(cpos + 2), sizeof(char)));
    if (textbuffer != nullptr) {
        datap = textbuffer;
        size_t n = fread(textbuffer, sizeof(char), cpos, fp);
        textbuffer[n] = '\n';
        textbuffer[n + 1] = 0;
        linestart = datap;
    }
}
//=============================================================================
bool
lexCheckForMoreInput(int ccount)
{
    try {
        while (yylex() > 0) {
            ;
        }
        return ((continuationCount > ccount)
            || ((bracketStackSize > 0)
                && ((bracketStack[bracketStackSize - 1] == '[')
                    || (bracketStack[bracketStackSize - 1] == '{')))
            || (inBlock != 0));
    } catch (Exception&) {
        continuationCount = 0;
        return false;
    }
}
//=============================================================================
int
getContinuationCount()
{
    return continuationCount;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
