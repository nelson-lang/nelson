//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsInterpreter_exports.h"
#include "characters_encoding.hpp"
#include <ctype.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#define WS 999

#include "AST.hpp"
#include "AstManager.hpp"
#include "i18n.hpp"
#define YYSTYPE Nelson::ParseRHS

#include "Exception.hpp"
#include "FileParser.hpp"
#include "Keywords.hpp"
#include "NelSonParser.h"
using namespace Nelson;

extern YYSTYPE yylval;
extern bool interactiveMode;
extern int charcontext;
char* textbuffer = nullptr;
char* datap = nullptr;
char* linestart = nullptr;
int lineNumber;
int continuationCount;
int inBlock;
typedef enum
{
    Initial,
    Scanning,
    SpecScan
} LexingStates;

#define DEFAULT_BUFFER_SIZE_LEXER 256

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

keywordStruct tSearch, *pSearch;
//=============================================================================
void
clearTextBufferLexer()
{
    if (textbuffer) {
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
    } else {
        return ((datap - linestart + 1) << 16) | (lineNumber + 1);
    }
}
//=============================================================================
void
NextLine()
{
    lineNumber++;
    linestart = datap;
}
//=============================================================================
void
LexerException(std::string msg)
{
    char buffer[4906];
    if (!interactiveMode && (getParserFilenameU().size() != 0) && !msg.empty()) {
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
        LexerException(_("mismatched parenthesis").c_str());
    }
    if (bracketStack[--bracketStackSize] != t) {
        LexerException(_("mismatched parenthesis").c_str());
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
testSpecialFuncs()
{
    if (!isalpha(datap[0])) {
        return false;
    }
    // cd ..
    // cd .
    // dir *.txt
    // dir ?
    // cd c:/Windows
    // dir c:/Windows
    // dir ('c:/Windows')
    // dir('c:/Windows')
    // FIXME - this should check the current context to see if any of these have been
    // masked or assigned
    /*
    bool test1 = ((strncmp(datap, "cd ", 3) == 0) ||
                  (strncmp(datap, "ls ", 3) == 0) ||
                 (strncmp(datap, "dir ", 4) == 0) ||
                  (strncmp(datap, "global ", 7) == 0) ||
                  (strncmp(datap, "persistent ", 11) == 0));
    */
    bool test1 = ((strncmp(datap, "cd ..", 5) == 0) || (strncmp(datap, "cd .", 4) == 0)
        || (strncmp(datap, "dir ?", 4) == 0) || (strncmp(datap, "dir *", 4) == 0));
    if (test1) {
        return test1;
    }
    // Check for non-keyword identifier followed by whitespace followed by alphanum
    char keyword[IDENTIFIER_LENGTH_MAX + 1];
    char* cp = datap;
    while (isalnum(*cp)) {
        keyword[cp - datap] = *cp;
        cp++;
    }
    size_t lenKeyword = strlen(datap) - strlen(cp);
    if (lenKeyword > IDENTIFIER_LENGTH_MAX) {
        Error(_("Maximum name length exceeded."));
    }
    keyword[cp - datap] = 0;
    tSearch.word = keyword;
    pSearch = (keywordStruct*)bsearch(
        &tSearch, keyWord, KEYWORDCOUNT, sizeof(keywordStruct), compareKeyword);
    if (pSearch != nullptr) {
        return false;
    }
    while ((*cp == ' ') || (*cp == '\t')) {
        cp++;
    }
    if (isalnum(*cp)) {
        return true;
    }
    return false;
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
match(char* str)
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
    return ((p == 'e') || (p == 'E') || (p == 'd') || (p == 'D'));
}
//=============================================================================
inline int
isWhitespace()
{
    return (match(" ") || match("\t"));
}
//=============================================================================
inline int
isNewline()
{
    return (match("\n") || match("\r\n"));
}
//=============================================================================
inline int
testAlphaChar()
{
    int c = (int)datap[0];
    if (c < 0) {
        return 0;
    }
    return (isalpha(c));
}
//=============================================================================
inline int
testAlphaNumChar()
{
    int c = (int)datap[0];
    if (c < 0) {
        return 0;
    }
    return (isalnum(c) || (c == '_'));
}
//=============================================================================
inline int
_isDigit(char c)
{
    return (c >= 48 && c <= 57);
}
//=============================================================================
inline int
testDigit()
{
    int c = (int)datap[0];
    return (_isDigit(c));
}
//=============================================================================
inline int
testNewline()
{
    return ((datap[0] == 0) || (datap[0] == '\n') || ((datap[0] == '\r') && (datap[1] == '\n')));
}
//=============================================================================
inline int
previousChar()
{
    if (datap == textbuffer) {
        return 0;
    } else {
        return datap[-1];
    }
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
    return ((datap[0] == '\n') || (datap[0] == '\r') || (datap[0] == ';') || (datap[0] == ',')
        || (datap[0] == ' '));
}
//=============================================================================
void
lexUntermCharacterArray()
{
    char stringval[IDENTIFIER_LENGTH_MAX + 1];
    char* strptr;
    strptr = stringval;
    while (isWhitespace())
        ;
    if (testNewline()) {
        lexState = Scanning;
        return;
    }
    while (!testCharacterArrayTerm()) {
        *strptr++ = currentChar();
        discardChar();
    }
    *strptr++ = '\0';
    setTokenType(CHARACTER);
    tokenValue.isToken = false;
    tokenValue.v.p
        = allocateAbstractSyntaxTree(const_character_array_node, stringval, (int)ContextInt());
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
    while ((curchar != '"') || ((curchar == '"') && (ch == '"')) && !testNewline()) {
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
    if (testNewline()) {
        LexerException(_("unterminated string").c_str());
    }
    discardChar();
    *strptr++ = '\0';
    setTokenType(STRING);
    tokenValue.isToken = false;
    tokenValue.v.p = allocateAbstractSyntaxTree(const_string_node, stringval, (int)ContextInt());
    return;
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
    while ((curchar != '\'') || ((curchar == '\'') && (ch == '\'')) && !testNewline()) {
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
    if (testNewline()) {
        LexerException(_("unterminated character array").c_str());
    }
    discardChar();
    *strptr++ = '\0';
    setTokenType(CHARACTER);
    tokenValue.isToken = false;
    tokenValue.v.p
        = allocateAbstractSyntaxTree(const_character_array_node, stringval, (int)ContextInt());
    return;
}

void
lexIdentifier()
{
    int i = 0;
    char ident[IDENTIFIER_LENGTH_MAX + 1];
    while (testAlphaNumChar()) {
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
    pSearch = (keywordStruct*)bsearch(
        &tSearch, keyWord, KEYWORDCOUNT, sizeof(keywordStruct), compareKeyword);
    if (pSearch != nullptr) {
        setTokenType(pSearch->token);
        if (strcmp(ident, "end") == 0) {
            if (bracketStackSize == 0) {
                setTokenType(END);
                inBlock--;
            } else {
                setTokenType(MAGICEND);
            }
        }
        // The lexer no longer _has_ to keep track of the "end" keywords
        // to match them up.  But we need this information to determine
        // if more text is needed...
        tokenValue.isToken = false;
        tokenValue.v.p
            = allocateAbstractSyntaxTree(reserved_node, pSearch->ordinal, (int)ContextInt());
        if ((pSearch->token == FOR) || (pSearch->token == WHILE) || (pSearch->token == IF)
            || (pSearch->token == ELSEIF) || (pSearch->token == CASE)) {
            vcFlag = 1;
            inBlock++;
        }
        return;
    } else {
        setTokenType(IDENT);
        tokenValue.isToken = false;
        tokenValue.v.p = allocateAbstractSyntaxTree(id_node, ident, (int)ContextInt());
    }
}
//=============================================================================
int
lexNumber()
{
    int state;
    int cp;
    int i;
    char buffer[DEFAULT_BUFFER_SIZE_LEXER];
    int intonly;
    int vtype;
    // Initialize the state...
    state = 0;
    cp = 0;
    intonly = 1;
    while (state != 7) {
        switch (state) {
        case 0:
            if (datap[cp] == '.') {
                cp++;
                state = 3;
                intonly = 0;
            } else if (_isDigit(datap[cp])) {
                while (_isDigit(datap[cp])) {
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
            } else if (isE(datap[cp])) {
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
            } else if (_isDigit(datap[cp])) {
                state = 6;
            } else {
                LexerException(_("malformed floating point constant"));
            }
            break;
        case 3:
            if (_isDigit(datap[cp])) {
                while (_isDigit(datap[cp])) {
                    cp++;
                }
            } else {
                return 0;
            }
            state = 4;
            break;
        case 4:
            if (isE(datap[cp])) {
                intonly = 0;
                cp++;
                state = 2;
                break;
            } else {
                state = 7;
            }
            break;
        case 5:
            if (isE(datap[cp])) {
                intonly = 0;
                cp++;
                state = 2;
                break;
            } else if (_isDigit(datap[cp])) {
                while (_isDigit(datap[cp])) {
                    cp++;
                }
                state = 4;
                break;
            } else {
                state = 7;
            }
            break;
        case 6:
            if (_isDigit(datap[cp])) {
                while (_isDigit(datap[cp])) {
                    cp++;
                }
                state = 7;
            } else {
                LexerException(_("malformed floating point constant"));
            }
        }
    }
    if ((datap[cp] == 'f') || (datap[cp] == 'F')) {
        cp++;
        vtype = 1;
    } else if ((datap[cp] == 'u') || (datap[cp] == 'U')) {
        cp++;
        vtype = 4;
    } else if ((datap[cp] == 'd') || (datap[cp] == 'D')) {
        cp++;
        vtype = 2;
    } else if (!intonly) {
        vtype = 2;
    } else {
        vtype = 3;
    }
    for (i = 0; i < cp; i++) {
        buffer[i] = datap[i];
    }
    for (i = 0; i < cp; i++) {
        discardChar();
    }
    buffer[cp] = '\0';
    setTokenType(NUMERIC);
    switch (vtype) {
    case 1:
        tokenValue.isToken = false;
        if ((currentChar() == 'i') || (currentChar() == 'I')) {
            tokenValue.v.p
                = allocateAbstractSyntaxTree(const_complex_node, buffer, (int)ContextInt());
            discardChar();
        } else {
            tokenValue.v.p
                = allocateAbstractSyntaxTree(const_float_node, buffer, (int)ContextInt());
        }
        break;
    case 2:
        tokenValue.isToken = false;
        if ((currentChar() == 'i') || (currentChar() == 'I')) {
            tokenValue.v.p
                = allocateAbstractSyntaxTree(const_dcomplex_node, buffer, (int)ContextInt());
            discardChar();
        } else {
            tokenValue.v.p
                = allocateAbstractSyntaxTree(const_double_node, buffer, (int)ContextInt());
        }
        break;
    case 3:
        tokenValue.isToken = false;
        if ((currentChar() == 'i') || (currentChar() == 'I')) {
            tokenValue.v.p
                = allocateAbstractSyntaxTree(const_dcomplex_node, buffer, (int)ContextInt());
            discardChar();
        } else {
            tokenValue.v.p = allocateAbstractSyntaxTree(const_int_node, buffer, (int)ContextInt());
        }
        break;
    case 4:
        tokenValue.isToken = false;
        if ((currentChar() == 'i') || (currentChar() == 'I')) {
            tokenValue.v.p
                = allocateAbstractSyntaxTree(const_dcomplex_node, buffer, (int)ContextInt());
            discardChar();
        } else {
            tokenValue.v.p
                = allocateAbstractSyntaxTree(const_uint64_node, buffer, (int)ContextInt());
        }
        break;
    }
    return 1;
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
    if (match("...")) {
        while (!isNewline()) {
            discardChar();
        }
        NextLine();
        continuationCount++;
    }
    // comments suppported
    if (match("//") || match("%") || match("#")) {
        while (!isNewline()) {
            discardChar();
        }
        setTokenType(ENDSTMNT);
        NextLine();
        return;
    }
    if (currentChar() == '\"') {
        lexString();
        return;
    }
    if (currentChar() == '\'')
        if ((previousChar() == ')') || (previousChar() == ']') || (previousChar() == '}')
            || (isalnum(previousChar()))) {
            /* Not a string... */
            setTokenType((int)'\'');
            discardChar();
            return;
        } else {
            lexCharacterArray();
            return;
        }
    if (isWhitespace()) {
        while (isWhitespace())
            ;
        setTokenType(WS);
        return;
    }
    if (match(";\n") || match(";\r\n")) {
        setTokenType(ENDQSTMNT);
        tokenValue.isToken = true;
        tokenValue.v.i = (int)ContextInt();
        NextLine();
        lexState = Initial;
        if (bracketStackSize == 0) {
            vcFlag = 0;
        }
        return;
    }
    if (match(";")) {
        setTokenType(ENDQSTMNT);
        if (bracketStackSize == 0) {
            vcFlag = 0;
        }
        lexState = Initial;
        return;
    }
    if (match("\r\n") || match("\n")) {
        NextLine();
        setTokenType(ENDSTMNT);
        lexState = Initial;
        if (bracketStackSize == 0) {
            vcFlag = 0;
        }
        return;
    }
    if (match(".*")) {
        setTokenType(DOTTIMES);
        return;
    }
    if (match("./")) {
        setTokenType(DOTRDIV);
        return;
    }
    if (match(".\\")) {
        setTokenType(DOTLDIV);
        return;
    }
    if (match(".^")) {
        setTokenType(DOTPOWER);
        return;
    }
    if (match(".'")) {
        setTokenType(DOTTRANSPOSE);
        return;
    }
    if (match("!=")) {
        setTokenType(NE);
        return;
    }
    if (match("<>")) {
        setTokenType(NE);
        return;
    }
    if (match("~=")) {
        setTokenType(NE);
        return;
    }
    if (match("<=")) {
        setTokenType(LE);
        return;
    }
    if (match(">=")) {
        setTokenType(GE);
        return;
    }
    if (match("==")) {
        setTokenType(EQ);
        return;
    }
    if (match("||")) {
        setTokenType(SOR);
        return;
    }
    if (match("&&")) {
        setTokenType(SAND);
        return;
    }
    if (testAlphaChar() || currentChar() == '_') {
        lexIdentifier();
        // Are we inside a bracket? If so, leave well enough alone
        if ((tokenType != IDENT) || bracketStackSize) {
            return;
        }
        // No, so... munch the whitespace
        while (isWhitespace())
            ;
        // How do you know ident /ident is not ident/ident and is ident('/ident')?
        if (testAlphaChar()) {
            lexState = SpecScan;
        }
        return;
    }
    if (testDigit() || currentChar() == '.')
        if (lexNumber()) {
            return;
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
    if (isNewline()) {
        NextLine();
    } else if (isWhitespace()) {
    } else if (match(";")) {
		// nothing
    } else if (match("%") || match("//") || match("#")) {
        while (!isNewline()) {
            discardChar();
        }
        NextLine();
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
    while (!tokenActive) {
        yylexDoLex();
    }
    if ((tokenType == WS) && vcFlag) {
        /* Check for virtual commas... */
        if ((previousToken == ')') || (previousToken == '\'') || (previousToken == NUMERIC)
            || (previousToken == CHARACTER) || (previousToken == STRING) || (previousToken == ']')
            || (previousToken == '}') || (previousToken == IDENT) || (previousToken == MAGICEND)) {
            /* Test if next character indicates the start of an expression */
            if ((currentChar() == '(') || (currentChar() == '+') || (currentChar() == '-')
                || (currentChar() == '~') || (currentChar() == '[') || (currentChar() == '{')
                || (currentChar() == '\'') || (isalnum(currentChar()))
                || ((currentChar() == '.') && (_isDigit(datap[1])))
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
    if (!yylval.v.i) {
        yylval.isToken = true;
        yylval.v.i = (int)ContextInt();
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
    lexState = Initial;
    vcStackSize = 0;
    clearTextBufferLexer();
    textbuffer = (char*)calloc(strlen(buffer.c_str()) + 1, sizeof(char));
    datap = textbuffer;
    if (textbuffer) {
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
    long cpos = st.st_size;
    clearTextBufferLexer();
    // Allocate enough for the text, an extra newline, and null
    textbuffer = (char*)calloc(cpos + 2, sizeof(char));
    if (textbuffer) {
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
        while (yylex() > 0)
            ;
        return ((continuationCount > ccount)
            || ((bracketStackSize > 0)
                   && ((bracketStack[bracketStackSize - 1] == '[')
                          || (bracketStack[bracketStackSize - 1] == '{')))
            || inBlock);
    } catch (Exception& e) {
        e.what();
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
