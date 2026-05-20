//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <cctype>
#include <cstring>
#include <vector>
#include "NelsonParserHelpers.hpp"
#include "LexerInterface.hpp"
#include "ParserInterface.hpp"
#include "FileParser.hpp"
#include "ParserState.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static thread_local ParserContext lastParserContext;
static thread_local ParserContext* activeParserContext = &lastParserContext;
//=============================================================================
static ParserContext&
currentParserContext()
{
    return *activeParserContext;
}
//=============================================================================
static ParserContext
parseStringWithContext(LexerContext& lexerContext, const std::string& txt, bool rethrowException,
    bool allowMixedScript);
//=============================================================================
class ActiveParserContextScope
{
public:
    explicit ActiveParserContextScope(ParserContext& parserContext) : _previous(activeParserContext)
    {
        activeParserContext = &parserContext;
        resetParser();
    }

    ~ActiveParserContextScope() { activeParserContext = _previous; }

    ActiveParserContextScope(const ActiveParserContextScope&) = delete;
    ActiveParserContextScope&
    operator=(const ActiveParserContextScope&)
        = delete;

private:
    ParserContext* _previous;
};
//=============================================================================
static void
chainFunction(MacroFunctionDef* r)
{
    ParserContext& parserContext = currentParserContext();
    parserContext.parserState = FuncDef;
    r->nextFunction = nullptr;
    r->prevFunction = nullptr;
    if (parserContext.macroFunctionDef == nullptr) {
        parserContext.macroFunctionDef = r;
    } else {
        r->localFunction = true;
        r->nextFunction = parserContext.macroFunctionDef->nextFunction;
        if (r->nextFunction != nullptr) {
            r->nextFunction->prevFunction = r;
        }
        parserContext.macroFunctionDef->nextFunction = r;
        r->prevFunction = parserContext.macroFunctionDef;
    }
}
//=============================================================================
static void
getLinePosition(const ParseRHS& val, int& lineNumber, int& columnNumber)
{
    int tokenID = 0;
    if (val.isToken) {
        tokenID = val.v.i;
    } else {
        tokenID = val.v.p->getContext();
    }
    lineNumber = tokenID & 0xFFFF;
    columnNumber = tokenID >> 16;
}
//=============================================================================
int
yyxpt(const std::string& xStr, const ParseRHS& val)
{
    int linenumber = 0;
    int colnumber = 0;
    getLinePosition(val, linenumber, colnumber);
    std::string msg;
    if (getParserFilenameU() == "") {
        msg = fmt::format(_("Expecting {0}"), xStr);
    } else {
        msg = fmt::format(_("Expecting {0}\n\tat line {1}, column {2} of file {3}"), xStr,
            linenumber, colnumber, getParserFilenameU());
    }
    setParserDiagnostic(msg, linenumber, colnumber);
    Error(msg);
    return 0;
}
//=============================================================================
int
yyxpt(const std::string& xStr)
{
    std::string msg;
    if (getParserFilenameU() == "") {
        msg = fmt::format(_("Expecting {0}"), xStr);
    } else {
        msg = fmt::format(_("Expecting {0}\n\tat line {1}, column {2} of file {3}"), xStr, 0, 0,
            getParserFilenameU());
    }
    setParserDiagnostic(msg, 0, 0);
    Error(msg);
    return 0;
}
//=============================================================================
void
functionBody(const ParseRHS& lhsRhs, const ParseRHS& nameRhs, const ParseRHS& rhsRhs,
    const ParseRHS& codeRhs)
{
    auto* r = new MacroFunctionDef();
    if (lhsRhs.v.p != nullptr) {
        r->returnVals = lhsRhs.v.p->toStringList();
    }
    if (nameRhs.v.p != nullptr) {
        r->setName(nameRhs.v.p->text);
    }
    if (rhsRhs.v.p != nullptr) {
        r->arguments = rhsRhs.v.p->toStringList();
    }
    if (codeRhs.v.p != nullptr) {
        r->code = codeRhs.v.p;
    }
    r->setFilename(getParserFilenameW());
    chainFunction(r);
}
//=============================================================================
std::string
decodeline(const ParseRHS& val)
{
    int linenumber = 0;
    int colnumber = 0;
    getLinePosition(val, linenumber, colnumber);
    return fmt::to_string(linenumber);
}
//=============================================================================
void
resetParser()
{
    currentParserContext() = ParserContext {};
}
//=============================================================================
void
setParserDiagnostic(const std::string& message, int line, int column)
{
    ParserContext& parserContext = currentParserContext();
    parserContext.parserState = ParseError;
    parserContext.diagnostic.message = message;
    parserContext.diagnostic.filename = getParserFilenameU();
    parserContext.diagnostic.line = line;
    parserContext.diagnostic.column = column;
}
//=============================================================================
static void
setParserDiagnosticIfMissing(ParserContext& parserContext, const std::string& message)
{
    if (parserContext.diagnostic.message.empty()) {
        setParserDiagnostic(message, 0, 0);
    }
}
//=============================================================================
static bool
lineStartsWithFunctionKeyword(const std::string& txt, size_t lineStart, size_t lineEnd)
{
    size_t pos = lineStart;
    while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
        pos++;
    }
    static constexpr const char* functionKeyword = "function";
    static constexpr size_t functionKeywordLength = 8;
    if ((lineEnd - pos) < functionKeywordLength) {
        return false;
    }
    if (txt.compare(pos, functionKeywordLength, functionKeyword) != 0) {
        return false;
    }
    const size_t after = pos + functionKeywordLength;
    if (after >= lineEnd) {
        return true;
    }
    const unsigned char next = static_cast<unsigned char>(txt[after]);
    return (std::isspace(next) != 0);
}
//=============================================================================
static bool
lineStartsWithKeyword(const std::string& txt, size_t lineStart, size_t lineEnd, const char* keyword)
{
    size_t pos = lineStart;
    while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
        pos++;
    }
    const size_t keywordLength = strlen(keyword);
    if ((lineEnd - pos) < keywordLength) {
        return false;
    }
    if (txt.compare(pos, keywordLength, keyword) != 0) {
        return false;
    }
    const size_t after = pos + keywordLength;
    if (after >= lineEnd) {
        return true;
    }
    const unsigned char next = static_cast<unsigned char>(txt[after]);
    return (std::isspace(next) != 0) || (next == '(') || (next == ';') || (next == ',');
}
//=============================================================================
static bool
lineStartsWithBlockOpener(const std::string& txt, size_t lineStart, size_t lineEnd)
{
    static constexpr const char* openers[]
        = { "function", "if", "for", "while", "switch", "try", "arguments" };
    for (const auto* opener : openers) {
        if (lineStartsWithKeyword(txt, lineStart, lineEnd, opener)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
lineStartsWithBlockEnd(const std::string& txt, size_t lineStart, size_t lineEnd)
{
    return lineStartsWithKeyword(txt, lineStart, lineEnd, "end");
}
//=============================================================================
static bool
lineHasScriptContent(const std::string& txt, size_t lineStart, size_t lineEnd)
{
    size_t pos = lineStart;
    while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
        pos++;
    }
    return pos < lineEnd;
}
//=============================================================================
static void
appendFunctionChain(MacroFunctionDef* head, MacroFunctionDef* toAppend, bool markNested)
{
    if (head == nullptr || toAppend == nullptr) {
        return;
    }
    MacroFunctionDef* tail = head;
    while (tail->nextFunction != nullptr) {
        tail = tail->nextFunction;
    }
    MacroFunctionDef* cp = toAppend;
    bool first = true;
    while (cp != nullptr) {
        MacroFunctionDef* next = cp->nextFunction;
        cp->localFunction = true;
        cp->nestedFunction = markNested;
        if (first) {
            cp->prevFunction = head;
            first = false;
        }
        tail->nextFunction = cp;
        tail = cp;
        cp = next;
    }
}
//=============================================================================
static std::string
replaceRangesWithNewlines(
    const std::string& txt, const std::vector<std::pair<size_t, size_t>>& ranges)
{
    std::string result = txt;
    for (const auto& range : ranges) {
        for (size_t k = range.first; k < range.second && k < result.size(); ++k) {
            if (result[k] != '\n' && result[k] != '\r') {
                result[k] = ' ';
            }
        }
    }
    return result;
}
//=============================================================================
static bool
findNestedFunctionBlocks(const std::string& txt, std::vector<std::pair<size_t, size_t>>& ranges)
{
    bool sawFirstFunction = false;
    bool inMultilineComment = false;
    int blockDepth = 0;
    size_t nestedStart = std::string::npos;
    size_t lineStart = 0;
    while (lineStart < txt.size()) {
        size_t lineEnd = txt.find('\n', lineStart);
        if (lineEnd == std::string::npos) {
            lineEnd = txt.size();
        }
        const size_t nextLineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
        size_t pos = lineStart;
        while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
            pos++;
        }
        if (inMultilineComment) {
            size_t endComment = txt.find("%}", pos);
            if (endComment != std::string::npos && endComment < lineEnd) {
                inMultilineComment = false;
                pos = endComment + 2;
                while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
                    pos++;
                }
            } else {
                lineStart = nextLineStart;
                continue;
            }
        }
        if (pos >= lineEnd) {
            lineStart = nextLineStart;
            continue;
        }
        if (txt.compare(pos, 2, "%{") == 0) {
            size_t endComment = txt.find("%}", pos + 2);
            if (endComment == std::string::npos || endComment > lineEnd) {
                inMultilineComment = true;
                lineStart = nextLineStart;
                continue;
            }
            pos = endComment + 2;
            while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
                pos++;
            }
            if (pos >= lineEnd) {
                lineStart = nextLineStart;
                continue;
            }
        }
        if (txt[pos] == '%') {
            lineStart = nextLineStart;
            continue;
        }
        if (lineStartsWithFunctionKeyword(txt, lineStart, lineEnd)) {
            if (!sawFirstFunction) {
                sawFirstFunction = true;
                blockDepth = 1;
            } else {
                if (blockDepth == 1 && nestedStart == std::string::npos) {
                    nestedStart = lineStart;
                }
                blockDepth++;
            }
        } else if (lineStartsWithBlockOpener(txt, lineStart, lineEnd)) {
            if (sawFirstFunction) {
                blockDepth++;
            }
        } else if (lineStartsWithBlockEnd(txt, lineStart, lineEnd)) {
            if (sawFirstFunction && blockDepth > 0) {
                blockDepth--;
                if (nestedStart != std::string::npos && blockDepth == 1) {
                    ranges.emplace_back(nestedStart, nextLineStart);
                    nestedStart = std::string::npos;
                }
            }
        }
        lineStart = nextLineStart;
    }
    return !ranges.empty();
}
//=============================================================================
static bool
tryParseFunctionWithNestedFunctions(
    LexerContext& lexerContext, ParserContext& parserContext, const std::string& txt)
{
    std::vector<std::pair<size_t, size_t>> nestedRanges;
    if (!findNestedFunctionBlocks(txt, nestedRanges)) {
        return false;
    }
    const std::string parentSource = replaceRangesWithNewlines(txt, nestedRanges);
    const std::string originalFilename = getParserFilenameU();
    const std::wstring originalFilenameW = getParserFilenameW();

    LexerContext parentLexerContext;
    ParserContext parentContext
        = parseStringWithContext(parentLexerContext, parentSource, false, false);
    setParserFilename(originalFilename);
    if (parentContext.state() != FuncDef || parentContext.macroFunctionDef == nullptr) {
        return false;
    }

    for (const auto& range : nestedRanges) {
        std::string nestedSource = txt.substr(range.first, range.second - range.first);
        if (!nestedSource.empty() && nestedSource.back() != '\n') {
            nestedSource += '\n';
        }
        LexerContext nestedLexerContext;
        ParserContext nestedContext
            = parseStringWithContext(nestedLexerContext, nestedSource, false, true);
        setParserFilename(originalFilename);
        if (nestedContext.state() != FuncDef || nestedContext.macroFunctionDef == nullptr) {
            return false;
        }
        if (!originalFilenameW.empty()) {
            MacroFunctionDef* cp = nestedContext.macroFunctionDef;
            while (cp != nullptr) {
                cp->setFilename(originalFilenameW);
                cp = cp->nextFunction;
            }
        }
        appendFunctionChain(parentContext.macroFunctionDef, nestedContext.macroFunctionDef, true);
    }
    parserContext = parentContext;
    return true;
}
//=============================================================================
static bool
findMixedScriptLocalFunctionSplit(const std::string& txt, size_t& splitOffset)
{
    bool haveScriptContent = false;
    bool inMultilineComment = false;
    int scriptBlockDepth = 0;
    size_t lineStart = 0;
    while (lineStart < txt.size()) {
        size_t lineEnd = txt.find('\n', lineStart);
        if (lineEnd == std::string::npos) {
            lineEnd = txt.size();
        }
        size_t pos = lineStart;
        while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
            pos++;
        }
        if (inMultilineComment) {
            size_t endComment = txt.find("%}", pos);
            if (endComment != std::string::npos && endComment < lineEnd) {
                inMultilineComment = false;
                pos = endComment + 2;
                while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
                    pos++;
                }
            } else {
                lineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
                continue;
            }
        }
        if (pos >= lineEnd) {
            lineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
            continue;
        }
        if (txt.compare(pos, 2, "%{") == 0) {
            size_t endComment = txt.find("%}", pos + 2);
            if (endComment == std::string::npos || endComment > lineEnd) {
                inMultilineComment = true;
                lineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
                continue;
            }
            pos = endComment + 2;
            while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
                pos++;
            }
            if (pos >= lineEnd) {
                lineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
                continue;
            }
        }
        if (txt[pos] == '%') {
            lineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
            continue;
        }
        if (haveScriptContent && scriptBlockDepth == 0
            && lineStartsWithFunctionKeyword(txt, lineStart, lineEnd)) {
            splitOffset = lineStart;
            return true;
        }
        if (lineStartsWithBlockOpener(txt, lineStart, lineEnd)) {
            scriptBlockDepth++;
        } else if (lineStartsWithBlockEnd(txt, lineStart, lineEnd) && scriptBlockDepth > 0) {
            scriptBlockDepth--;
        }
        if (lineHasScriptContent(txt, pos, lineEnd)) {
            haveScriptContent = true;
        }
        lineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
    }
    return false;
}
//=============================================================================
static bool
tryParseMixedScriptLocalFunctions(
    LexerContext& lexerContext, ParserContext& parserContext, const std::string& txt)
{
    size_t splitOffset = 0;
    if (!findMixedScriptLocalFunctionSplit(txt, splitOffset)) {
        return false;
    }
    const std::string originalFilename = getParserFilenameU();
    const std::wstring originalFilenameW = getParserFilenameW();
    const std::string scriptPart = txt.substr(0, splitOffset);
    const std::string functionPart = txt.substr(splitOffset);

    LexerContext scriptLexerContext;
    ParserContext scriptContext
        = parseStringWithContext(scriptLexerContext, scriptPart, false, false);
    setParserFilename(originalFilename);
    if (scriptContext.state() != ScriptBlock || scriptContext.ast == nullptr) {
        return false;
    }

    LexerContext functionLexerContext;
    ParserContext functionContext
        = parseStringWithContext(functionLexerContext, functionPart, false, false);
    setParserFilename(originalFilename);
    if (functionContext.state() != FuncDef || functionContext.macroFunctionDef == nullptr) {
        return false;
    }
    if (!originalFilenameW.empty()) {
        MacroFunctionDef* cp = functionContext.macroFunctionDef;
        while (cp != nullptr) {
            cp->setFilename(originalFilenameW);
            cp = cp->nextFunction;
        }
    }

    parserContext = ParserContext {};
    parserContext.ast = scriptContext.ast;
    parserContext.macroFunctionDef = functionContext.macroFunctionDef;
    parserContext.parserState = ScriptBlock;
    return true;
}
//=============================================================================
void
setParsedScriptBlock(AbstractSyntaxTreePtr ast)
{
    ParserContext& parserContext = currentParserContext();
    parserContext.ast = ast;
    parserContext.parserState = ScriptBlock;
}
//=============================================================================
void
setParsedFunctionDef(MacroFunctionDef* r)
{
    ParserContext& parserContext = currentParserContext();
    parserContext.macroFunctionDef = r;
    parserContext.parserState = FuncDef;
}
//=============================================================================
AbstractSyntaxTreePtr
getParsedScriptBlock()
{
    return currentParserContext().ast;
}
//=============================================================================
MacroFunctionDef*
getParsedFunctionDef()
{
    return currentParserContext().macroFunctionDef;
}
//=============================================================================
ParserState
parseState()
{
    return currentParserContext().state();
}
//=============================================================================
static ParserContext
parseStringWithContext(LexerContext& lexerContext, const std::string& txt, bool rethrowException,
    bool allowMixedScript)
{
    ParserContext parserContext;
    ActiveParserContextScope activeContext(parserContext);
    setParserFilename("");
    setLexBuffer(lexerContext, txt);
    try {
        if (callyyparse(lexerContext, parserContext) != 0) {
            setParserDiagnosticIfMissing(parserContext, _("Unexpected parser error."));
            if (allowMixedScript
                && tryParseFunctionWithNestedFunctions(lexerContext, parserContext, txt)) {
                lastParserContext = parserContext;
                return lastParserContext;
            }
            if (allowMixedScript
                && tryParseMixedScriptLocalFunctions(lexerContext, parserContext, txt)) {
                lastParserContext = parserContext;
                return lastParserContext;
            }
        }
    } catch (const Exception& exception) {
        setParserDiagnosticIfMissing(parserContext, wstring_to_utf8(exception.getMessage()));
        if (allowMixedScript
            && tryParseFunctionWithNestedFunctions(lexerContext, parserContext, txt)) {
            lastParserContext = parserContext;
            return lastParserContext;
        }
        if (allowMixedScript
            && tryParseMixedScriptLocalFunctions(lexerContext, parserContext, txt)) {
            lastParserContext = parserContext;
            return lastParserContext;
        }
        lastParserContext = parserContext;
        if (rethrowException) {
            throw;
        }
        return lastParserContext;
    } catch (...) {
        setParserDiagnosticIfMissing(parserContext, _("Unexpected parser error."));
        if (allowMixedScript
            && tryParseFunctionWithNestedFunctions(lexerContext, parserContext, txt)) {
            lastParserContext = parserContext;
            return lastParserContext;
        }
        if (allowMixedScript
            && tryParseMixedScriptLocalFunctions(lexerContext, parserContext, txt)) {
            lastParserContext = parserContext;
            return lastParserContext;
        }
        lastParserContext = parserContext;
        if (rethrowException) {
            throw;
        }
        return lastParserContext;
    }
    lastParserContext = parserContext;
    return lastParserContext;
}
//=============================================================================
ParserState
parseString(LexerContext& lexerContext, const std::string& txt)
{
    return parseStringWithContext(lexerContext, txt, true, true).state();
}
//=============================================================================
ParserContext
parseStringResult(LexerContext& lexerContext, const std::string& txt)
{
    return parseStringWithContext(lexerContext, txt, false, true);
}
//=============================================================================
static ParserContext
parseFileWithContext(
    LexerContext& lexerContext, FILE* fp, const std::string& fname, bool rethrowException)
{
    ParserContext parserContext;
    ActiveParserContextScope activeContext(parserContext);
    setParserFilename(fname);
    std::string fileContent;
    if (fp != nullptr) {
        char buffer[4096];
        while (fgets(buffer, sizeof(buffer), fp)) {
            fileContent += buffer;
        }
    }
    setLexBuffer(lexerContext, fileContent);
    try {
        if (callyyparse(lexerContext, parserContext) != 0) {
            setParserDiagnosticIfMissing(parserContext, _("Unexpected parser error."));
            if (tryParseFunctionWithNestedFunctions(lexerContext, parserContext, fileContent)) {
                setParserFilename("");
                lastParserContext = parserContext;
                return lastParserContext;
            }
            if (tryParseMixedScriptLocalFunctions(lexerContext, parserContext, fileContent)) {
                setParserFilename("");
                lastParserContext = parserContext;
                return lastParserContext;
            }
        }
    } catch (const Exception& exception) {
        setParserDiagnosticIfMissing(parserContext, wstring_to_utf8(exception.getMessage()));
        if (tryParseFunctionWithNestedFunctions(lexerContext, parserContext, fileContent)) {
            setParserFilename("");
            lastParserContext = parserContext;
            return lastParserContext;
        }
        if (tryParseMixedScriptLocalFunctions(lexerContext, parserContext, fileContent)) {
            setParserFilename("");
            lastParserContext = parserContext;
            return lastParserContext;
        }
        setParserFilename("");
        lastParserContext = parserContext;
        if (rethrowException) {
            throw;
        }
        return lastParserContext;
    } catch (...) {
        setParserDiagnosticIfMissing(parserContext, _("Unexpected parser error."));
        if (tryParseFunctionWithNestedFunctions(lexerContext, parserContext, fileContent)) {
            setParserFilename("");
            lastParserContext = parserContext;
            return lastParserContext;
        }
        if (tryParseMixedScriptLocalFunctions(lexerContext, parserContext, fileContent)) {
            setParserFilename("");
            lastParserContext = parserContext;
            return lastParserContext;
        }
        setParserFilename("");
        lastParserContext = parserContext;
        if (rethrowException) {
            throw;
        }
        return lastParserContext;
    }
    setParserFilename("");
    lastParserContext = parserContext;
    return lastParserContext;
}
//=============================================================================
ParserState
parseFile(LexerContext& lexerContext, FILE* fp, const std::string& fname)
{
    return parseFileWithContext(lexerContext, fp, fname, true).state();
}
//=============================================================================
ParserContext
parseFileResult(LexerContext& lexerContext, FILE* fp, const std::string& fname)
{
    return parseFileWithContext(lexerContext, fp, fname, false);
}
//=============================================================================
}
//=============================================================================
