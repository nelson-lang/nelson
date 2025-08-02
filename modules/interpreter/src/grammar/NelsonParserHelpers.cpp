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
#include <fmt/printf.h>
#include <fmt/format.h>
#include "NelsonParserHelpers.hpp"
#include "LexerInterface.hpp"
#include "ParserInterface.hpp"
#include "FileParser.hpp"
#include "ParserState.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static AbstractSyntaxTreePtr mainAST = nullptr;
static MacroFunctionDef* mainMDef = nullptr;
//=============================================================================
static void
chainFunction(MacroFunctionDef* r)
{
    r->nextFunction = nullptr;
    r->prevFunction = nullptr;
    if (mainMDef == nullptr) {
        mainMDef = r;
    } else {
        r->localFunction = true;
        r->nextFunction = mainMDef->nextFunction;
        if (r->nextFunction != nullptr) {
            r->nextFunction->prevFunction = r;
        }
        mainMDef->nextFunction = r;
        r->prevFunction = mainMDef;
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
        msg = fmt::sprintf(_("Expecting %s"), xStr);
    } else {
        msg = fmt::sprintf(_("Expecting %s\n\tat line %d, column %d of file %s"), xStr, linenumber,
            colnumber, getParserFilenameU());
    }
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
    if (mainAST != nullptr) {
        mainAST = nullptr;
    }
    if (mainMDef != nullptr) {
        mainMDef = nullptr;
    }
}
//=============================================================================
void
setParsedScriptBlock(AbstractSyntaxTreePtr ast)
{
    mainAST = ast;
}
//=============================================================================
void
setParsedFunctionDef(MacroFunctionDef* r)
{
    mainMDef = r;
}
//=============================================================================
AbstractSyntaxTreePtr
getParsedScriptBlock()
{
    return mainAST;
}
//=============================================================================
MacroFunctionDef*
getParsedFunctionDef()
{
    return mainMDef;
}
//=============================================================================
ParserState
parseState()
{
    if (mainAST != nullptr) {
        return ScriptBlock;
    }
    return FuncDef;
}
//=============================================================================
ParserState
parseString(LexerContext& lexerContext, const std::string& txt)
{
    resetParser();
    setParserFilename("");
    setLexBuffer(lexerContext, txt);
    callyyparse(lexerContext);
    return parseState();
}
//=============================================================================
ParserState
parseFile(LexerContext& lexerContext, FILE* fp, const std::string& fname)
{
    resetParser();
    setParserFilename(fname);
    setLexFile(lexerContext, fp);
    callyyparse(lexerContext);
    setParserFilename("");
    return parseState();
}
//=============================================================================
}
//=============================================================================
