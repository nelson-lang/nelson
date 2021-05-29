//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/format.hpp>
#include "NelsonParserHelpers.hpp"
#include "LexerInterface.hpp"
#include "ParserInterface.hpp"
#include "FileParser.hpp"
#include "ParserState.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ASTPtr mainAST = nullptr;
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
        tokenID = val.v.p->context();
    }
    lineNumber = tokenID & 0xFFFF;
    columnNumber = tokenID >> 16;
}
//=============================================================================
int
yyxpt(const std::string& xStr, const ParseRHS &val)
{
    int linenumber = 0;
    int colnumber = 0;
    getLinePosition(val, linenumber, colnumber);
    std::string msg;
    if (getParserFilenameU() == "") {
        msg = str(boost::format(_("Expecting %s")) % xStr);
    } else {
        msg = str(boost::format(_("Expecting %s\n\tat line %d, column %d of file %s")) % xStr
            % linenumber % colnumber % getParserFilenameU());
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
        r->name = nameRhs.v.p->text;
    }
    if (rhsRhs.v.p != nullptr) {
        r->arguments = rhsRhs.v.p->toStringList();
    }
    if (codeRhs.v.p != nullptr) {
        r->code = codeRhs.v.p;
    }
    r->fileName = getParserFilenameW();
    chainFunction(r);
}
//=============================================================================
std::string
decodeline(const ParseRHS &val)
{
    int linenumber = 0;
    int colnumber = 0;
    getLinePosition(val, linenumber, colnumber);
    return std::to_string(linenumber);
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
setParsedScriptBlock(ASTPtr ast)
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
ASTPtr
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
parseString(const std::string& txt)
{
    resetParser();
    setParserFilename("");
    setLexBuffer(txt);
    callyyparse();
    return parseState();
}
//=============================================================================
ParserState
parseFile(FILE* fp, const std::string& fname)
{
    resetParser();
    setParserFilename(fname);
    setLexFile(fp);
    callyyparse();
    setParserFilename("");
    return parseState();
}
//=============================================================================
}
//=============================================================================
