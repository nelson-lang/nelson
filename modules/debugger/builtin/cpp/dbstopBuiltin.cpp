//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dbstopBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include "ParseFile.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserInterface.hpp"
#include "StringHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static size_t
getLineFromContext(int contextValue);
static size_t
getMaxLineInSubtreeDown(AbstractSyntaxTreePtr t);
static size_t
getMaxLineFromChain(AbstractSyntaxTreePtr t);
static size_t
getLineNumberFromCode(AbstractSyntaxTreePtr code, size_t lineNumber, size_t maxLinesInFile);
//=============================================================================
static void
dbstopInAt(Evaluator* eval, const std::wstring& functioOrFilename, size_t position,
    std::wstring& errorMessage);
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstopBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dbstop in file
    // dbstop in file at location
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 0);
    if (argIn.size() == 3) {
        Error("Wrong number of input arguments.");
    }

    std::wstring inArg = argIn[0].getContentAsWideString();
    if (inArg != L"in") {
        Error("Second argument must be 'in'.");
    }

    size_t position = 1;

    if (argIn.size() == 4) {
        std::wstring atArg = argIn[2].getContentAsWideString();
        if (atArg != L"at") {
            Error(_W("Third argument must be 'at'."));
        }
        std::wstring posArg = argIn[3].getContentAsWideString();
        size_t idx = 0;
        position = static_cast<size_t>(std::stoul(posArg, &idx));
        if (idx != posArg.size() || position == 0) {
            Error(_W("Invalid position argument."));
        }
    }

    std::wstring functioOrFilename = argIn[1].getContentAsWideString();

    std::wstring errorMessage;
    dbstopInAt(eval, functioOrFilename, position, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    return {};
}
//=============================================================================
void
dbstopInAt(Evaluator* eval, const std::wstring& functioOrFilename, size_t position,
    std::wstring& errorMessage)
{
    bool isSimpleQuoted = StringHelpers::starts_with(functioOrFilename, L"'")
        && StringHelpers::ends_with(functioOrFilename, L"'");
    bool isDoubleQuoted = StringHelpers::starts_with(functioOrFilename, L"\"")
        && StringHelpers::ends_with(functioOrFilename, L"\"");
    if (isSimpleQuoted || isDoubleQuoted) {
        std::wstring unquotedFilename = functioOrFilename.substr(1, functioOrFilename.size() - 2);
        dbstopInAt(eval, unquotedFilename, position, errorMessage);
        return;
    }

    Breakpoint breakpoint;
    AbstractSyntaxTreePtr code = nullptr;
    size_t maxLinesInFile = 0;

    // Check for subfunction syntax: mainfunction>subfunction
    std::wstring mainFunctionName = functioOrFilename;
    std::wstring subFunctionName;
    size_t separatorPos = functioOrFilename.find(L'>');
    if (separatorPos != std::wstring::npos) {
        mainFunctionName = functioOrFilename.substr(0, separatorPos);
        subFunctionName = functioOrFilename.substr(separatorPos + 1);
    }

    FunctionDef* funcDef = nullptr;
    std::string asFunctionName = wstring_to_utf8(mainFunctionName);
    std::string asSubFunctionName = wstring_to_utf8(subFunctionName);

    if (eval->lookupFunction(asFunctionName, funcDef)) {
        if (funcDef->type() != NLS_MACRO_FUNCTION) {
            Error(_W("Breakpoints can only be set in macro functions."));
        }
        funcDef->updateCode();
        MacroFunctionDef* mFuncDef = static_cast<MacroFunctionDef*>(funcDef);

        // If subfunction is specified, find it in the nextFunction chain
        if (!subFunctionName.empty()) {
            MacroFunctionDef* targetFunc = nullptr;
            MacroFunctionDef* searchFunc = mFuncDef->nextFunction;
            while (searchFunc != nullptr) {
                if (searchFunc->getName() == asSubFunctionName) {
                    targetFunc = searchFunc;
                    break;
                }
                searchFunc = searchFunc->nextFunction;
            }
            if (targetFunc == nullptr) {
                errorMessage = _W("Cannot find subfunction '") + subFunctionName + L"' in '"
                    + mainFunctionName + L"'.";
                return;
            }
            code = targetFunc->code;
            breakpoint.filename = funcDef->getFilename();
            breakpoint.functionName = asSubFunctionName;
        } else {
            code = mFuncDef->code;
            breakpoint.filename = funcDef->getFilename();
            breakpoint.functionName = asFunctionName;
        }

        // Calculate max lines in file by traversing all functions in the file
        // (including local/subfunctions linked via nextFunction)
        maxLinesInFile = getMaxLineFromChain(mFuncDef->code);
        MacroFunctionDef* nextFunc = mFuncDef->nextFunction;
        while (nextFunc != nullptr) {
            size_t nextFuncMax = getMaxLineFromChain(nextFunc->code);
            if (nextFuncMax > maxLinesInFile) {
                maxLinesInFile = nextFuncMax;
            }
            nextFunc = nextFunc->nextFunction;
        }
    } else {
        ParserState state = ParseFile(eval, mainFunctionName);
        if (state != ScriptBlock) {
            errorMessage = _W("Cannot set breakpoint: unable to parse script file '")
                + mainFunctionName + L"'.";
            return;
        }
        code = getParsedScriptBlock();
        breakpoint.filename = mainFunctionName;
        maxLinesInFile = getMaxLineFromChain(code);
    }

    size_t adjustedLineNumber = getLineNumberFromCode(code, position, maxLinesInFile);

    if (adjustedLineNumber == 0) {
        Error(_W("Setting or clearing breakpoints past the start of the last expression is not "
                 "supported."));
    }
    breakpoint.line = adjustedLineNumber;
    breakpoint.maxLines = getMaxLineFromChain(code);

    eval->addBreakpoint(breakpoint);
}
//=============================================================================
// Helper function to get the line from context value
size_t
getLineFromContext(int contextValue)
{
    size_t linePosition = static_cast<size_t>(contextValue & 0x0000FFFF);
    if (linePosition == 0) {
        if (contextValue == 0) {
            linePosition = 1;
        } else {
            linePosition = static_cast<size_t>(contextValue);
        }
    }
    return linePosition;
}
//=============================================================================
// Helper function to recursively find the maximum line number in a subtree
// Only traverses DOWN to stay within the expression, not RIGHT
size_t
getMaxLineInSubtreeDown(AbstractSyntaxTreePtr t)
{
    if (t == nullptr) {
        return 0;
    }

    size_t maxLine = getLineFromContext(t->getContext());

    // Only traverse down child, not right siblings
    if (t->down != nullptr) {
        size_t downMax = getMaxLineInSubtreeDown(t->down);
        if (downMax > maxLine) {
            maxLine = downMax;
        }
    }

    return maxLine;
}
//=============================================================================
// Helper function to get max line from a node and ALL its children (down+right chain)
size_t
getMaxLineFromChain(AbstractSyntaxTreePtr t)
{
    if (t == nullptr) {
        return 0;
    }

    size_t maxLine = getLineFromContext(t->getContext());

    // Get max from the down subtree
    if (t->down != nullptr) {
        size_t downMax = getMaxLineFromChain(t->down);
        if (downMax > maxLine) {
            maxLine = downMax;
        }
    }

    // Get max from right siblings at this level
    if (t->right != nullptr) {
        size_t rightMax = getMaxLineFromChain(t->right);
        if (rightMax > maxLine) {
            maxLine = rightMax;
        }
    }

    return maxLine;
}
//=============================================================================
size_t
getLineNumberFromCode(AbstractSyntaxTreePtr code, size_t lineNumber, size_t maxLinesInFile)
{
    if (code == nullptr || lineNumber == 0) {
        return 0;
    }

    // Start from the first statement
    AbstractSyntaxTreePtr current = code;
    if (code->opNum == OP_BLOCK && code->down != nullptr) {
        current = code->down;
    }

    // Collect all statements and their line ranges, in order
    std::vector<std::pair<size_t, size_t>> statementRanges; // (startLine, endLine)
    AbstractSyntaxTreePtr stmt = current;
    while (stmt != nullptr) {
        size_t startLine = getLineFromContext(stmt->getContext());

        // Sometimes the statement node itself has the end line, not the start line
        // Check if down child has an earlier line
        if (stmt->down != nullptr) {
            size_t downStartLine = getLineFromContext(stmt->down->getContext());
            if (downStartLine > 0 && downStartLine < startLine) {
                startLine = downStartLine;
            }
        }

        // Calculate end line: traverse only the down subtree of this statement
        // Don't traverse stmt->right as that's the next statement
        size_t endLine = startLine;
        if (stmt->down != nullptr) {
            // Get max line from the entire down chain (including right siblings within the
            // expression)
            size_t downMax = getMaxLineFromChain(stmt->down);
            if (downMax > endLine) {
                endLine = downMax;
            }
        }

        // Also check the statement node itself for endLine
        size_t stmtLine = getLineFromContext(stmt->getContext());
        if (stmtLine > endLine) {
            endLine = stmtLine;
        }

        if (startLine > 0) {
            statementRanges.push_back(std::make_pair(startLine, endLine));
        }

        // Move to next statement
        stmt = stmt->right;
    }

    if (statementRanges.empty()) {
        return 0;
    }

    // Find which statement contains the requested line
    for (size_t i = 0; i < statementRanges.size(); ++i) {
        size_t stmtStart = statementRanges[i].first;
        size_t stmtEnd = statementRanges[i].second;

        // Check if the requested line falls within this statement
        if (lineNumber >= stmtStart && lineNumber <= stmtEnd) {
            // Check if this is the start line of the statement
            if (lineNumber == stmtStart) {
                // Breakpoint on the first line of the statement - keep it there
                return stmtStart;
            }

            // Breakpoint is on a line within a multi-line statement (not the first line)
            // Adapt it to the next statement
            if (i + 1 < statementRanges.size()) {
                // Return the start of the next statement
                return statementRanges[i + 1].first;
            }

            // Line is within the last statement but not on its start - return the statement start
            return stmtStart;
        }
    }

    // Line is not within any statement - find the next executable line
    for (const auto& range : statementRanges) {
        if (range.first > lineNumber) {
            return range.first;
        }
    }

    // Line is after all statements in this function/script
    // Check if the line is still within the file bounds (for files with multiple functions)
    if (lineNumber <= maxLinesInFile) {
        // Return the last statement's start line - this allows setting breakpoints
        // on lines between functions or after the function ends but within the file
        return statementRanges.back().first;
    }

    // Line is past the end of the file - invalid
    return 0;
}
//=============================================================================
