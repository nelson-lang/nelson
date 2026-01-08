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
        try {
            position = static_cast<size_t>(std::stoul(posArg, &idx));
            if (idx != posArg.size()) {
                Error(_W("Invalid position argument."));
            }
        } catch (...) {
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

    // Check for subfunction syntax: mainfunction>subfunction
    std::wstring mainFunctionName = functioOrFilename;
    std::wstring subFunctionName;
    size_t separatorPos = functioOrFilename.find(L'>');
    if (separatorPos != std::wstring::npos) {
        mainFunctionName = functioOrFilename.substr(0, separatorPos);
        subFunctionName = functioOrFilename.substr(separatorPos + 1);
    }

    // Handle subfunction case
    if (!subFunctionName.empty()) {
        FunctionDef* funcDef = nullptr;
        std::string asFunctionName = wstring_to_utf8(mainFunctionName);
        std::string asSubFunctionName = wstring_to_utf8(subFunctionName);

        if (!eval->lookupFunction(asFunctionName, funcDef)) {
            errorMessage = _W("Cannot find function '") + mainFunctionName + L"'.";
            return;
        }

        if (funcDef->type() != NLS_MACRO_FUNCTION) {
            Error(_W("Breakpoints can only be set in macro functions."));
        }

        funcDef->updateCode();
        MacroFunctionDef* mFuncDef = static_cast<MacroFunctionDef*>(funcDef);

        // Find subfunction in the nextFunction chain
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

        breakpoint.filename = funcDef->getFilename();
        breakpoint.functionName = asSubFunctionName;
    } else {
        // Simple case: just function or file name
        breakpoint.filename = mainFunctionName;

        FunctionDef* funcDef = nullptr;
        std::string asFunctionName = wstring_to_utf8(mainFunctionName);
        if (eval->lookupFunction(asFunctionName, funcDef)) {
            breakpoint.functionName = asFunctionName;
        }
    }

    // Use Evaluator's line adjustment method
    size_t adjustedLineNumber;
    std::wstring adjustError;
    if (!eval->adjustBreakpointLine(mainFunctionName, position, adjustedLineNumber, adjustError)) {
        errorMessage = adjustError;
        return;
    }

    breakpoint.line = adjustedLineNumber;
    eval->addBreakpoint(breakpoint);
}
//=============================================================================
