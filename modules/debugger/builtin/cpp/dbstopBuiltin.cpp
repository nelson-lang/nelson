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
#include "PathFunctionIndexerManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static size_t
parsePositionArgument(const ArrayOfVector& argIn);
static void
dbstopInAt(Evaluator* eval, const std::wstring& functioOrFilename, size_t position,
    std::wstring& errorMessage);
static void
applyBreakpointStruct(Evaluator* eval, const ArrayOf& bpStruct, std::wstring& errorMessage);
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstopBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dbstop in file
    // dbstop in file at location
    // dbstop(struct)

    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 0);
    std::wstring errorMessage;
    if (argIn.size() == 1) {
        applyBreakpointStruct(eval, argIn[0], errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        return {};
    }

    if (argIn.size() == 3) {
        Error("Wrong number of input arguments.");
    }

    if (argIn[0].getContentAsWideString() != L"in") {
        Error("Second argument must be 'in'.");
    }

    size_t position = parsePositionArgument(argIn);
    std::wstring target = argIn[1].getContentAsWideString();

    dbstopInAt(eval, target, position, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }

    return {};
}
//=============================================================================
size_t
parsePositionArgument(const ArrayOfVector& argIn)
{
    if (argIn.size() != 4) {
        return 1;
    }

    if (argIn[2].getContentAsWideString() != L"at") {
        Error(_W("Third argument must be 'at'."));
    }

    std::wstring posArg = argIn[3].getContentAsWideString();
    size_t idx = 0;

    try {
        size_t pos = std::stoul(posArg, &idx);
        if (idx != posArg.size()) {
            Error(_W("Invalid position argument."));
        }
        return pos;
    } catch (...) {
        Error(_W("Invalid position argument."));
    }

    return 1; // unreachable
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
        FunctionDef* funcDef = nullptr;
        std::string asFunctionName = wstring_to_utf8(mainFunctionName);
        if (eval->lookupFunction(asFunctionName, funcDef)) {
            // Found as a function - but check if it's a script-like macro function
            breakpoint.filename = funcDef->getFilename();

            // For now, treat all found functions as scripts (don't store function name)
            // because they execute in the base/calling context
            breakpoint.functionName = "";
        } else {
            // It's a script file name - try to resolve it using PathFunctionIndexerManager
            std::wstring resolvedFilename;
            if (PathFunctionIndexerManager::getInstance()->find(asFunctionName, resolvedFilename)) {
                // File found in path
                breakpoint.filename = resolvedFilename;
            } else {
                // File not found in path, use the name as-is (it might be a full path)
                breakpoint.filename = mainFunctionName;
            }
            // Scripts execute in the global scope, so leave functionName empty
            breakpoint.functionName = "";
        }
    }

    // Use Evaluator's line adjustment method
    size_t adjustedLineNumber;
    std::wstring adjustError;
    // For subfunctions, use the full syntax (mainfunction>subfunction) to get correct line numbers
    std::wstring functionNameForLineAdjustment = functioOrFilename;
    if (!eval->adjustBreakpointLine(
            functionNameForLineAdjustment, position, adjustedLineNumber, adjustError)) {
        errorMessage = adjustError;
        return;
    }

    breakpoint.line = adjustedLineNumber;
    eval->addBreakpoint(breakpoint);
}
//=============================================================================
void
applyBreakpointStruct(Evaluator* eval, const ArrayOf& bpStruct, std::wstring& errorMessage)
{
    if (!bpStruct.isStruct()) {
        errorMessage = _W("Argument must be breakpoint struct.");
        return;
    }

    stringVector names = bpStruct.getFieldNames();
    if (names.size() != 3 || names[0] != "name" || names[1] != "file" || names[2] != "line") {
        errorMessage = _W("Invalid breakpoint struct.");
        return;
    }

    ArrayOfVector files = bpStruct.getFieldAsList("file");
    ArrayOfVector lines = bpStruct.getFieldAsList("line");

    size_t count = bpStruct.getDimensions().getElementCount();
    for (size_t i = 0; i < count; ++i) {
        ArrayOf lineArray = lines[i];
        if (!lineArray.isNumeric() || !lineArray.isRowVector()) {
            errorMessage = _W("Invalid line number in breakpoint struct.");
            return;
        }

        std::vector<size_t> lines = lineArray.getContentAsIndexVector();
        for (auto line : lines) {
            dbstopInAt(eval, files[i].getContentAsWideString(), line, errorMessage);
            if (!errorMessage.empty()) {
                return;
            }
        }
    }
}
//=============================================================================
