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
#define _CRT_SECURE_NO_WARNINGS /* _wfopen */
#endif
//=============================================================================
#include "dbstopBuiltin.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include "ParseFile.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserInterface.hpp"
#include "StringHelpers.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "ClassdefParser.hpp"
#include "ErrorID.hpp"
#include <cstdio>
//=============================================================================
using namespace Nelson;
//=============================================================================
static size_t
parsePositionArgument(const ArrayOfVector& argIn);
static void
dbstopInAt(Evaluator* eval, const std::wstring& functionOrFilename, size_t position,
    bool explicitPosition, std::wstring& errorMessage);
static void
applyBreakpointStruct(Evaluator* eval, const ArrayOf& bpStruct, std::wstring& errorMessage);
static size_t
firstExecutableLineInFunction(const std::wstring& filename, const std::string& functionName);
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
            Error(L"Nelson:dbstop:invalidArgument", errorMessage);
        }
        return {};
    }

    if (argIn.size() == 3) {
        Error("Nelson:wrongNumberOfInputs", "Wrong number of input arguments.");
    }

    if (argIn[0].asWideString() != L"in") {
        Error("Nelson:dbstop:expectedIn", "Second argument must be 'in'.");
    }

    size_t position = parsePositionArgument(argIn);
    std::wstring target = argIn[1].asWideString();

    dbstopInAt(eval, target, position, argIn.size() == 4, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:dbstop:invalidArgument", errorMessage);
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

    if (argIn[2].asWideString() != L"at") {
        Error(L"Nelson:debugger:expectedAt", L"Third argument must be 'at'.");
    }

    std::wstring posArg = argIn[3].asWideString();
    size_t idx = 0;

    try {
        size_t pos = std::stoul(posArg, &idx);
        if (idx != posArg.size()) {
            Error(L"Nelson:debugger:invalidPosition", L"Invalid position argument.");
        }
        return pos;
    } catch (...) {
        Error(L"Nelson:debugger:invalidPosition", L"Invalid position argument.");
    }

    return 1; // unreachable
}
//=============================================================================
void
dbstopInAt(Evaluator* eval, const std::wstring& functionOrFilename, size_t position,
    bool explicitPosition, std::wstring& errorMessage)
{
    bool isSimpleQuoted = StringHelpers::starts_with(functionOrFilename, L"'")
        && StringHelpers::ends_with(functionOrFilename, L"'");
    bool isDoubleQuoted = StringHelpers::starts_with(functionOrFilename, L"\"")
        && StringHelpers::ends_with(functionOrFilename, L"\"");
    if (isSimpleQuoted || isDoubleQuoted) {
        std::wstring unquotedFilename = functionOrFilename.substr(1, functionOrFilename.size() - 2);
        dbstopInAt(eval, unquotedFilename, position, explicitPosition, errorMessage);
        return;
    }

    Breakpoint breakpoint;

    std::wstring mainFunctionName = functionOrFilename;
    std::wstring subFunctionName;
    size_t separatorPos = functionOrFilename.find(L'>');
    if (separatorPos != std::wstring::npos) {
        mainFunctionName = functionOrFilename.substr(0, separatorPos);
        subFunctionName = functionOrFilename.substr(separatorPos + 1);
    }

    if (!subFunctionName.empty()) {
        FunctionDef* funcDef = nullptr;
        std::string asFunctionName = wstring_to_utf8(mainFunctionName);
        std::string asSubFunctionName = wstring_to_utf8(subFunctionName);

        if (!eval->lookupFunction(asFunctionName, funcDef)) {
            errorMessage
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:dbstop:functionNotFound",
                                        L"Cannot find function '%s'."),
                    mainFunctionName);
            return;
        }

        if (funcDef->type() != NLS_MACRO_FUNCTION) {
            Error(L"Nelson:dbstop:breakpointNotInMacro",
                L"Breakpoints can only be set in macro functions.");
        }

        funcDef->updateCode();
        MacroFunctionDef* mFuncDef = static_cast<MacroFunctionDef*>(funcDef);

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
            errorMessage
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:dbstop:subfunctionNotFound",
                                        L"Cannot find subfunction '%s' in '%s'."),
                    subFunctionName, mainFunctionName);
            return;
        }

        breakpoint.filename = funcDef->getFilename();
        breakpoint.functionName = asSubFunctionName;
        if (!explicitPosition && targetFunc->code != nullptr) {
            const size_t firstExecutable
                = firstExecutableLineInFunction(funcDef->getFilename(), asSubFunctionName);
            if (firstExecutable > 0) {
                breakpoint.line = firstExecutable;
                eval->addBreakpoint(breakpoint);
                return;
            }
        }
    } else {
        FunctionDef* funcDef = nullptr;
        std::string asFunctionName = wstring_to_utf8(mainFunctionName);
        if (eval->lookupFunction(asFunctionName, funcDef)) {
            breakpoint.filename = funcDef->getFilename();
            // A classdef file can contain multiple method bodies. A file/line breakpoint in the
            // class file should match whichever class method owns that source line.
            if (classdefIsClassdefFile(breakpoint.filename)) {
                breakpoint.functionName = "";
            } else {
                breakpoint.functionName = asFunctionName;
            }
        } else {
            std::wstring resolvedFilename;
            if (PathFunctionIndexerManager::getInstance()->find(asFunctionName, resolvedFilename)) {
                breakpoint.filename = resolvedFilename;
            } else {
                // not in the path: keep the name, it may be a full path
                breakpoint.filename = mainFunctionName;
            }
            // Scripts execute in the global scope, so leave functionName empty
            breakpoint.functionName = "";
        }
    }

    size_t adjustedLineNumber;
    std::wstring adjustError;
    // For subfunctions, use the full syntax (mainfunction>subfunction) to get correct line numbers
    std::wstring functionNameForLineAdjustment = functionOrFilename;
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
        errorMessage = resolveMessageTemplate(
            L"Nelson:dbstop:notBreakpointStruct", L"Argument must be breakpoint struct.");
        return;
    }

    stringVector names = bpStruct.fieldNames();
    if (names.size() != 3 || names[0] != "name" || names[1] != "file" || names[2] != "line") {
        errorMessage = resolveMessageTemplate(
            L"Nelson:dbstop:invalidBreakpointStruct", L"Invalid breakpoint struct.");
        return;
    }

    ArrayOfVector files = bpStruct.fieldAsList("file");
    ArrayOfVector lines = bpStruct.fieldAsList("line");

    size_t count = bpStruct.dimensions().elementCount();
    for (size_t i = 0; i < count; ++i) {
        ArrayOf lineArray = lines[i];
        if (!lineArray.isNumeric() || !lineArray.isRowVector()) {
            errorMessage = resolveMessageTemplate(
                L"Nelson:dbstop:invalidLineNumber", L"Invalid line number in breakpoint struct.");
            return;
        }

        for (auto line : lineArray.asIndexVector()) {
            dbstopInAt(eval, files[i].asWideString(), (size_t)line, true, errorMessage);
            if (!errorMessage.empty()) {
                return;
            }
        }
    }
}
//=============================================================================
size_t
firstExecutableLineInFunction(const std::wstring& filename, const std::string& functionName)
{
    if (filename.empty() || functionName.empty()) {
        return 0;
    }
    FILE* fp = nullptr;
#ifdef _MSC_VER
    fp = _wfopen(filename.c_str(), L"rb");
#else
    fp = fopen(wstring_to_utf8(filename).c_str(), "rb");
#endif
    if (fp == nullptr) {
        return 0;
    }
    size_t currentLine = 1;
    bool inTargetFunction = false;
    char buffer[4096];
    while (fgets(buffer, sizeof(buffer), fp) != nullptr) {
        std::string line(buffer);
        while (!line.empty() && (line.back() == '\n' || line.back() == '\r')) {
            line.pop_back();
        }
        size_t first = 0;
        while (first < line.size() && (line[first] == ' ' || line[first] == '\t')) {
            first++;
        }
        if (first < line.size() && line[first] != '%') {
            std::string trimmed = line.substr(first);
            if (!inTargetFunction) {
                if (StringHelpers::starts_with(trimmed, "function")
                    && trimmed.find(functionName) != std::string::npos) {
                    inTargetFunction = true;
                }
            } else {
                fclose(fp);
                return currentLine;
            }
        }
        currentLine++;
    }
    fclose(fp);
    return 0;
}
//=============================================================================
