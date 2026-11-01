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
#include <cstdio>
#include <cwctype>
#include <algorithm>
#include "BreakpointTarget.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "MacroFunctionDef.hpp"
#include "StringHelpers.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "FileSystemWrapper.hpp"
#include "ClassdefParser.hpp"
//=============================================================================
namespace Nelson::DebuggerGateway {
//=============================================================================
static size_t
firstExecutableLineInFunction(const std::wstring& filename, const std::string& functionName);
static std::string
functionOwningLine(const std::wstring& filename, size_t line);
//=============================================================================
static std::wstring
stripQuotes(const std::wstring& name)
{
    bool isSimpleQuoted
        = StringHelpers::starts_with(name, L"'") && StringHelpers::ends_with(name, L"'");
    bool isDoubleQuoted
        = StringHelpers::starts_with(name, L"\"") && StringHelpers::ends_with(name, L"\"");
    if ((isSimpleQuoted || isDoubleQuoted) && name.size() >= 2) {
        return stripQuotes(name.substr(1, name.size() - 2));
    }
    return name;
}
//=============================================================================
bool
resolveBreakpointTarget(Evaluator* eval, const std::wstring& nameOrFile, BreakpointTarget& target,
    std::wstring& errorMessage)
{
    errorMessage.clear();
    target = BreakpointTarget();
    std::wstring name = stripQuotes(nameOrFile);
    std::wstring mainFunctionName = name;
    std::wstring subFunctionName;
    size_t separatorPos = name.find(L'>');
    if (separatorPos != std::wstring::npos) {
        mainFunctionName = name.substr(0, separatorPos);
        subFunctionName = name.substr(separatorPos + 1);
    }
    target.adjustName = name;
    std::string asFunctionName = wstring_to_utf8(mainFunctionName);
    if (!subFunctionName.empty()) {
        FunctionDef* funcDef = nullptr;
        if (!eval->lookupFunction(asFunctionName, funcDef)) {
            errorMessage
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:dbstop:functionNotFound",
                                        L"Cannot find function \"%s\"."),
                    mainFunctionName);
            return false;
        }
        if (funcDef->type() != NLS_MACRO_FUNCTION) {
            errorMessage = resolveMessageTemplate(L"Nelson:dbstop:breakpointNotInMacro",
                L"Breakpoints can only be set in macro functions.");
            return false;
        }
        funcDef->updateCode();
        auto* mFuncDef = static_cast<MacroFunctionDef*>(funcDef);
        std::string asSubFunctionName = wstring_to_utf8(subFunctionName);
        MacroFunctionDef* searchFunc = mFuncDef->nextFunction;
        bool found = false;
        while (searchFunc != nullptr) {
            if (searchFunc->getName() == asSubFunctionName) {
                found = true;
                break;
            }
            searchFunc = searchFunc->nextFunction;
        }
        if (!found) {
            errorMessage
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:dbstop:subfunctionNotFound",
                                        L"Cannot find subfunction '%s' in '%s'."),
                    subFunctionName, mainFunctionName);
            return false;
        }
        target.filename = funcDef->getFilename();
        target.functionName = asSubFunctionName;
        target.isLocalFunction = true;
        return true;
    }
    FunctionDef* funcDef = nullptr;
    if (eval->lookupFunction(asFunctionName, funcDef)) {
        target.filename = funcDef->getFilename();
        // A classdef file holds several method bodies: match on the file only.
        if (classdefIsClassdefFile(target.filename)) {
            target.functionName = "";
        } else {
            target.functionName = asFunctionName;
        }
        return true;
    }
    std::wstring resolvedFilename;
    if (PathFunctionIndexerManager::getInstance()->find(asFunctionName, resolvedFilename)) {
        target.filename = resolvedFilename;
    } else if (FileSystemWrapper::Path::is_regular_file(mainFunctionName)) {
        // not on the path: the full path of a script file
        target.filename = mainFunctionName;
    } else {
        errorMessage = formatI18nMessage(resolveMessageTemplate(L"Nelson:dbstop:functionNotFound",
                                             L"Cannot find function \"%s\"."),
            mainFunctionName);
        return false;
    }
    // scripts run in the caller scope: no owning function
    target.functionName = "";
    return true;
}
//=============================================================================
bool
setBreakpointAt(Evaluator* eval, const std::wstring& nameOrFile, size_t position,
    bool explicitPosition, size_t& adjustedLine, bool& created, std::wstring& errorMessage)
{
    adjustedLine = 0;
    created = false;
    BreakpointTarget target;
    if (!resolveBreakpointTarget(eval, nameOrFile, target, errorMessage)) {
        return false;
    }
    Breakpoint breakpoint;
    breakpoint.filename = target.filename;
    breakpoint.functionName = target.functionName;
    if (target.isLocalFunction && !explicitPosition) {
        size_t firstExecutable
            = firstExecutableLineInFunction(target.filename, target.functionName);
        if (firstExecutable > 0) {
            breakpoint.line = firstExecutable;
            created = eval->findBreakpoint(breakpoint.filename, breakpoint.line) == nullptr;
            eval->addBreakpoint(breakpoint);
            adjustedLine = breakpoint.line;
            return true;
        }
    }
    std::wstring adjustError;
    if (!eval->adjustBreakpointLine(target.adjustName, position, adjustedLine, adjustError)) {
        errorMessage = adjustError;
        return false;
    }
    breakpoint.line = adjustedLine;
    // "dbstop in F at N" inside a local function is named F>local
    if (!target.isLocalFunction && !target.functionName.empty()) {
        std::string owner = functionOwningLine(target.filename, adjustedLine);
        if (!owner.empty()) {
            breakpoint.functionName = owner;
        }
    }
    created = eval->findBreakpoint(breakpoint.filename, breakpoint.line) == nullptr;
    eval->addBreakpoint(breakpoint);
    return true;
}
//=============================================================================
wstringVector
breakpointCommandTokens(const ArrayOfVector& argIn)
{
    wstringVector tokens;
    tokens.reserve(argIn.size());
    for (const ArrayOf& arg : argIn) {
        if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
            tokens.push_back(arg.asWideString());
        } else if (arg.isNumeric() && arg.isScalar()) {
            tokens.push_back(std::to_wstring(static_cast<long long>(arg.asDoubleScalar())));
        } else {
            Error(L"Nelson:debugger:invalidArgumentType",
                L"Invalid argument type: character vector or string scalar expected.");
        }
    }
    return tokens;
}
//=============================================================================
bool
parseBreakpointLineToken(const std::wstring& token, size_t& line)
{
    line = 0;
    if (token.empty()) {
        return false;
    }
    for (wchar_t ch : token) {
        if (!std::iswdigit(ch)) {
            return false;
        }
    }
    try {
        line = static_cast<size_t>(std::stoull(token));
    } catch (...) {
        return false;
    }
    return true;
}
//=============================================================================
bool
sameBreakpointFile(const std::wstring& path1, const std::wstring& path2)
{
    std::wstring a = path1;
    std::wstring b = path2;
    std::replace(a.begin(), a.end(), L'\\', L'/');
    std::replace(b.begin(), b.end(), L'\\', L'/');
#ifdef _MSC_VER
    if (a.size() != b.size()) {
        return false;
    }
    for (size_t k = 0; k < a.size(); ++k) {
        if (std::towlower(a[k]) != std::towlower(b[k])) {
            return false;
        }
    }
    return true;
#else
    return a == b;
#endif
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
//=============================================================================
// name of the function whose header is the last one before `line` in a code file
static std::string
functionOwningLine(const std::wstring& filename, size_t line)
{
    if (filename.empty() || line == 0) {
        return std::string();
    }
    FILE* fp = nullptr;
#ifdef _MSC_VER
    fp = _wfopen(filename.c_str(), L"rb");
#else
    fp = fopen(wstring_to_utf8(filename).c_str(), "rb");
#endif
    if (fp == nullptr) {
        return std::string();
    }
    std::string owner;
    size_t currentLine = 1;
    char buffer[4096];
    while (currentLine <= line && fgets(buffer, sizeof(buffer), fp) != nullptr) {
        std::string text(buffer);
        size_t first = text.find_first_not_of(" \t");
        if (first != std::string::npos && text.compare(first, 8, "function") == 0
            && (first + 8 >= text.size()
                || !isalnum(static_cast<unsigned char>(text[first + 8])))) {
            std::string header = text.substr(first + 8);
            size_t comment = header.find('%');
            if (comment != std::string::npos) {
                header = header.substr(0, comment);
            }
            size_t open = header.find('(');
            size_t equal = header.find('=');
            std::string nameArea = header;
            if (equal != std::string::npos && (open == std::string::npos || equal < open)) {
                nameArea = header.substr(equal + 1);
            }
            std::string name;
            for (char c : nameArea) {
                if (isalnum(static_cast<unsigned char>(c)) || c == '_') {
                    name.push_back(c);
                } else if (!name.empty()) {
                    break;
                }
            }
            owner = name;
        }
        currentLine++;
    }
    fclose(fp);
    return owner;
}
//=============================================================================
} // namespace Nelson::DebuggerGateway
//=============================================================================
