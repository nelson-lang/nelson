//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <regex>
#include <vector>
#include "EditorBreakpoints.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<std::string>
readSourceLines(const std::wstring& filename)
{
    std::vector<std::string> lines;
#ifdef _MSC_VER
    std::ifstream stream(filename, std::ios::binary);
#else
    std::ifstream stream(wstring_to_utf8(filename), std::ios::binary);
#endif
    if (!stream.is_open()) {
        return lines;
    }
    std::string current;
    while (std::getline(stream, current)) {
        while (!current.empty() && (current.back() == '\r' || current.back() == '\n')) {
            current.pop_back();
        }
        lines.push_back(current);
    }
    return lines;
}
//=============================================================================
std::string
breakpointFunctionNameAtLine(const std::wstring& filename, size_t line)
{
    std::vector<std::string> lines = readSourceLines(filename);
    if (line == 0 || line > lines.size()) {
        return "";
    }
    static const std::regex functionPattern(
        R"(^\s*function\s+(?:\[.*?\]\s*=\s*|\w+\s*=\s*)?(\w+))");
    for (size_t k = line; k > 0; --k) {
        std::smatch match;
        if (std::regex_search(lines[k - 1], match, functionPattern)) {
            return match[1].str();
        }
    }
    return "";
}
//=============================================================================
size_t
addEditorBreakpoint(
    Evaluator* eval, const std::wstring& filename, size_t line, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (eval == nullptr || filename.empty() || line == 0) {
        return 0;
    }
    size_t adjustedLine = line;
    if (!eval->adjustBreakpointLine(filename, line, adjustedLine, errorMessage)) {
        return 0;
    }
    Breakpoint bp;
    bp.filename = filename;
    bp.line = adjustedLine;
    bp.enabled = true;
    bp.functionName = breakpointFunctionNameAtLine(filename, adjustedLine);
    eval->addBreakpoint(bp);
    return adjustedLine;
}
//=============================================================================
size_t
toggleEditorBreakpoint(Evaluator* eval, const std::wstring& filename, size_t line, bool& added,
    std::wstring& errorMessage)
{
    added = false;
    errorMessage.clear();
    if (eval == nullptr || filename.empty() || line == 0) {
        return 0;
    }
    // a click on an existing marker removes it, whatever the adjustment says
    if (eval->findBreakpoint(filename, line) != nullptr) {
        eval->removeBreakpoint(filename, line);
        return line;
    }
    size_t adjustedLine = line;
    if (!eval->adjustBreakpointLine(filename, line, adjustedLine, errorMessage)) {
        return 0;
    }
    if (eval->findBreakpoint(filename, adjustedLine) != nullptr) {
        eval->removeBreakpoint(filename, adjustedLine);
        return adjustedLine;
    }
    Breakpoint bp;
    bp.filename = filename;
    bp.line = adjustedLine;
    bp.enabled = true;
    bp.functionName = breakpointFunctionNameAtLine(filename, adjustedLine);
    eval->addBreakpoint(bp);
    added = true;
    return adjustedLine;
}
//=============================================================================
bool
runToEditorLine(
    Evaluator* eval, const std::wstring& filename, size_t line, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (eval == nullptr || filename.empty() || line == 0) {
        return false;
    }
    if (eval->addRunToLineBreakpoint(filename, line, errorMessage) == 0) {
        return false;
    }
    if (eval->isBreakpointActive()) {
        setNextDebugStopQuiet(true);
        eval->addCommandToQueue(L"dbcont", true);
        return true;
    }
    std::wstring quoted = filename;
    StringHelpers::replace_all(quoted, L"'", L"''");
    setNextDebugStopQuiet(true);
    eval->addCommandToQueue(L"run('" + quoted + L"');", true);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
