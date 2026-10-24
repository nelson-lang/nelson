//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerLineMarkers.hpp"
#include "CodeAnalyzerAstUtilities.hpp"
#include <cctype>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    using namespace CodeAnalyzerAst;
    //=============================================================================
    bool
    isImportCommand(const std::string& line)
    {
        const std::string lower = toLowerAscii(trim(line));
        if (!startsWithWord(lower, "import")) {
            return false;
        }
        size_t cursor = 6;
        while (cursor < lower.size() && std::isspace(static_cast<unsigned char>(lower[cursor]))) {
            ++cursor;
        }
        return cursor == lower.size() || lower[cursor] != '(';
    }
    //=============================================================================
    bool
    isFlowBoundary(const std::string& line)
    {
        return startsWithWord(line, "end") || startsWithWord(line, "else")
            || startsWithWord(line, "elseif") || startsWithWord(line, "case")
            || startsWithWord(line, "otherwise") || startsWithWord(line, "catch");
    }
    //=============================================================================
    bool
    isFlowBlockStartFallback(const std::string& line)
    {
        return startsWithWord(line, "if") || startsWithWord(line, "for")
            || startsWithWord(line, "while") || startsWithWord(line, "switch")
            || startsWithWord(line, "try");
    }
    //=============================================================================
} // namespace
//=============================================================================
CodeAnalyzerLineMarkers
collectCodeAnalyzerLineMarkers(const stringVector& lines, int lineNumber)
{
    CodeAnalyzerLineMarkers markers;
    if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
        return markers;
    }
    const std::string code = codeWithoutStringsAndComments(lines[lineNumber - 1]);
    const std::string lower = toLowerAscii(trim(code));
    markers.importLine = isImportCommand(code);
    markers.codeLine = !lower.empty();
    markers.flowBoundary = isFlowBoundary(lower);
    markers.flowBlockStart = isFlowBlockStartFallback(lower);
    markers.startsArgumentsBlock = startsWithWord(lower, "arguments");
    markers.endsBlock = startsWithWord(lower, "end");
    markers.functionDeclaration = startsWithWord(lower, "function");
    return markers;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
