//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerAst.hpp"
#include "CodeAnalyzerAstUtilities.hpp"
#include "CodeAnalyzerLineMarkers.hpp"
#include <cctype>
#include <set>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    using namespace CodeAnalyzerAst;
    //=============================================================================
    bool
    isCallableToken(const std::string& line, size_t end)
    {
        size_t cursor = end;
        while (cursor < line.size() && std::isspace(static_cast<unsigned char>(line[cursor]))) {
            ++cursor;
        }
        return cursor < line.size() && line[cursor] == '(';
    }
    //=============================================================================
    int
    callableTokenColumnInLine(const std::string& line, const std::string& name)
    {
        for (size_t k = 0; k < line.size();) {
            if (!isIdentifierStart(line[k])) {
                ++k;
                continue;
            }
            const size_t first = k++;
            while (k < line.size() && isIdentifierChar(line[k])) {
                ++k;
            }
            if (line.substr(first, k - first) == name && isCallableToken(line, k)) {
                return static_cast<int>(first + 1);
            }
        }
        return 1;
    }
    //=============================================================================
    stringVector
    usedTokensIn(const std::string& line)
    {
        const std::set<std::string>& reserved = reservedWords();
        stringVector tokens;
        for (size_t k = 0; k < line.size();) {
            if (!isIdentifierStart(line[k])) {
                ++k;
                continue;
            }
            const size_t first = k++;
            while (k < line.size() && isIdentifierChar(line[k])) {
                ++k;
            }
            if ((first > 0 && (line[first - 1] == '.' || line[first - 1] == '@'))
                || isCallableToken(line, k)) {
                continue;
            }
            std::string token = line.substr(first, k - first);
            if (reserved.find(toLowerAscii(token)) == reserved.end()) {
                tokens.push_back(token);
            }
        }
        return tokens;
    }
    //=============================================================================
    void
    parseStorageDeclarationLine(const std::string& line, stringVector& globalDeclarations,
        stringVector& persistentDeclarations)
    {
        const std::string trimmed = trim(line);
        const std::string lower = toLowerAscii(trimmed);
        const bool global = startsWithWord(lower, "global");
        const bool persistent = startsWithWord(lower, "persistent");
        if (!global && !persistent) {
            return;
        }
        size_t cursor = global ? 6 : 10;
        while (
            cursor < trimmed.size() && std::isspace(static_cast<unsigned char>(trimmed[cursor]))) {
            ++cursor;
        }
        if (cursor < trimmed.size() && trimmed[cursor] == '(') {
            return;
        }
        stringVector& declarations = global ? globalDeclarations : persistentDeclarations;
        while (cursor < trimmed.size()) {
            if (!isIdentifierStart(trimmed[cursor])) {
                ++cursor;
                continue;
            }
            const size_t first = cursor++;
            while (cursor < trimmed.size() && isIdentifierChar(trimmed[cursor])) {
                ++cursor;
            }
            declarations.push_back(trimmed.substr(first, cursor - first));
        }
    }
    //=============================================================================
    bool
    parseArgumentsBlockDeclaration(
        const std::string& line, stringVector& declarations, stringVector& validationUsed)
    {
        std::string trimmed = trim(line);
        std::string lower = toLowerAscii(trimmed);
        if (trimmed.empty() || startsWithWord(lower, "arguments") || startsWithWord(lower, "end")
            || !isIdentifierStart(trimmed[0])) {
            return false;
        }
        size_t cursor = 1;
        while (cursor < trimmed.size() && isIdentifierChar(trimmed[cursor])) {
            ++cursor;
        }
        declarations.push_back(trimmed.substr(0, cursor));
        validationUsed = usedTokensIn(trimmed.substr(cursor));
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
CodeAnalyzerLineUsage
collectCodeAnalyzerLineUsage(const stringVector& lines, int lineNumber)
{
    CodeAnalyzerLineUsage usage;
    if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
        return usage;
    }
    usage.line = lineNumber;
    std::string code = codeWithoutStringsAndComments(lines[lineNumber - 1]);
    CodeAnalyzerLineMarkers markers = collectCodeAnalyzerLineMarkers(lines, lineNumber);
    usage.importLine = markers.importLine;
    usage.codeLine = markers.codeLine;
    usage.flowBoundary = markers.flowBoundary;
    usage.flowBlockStart = markers.flowBlockStart;
    usage.startsArgumentsBlock = markers.startsArgumentsBlock;
    usage.endsBlock = markers.endsBlock;
    usage.functionDeclaration = markers.functionDeclaration;
    parseStorageDeclarationLine(code, usage.globalDeclarations, usage.persistentDeclarations);
    parseArgumentsBlockDeclaration(code, usage.argumentDeclarations, usage.argumentValidationUsed);
    return usage;
}
//=============================================================================
int
codeAnalyzerWordColumnInLine(const std::string& line, const std::string& word)
{
    for (size_t k = 0; k < line.size();) {
        if (!isIdentifierStart(line[k])) {
            ++k;
            continue;
        }
        const size_t first = k++;
        while (k < line.size() && isIdentifierChar(line[k])) {
            ++k;
        }
        if (line.substr(first, k - first) == word) {
            return static_cast<int>(first + 1);
        }
    }
    return 1;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
