//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Types.hpp"
#include <set>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct CodeAnalyzerImportSpec
{
    std::string spec;
    std::string name;
    int lineStart = 0;
    int columnStart = 0;
    int lineEnd = 0;
    int columnEnd = 0;
    bool wildcard = false;
    bool hasRange = false;
};
//=============================================================================
bool
codeAnalyzerStartsWithImport(const std::string& line);
//=============================================================================
std::string
codeAnalyzerImportedSymbolName(const std::string& spec);
//=============================================================================
std::vector<CodeAnalyzerImportSpec>
collectCodeAnalyzerImportSpecs(const stringVector& lines, int lineNumber);
//=============================================================================
void
collectCodeAnalyzerImportSymbols(
    const std::string& line, std::set<std::string>& importedNames, bool& wildcardImport);
//=============================================================================
void
collectCodeAnalyzerImportSymbols(const stringVector& lines, int lineNumber,
    std::set<std::string>& importedNames, bool& wildcardImport);
//=============================================================================
bool
isSimpleCodeAnalyzerImportLineFor(const std::string& line, const std::string& symbol);
//=============================================================================
bool
codeAnalyzerImportRemovalEditFor(const std::string& line, const std::string& symbol,
    int& columnStart, int& columnEnd, std::string& replacementText);
//=============================================================================
bool
codeAnalyzerImportRemovalEditFor(const stringVector& lines, int lineNumber,
    const std::string& symbol, int& lineStart, int& columnStart, int& lineEnd, int& columnEnd,
    std::string& replacementText);
//=============================================================================
} // namespace Nelson
//=============================================================================
