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
#include "nlohmann/json.hpp"
#include <filesystem>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
struct LspModelSymbol
{
    std::string name;
    std::string detail;
    std::vector<std::string> parameters;
    int kind = 12;
    int line = 1;
    int column = 1;
    int endLine = 1;
    int endColumn = 1;
};
//=============================================================================
struct LspDocumentModel
{
    std::string uri;
    std::string text;
    std::vector<std::string> lines;
    std::vector<LspModelSymbol> symbols;
    std::vector<LspModelSymbol> variables;
    std::vector<LspModelSymbol> imports;
};
//=============================================================================
LspDocumentModel
buildLspDocumentModel(const std::string& uri, const std::string& text);
//=============================================================================
std::string
lspWordAt(const LspDocumentModel& model, int zeroBasedLine, int zeroBasedCharacter);
//=============================================================================
std::string
lspPrefixAt(const LspDocumentModel& model, int zeroBasedLine, int zeroBasedCharacter);
//=============================================================================
nlohmann::json
lspLocation(const std::string& uri, const LspModelSymbol& symbol);
//=============================================================================
nlohmann::json
lspDocumentSymbols(const LspDocumentModel& model);
//=============================================================================
nlohmann::json
lspCompletionItems(const LspDocumentModel& model, const std::string& prefix);
//=============================================================================
nlohmann::json
lspReferences(const LspDocumentModel& model, const std::string& name);
//=============================================================================
nlohmann::json
lspDocumentHighlights(const LspDocumentModel& model, const std::string& name);
//=============================================================================
nlohmann::json
lspFoldingRanges(const LspDocumentModel& model);
//=============================================================================
nlohmann::json
lspSelectionRanges(const LspDocumentModel& model, int zeroBasedLine, int zeroBasedCharacter);
//=============================================================================
nlohmann::json
lspSemanticTokens(const LspDocumentModel& model);
//=============================================================================
nlohmann::json
lspDocumentLinks(const LspDocumentModel& model);
//=============================================================================
nlohmann::json
lspCodeLens(const LspDocumentModel& model);
//=============================================================================
nlohmann::json
lspSignatureHelp(const LspDocumentModel& model, int zeroBasedLine, int zeroBasedCharacter);
//=============================================================================
const std::vector<std::string>&
lspKeywords();
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
