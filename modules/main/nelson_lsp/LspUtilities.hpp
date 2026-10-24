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
#include "CodeAnalyzer.hpp"
#include <filesystem>
#include <map>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
std::string
optionalString(const nlohmann::json& value, const std::string& key);
//=============================================================================
std::string
pathToUri(const std::filesystem::path& path);
//=============================================================================
std::filesystem::path
pathFromUri(const std::string& uri);
//=============================================================================
void
applyDocumentChange(std::string& text, const nlohmann::json& change);
//=============================================================================
nlohmann::json
lspRange(int lineStart, int columnStart, int lineEnd, int columnEnd);
//=============================================================================
nlohmann::json
lspDiagnostic(const Nelson::CodeAnalyzerDiagnostic& diagnostic);
//=============================================================================
nlohmann::json
documentDiagnosticReport(const std::string& uri, int version,
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics,
    const std::string& previousResultId = "");
//=============================================================================
std::map<std::string, std::string>
previousWorkspaceResultIds(const nlohmann::json& params);
//=============================================================================
void
addDiagnosticsByUri(std::map<std::string, std::vector<Nelson::CodeAnalyzerDiagnostic>>& byUri,
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics);
//=============================================================================
nlohmann::json
fullDocumentRange(const std::string& text);
//=============================================================================
bool
codeAnalyzerFixConflictsWith(
    const CodeAnalyzerFix& fix, const std::vector<CodeAnalyzerTextEdit>& acceptedEdits);
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
