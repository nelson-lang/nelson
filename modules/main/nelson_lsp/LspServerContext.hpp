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
#include "CodeAnalyzer.hpp"
#include "LspDocumentModel.hpp"
#include "nlohmann/json.hpp"
#include <filesystem>
#include <map>
#include <set>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
using json = nlohmann::json;
//=============================================================================
struct OpenDocument
{
    std::string uri;
    std::string text;
    int version = 0;
    std::vector<Nelson::CodeAnalyzerDiagnostic> diagnostics;
    LspDocumentModel model;
};
//=============================================================================
struct LspIndexedFile
{
    std::string uri;
    std::filesystem::path path;
    uintmax_t size = 0;
    int64_t mtime = 0;
    uint64_t hash = 0;
    LspDocumentModel model;
};
//=============================================================================
struct LspWorkspaceIndex
{
    std::string rootUri;
    std::filesystem::path rootPath;
    uint64_t configHash = 0;
    std::map<std::string, LspIndexedFile> files;
    bool ready = false;
};
//=============================================================================
struct LspServerContext
{
    std::map<std::string, OpenDocument> documents;
    std::vector<std::string> workspaceRootUris;
    std::set<std::string> cancelledRequestIds;
    json lspLintSettings = json::object();
    bool shouldExit = false;
    std::map<std::string, LspWorkspaceIndex> workspaceIndexes;
};
//=============================================================================
extern LspServerContext lspServerContext;
extern std::map<std::string, OpenDocument>& documents;
extern std::vector<std::string>& workspaceRootUris;
extern std::set<std::string>& cancelledRequestIds;
extern json& lspLintSettings;
extern bool& shouldExit;
extern std::map<std::string, LspWorkspaceIndex>& workspaceIndexes;
//=============================================================================
void
sendNotification(const std::string& method, const json& params);
//=============================================================================
void
sendWorkDoneProgress(const json& token, const std::string& kind, const std::string& message);
//=============================================================================
std::wstring
filenameFromUri(const std::string& uri);
//=============================================================================
std::string
normalizedPathKey(const std::filesystem::path& path);
//=============================================================================
bool
pathIsInsideOrEqual(const std::filesystem::path& path, const std::filesystem::path& root);
//=============================================================================
std::filesystem::path
workspaceRootForPath(const std::filesystem::path& path);
//=============================================================================
std::filesystem::path
workspaceConfigForRoot(const std::filesystem::path& root);
//=============================================================================
Nelson::CodeAnalyzerOptions
analyzerOptionsForPath(const std::filesystem::path& path);
//=============================================================================
void
analyzeDocument(OpenDocument& document);
//=============================================================================
json
workspaceDiagnosticReportWithPartialProgress(const json& params);
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
