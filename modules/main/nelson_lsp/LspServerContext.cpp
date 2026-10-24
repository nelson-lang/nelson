//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LspServerContext.hpp"
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
LspServerContext lspServerContext;
std::map<std::string, OpenDocument>& documents = lspServerContext.documents;
std::vector<std::string>& workspaceRootUris = lspServerContext.workspaceRootUris;
std::set<std::string>& cancelledRequestIds = lspServerContext.cancelledRequestIds;
json& lspLintSettings = lspServerContext.lspLintSettings;
bool& shouldExit = lspServerContext.shouldExit;
std::map<std::string, LspWorkspaceIndex>& workspaceIndexes = lspServerContext.workspaceIndexes;
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
