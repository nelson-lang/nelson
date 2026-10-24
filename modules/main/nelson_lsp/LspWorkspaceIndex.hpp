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
#include "LspServerContext.hpp"
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
bool
readTextFile(const std::filesystem::path& path, std::string& text);
//=============================================================================
void
clearWorkspaceIndexes();
//=============================================================================
void
invalidateWorkspaceIndexForUri(const std::string& uri);
//=============================================================================
LspWorkspaceIndex&
workspaceIndexForRoot(const std::string& rootUri);
//=============================================================================
void
appendWorkspaceCompletionItems(json& items, const std::string& uri, const std::string& prefix);
//=============================================================================
json
workspaceSignatureHelpFor(const std::string& uri, const std::string& name);
//=============================================================================
json
workspaceDefinitionFor(const std::string& uri, const std::string& name);
//=============================================================================
bool
workspaceHasPublicSymbol(const std::string& uri, const std::string& name);
//=============================================================================
json
workspaceReferencesFor(
    const std::string& uri, const std::string& name, const json& localReferences);
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
