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
#include "LspDocumentModel.hpp"
#include "LspServerContext.hpp"
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
json
codeActions(const std::string& uri, const json& params);
//=============================================================================
json
formattingEdits(const std::string& uri, const json& params);
//=============================================================================
bool
documentModelForUri(const std::string& uri, LspDocumentModel& model);
//=============================================================================
json
completionFor(const std::string& uri, const json& params);
//=============================================================================
json
signatureHelpFor(const std::string& uri, const json& params);
//=============================================================================
json
definitionFor(const std::string& uri, const json& params);
//=============================================================================
json
referencesFor(const std::string& uri, const json& params);
//=============================================================================
json
documentHighlightFor(const std::string& uri, const json& params);
//=============================================================================
json
renameFor(const std::string& uri, const json& params);
//=============================================================================
json
prepareRenameFor(const std::string& uri, const json& params);
//=============================================================================
json
documentSymbolFor(const std::string& uri);
//=============================================================================
json
workspaceSymbols(const json& params);
//=============================================================================
json
foldingRangeFor(const std::string& uri);
//=============================================================================
json
selectionRangeFor(const std::string& uri, const json& params);
//=============================================================================
json
semanticTokensFor(const std::string& uri);
//=============================================================================
json
documentLinksFor(const std::string& uri);
//=============================================================================
json
codeLensFor(const std::string& uri);
//=============================================================================
json
executeCommandFor(const json& params);
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
