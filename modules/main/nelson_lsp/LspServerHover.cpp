//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LspServerHover.hpp"
#include "LspDocumentModel.hpp"
#include "LspServerFeatures.hpp"
#include "LspWorkspaceIndex.hpp"
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
json
symbolHoverFor(const std::string& uri, int line, int character)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return nullptr;
    }
    const std::string name = lspWordAt(model, line, character);
    auto makeHover = [](const LspModelSymbol& symbol) {
        json hover;
        hover["contents"]["kind"] = "markdown";
        hover["contents"]["value"] = "**" + symbol.name + "**\n\n" + symbol.detail;
        hover["range"] = lspLocation("", symbol)["range"];
        return hover;
    };
    for (const auto& symbol : model.symbols) {
        if (symbol.name == name) {
            return makeHover(symbol);
        }
    }
    for (const auto& variable : model.variables) {
        if (variable.name == name) {
            return makeHover(variable);
        }
    }
    for (const auto& imported : model.imports) {
        if (imported.name == name) {
            return makeHover(imported);
        }
    }
    json location = workspaceDefinitionFor(uri, name);
    if (!location.is_null()) {
        json hover;
        hover["contents"]["kind"] = "markdown";
        hover["contents"]["value"] = "**" + name + "**\n\n" + location.value("uri", "");
        hover["range"] = location["range"];
        return hover;
    }
    return nullptr;
}
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
