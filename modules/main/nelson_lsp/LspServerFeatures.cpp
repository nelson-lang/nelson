//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LspServerFeatures.hpp"
#include "LspWorkspaceIndex.hpp"
#include "CodeAnalyzer.hpp"
#include "LspConfiguration.hpp"
#include "LspUtilities.hpp"
#include "NelsonCodeIndent.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <set>
#include <sstream>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
bool
contextOnlyContains(const json& params, const std::string& kind)
{
    const json context = params.value("context", json::object());
    if (!context.is_object() || !context.contains("only") || !context["only"].is_array()) {
        return false;
    }
    for (const auto& item : context["only"]) {
        if (item.is_string() && item.get<std::string>() == kind) {
            return true;
        }
    }
    return false;
}
//=============================================================================
json
organizeImportsActionFor(const std::string& uri, const OpenDocument& document)
{
    if (document.model.imports.size() < 2) {
        return nullptr;
    }
    std::vector<std::string> imports;
    int firstLine = document.model.imports.front().line;
    int lastLine = document.model.imports.front().line;
    for (const auto& import : document.model.imports) {
        imports.push_back(import.detail);
        firstLine = std::min(firstLine, import.line);
        lastLine = std::max(lastLine, import.line);
    }
    std::sort(imports.begin(), imports.end());
    imports.erase(std::unique(imports.begin(), imports.end()), imports.end());
    std::ostringstream replacement;
    for (const auto& import : imports) {
        replacement << "import " << import << "\n";
    }
    int endLine = lastLine + 1;
    int endColumn = 1;
    if (lastLine >= static_cast<int>(document.model.lines.size())) {
        endLine = lastLine;
        endColumn = static_cast<int>(document.model.lines.back().size() + 1);
    }
    json action;
    action["title"] = "Organize imports";
    action["kind"] = "source.organizeImports";
    action["edit"]["changes"][uri] = json::array();
    json edit;
    edit["range"] = lspRange(firstLine, 1, endLine, endColumn);
    edit["newText"] = replacement.str();
    action["edit"]["changes"][uri].push_back(edit);
    return action;
}
//=============================================================================
json
helpSkeletonActionFor(const std::string& uri)
{
    json action;
    action["title"] = "Generate help skeleton";
    action["kind"] = "source.nelson.generateHelpSkeleton";
    action["command"]["title"] = "Generate help skeleton";
    action["command"]["command"] = "nelson.openHelp";
    action["command"]["arguments"] = json::array({ uri, "generateSkeleton" });
    return action;
}
//=============================================================================
json
missingSymbolActionsFor(const std::string& uri, const json& params, const OpenDocument& document)
{
    json actions = json::array();
    const json context = params.value("context", json::object());
    if (context.contains("diagnostics") && context["diagnostics"].is_array()
        && !context["diagnostics"].empty()) {
        return actions;
    }
    const json range = params.value("range", json::object());
    const int line = range.value("start", json::object()).value("line", 0);
    const int character = range.value("start", json::object()).value("character", 0);
    const std::string name = lspWordAt(document.model, line, character);
    if (name.empty() || !workspaceDefinitionFor(uri, name).is_null()) {
        return actions;
    }
    json createFile;
    createFile["title"] = "Create function file '" + name + ".m'";
    createFile["kind"] = "quickfix";
    createFile["command"]["title"] = createFile["title"];
    createFile["command"]["command"] = "nelson.createFunctionFile";
    createFile["command"]["arguments"] = json::array({ uri, name });
    actions.push_back(createFile);
    json stub;
    stub["title"] = "Add local function stub '" + name + "'";
    stub["kind"] = "quickfix";
    json edit;
    const int endLine = static_cast<int>(document.model.lines.size());
    edit["range"] = lspRange(endLine, 1, endLine, 1);
    edit["newText"]
        = "\nfunction varargout = " + name + "(varargin)\n  varargout = cell(1, nargout);\nend\n";
    stub["edit"]["changes"][uri] = json::array({ edit });
    actions.push_back(stub);
    return actions;
}
//=============================================================================
json
sourceActionsFor(const std::string& uri, const json& params, const OpenDocument& document)
{
    json actions = json::array();
    if (contextOnlyContains(params, "source.organizeImports")) {
        json action = organizeImportsActionFor(uri, document);
        if (!action.is_null()) {
            actions.push_back(action);
        }
    }
    if (contextOnlyContains(params, "source.nelson.generateHelpSkeleton")) {
        actions.push_back(helpSkeletonActionFor(uri));
    }
    if (actions.empty()) {
        actions = missingSymbolActionsFor(uri, params, document);
    }
    return actions;
}
//=============================================================================
size_t
lineStartOffsetForFormatting(const std::string& text, int zeroBasedLine)
{
    size_t offset = 0;
    for (int line = 0; line < zeroBasedLine && offset < text.size(); ++line) {
        size_t newline = text.find('\n', offset);
        if (newline == std::string::npos) {
            return text.size();
        }
        offset = newline + 1;
    }
    return offset;
}
//=============================================================================
size_t
positionOffsetForFormatting(const std::string& text, int zeroBasedLine, int zeroBasedCharacter)
{
    size_t start = lineStartOffsetForFormatting(text, zeroBasedLine);
    size_t end = text.find('\n', start);
    if (end == std::string::npos) {
        end = text.size();
    }
    return std::min(start + static_cast<size_t>(std::max(zeroBasedCharacter, 0)), end);
}
//=============================================================================
json
formattingEdits(const std::string& uri, const json& params)
{
    json edits = json::array();
    auto found = documents.find(uri);
    if (found == documents.end()) {
        return edits;
    }
    int tabSize = 2;
    if (params.contains("options") && params["options"].is_object()
        && params["options"].contains("tabSize")
        && params["options"]["tabSize"].is_number_integer()) {
        tabSize = std::max(params["options"]["tabSize"].get<int>(), 0);
    }
    try {
        const std::wstring source = Nelson::utf8_to_wstring(found->second.text);
        const std::wstring formatted
            = Nelson::smartIndentCode(source, tabSize, filenameFromUri(uri), true);
        std::string formattedText;
        if (!Nelson::wstring_to_utf8(formatted, formattedText)
            || formattedText == found->second.text) {
            return edits;
        }
        json edit;
        if (params.contains("range") && params["range"].is_object()) {
            const json& range = params["range"];
            const int startLine = range.value("start", json::object()).value("line", 0);
            const int startCharacter = range.value("start", json::object()).value("character", 0);
            const int endLine = range.value("end", json::object()).value("line", startLine);
            const int endCharacter
                = range.value("end", json::object()).value("character", startCharacter);
            size_t startOffset
                = positionOffsetForFormatting(formattedText, startLine, startCharacter);
            size_t endOffset = positionOffsetForFormatting(formattedText, endLine, endCharacter);
            edit["range"] = range;
            edit["newText"] = formattedText.substr(startOffset, endOffset - startOffset);
        } else {
            edit["range"] = fullDocumentRange(found->second.text);
            edit["newText"] = formattedText;
        }
        edits.push_back(edit);
    } catch (...) {
    }
    return edits;
}
//=============================================================================
bool
documentModelForUri(const std::string& uri, LspDocumentModel& model)
{
    auto found = documents.find(uri);
    if (found != documents.end()) {
        if (found->second.model.text.empty()) {
            found->second.model = buildLspDocumentModel(uri, found->second.text);
        }
        model = found->second.model;
        return true;
    }
    std::filesystem::path path = pathFromUri(uri);
    std::string text;
    if (path.empty() || !readTextFile(path, text)) {
        return false;
    }
    model = buildLspDocumentModel(uri, text);
    return true;
}
//=============================================================================
json
completionFor(const std::string& uri, const json& params)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return json::array();
    }
    const int line = params["position"].value("line", 0);
    const int character = params["position"].value("character", 0);
    const std::string prefix = lspPrefixAt(model, line, character);
    json items = lspCompletionItems(model, prefix);
    appendWorkspaceCompletionItems(items, uri, prefix);
    return items;
}
//=============================================================================
json
signatureHelpFor(const std::string& uri, const json& params)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        json result;
        result["signatures"] = json::array();
        result["activeSignature"] = 0;
        result["activeParameter"] = 0;
        return result;
    }
    const int line = params["position"].value("line", 0);
    const int character = params["position"].value("character", 0);
    json localHelp = lspSignatureHelp(model, line, character);
    if (!localHelp.value("signatures", json::array()).empty()) {
        return localHelp;
    }
    if (line >= 0 && line < static_cast<int>(model.lines.size())) {
        const std::string& textLine = model.lines[static_cast<size_t>(line)];
        size_t cursor = std::min(static_cast<size_t>(std::max(character, 0)), textLine.size());
        size_t paren = textLine.rfind('(', cursor);
        if (paren != std::string::npos && paren > 0) {
            size_t first = paren;
            while (first > 0
                && (std::isalnum(static_cast<unsigned char>(textLine[first - 1]))
                    || textLine[first - 1] == '_')) {
                --first;
            }
            json workspaceHelp
                = workspaceSignatureHelpFor(uri, textLine.substr(first, paren - first));
            if (!workspaceHelp.is_null()) {
                return workspaceHelp;
            }
        }
    }
    return localHelp;
}
//=============================================================================
json
definitionFor(const std::string& uri, const json& params)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return nullptr;
    }
    const std::string name = lspWordAt(
        model, params["position"].value("line", 0), params["position"].value("character", 0));
    if (name.empty()) {
        return nullptr;
    }
    for (const auto& symbol : model.symbols) {
        if (symbol.name == name) {
            return lspLocation(uri, symbol);
        }
    }
    for (const auto& variable : model.variables) {
        if (variable.name == name) {
            return lspLocation(uri, variable);
        }
    }
    for (const auto& import : model.imports) {
        if (import.name == name) {
            return lspLocation(uri, import);
        }
    }
    json workspaceLocation = workspaceDefinitionFor(uri, name);
    if (!workspaceLocation.is_null()) {
        return workspaceLocation;
    }
    std::filesystem::path root = workspaceRootForPath(pathFromUri(uri));
    if (!root.empty()) {
        std::filesystem::path file = root / (name + ".m");
        if (std::filesystem::exists(file)) {
            LspModelSymbol symbol;
            symbol.name = name;
            return lspLocation(pathToUri(file), symbol);
        }
    }
    return nullptr;
}
//=============================================================================
json
referencesFor(const std::string& uri, const json& params)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return json::array();
    }
    const std::string name = lspWordAt(
        model, params["position"].value("line", 0), params["position"].value("character", 0));
    return workspaceReferencesFor(uri, name, lspReferences(model, name));
}
//=============================================================================
json
documentHighlightFor(const std::string& uri, const json& params)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return json::array();
    }
    const std::string name = lspWordAt(
        model, params["position"].value("line", 0), params["position"].value("character", 0));
    return lspDocumentHighlights(model, name);
}
//=============================================================================
json
renameFor(const std::string& uri, const json& params)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return nullptr;
    }
    const std::string oldName = lspWordAt(
        model, params["position"].value("line", 0), params["position"].value("character", 0));
    const std::string newName = optionalString(params, "newName");
    if (oldName.empty() || newName.empty()) {
        return nullptr;
    }
    json locations = lspReferences(model, oldName);
    json result;
    for (const auto& location : locations) {
        json edit;
        edit["range"] = location["range"];
        edit["newText"] = newName;
        result["changes"][location.value("uri", uri)].push_back(edit);
    }
    return result;
}
//=============================================================================
json
prepareRenameFor(const std::string& uri, const json& params)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return nullptr;
    }
    const std::string name = lspWordAt(
        model, params["position"].value("line", 0), params["position"].value("character", 0));
    if (name.empty()) {
        return nullptr;
    }
    for (const auto& location : lspReferences(model, name)) {
        return location["range"];
    }
    return nullptr;
}
//=============================================================================
json
documentSymbolFor(const std::string& uri)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return json::array();
    }
    return lspDocumentSymbols(model);
}
//=============================================================================
json
workspaceSymbols(const json& params)
{
    const json workDoneToken = params.value("workDoneToken", json());
    sendWorkDoneProgress(workDoneToken, "begin", "Nelson workspace index");
    const std::string query = optionalString(params, "query");
    json result = json::array();
    auto append = [&](const LspDocumentModel& model) {
        for (const auto& symbol : model.symbols) {
            if (!query.empty() && symbol.name.find(query) == std::string::npos) {
                continue;
            }
            json item;
            item["name"] = symbol.name;
            item["kind"] = symbol.kind;
            item["location"] = lspLocation(model.uri, symbol);
            result.push_back(item);
        }
    };
    for (const auto& item : documents) {
        append(item.second.model.text.empty() ? buildLspDocumentModel(item.first, item.second.text)
                                              : item.second.model);
    }
    for (const std::string& uri : workspaceRootUris) {
        LspWorkspaceIndex& index = workspaceIndexForRoot(uri);
        for (const auto& indexed : index.files) {
            append(indexed.second.model);
        }
    }
    sendWorkDoneProgress(workDoneToken, "end", "");
    return result;
}
//=============================================================================
json
foldingRangeFor(const std::string& uri)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return json::array();
    }
    return lspFoldingRanges(model);
}
//=============================================================================
json
selectionRangeFor(const std::string& uri, const json& params)
{
    LspDocumentModel model;
    json result = json::array();
    if (!documentModelForUri(uri, model) || !params.contains("positions")) {
        return result;
    }
    for (const auto& position : params["positions"]) {
        json ranges
            = lspSelectionRanges(model, position.value("line", 0), position.value("character", 0));
        if (!ranges.empty()) {
            result.push_back(ranges[0]);
        }
    }
    return result;
}
//=============================================================================
json
semanticTokensFor(const std::string& uri)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        json result;
        result["data"] = json::array();
        return result;
    }
    return lspSemanticTokens(model);
}
//=============================================================================
json
documentLinksFor(const std::string& uri)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return json::array();
    }
    return lspDocumentLinks(model);
}
//=============================================================================
json
codeLensFor(const std::string& uri)
{
    LspDocumentModel model;
    if (!documentModelForUri(uri, model)) {
        return json::array();
    }
    json lenses = lspCodeLens(model);
    auto found = documents.find(uri);
    if (found != documents.end() && !model.symbols.empty()) {
        json issues;
        issues["range"] = lspLocation(uri, model.symbols.front())["range"];
        issues["command"]["title"] = "Issues: " + std::to_string(found->second.diagnostics.size());
        issues["command"]["command"] = "nelson.lintWorkspace";
        issues["command"]["arguments"] = json::array({ uri });
        lenses.push_back(issues);
    }
    return lenses;
}
//=============================================================================
json
executeCommandFor(const json& params)
{
    json result;
    const std::string command = optionalString(params, "command");
    json arguments = params.value("arguments", json::array());
    result["command"] = command;
    result["status"] = "accepted";
    result["executed"] = json::boolean_t(false);
    if (command == "nelson.clearLspCache") {
        clearWorkspaceIndexes();
        result["executed"] = json::boolean_t(true);
        result["cleared"] = json::boolean_t(true);
    } else if (command == "nelson.lintWorkspace") {
        json paramsWithProgress;
        paramsWithProgress["previousResultIds"] = json::array();
        paramsWithProgress["partialResultToken"] = "nelson-lsp-command-lintWorkspace";
        if (params.contains("workDoneToken")) {
            paramsWithProgress["workDoneToken"] = params["workDoneToken"];
        }
        json report = workspaceDiagnosticReportWithPartialProgress(paramsWithProgress);
        result["executed"] = json::boolean_t(true);
        result["items"] = report.value("items", json::array()).size();
    } else if (command == "nelson.runFile" || command == "nelson.runSelection"
        || command == "nelson.runTestsForModule" || command == "nelson.openHelp"
        || command == "nelson.createFunctionFile") {
        result["status"] = "prepared";
        result["requiresClientExecution"] = json::boolean_t(true);
        result["arguments"] = arguments;
        if (command == "nelson.runFile" && arguments.is_array() && !arguments.empty()) {
            std::filesystem::path path = pathFromUri(arguments[0].get<std::string>());
            result["nelsonCode"] = "run('" + path.generic_string() + "')";
        } else if (command == "nelson.runSelection" && arguments.is_array() && !arguments.empty()) {
            result["nelsonCode"] = arguments[0].is_string() ? arguments[0].get<std::string>() : "";
        } else if (command == "nelson.runTestsForModule" && arguments.is_array()
            && !arguments.empty()) {
            std::filesystem::path path = pathFromUri(arguments[0].get<std::string>());
            std::string module = path.parent_path().filename().generic_string();
            for (auto it = path.begin(); it != path.end(); ++it) {
                if (it->generic_string() == "modules") {
                    auto moduleIt = it;
                    ++moduleIt;
                    if (moduleIt != path.end()) {
                        module = moduleIt->generic_string();
                    }
                    break;
                }
            }
            result["nelsonCode"] = "test_run('" + module + "')";
        } else if (command == "nelson.openHelp" && arguments.is_array() && !arguments.empty()) {
            result["helpTarget"] = arguments[0];
        } else if (command == "nelson.createFunctionFile" && arguments.is_array()
            && arguments.size() >= 2) {
            result["fileName"] = arguments[1].get<std::string>() + ".m";
            result["template"] = "function varargout = " + arguments[1].get<std::string>()
                + "(varargin)\n  varargout = cell(1, nargout);\nend\n";
        }
    }
    return result;
}
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
