//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzer.hpp"
#include "CodeAnalyzerConfig.hpp"
#include "CodeAnalyzerRules.hpp"
#include "LspConfiguration.hpp"
#include "LspDocumentModel.hpp"
#include "LspServerContext.hpp"
#include "LspServerFeatures.hpp"
#include "LspServerHover.hpp"
#include "LspWorkspaceIndex.hpp"
#include "LspUtilities.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
void
normalizeWorkspaceRootUris()
{
    std::sort(workspaceRootUris.begin(), workspaceRootUris.end());
    workspaceRootUris.erase(
        std::unique(workspaceRootUris.begin(), workspaceRootUris.end()), workspaceRootUris.end());
}
//=============================================================================
std::string
readMessage()
{
    std::string line;
    size_t contentLength = 0;
    while (std::getline(std::cin, line)) {
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        if (line.empty()) {
            break;
        }
        const std::string prefix = "Content-Length:";
        if (line.rfind(prefix, 0) == 0) {
            contentLength = static_cast<size_t>(std::stoul(line.substr(prefix.size())));
        }
    }
    if (contentLength == 0) {
        return {};
    }
    std::string body(contentLength, '\0');
    std::cin.read(&body[0], static_cast<std::streamsize>(contentLength));
    return body;
}
//=============================================================================
void
writeMessage(const json& message)
{
    std::string body = message.dump();
    std::cout << "Content-Length: " << body.size() << "\r\n\r\n" << body << std::flush;
}
//=============================================================================
void
sendResponse(const json& id, const json& result)
{
    json response;
    response["jsonrpc"] = "2.0";
    response["id"] = id;
    response["result"] = result;
    writeMessage(response);
}
//=============================================================================
void
sendErrorResponse(const json& id, int code, const std::string& message)
{
    json response;
    response["jsonrpc"] = "2.0";
    response["id"] = id;
    response["error"]["code"] = code;
    response["error"]["message"] = message;
    writeMessage(response);
}
//=============================================================================
void
sendNotification(const std::string& method, const json& params)
{
    json notification;
    notification["jsonrpc"] = "2.0";
    notification["method"] = method;
    notification["params"] = params;
    writeMessage(notification);
}
//=============================================================================
void
sendWorkDoneProgress(const json& token, const std::string& kind, const std::string& message)
{
    if (token.is_null()) {
        return;
    }
    json progressParams;
    progressParams["token"] = token;
    progressParams["value"]["kind"] = kind;
    if (kind == "begin") {
        progressParams["value"]["title"] = message;
        progressParams["value"]["cancellable"] = json::boolean_t(true);
    } else if (!message.empty()) {
        progressParams["value"]["message"] = message;
    }
    sendNotification("$/progress", progressParams);
}
//=============================================================================
std::wstring
filenameFromUri(const std::string& uri)
{
    std::filesystem::path path = pathFromUri(uri);
    if (!path.empty()) {
        return path.wstring();
    }
    return Nelson::utf8_to_wstring("buffer.m");
}
//=============================================================================
std::string
lowerAscii(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return value;
}
//=============================================================================
std::string
normalizedPathKey(const std::filesystem::path& path)
{
    return lowerAscii(std::filesystem::absolute(path).lexically_normal().generic_string());
}
//=============================================================================
bool
pathIsInsideOrEqual(const std::filesystem::path& path, const std::filesystem::path& root)
{
    const std::string pathKey = normalizedPathKey(path);
    std::string rootKey = normalizedPathKey(root);
    while (!rootKey.empty() && rootKey.back() == '/') {
        rootKey.pop_back();
    }
    return pathKey == rootKey || pathKey.rfind(rootKey + "/", 0) == 0;
}
//=============================================================================
std::filesystem::path
workspaceRootForPath(const std::filesystem::path& path)
{
    std::filesystem::path bestRoot;
    size_t bestLength = 0;
    for (const std::string& uri : workspaceRootUris) {
        std::filesystem::path root = pathFromUri(uri);
        if (root.empty() || !pathIsInsideOrEqual(path, root)) {
            continue;
        }
        const size_t length = normalizedPathKey(root).size();
        if (length > bestLength) {
            bestRoot = root;
            bestLength = length;
        }
    }
    return bestRoot;
}
//=============================================================================
std::filesystem::path
workspaceConfigForRoot(const std::filesystem::path& root)
{
    if (root.empty()) {
        return {};
    }
    std::filesystem::path config = root / "nelson-lint.json";
    return std::filesystem::exists(config) ? config : std::filesystem::path();
}
//=============================================================================
Nelson::CodeAnalyzerOptions
baseAnalyzerOptions()
{
    Nelson::CodeAnalyzerOptions options;
    options.includeSuppressed = false;
    options.includeCyclomatic = true;
    options.includeModifiedCyclomatic = true;
    return options;
}
//=============================================================================
Nelson::CodeAnalyzerOptions
analyzerOptionsForPath(const std::filesystem::path& path)
{
    Nelson::CodeAnalyzerOptions options = baseAnalyzerOptions();
    std::filesystem::path root = workspaceRootForPath(path);
    std::filesystem::path config = workspaceConfigForRoot(root);
    if (!config.empty()) {
        options.configFile = config.wstring();
        Nelson::readCodeAnalyzerConfig(options);
        if (options.configError.empty()) {
            options.configFile.clear();
        }
    }
    Nelson::Lsp::applyLspLintSettings(options, lspLintSettings);
    return options;
}
//=============================================================================
void
analyzeDocument(OpenDocument& document)
{
    document.model = buildLspDocumentModel(document.uri, document.text);
    Nelson::CodeAnalyzerOptions options = analyzerOptionsForPath(pathFromUri(document.uri));
    document.diagnostics
        = Nelson::analyzeCodeBuffer(filenameFromUri(document.uri), document.text, options);
}
//=============================================================================
void
publishDiagnostics(OpenDocument& document)
{
    analyzeDocument(document);
    json params;
    params["uri"] = document.uri;
    if (document.version > 0) {
        params["version"] = document.version;
    }
    params["diagnostics"] = json::array();
    for (const auto& diagnostic : document.diagnostics) {
        params["diagnostics"].push_back(lspDiagnostic(diagnostic));
    }
    sendNotification("textDocument/publishDiagnostics", params);
}
//=============================================================================
void
publishEmptyDiagnostics(const std::string& uri)
{
    json params;
    params["uri"] = uri;
    params["diagnostics"] = json::array();
    sendNotification("textDocument/publishDiagnostics", params);
}
//=============================================================================
void
publishDiagnosticsForOpenDocuments()
{
    for (auto& item : documents) {
        publishDiagnostics(item.second);
    }
}
//=============================================================================
json
workspaceDiagnosticReport(const json& params)
{
    const std::map<std::string, std::string> previousIds = previousWorkspaceResultIds(params);
    std::map<std::string, std::vector<Nelson::CodeAnalyzerDiagnostic>> diagnosticsByUri;
    for (const std::string& uri : workspaceRootUris) {
        std::filesystem::path path = pathFromUri(uri);
        if (path.empty()) {
            continue;
        }
        Nelson::CodeAnalyzerOptions options = analyzerOptionsForPath(path);
        options.includeSubfolders = true;
        options.fullPath = true;
        for (const std::wstring& file :
            Nelson::codeAnalyzerIncludedFiles({ path.wstring() }, options)) {
            diagnosticsByUri.try_emplace(pathToUri(std::filesystem::path(file)),
                std::vector<Nelson::CodeAnalyzerDiagnostic> {});
        }
        addDiagnosticsByUri(diagnosticsByUri, Nelson::analyzeCode({ path.wstring() }, options));
    }
    for (auto& item : documents) {
        analyzeDocument(item.second);
        diagnosticsByUri[item.first] = item.second.diagnostics;
    }
    json result;
    result["items"] = json::array();
    for (const auto& item : diagnosticsByUri) {
        auto foundOpenDocument = documents.find(item.first);
        const int version
            = foundOpenDocument == documents.end() ? 0 : foundOpenDocument->second.version;
        auto previous = previousIds.find(item.first);
        result["items"].push_back(documentDiagnosticReport(item.first, version, item.second,
            previous == previousIds.end() ? "" : previous->second));
    }
    return result;
}
//=============================================================================
json
workspaceDiagnosticReportWithPartialProgress(const json& params)
{
    json workDoneToken = params.value("workDoneToken", json());
    sendWorkDoneProgress(workDoneToken, "begin", "Nelson workspace diagnostics");
    json result = workspaceDiagnosticReport(params);
    if (!params.contains("partialResultToken")) {
        sendWorkDoneProgress(workDoneToken, "end", "");
        return result;
    }
    sendWorkDoneProgress(workDoneToken, "report", "Publishing diagnostics");
    json progressParams;
    progressParams["token"] = params["partialResultToken"];
    progressParams["value"]["items"] = result.value("items", json::array());
    sendNotification("$/progress", progressParams);
    result["items"] = json::array();
    sendWorkDoneProgress(workDoneToken, "end", "");
    return result;
}
//=============================================================================
void
setWorkspaceRootsFromInitialize(const json& params)
{
    workspaceRootUris.clear();
    if (params.contains("workspaceFolders") && params["workspaceFolders"].is_array()) {
        for (const auto& folder : params["workspaceFolders"]) {
            std::string uri = optionalString(folder, "uri");
            if (!uri.empty()) {
                workspaceRootUris.push_back(uri);
            }
        }
    }
    if (!workspaceRootUris.empty()) {
        normalizeWorkspaceRootUris();
        return;
    }
    std::string rootUri = optionalString(params, "rootUri");
    if (!rootUri.empty()) {
        workspaceRootUris.push_back(rootUri);
    }
    std::string rootPath = optionalString(params, "rootPath");
    if (!rootPath.empty()) {
        std::filesystem::path path(Nelson::utf8_to_wstring(rootPath));
        workspaceRootUris.push_back(pathToUri(path));
    }
    normalizeWorkspaceRootUris();
}
//=============================================================================
void
updateWorkspaceRootsFromChange(const json& params)
{
    if (!params.contains("event") || !params["event"].is_object()) {
        return;
    }
    const json& event = params["event"];
    if (event.contains("removed") && event["removed"].is_array()) {
        for (const auto& folder : event["removed"]) {
            const std::string uri = optionalString(folder, "uri");
            workspaceRootUris.erase(
                std::remove(workspaceRootUris.begin(), workspaceRootUris.end(), uri),
                workspaceRootUris.end());
        }
    }
    if (event.contains("added") && event["added"].is_array()) {
        for (const auto& folder : event["added"]) {
            const std::string uri = optionalString(folder, "uri");
            if (!uri.empty()) {
                workspaceRootUris.push_back(uri);
            }
        }
    }
    normalizeWorkspaceRootUris();
}
//=============================================================================
json
textDocumentDiagnosticReport(const std::string& uri, const json& params)
{
    const std::string previousResultId = optionalString(params, "previousResultId");
    auto found = documents.find(uri);
    if (found != documents.end()) {
        analyzeDocument(found->second);
        json report = documentDiagnosticReport(
            uri, found->second.version, found->second.diagnostics, previousResultId);
        report.erase("uri");
        return report;
    }
    json report;
    std::vector<Nelson::CodeAnalyzerDiagnostic> diagnostics;
    std::filesystem::path path = pathFromUri(uri);
    if (!path.empty() && std::filesystem::exists(path)) {
        Nelson::CodeAnalyzerOptions options = analyzerOptionsForPath(path);
        options.fullPath = true;
        std::vector<std::wstring> names = { path.wstring() };
        diagnostics = Nelson::analyzeCode(names, options);
    }
    report = documentDiagnosticReport(uri, 0, diagnostics, previousResultId);
    report.erase("uri");
    return report;
}
//=============================================================================
json
codeActionFor(const std::string& uri, const Nelson::CodeAnalyzerDiagnostic& diagnostic,
    const Nelson::CodeAnalyzerFix& fix)
{
    json action;
    action["title"] = fix.title;
    action["kind"] = "quickfix";
    action["diagnostics"] = json::array({ lspDiagnostic(diagnostic) });
    action["edit"]["changes"][uri] = json::array();
    for (const auto& edit : fix.edits) {
        json textEdit;
        textEdit["range"]
            = lspRange(edit.lineStart, edit.columnStart, edit.lineEnd, edit.columnEnd);
        textEdit["newText"] = edit.replacementText;
        action["edit"]["changes"][uri].push_back(textEdit);
    }
    return action;
}
//=============================================================================
void
appendFixEdits(json& changes, const std::string& uri, const Nelson::CodeAnalyzerFix& fix)
{
    for (const auto& edit : fix.edits) {
        json textEdit;
        textEdit["range"]
            = lspRange(edit.lineStart, edit.columnStart, edit.lineEnd, edit.columnEnd);
        textEdit["newText"] = edit.replacementText;
        changes[uri].push_back(textEdit);
    }
}
//=============================================================================
json
fixAllActionFor(
    const std::string& uri, const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics)
{
    json action;
    action["title"] = "Fix all Nelson lint issues";
    action["kind"] = "source.fixAll.nelson-lint";
    action["edit"]["changes"][uri] = json::array();
    std::vector<Nelson::CodeAnalyzerTextEdit> acceptedEdits;
    for (const auto& diagnostic : diagnostics) {
        if (diagnostic.suppressed || diagnostic.fixability != "safe") {
            continue;
        }
        for (const auto& fix : diagnostic.fixes) {
            if (fix.safe && !codeAnalyzerFixConflictsWith(fix, acceptedEdits)) {
                appendFixEdits(action["edit"]["changes"], uri, fix);
                acceptedEdits.insert(acceptedEdits.end(), fix.edits.begin(), fix.edits.end());
            }
        }
    }
    if (action["edit"]["changes"][uri].empty()) {
        return nullptr;
    }
    return action;
}
//=============================================================================
bool
diagnosticIntersectsRange(const Nelson::CodeAnalyzerDiagnostic& diagnostic, const json& range)
{
    if (!range.is_object()) {
        return true;
    }
    const int diagnosticStartLine = std::max(diagnostic.lineStart - 1, 0);
    const int diagnosticEndLine = std::max(diagnostic.lineEnd - 1, diagnosticStartLine);
    const int rangeStartLine = range.value("start", json::object()).value("line", 0);
    const int rangeEndLine = range.value("end", json::object()).value("line", rangeStartLine);
    return diagnosticStartLine <= rangeEndLine && diagnosticEndLine >= rangeStartLine;
}
//=============================================================================
bool
contextRequestsFixAll(const json& context)
{
    if (!context.is_object() || !context.contains("only") || !context["only"].is_array()) {
        return false;
    }
    for (const auto& item : context["only"]) {
        if (item.is_string()) {
            const std::string kind = item.get<std::string>();
            if (kind == "source.fixAll" || kind == "source.fixAll.nelson-lint") {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
json
sourceActionsFor(const std::string& uri, const json& params, const OpenDocument& document);
//=============================================================================
bool
diagnosticMatchesContext(const Nelson::CodeAnalyzerDiagnostic& diagnostic, const json& context)
{
    if (!context.is_object() || !context.contains("diagnostics")
        || !context["diagnostics"].is_array() || context["diagnostics"].empty()) {
        return true;
    }
    for (const auto& item : context["diagnostics"]) {
        if (item.is_object() && item.value("code", "") == diagnostic.checkId) {
            return true;
        }
    }
    return false;
}
//=============================================================================
json
suppressionActionFor(const std::string& uri, const Nelson::CodeAnalyzerDiagnostic& diagnostic)
{
    json action;
    action["title"] = "Suppress " + diagnostic.checkId;
    action["kind"] = "quickfix";
    action["diagnostics"] = json::array({ lspDiagnostic(diagnostic) });
    json textEdit;
    textEdit["range"] = lspRange(
        diagnostic.lineEnd, diagnostic.columnEnd, diagnostic.lineEnd, diagnostic.columnEnd);
    textEdit["newText"] = " %#ok<" + diagnostic.checkId + ">";
    action["edit"]["changes"][uri] = json::array({ textEdit });
    return action;
}
//=============================================================================
json
fileSuppressionActionFor(const std::string& uri, const Nelson::CodeAnalyzerDiagnostic& diagnostic)
{
    json action;
    action["title"] = "Suppress " + diagnostic.checkId + " in file";
    action["kind"] = "quickfix";
    action["diagnostics"] = json::array({ lspDiagnostic(diagnostic) });
    json textEdit;
    textEdit["range"] = lspRange(1, 1, 1, 1);
    textEdit["newText"] = "%#ok<" + diagnostic.checkId + ">\n";
    action["edit"]["changes"][uri] = json::array({ textEdit });
    return action;
}
//=============================================================================
json
codeActions(const std::string& uri, const json& params)
{
    json actions = json::array();
    auto found = documents.find(uri);
    if (found == documents.end()) {
        return actions;
    }
    const json context = params.value("context", json::object());
    if (contextRequestsFixAll(context)) {
        json action = fixAllActionFor(uri, found->second.diagnostics);
        if (!action.is_null()) {
            actions.push_back(action);
        }
        return actions;
    }
    json sourceActions = sourceActionsFor(uri, params, found->second);
    if (!sourceActions.empty()) {
        return sourceActions;
    }
    for (const auto& diagnostic : found->second.diagnostics) {
        if (diagnostic.suppressed) {
            continue;
        }
        if (!diagnosticIntersectsRange(diagnostic, params.value("range", json::object()))) {
            continue;
        }
        if (!diagnosticMatchesContext(diagnostic, context)) {
            continue;
        }
        actions.push_back(suppressionActionFor(uri, diagnostic));
        actions.push_back(fileSuppressionActionFor(uri, diagnostic));
        for (const auto& fix : diagnostic.fixes) {
            actions.push_back(codeActionFor(uri, diagnostic, fix));
        }
    }
    return actions;
}
//=============================================================================
bool
positionMatchesDiagnostic(
    const Nelson::CodeAnalyzerDiagnostic& diagnostic, int zeroBasedLine, int zeroBasedCharacter)
{
    const int startLine = std::max(diagnostic.lineStart - 1, 0);
    const int endLine = std::max(diagnostic.lineEnd - 1, 0);
    const int startCharacter = std::max(diagnostic.columnStart - 1, 0);
    const int endCharacter = std::max(diagnostic.columnEnd - 1, startCharacter);
    if (zeroBasedLine < startLine || zeroBasedLine > endLine) {
        return false;
    }
    if (zeroBasedLine == startLine && zeroBasedCharacter < startCharacter) {
        return false;
    }
    if (zeroBasedLine == endLine && zeroBasedCharacter > endCharacter) {
        return false;
    }
    return true;
}
//=============================================================================
std::string
hoverTextFor(const Nelson::CodeAnalyzerDiagnostic& diagnostic)
{
    const Nelson::CodeAnalyzerRuleInfo* ruleInfo = Nelson::codeAnalyzerRuleInfo(diagnostic.checkId);
    std::ostringstream text;
    text << "**" << diagnostic.checkId;
    if (ruleInfo != nullptr && !ruleInfo->name.empty()) {
        text << ": " << ruleInfo->name;
    }
    text << "**\n\n";
    text << "Severity: " << diagnostic.severity << "\n\n";
    text << "Category: " << diagnostic.category << "\n\n";
    text << "Fixability: " << diagnostic.fixability << "\n\n";
    if (ruleInfo != nullptr && !ruleInfo->description.empty()) {
        text << ruleInfo->description << "\n\n";
    }
    text << diagnostic.message;
    if (!diagnostic.helpUri.empty()) {
        text << "\n\n" << diagnostic.helpUri;
    }
    return text.str();
}
//=============================================================================
json
hoverFor(const std::string& uri, int zeroBasedLine, int zeroBasedCharacter)
{
    auto found = documents.find(uri);
    if (found == documents.end()) {
        return nullptr;
    }
    for (const auto& diagnostic : found->second.diagnostics) {
        if (diagnostic.suppressed) {
            continue;
        }
        if (positionMatchesDiagnostic(diagnostic, zeroBasedLine, zeroBasedCharacter)) {
            json hover;
            hover["contents"]["kind"] = "markdown";
            hover["contents"]["value"] = hoverTextFor(diagnostic);
            hover["range"] = lspRange(diagnostic.lineStart, diagnostic.columnStart,
                diagnostic.lineEnd, diagnostic.columnEnd);
            return hover;
        }
    }
    return nullptr;
}
//=============================================================================
std::string
requestIdKey(const json& id)
{
    return id.is_discarded() || id.is_null() ? "" : id.dump();
}
//=============================================================================
void
handleMessage(const json& message)
{
    std::string method = message.value("method", "");
    json id = message.contains("id") ? message["id"] : json();
    if (method == "$/cancelRequest") {
        const json& params = message.value("params", json::object());
        if (params.contains("id")) {
            std::string key = requestIdKey(params["id"]);
            if (!key.empty()) {
                cancelledRequestIds.insert(key);
            }
        }
        return;
    }
    std::string key = requestIdKey(id);
    if (!key.empty() && cancelledRequestIds.erase(key) > 0) {
        sendErrorResponse(id, -32800, "Request cancelled.");
        return;
    }
    if (method == "initialize") {
        setWorkspaceRootsFromInitialize(message.value("params", json::object()));
        json capabilities;
        json textDocumentSync;
        textDocumentSync["openClose"] = json::boolean_t(true);
        textDocumentSync["change"] = 2;
        textDocumentSync["save"] = json::boolean_t(true);
        textDocumentSync["willSaveWaitUntil"] = json::boolean_t(true);
        capabilities["textDocumentSync"] = textDocumentSync;
        json codeActionProvider;
        codeActionProvider["resolveProvider"] = json::boolean_t(true);
        codeActionProvider["codeActionKinds"]
            = json::array({ "quickfix", "source.fixAll", "source.fixAll.nelson-lint",
                "source.organizeImports", "source.nelson.generateHelpSkeleton" });
        capabilities["codeActionProvider"] = codeActionProvider;
        capabilities["hoverProvider"] = json::boolean_t(true);
        json completionProvider;
        completionProvider["resolveProvider"] = json::boolean_t(true);
        completionProvider["triggerCharacters"] = json::array({ ".", "/", "\\", "@" });
        capabilities["completionProvider"] = completionProvider;
        json signatureProvider;
        signatureProvider["triggerCharacters"] = json::array({ "(", "," });
        capabilities["signatureHelpProvider"] = signatureProvider;
        capabilities["definitionProvider"] = json::boolean_t(true);
        capabilities["declarationProvider"] = json::boolean_t(true);
        capabilities["implementationProvider"] = json::boolean_t(true);
        capabilities["referencesProvider"] = json::boolean_t(true);
        capabilities["documentHighlightProvider"] = json::boolean_t(true);
        json renameProvider;
        renameProvider["prepareProvider"] = json::boolean_t(true);
        capabilities["renameProvider"] = renameProvider;
        capabilities["documentSymbolProvider"] = json::boolean_t(true);
        capabilities["workspaceSymbolProvider"] = json::boolean_t(true);
        capabilities["foldingRangeProvider"] = json::boolean_t(true);
        capabilities["selectionRangeProvider"] = json::boolean_t(true);
        capabilities["documentRangeFormattingProvider"] = json::boolean_t(true);
        json onTypeFormattingProvider;
        onTypeFormattingProvider["firstTriggerCharacter"] = "\n";
        onTypeFormattingProvider["moreTriggerCharacter"] = json::array({ ";", "," });
        capabilities["documentOnTypeFormattingProvider"] = onTypeFormattingProvider;
        capabilities["documentFormattingProvider"] = json::boolean_t(true);
        json semanticTokensProvider;
        semanticTokensProvider["legend"]["tokenTypes"] = json::array({ "keyword", "function",
            "variable", "class", "property", "method", "string", "comment" });
        semanticTokensProvider["legend"]["tokenModifiers"] = json::array();
        semanticTokensProvider["full"] = json::boolean_t(true);
        capabilities["semanticTokensProvider"] = semanticTokensProvider;
        capabilities["codeLensProvider"]["resolveProvider"] = json::boolean_t(false);
        capabilities["documentLinkProvider"]["resolveProvider"] = json::boolean_t(false);
        json executeCommandProvider;
        executeCommandProvider["commands"] = json::array({ "nelson.runFile", "nelson.runSelection",
            "nelson.runTestsForModule", "nelson.openHelp", "nelson.lintWorkspace",
            "nelson.clearLspCache", "nelson.createFunctionFile" });
        capabilities["executeCommandProvider"] = executeCommandProvider;
        capabilities["workspace"]["workspaceFolders"]["supported"] = json::boolean_t(true);
        capabilities["workspace"]["workspaceFolders"]["changeNotifications"]
            = json::boolean_t(true);
        json diagnosticProvider;
        diagnosticProvider["interFileDependencies"] = json::boolean_t(false);
        diagnosticProvider["workspaceDiagnostics"] = json::boolean_t(true);
        capabilities["diagnosticProvider"] = diagnosticProvider;
        sendResponse(id, { { "capabilities", capabilities } });
    } else if (method == "shutdown") {
        sendResponse(id, nullptr);
    } else if (method == "exit") {
        shouldExit = true;
    } else if (method == "textDocument/didOpen") {
        const json& textDocument = message["params"]["textDocument"];
        OpenDocument document;
        document.uri = textDocument.value("uri", "");
        document.text = textDocument.value("text", "");
        document.version = textDocument.value("version", 0);
        documents[document.uri] = std::move(document);
        invalidateWorkspaceIndexForUri(textDocument.value("uri", ""));
        publishDiagnostics(documents[textDocument.value("uri", "")]);
    } else if (method == "textDocument/didClose") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        documents.erase(uri);
        invalidateWorkspaceIndexForUri(uri);
        publishEmptyDiagnostics(uri);
    } else if (method == "textDocument/didChange") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        auto& document = documents[uri];
        document.uri = uri;
        const int incomingVersion = message["params"]["textDocument"].value("version", 0);
        if (incomingVersion > 0 && document.version > 0 && incomingVersion <= document.version) {
            return;
        }
        const auto& changes = message["params"]["contentChanges"];
        if (changes.is_array()) {
            for (const auto& change : changes) {
                applyDocumentChange(document.text, change);
            }
        }
        if (incomingVersion > 0) {
            document.version = incomingVersion;
        }
        invalidateWorkspaceIndexForUri(uri);
        publishDiagnostics(document);
    } else if (method == "textDocument/didSave") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        auto& document = documents[uri];
        document.uri = uri;
        const int incomingVersion = message["params"]["textDocument"].value("version", 0);
        if (message["params"].contains("text")) {
            document.text = message["params"].value("text", document.text);
        }
        if (incomingVersion > 0) {
            document.version = incomingVersion;
        }
        invalidateWorkspaceIndexForUri(uri);
        publishDiagnostics(document);
    } else if (method == "textDocument/willSaveWaitUntil") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, formattingEdits(uri, message.value("params", json::object())));
    } else if (method == "textDocument/codeAction") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, codeActions(uri, message.value("params", json::object())));
    } else if (method == "textDocument/hover") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        const int line = message["params"]["position"].value("line", 0);
        const int character = message["params"]["position"].value("character", 0);
        json hover = hoverFor(uri, line, character);
        sendResponse(id, hover.is_null() ? symbolHoverFor(uri, line, character) : hover);
    } else if (method == "textDocument/formatting") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, formattingEdits(uri, message.value("params", json::object())));
    } else if (method == "textDocument/rangeFormatting"
        || method == "textDocument/onTypeFormatting") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, formattingEdits(uri, message.value("params", json::object())));
    } else if (method == "textDocument/completion") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, completionFor(uri, message.value("params", json::object())));
    } else if (method == "textDocument/signatureHelp") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, signatureHelpFor(uri, message.value("params", json::object())));
    } else if (method == "textDocument/definition" || method == "textDocument/declaration"
        || method == "textDocument/implementation") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, definitionFor(uri, message.value("params", json::object())));
    } else if (method == "textDocument/references") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, referencesFor(uri, message.value("params", json::object())));
    } else if (method == "textDocument/documentHighlight") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, documentHighlightFor(uri, message.value("params", json::object())));
    } else if (method == "textDocument/prepareRename") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, prepareRenameFor(uri, message.value("params", json::object())));
    } else if (method == "textDocument/rename") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, renameFor(uri, message.value("params", json::object())));
    } else if (method == "textDocument/documentSymbol") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, documentSymbolFor(uri));
    } else if (method == "workspace/symbol") {
        sendResponse(id, workspaceSymbols(message.value("params", json::object())));
    } else if (method == "textDocument/foldingRange") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, foldingRangeFor(uri));
    } else if (method == "textDocument/selectionRange") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, selectionRangeFor(uri, message.value("params", json::object())));
    } else if (method == "textDocument/semanticTokens/full") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, semanticTokensFor(uri));
    } else if (method == "textDocument/codeLens") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, codeLensFor(uri));
    } else if (method == "textDocument/documentLink") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(id, documentLinksFor(uri));
    } else if (method == "workspace/executeCommand") {
        sendResponse(id, executeCommandFor(message.value("params", json::object())));
    } else if (method == "codeAction/resolve" || method == "completionItem/resolve") {
        sendResponse(id, message.value("params", json::object()));
    } else if (method == "textDocument/diagnostic") {
        const std::string uri = message["params"]["textDocument"].value("uri", "");
        sendResponse(
            id, textDocumentDiagnosticReport(uri, message.value("params", json::object())));
    } else if (method == "workspace/didChangeWorkspaceFolders") {
        updateWorkspaceRootsFromChange(message.value("params", json::object()));
        clearWorkspaceIndexes();
    } else if (method == "workspace/didChangeConfiguration") {
        lspLintSettings = Nelson::Lsp::lintSettingsFromChangeConfigurationParams(
            message.value("params", json::object()));
        clearWorkspaceIndexes();
        publishDiagnosticsForOpenDocuments();
    } else if (method == "workspace/diagnostic") {
        sendResponse(id,
            workspaceDiagnosticReportWithPartialProgress(message.value("params", json::object())));
    }
}
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
int
main()
{
    while (!Nelson::Lsp::shouldExit && std::cin.good()) {
        std::string body = Nelson::Lsp::readMessage();
        if (body.empty()) {
            break;
        }
        try {
            Nelson::Lsp::handleMessage(Nelson::Lsp::json::parse(body));
        } catch (...) {
        }
    }
    return 0;
}
//=============================================================================
