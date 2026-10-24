//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiRpc.hpp"
#include "ExamplesBridge.hpp"
#include "ExamplesCatalogHost.hpp"
#include "WebToolsHost.hpp"
#include "UiHtmlFileHandler.hpp"
#include "WebGuiJson.hpp"
#include "WebGuiServer.hpp"
#include "WebGuiTerminal.hpp"
#include "MainThreadTaskQueue.hpp"
#include "WebGuiPreferences.hpp"
#include "CompleterHelper.hpp"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "RecentDirectories.hpp"
#include "DialogDirectories.hpp"
#include "ModulesManager.hpp"
#include "DynamicLibrary.hpp"
#include "characters_encoding.hpp"
#include "Evaluator.hpp"
#include "Context.hpp"
#include "Scope.hpp"
#include "ArrayOf.hpp"
#include "ClassToString.hpp"
#include "NelsonHistory.hpp"
#include "HistoryManager.hpp"
#include "SystemCommand.hpp"
#include "GuiDialogProvider.hpp"
#include "DebugStack.hpp"
#include "PositionScript.hpp"
#include "EvaluateCommand.hpp"
#include "CodeAnalyzer.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "Profiler.hpp"
#include "PackageUpdatesCount.hpp"
#include "Nelson_VERSION.h"
#include <string>
#include <vector>
#include <filesystem>
#include <algorithm>
#include <fstream>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::filesystem::path
pathFromUtf8(const std::string& value)
{
#ifdef _WIN32
    return std::filesystem::path(utf8_to_wstring(value));
#else
    return std::filesystem::path(value);
#endif
}
//=============================================================================
static std::string
pathToUtf8(const std::filesystem::path& value)
{
    // Generic (forward-slash) form on every platform: cwd, recentDirectories and
    // the entries given to addRecentDirectory share one separator convention.
    // RecentDirectoriesStore persists this form, so anything else diverges on
    // Windows.
    return value.generic_u8string();
}
//=============================================================================
static std::string
pathSeparatorUtf8()
{
    // Must match the generic form returned by pathToUtf8.
    return std::string("/");
}
//=============================================================================
static nlohmann::json
recentDirectoriesJson()
{
    nlohmann::json result = nlohmann::json::array();
    for (const std::wstring& directory : getRecentDirectories()) {
        result.push_back(wstring_to_utf8(directory));
    }
    return result;
}
//=============================================================================
static std::wstring
fileDialogFilter(const nlohmann::json& params)
{
    std::wstring pattern;
    if (params.contains("filters") && params["filters"].is_array()) {
        for (const auto& filter : params["filters"]) {
            if (!filter.is_string()) {
                continue;
            }
            std::string extension = filter.get<std::string>();
            if (extension.empty()) {
                continue;
            }
            if (extension.front() == '.') {
                extension = "*" + extension;
            } else if (extension.front() != '*') {
                extension = "*." + extension;
            }
            if (!pattern.empty()) {
                pattern += L" ";
            }
            pattern += utf8_to_wstring(extension);
        }
    }
    return pattern.empty() ? L"All Files (*)" : L"Files (" + pattern + L");;All Files (*)";
}
//=============================================================================
static std::string
fileUrlFromPath(const std::filesystem::path& path)
{
    std::string p = path.generic_string();
#ifdef _WIN32
    std::string url = "file:///";
#else
    std::string url = "file://";
#endif
    for (unsigned char c : p) {
        if (c <= ' ' || c == '%' || c == '#' || c == '?') {
            constexpr char hex[] = "0123456789ABCDEF";
            url += '%';
            url += hex[(c >> 4) & 0x0F];
            url += hex[c & 0x0F];
        } else {
            url += static_cast<char>(c);
        }
    }
    return url;
}
//=============================================================================
using RunFileDialogFn = bool (*)(void*, const GuiDialogRequest&, GuiDialogResponse&);
//=============================================================================
static RunFileDialogFn
resolveRunFileDialog()
{
    static RunFileDialogFn fn = nullptr;
    static bool tried = false;
    if (tried) {
        return fn;
    }
    tried = true;
    const std::wstring libName = L"libnlsGui" + get_dynamic_library_extensionW();
    const std::wstring dir = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
    library_handle handle = load_dynamic_libraryW(dir + L"/" + libName);
    if (handle == nullptr) {
        return nullptr;
    }
    fn = reinterpret_cast<RunFileDialogFn>(get_function(handle, "nlsGuiRunFileDialog"));
    return fn;
}
//=============================================================================
WebGuiRpc::WebGuiRpc(WebGuiServer* server, WebGuiTerminal* terminal)
    : m_server(server), m_terminal(terminal)
{
    registerDialogProvider();
    // With a desktop (a terminal is served), the web tools (examples gallery,
    // package manager) show as docked panels and their events reach the desktop.
    // The figures-only server (advanced CLI --webview) has no desktop to host
    // them, so the tools keep their own windows there.
    if (terminal != nullptr) {
        Nelson::WebToolHost host;
        host.show = [server](const std::string& tool, const std::string& argsJson) {
            nlohmann::json args = nlohmann::json::parse(argsJson, nullptr, false);
            server->pushJson("tool.show",
                { { "tool", tool },
                    { "args", args.is_object() ? args : nlohmann::json::object() } });
            return true;
        };
        host.push = [server](const std::string& tool, const std::string& channel,
                        const std::string& payloadJson) {
            nlohmann::json payload = nlohmann::json::parse(payloadJson, nullptr, false);
            server->pushJson("tool.event",
                { { "tool", tool }, { "channel", channel },
                    { "payload",
                        payload.is_discarded() ? nlohmann::json(payloadJson) : payload } });
        };
        Nelson::SetWebToolHost(host);
        m_hostsWebTools = true;
    }
}
//=============================================================================
WebGuiRpc::~WebGuiRpc()
{
    if (m_hostsWebTools) {
        Nelson::SetWebToolHost(Nelson::WebToolHost());
    }
}
//=============================================================================
void
WebGuiRpc::setEvaluator(Evaluator* eval)
{
    m_evaluator = eval;
}
//=============================================================================
bool
WebGuiRpc::runOnInterpreterThread(const std::function<void()>& fn, int timeoutMs)
{
    // While the terminal is parked at the prompt the interpreter is idle, so the
    // figure interaction runs in place (no prompt wake). During a running command
    // it is queued and drained at the next statement boundary.
    return MainThreadTaskQueue::getInstance()->runNowIfIdleOrQueue(
        [fn](Evaluator*) { fn(); }, timeoutMs);
}
//=============================================================================
std::string
WebGuiRpc::dispatch(const std::string& envelope)
{
    nlohmann::json response;
    nlohmann::json id = nullptr;
    try {
        const nlohmann::json request = nlohmann::json::parse(envelope);
        id = request.value("id", nlohmann::json(nullptr));
        const std::string method = request.value("method", std::string());
        const nlohmann::json params = request.value("params", nlohmann::json::object());
        response["id"] = id;
        response["result"] = handle(method, params);
    } catch (const std::exception& e) {
        response["id"] = id;
        response["error"] = { { "message", e.what() } };
    }
    return serializeWebGuiJson(response);
}
//=============================================================================
nlohmann::json
WebGuiRpc::handle(const std::string& method, const nlohmann::json& params)
{
    if (method == "ping") {
        return ping();
    }
    if (method == "app.heartbeat") {
        m_server->heartbeat();
        return { { "ok", true } };
    }
    if (method == "app.capabilities") {
        return appCapabilities();
    }
    if (method == "diagnostics.info") {
        return diagnosticsInfo();
    }
    if (method == "prefs.load") {
        return preferencesLoad();
    }
    if (method == "prefs.save") {
        return preferencesSave(params);
    }
    if (method == "term.input") {
        return termInput(params);
    }
    if (method == "term.interrupt") {
        return termInterrupt();
    }
    if (method == "term.resize") {
        return termResize(params);
    }
    if (method == "term.sync") {
        // No WebGuiServer::pushJson from a WebUI RPC callback: WebUI may hold its
        // socket mutex while dispatching, and re-entering webui_run/webui_send_all
        // can deadlock the first browser connection. The prompt is returned as
        // RPC data instead.
        return { { "ok", true },
            { "prompt",
                m_terminal != nullptr ? wstring_to_utf8(m_terminal->currentPrompt())
                                      : std::string() } };
    }
    if (method == "completion.request") {
        return completionRequest(params);
    }
    if (method == "figure.action") {
        return figureAction(params);
    }
    if (method == "figure.list") {
        return figureList();
    }
    if (method == "figure.close") {
        return figureClose(params);
    }
    if (method == "figure.getImage") {
        return figureGetImage(params);
    }
    if (method == "notebook.run") {
        return notebookRun(params);
    }
    if (method == "notebook.closeFigures") {
        return notebookCloseFigures(params);
    }
    if (method == "notebook.releaseFigure") {
        return notebookReleaseFigure(params);
    }
    if (method == "figure.saveAs") {
        return figureSaveAs(params);
    }
    if (method == "figure.getDisplayList") {
        return figureGetDisplayList(params);
    }
    if (method == "figure.setView") {
        return figureSetView(params);
    }
    if (method == "figure.pan") {
        return figurePan(params);
    }
    if (method == "figure.setSize") {
        return figureSetSize(params);
    }
    if (method == "figure.setPosition") {
        return figureSetPosition(params);
    }
    if (method == "figure.setWindowSize") {
        return figureSetWindowSize(params);
    }
    if (method == "figure.keyEvent") {
        return figureKeyEvent(params);
    }
    if (method == "figure.mouseEvent") {
        return figureMouseEvent(params);
    }
    if (method == "uicontrol.action") {
        return uicontrolAction(params);
    }
    if (method == "uihtml.pull") {
        return uihtmlPull(params);
    }
    if (method == "figure.undock") {
        return figureUndock(params);
    }
    if (method == "figure.dock") {
        return figureDock(params);
    }
    if (method == "workspace.list") {
        return workspaceList();
    }
    if (method == "history.list") {
        return historyList();
    }
    if (method == "history.remove") {
        return historyRemove(params);
    }
    if (method == "history.clear") {
        return historyClear();
    }
    if (method == "fs.list") {
        return fsList(params);
    }
    if (method == "fs.cwd") {
        return fsCwd();
    }
    if (method == "fs.chdir") {
        return fsChdir(params);
    }
    if (method == "fs.dialog.start") {
        return fsDialogStart(params);
    }
    if (method == "fs.dialog.native") {
        return fsDialogNative(params);
    }
    if (method == "fs.dialog.accept") {
        return fsDialogAccept(params);
    }
    if (method == "fs.create") {
        return fsCreate(params);
    }
    if (method == "fs.rename") {
        return fsRename(params);
    }
    if (method == "fs.delete") {
        return fsDelete(params);
    }
    if (method == "path.list") {
        return pathList();
    }
    if (method == "help.text") {
        return helpText(params);
    }
    if (method == "examples.list") {
        return examplesList();
    }
    if (method == "examples.refresh") {
        return examplesList();
    }
    if (method == "examples.run") {
        return examplesRun(params);
    }
    if (method == "examples.open") {
        return examplesOpen(params);
    }
    if (method == "tool.dispatch") {
        return toolDispatch(params);
    }
    if (method == "profile.info") {
        return profileInfo();
    }
    if (method == "profile.control") {
        return profileControl(params);
    }
    if (method == "profile.export") {
        return profileExport(params);
    }
    if (method == "newsfeed.list") {
        return newsfeedList(params);
    }
    if (method == "fs.upload") {
        return fsUpload(params);
    }
    if (method == "fs.download") {
        return fsDownload(params);
    }
    if (method == "var.open") {
        return varOpen(params);
    }
    if (method == "var.getBlock") {
        return varGetBlock(params);
    }
    if (method == "var.setCell") {
        return varSetCell(params);
    }
    if (method == "var.replace") {
        return varReplace(params);
    }
    if (method == "var.setNested") {
        return varSetNested(params);
    }
    if (method == "shell.run") {
        return shellRun(params);
    }
    if (method == "editor.open") {
        return editorOpen(params);
    }
    if (method == "editor.save") {
        return editorSave(params);
    }
    if (method == "code.analyze") {
        return codeAnalyze(params);
    }
    if (method == "debugger.getBreakpoints") {
        return debuggerGetBreakpoints(params);
    }
    if (method == "debugger.toggleBreakpoint") {
        return debuggerToggleBreakpoint(params);
    }
    if (method == "dialog.reply") {
        return dialogReply(params);
    }
    if (method == "debugger.state") {
        return debuggerState();
    }
    throw std::runtime_error("method not implemented in backend yet: " + method);
}
//=============================================================================
nlohmann::json
WebGuiRpc::ping()
{
    return { { "pong", true }, { "backend", "engine" },
        { "version", std::string(NELSON_VERSION_STRING) + " (" + NELSON_RELEASE_NAME + ")" } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::appCapabilities()
{
    return { { "nflow", IsConfiguredModuleName(L"nflow_gui") },
        { "packageManager", IsConfiguredModuleName(L"nmm_gui") },
        { "packageUpdates", getPackageUpdatesCount() },
        { "launchMode", WebDesktopBridge::getInstance()->getWebLaunchMode() } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::diagnosticsInfo()
{
    return { { "backend",
                 { { "name", "engine" },
                     { "version",
                         std::string(NELSON_VERSION_STRING) + " (" + NELSON_RELEASE_NAME + ")" },
                     { "launchMode", WebDesktopBridge::getInstance()->getWebLaunchMode() } } },
        { "runtime", { { "name", "native" } } },
        { "delivery",
            { { "files", 0 }, { "totalBytes", 0 }, { "startupBytes", 0 },
                { "onDemandBytes", 0 } } },
        { "capabilities",
            { { "modules", { "web_gui" } }, { "capabilities", { "plots", "profiler", "help" } },
                { "features",
                    { { "nflow", IsConfiguredModuleName(L"nflow_gui") },
                        { "packageManager", IsConfiguredModuleName(L"nmm_gui") },
                        { "plots", true } } },
                { "limits", nlohmann::json::object() },
                { "unavailable", nlohmann::json::array() } } },
        { "persistence",
            { { "commandHistory", true }, { "preferencesPath", "" }, { "workspacePath", "" } } } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::preferencesLoad()
{
    return loadWebGuiPreferences();
}
//=============================================================================
nlohmann::json
WebGuiRpc::preferencesSave(const nlohmann::json& params)
{
    if (params.value("schema", 0) != 1 || !params.contains("values")) {
        throw std::runtime_error("invalid desktop preferences schema");
    }
    saveWebGuiPreferences(params["values"]);
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::termInput(const nlohmann::json& params)
{
    const std::string line = params.value("line", std::string());
    if (m_terminal == nullptr) {
        // Figures-only mode (nelson-adv-cli --webview): no browser console.
        return { { "accepted", false } };
    }
    m_terminal->pushInputLine(utf8_to_wstring(line));
    return { { "accepted", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::termInterrupt()
{
    // Ctrl+C: raise the interrupt flag the evaluator polls while a computation
    // runs; at the prompt, discard the current input line so Nelson re-emits it.
    if (m_terminal != nullptr && m_terminal->isAtPrompt()) {
        m_terminal->interruptGetLineByEvent();
    } else {
        NelsonConfiguration::getInstance()->setInterruptPending(true);
    }
    return { { "interrupted", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::termResize(const nlohmann::json& params)
{
    if (m_terminal != nullptr) {
        m_terminal->setTerminalSize(
            params.value("cols", size_t { 80 }), params.value("rows", size_t { 25 }));
    }
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::completionRequest(const nlohmann::json& params)
{
    // Frontend already truncates the line at the cursor.
    const std::wstring line = utf8_to_wstring(params.value("line", std::string()));

    std::wstring completionPrefix;
    wstringVector files, builtin, macros, variables, fields, properties, methods;
    // Field/property/method completion (`a.Vis` -> Visible) resolves the object
    // from the current scope, so it runs on the interpreter thread. Without an
    // evaluator, fall back to the context-free overload.
    if (m_evaluator != nullptr) {
        runOnInterpreterThread([&] {
            computeCompletion(m_evaluator, line, completionPrefix, files, builtin, macros,
                variables, fields, properties, methods);
        });
    } else {
        computeCompletion(
            line, completionPrefix, files, builtin, macros, variables, fields, properties, methods);
    }

    auto toUtf8 = [](const wstringVector& v) {
        nlohmann::json a = nlohmann::json::array();
        for (const auto& s : v) {
            a.push_back(wstring_to_utf8(s));
        }
        return a;
    };

    // The completion popup groups object members with builtins.
    nlohmann::json builtins = toUtf8(builtin);
    for (const auto& s : fields) {
        builtins.push_back(wstring_to_utf8(s));
    }
    for (const auto& s : properties) {
        builtins.push_back(wstring_to_utf8(s));
    }
    for (const auto& s : methods) {
        builtins.push_back(wstring_to_utf8(s));
    }

    return {
        { "prefix", wstring_to_utf8(completionPrefix) },
        { "builtin", builtins },
        { "macros", toUtf8(macros) },
        { "variables", toUtf8(variables) },
        { "files", toUtf8(files) },
    };
}
//=============================================================================
bool
WebGuiRpc::ensureGraphicsBridge()
{
    if (m_graphicsBridgeTried) {
        return m_renderPngFn != nullptr;
    }
    m_graphicsBridgeTried = true;
    const std::wstring libName = L"libnlsGraphics" + get_dynamic_library_extensionW();
    const std::wstring dir = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
    library_handle handle = load_dynamic_libraryW(dir + L"/" + libName);
    if (handle == nullptr) {
        return false;
    }
    m_listFiguresFn = reinterpret_cast<ListFiguresFn>(get_function(handle, "nlsWebGuiListFigures"));
    m_closeFigureFn = reinterpret_cast<CloseFigureFn>(get_function(handle, "nlsWebGuiCloseFigure"));
    m_clearCurrentFigureFn = reinterpret_cast<ClearCurrentFigureFn>(
        get_function(handle, "nlsWebGuiClearCurrentFigure"));
    m_renderPngFn
        = reinterpret_cast<RenderPngFn>(get_function(handle, "nlsWebGuiRenderFigurePngBase64"));
    m_displayListFn = reinterpret_cast<RenderDisplayListFn>(
        get_function(handle, "nlsWebGuiRenderFigureDisplayList"));
    m_displayListNoFallbackFn = reinterpret_cast<RenderDisplayListFn>(
        get_function(handle, "nlsWebGuiRenderFigureDisplayListNoFallback"));
    m_displayListAnimationFrameFn = reinterpret_cast<RenderDisplayListFn>(
        get_function(handle, "nlsWebGuiRenderFigureAnimationFrame"));
    m_displayListInteractiveFrameFn = reinterpret_cast<RenderDisplayListFn>(
        get_function(handle, "nlsWebGuiRenderFigureInteractiveFrame"));
    m_markViewOnlyFrameFn = reinterpret_cast<MarkFigureFrameFn>(
        get_function(handle, "nlsWebGuiMarkFigureViewOnlyFrame"));
    m_setAxesViewFn = reinterpret_cast<SetAxesViewFn>(get_function(handle, "nlsWebGuiSetAxesView"));
    m_panAxesFn = reinterpret_cast<PanAxesFn>(get_function(handle, "nlsWebGuiPanAxes"));
    m_setFigureSizeFn
        = reinterpret_cast<SetFigureSizeFn>(get_function(handle, "nlsWebGuiSetFigureSize"));
    m_uiControlActionFn
        = reinterpret_cast<UIControlActionFn>(get_function(handle, "nlsWebGuiUIControlAction"));
    m_uiHtmlPullFn = reinterpret_cast<UiHtmlPullFn>(get_function(handle, "nlsWebGuiUiHtmlPull"));
    m_figureKeyEventFn
        = reinterpret_cast<FigureKeyEventFn>(get_function(handle, "nlsWebGuiFigureKeyEvent"));
    m_figureMouseEventFn
        = reinterpret_cast<FigureMouseEventFn>(get_function(handle, "nlsWebGuiFigureMouseEvent"));
    m_getWindowStyleFn
        = reinterpret_cast<GetWindowStyleFn>(get_function(handle, "nlsWebGuiGetFigureWindowStyle"));
    m_getFigureSizeFn
        = reinterpret_cast<GetFigureSizeFn>(get_function(handle, "nlsWebGuiGetFigureSize"));
    m_setFigurePositionFn
        = reinterpret_cast<SetFigurePositionFn>(get_function(handle, "nlsWebGuiSetFigurePosition"));
    m_getFigureTitleFn
        = reinterpret_cast<GetFigureTitleFn>(get_function(handle, "nlsWebGuiGetFigureTitle"));
    // The figure-export bridge lives in nlsGraphics_io (it needs ExportGraphics,
    // which nlsGraphics cannot depend on). Optional.
    const std::wstring ioLibName = L"libnlsGraphics_io" + get_dynamic_library_extensionW();
    library_handle ioHandle = load_dynamic_libraryW(dir + L"/" + ioLibName);
    if (ioHandle != nullptr) {
        m_saveFigureFn
            = reinterpret_cast<SaveFigureFn>(get_function(ioHandle, "nlsWebGuiSaveFigure"));
    }
    return m_renderPngFn != nullptr;
}
//=============================================================================
void
WebGuiRpc::registerDialogProvider()
{
    // Resolved dynamically so nlsWeb_gui never links Qt.
    const std::wstring libName = L"libnlsGui" + get_dynamic_library_extensionW();
    const std::wstring dir = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
    library_handle handle = load_dynamic_libraryW(dir + L"/" + libName);
    if (handle == nullptr) {
        return;
    }
    using RegisterFn = void (*)(GuiDialogProviderRawFn, void*);
    RegisterFn registerFn
        = reinterpret_cast<RegisterFn>(get_function(handle, "nlsGuiRegisterDialogProvider"));
    if (registerFn != nullptr) {
        registerFn(&WebGuiRpc::dialogProviderThunk, this);
    }
}
//=============================================================================
bool
WebGuiRpc::dialogProviderThunk(
    void* context, const GuiDialogRequest& request, GuiDialogResponse& response)
{
    return static_cast<WebGuiRpc*>(context)->serveDialog(request, response);
}
//=============================================================================
static std::string
toUtf8(const std::wstring& text)
{
    return wstring_to_utf8(text);
}
//=============================================================================
bool
WebGuiRpc::serveDialog(const GuiDialogRequest& request, GuiDialogResponse& response)
{
    GuiDialogRequest resolvedRequest = request;
    if (request.kind == L"file") {
        const std::wstring key = request.historyKey.empty() ? L"desktop."
                + (request.fileMode == L"dir" ? std::wstring(L"directory") : request.fileMode)
                                                            : request.historyKey;
        std::error_code ec;
        resolvedRequest.historyKey = key;
        resolvedRequest.defaultPath = resolveDialogDirectory(key, request.defaultPath,
            utf8_to_wstring(pathToUtf8(std::filesystem::current_path(ec))));
    }
    if (request.kind == L"file"
        && WebDesktopBridge::getInstance()->getWebLaunchMode() == "webview") {
        RunFileDialogFn runDialog = resolveRunFileDialog();
        if (runDialog != nullptr && runDialog(nullptr, resolvedRequest, response)) {
            return true;
        }
    }

    if (request.kind == L"progress") {
        m_server->pushJson("dialog.progress",
            nlohmann::json {
                { "id", request.progressId },
                { "visible", request.progressVisible },
                { "title", toUtf8(request.title) },
                { "message", toUtf8(request.message) },
                { "value", request.progressValue },
                { "minimum", request.progressMinimum },
                { "maximum", request.progressMaximum },
                { "indeterminate", request.progressIndeterminate },
            });
        return true;
    }

    long long id = 0;
    {
        std::lock_guard<std::mutex> lock(m_dialogMutex);
        id = ++m_dialogSeq;
    }
    nlohmann::json payload;
    payload["id"] = id;
    payload["kind"] = toUtf8(request.kind);
    payload["title"] = toUtf8(request.title);
    payload["message"] = toUtf8(request.message);
    payload["icon"] = toUtf8(request.icon);
    payload["defaultButton"] = toUtf8(request.defaultButton);
    nlohmann::json fields = nlohmann::json::array();
    for (const GuiDialogField& field : request.fields) {
        fields.push_back({ { "prompt", toUtf8(field.prompt) }, { "value", toUtf8(field.value) },
            { "lines", field.lines } });
    }
    payload["fields"] = fields;
    nlohmann::json buttons = nlohmann::json::array();
    for (const std::wstring& button : request.buttons) {
        buttons.push_back(toUtf8(button));
    }
    payload["buttons"] = buttons;
    nlohmann::json buttonValues = nlohmann::json::array();
    for (const std::wstring& buttonValue : request.buttonValues) {
        buttonValues.push_back(toUtf8(buttonValue));
    }
    payload["buttonValues"] = buttonValues;
    nlohmann::json extraButtons = nlohmann::json::array();
    for (const std::wstring& extraButton : request.extraButtons) {
        extraButtons.push_back(toUtf8(extraButton));
    }
    payload["extraButtons"] = extraButtons;
    payload["checkboxVisible"] = request.checkboxVisible;
    payload["checkboxText"] = toUtf8(request.checkboxText);
    payload["checkboxState"] = request.checkboxState;
    payload["helpString"] = toUtf8(request.helpString);
    payload["helpFcn"] = toUtf8(request.helpFcn);
    // kind == "list"
    nlohmann::json items = nlohmann::json::array();
    for (const std::wstring& item : request.items) {
        items.push_back(toUtf8(item));
    }
    payload["items"] = items;
    payload["multiSelect"] = request.multiSelect;
    nlohmann::json initial = nlohmann::json::array();
    for (int idx : request.initialSelection) {
        initial.push_back(idx);
    }
    payload["initialSelection"] = initial;
    // kind == "file"
    payload["fileMode"] = toUtf8(resolvedRequest.fileMode);
    nlohmann::json filters = nlohmann::json::array();
    for (const std::wstring& f : resolvedRequest.fileFilters) {
        filters.push_back(toUtf8(f));
    }
    payload["fileFilters"] = filters;
    payload["defaultPath"] = toUtf8(resolvedRequest.defaultPath);
    payload["suggestedName"] = toUtf8(resolvedRequest.suggestedName);
    payload["historyKey"] = toUtf8(resolvedRequest.historyKey);
    // kind == "font"
    payload["fontFamily"] = toUtf8(request.fontFamily);
    payload["fontSize"] = request.fontSize;
    payload["fontBold"] = request.fontBold;
    payload["fontItalic"] = request.fontItalic;
    m_server->pushJson("dialog.show", payload);

    nlohmann::json reply;
    {
        std::unique_lock<std::mutex> lock(m_dialogMutex);
        m_dialogCv.wait(lock, [&] { return m_dialogReplies.count(id) > 0; });
        reply = m_dialogReplies[id];
        m_dialogReplies.erase(id);
    }
    response.ok = reply.value("ok", false);
    response.button = utf8_to_wstring(reply.value("button", std::string()));
    response.checkboxChecked = reply.value("checkboxChecked", false);
    response.values.clear();
    if (reply.contains("values") && reply["values"].is_array()) {
        for (const auto& value : reply["values"]) {
            response.values.push_back(utf8_to_wstring(value.get<std::string>()));
        }
    }
    response.selectedIndices.clear();
    if (reply.contains("selectedIndices") && reply["selectedIndices"].is_array()) {
        for (const auto& idx : reply["selectedIndices"]) {
            if (idx.is_number()) {
                response.selectedIndices.push_back(idx.get<int>());
            }
        }
    }
    response.path = utf8_to_wstring(reply.value("path", std::string()));
    if (request.kind == L"file" && response.ok && !response.path.empty()) {
        const std::filesystem::path selected = pathFromUtf8(toUtf8(response.path));
        const std::filesystem::path directory
            = request.fileMode == L"dir" ? selected : selected.parent_path();
        if (!directory.empty()) {
            setDialogDirectory(resolvedRequest.historyKey, utf8_to_wstring(pathToUtf8(directory)));
        }
    }
    return true;
}
//=============================================================================
nlohmann::json
WebGuiRpc::dialogReply(const nlohmann::json& params)
{
    // Served on the civetweb server thread: the interpreter thread is blocked in
    // serveDialog, so this must not marshal onto it. Just hand over the reply.
    long long id = params.value("id", 0LL);
    {
        std::lock_guard<std::mutex> lock(m_dialogMutex);
        m_dialogReplies[id] = params;
    }
    m_dialogCv.notify_all();
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::workspaceList()
{
    nlohmann::json variables = nlohmann::json::array();
    if (m_evaluator == nullptr) {
        return { { "scope", "base" }, { "variables", variables } };
    }
    runOnInterpreterThread([&] {
        Context* context = m_evaluator->getContext();
        if (context == nullptr) {
            return;
        }
        Scope* scope = context->getCurrentScope();
        if (scope == nullptr) {
            return;
        }
        wstringVector names;
        scope->getVariablesList(false, names);
        for (const std::wstring& name : names) {
            // Isolate each variable: a property accessor that throws (e.g.
            // byteSize() on a sparse array) must not drop the rest of the list.
            try {
                ArrayOf* v = context->lookupVariable(wstring_to_utf8(name));
                if (v == nullptr) {
                    continue;
                }
                Dimensions dims = v->dimensions();
                NelsonType cls = v->dataClass();
                std::wstring className = ClassToStringW(cls);
                if (cls == NLS_HANDLE) {
                    className = utf8_to_wstring(v->handleCategory());
                } else if (cls == NLS_CLASS_ARRAY || cls == NLS_CLASSDEF_ARRAY) {
                    className = utf8_to_wstring(v->classType());
                }
                if (v->isSparse()) {
                    className = L"sparse " + className;
                }
                // byteSize() throws for sparse arrays; report 0 rather than
                // aborting the enumeration.
                long long bytes = v->isSparse() ? 0LL : (long long)v->byteSize();
                variables.push_back({
                    { "name", wstring_to_utf8(name) },
                    { "className", wstring_to_utf8(className) },
                    { "size",
                        nlohmann::json::array(
                            { (long long)dims.rows(), (long long)dims.columns() }) },
                    { "bytes", bytes },
                    { "value", wstring_to_utf8(dims.toWideString() + L" " + className) },
                });
            } catch (...) {
                // Nelson's Error() throws its own Exception type (not
                // std::exception): keep listing the others regardless.
                variables.push_back({ { "name", wstring_to_utf8(name) }, { "className", "?" },
                    { "size", nlohmann::json::array({ 0, 0 }) }, { "bytes", 0 },
                    { "value", "unavailable" } });
            }
        }
    });
    return { { "scope", "base" }, { "variables", variables } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::historyList()
{
    nlohmann::json entries = nlohmann::json::array();
    runOnInterpreterThread([&] {
        for (const std::wstring& line : Nelson::History::get()) {
            entries.push_back(wstring_to_utf8(line));
        }
    });
    return { { "entries", entries } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::historyClear()
{
    runOnInterpreterThread([&] {
        auto* hm = static_cast<Nelson::HistoryManager*>(
            NelsonConfiguration::getInstance()->getHistoryManager());
        if (hm != nullptr) {
            hm->clear(false);
        }
    });
    m_server->pushJson("history.changed", nlohmann::json::object());
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::historyRemove(const nlohmann::json& params)
{
    const long long index = params.value("index", (long long)-1);
    if (index < 0) {
        return { { "error", { { "message", "index required" } } } };
    }
    runOnInterpreterThread([&] {
        auto* hm = static_cast<Nelson::HistoryManager*>(
            NelsonConfiguration::getInstance()->getHistoryManager());
        if (hm != nullptr) {
            hm->remove(static_cast<size_t>(index));
        }
    });
    m_server->pushJson("history.changed", nlohmann::json::object());
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsCwd()
{
    std::error_code ec;
    std::filesystem::path cwd = std::filesystem::current_path(ec);
    if (!ec) {
        // Canonicalize the way RecentDirectoriesStore does before storing, so the
        // returned cwd is byte-for-byte identical to recentDirectories[0].
        std::error_code canonicalEc;
        std::filesystem::path canonical = std::filesystem::weakly_canonical(cwd, canonicalEc);
        cwd = canonicalEc ? cwd.lexically_normal() : canonical;
        addRecentDirectory(utf8_to_wstring(pathToUtf8(cwd)));
    }
    return { { "cwd", ec ? std::string(".") : pathToUtf8(cwd) },
        { "separator", pathSeparatorUtf8() }, { "recentDirectories", recentDirectoriesJson() } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsChdir(const nlohmann::json& params)
{
    const std::string pathString = params.value("path", std::string());
    if (pathString.empty()) {
        throw std::runtime_error("directory path required");
    }

    std::filesystem::path resolved;
    std::string failure;
    const bool completed = m_terminal->runOnInterpreterThread([&] {
        std::error_code ec;
        std::filesystem::path candidate = pathFromUtf8(pathString);
        if (!candidate.is_absolute()) {
            candidate = std::filesystem::absolute(candidate, ec);
        }
        if (ec || !std::filesystem::exists(candidate, ec)
            || !std::filesystem::is_directory(candidate, ec)) {
            failure = "directory does not exist";
            return;
        }
        resolved = std::filesystem::weakly_canonical(candidate, ec);
        if (ec) {
            resolved = candidate.lexically_normal();
            ec.clear();
        }
        std::filesystem::current_path(resolved, ec);
        if (ec) {
            failure = "unable to change current directory";
            return;
        }
        // Keep the function index in sync, as after a 'cd' at the prompt.
        PathFunctionIndexerManager::getInstance()->setCurrentUserPath(
            utf8_to_wstring(pathToUtf8(resolved)));
    });
    if (!completed) {
        throw std::runtime_error("interpreter is busy");
    }
    if (!failure.empty()) {
        throw std::runtime_error(failure);
    }
    addRecentDirectory(utf8_to_wstring(pathToUtf8(resolved)));
    return { { "cwd", pathToUtf8(resolved) }, { "separator", pathSeparatorUtf8() },
        { "recentDirectories", recentDirectoriesJson() } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsDialogStart(const nlohmann::json& params)
{
    const std::string key = params.value("historyKey", std::string());
    if (key.rfind("desktop.", 0) != 0) {
        throw std::runtime_error("invalid dialog history key");
    }
    std::error_code ec;
    const std::string fallback = pathToUtf8(std::filesystem::current_path(ec));
    const std::wstring directory = resolveDialogDirectory(utf8_to_wstring(key),
        utf8_to_wstring(params.value("defaultPath", std::string())),
        ec ? std::wstring() : utf8_to_wstring(fallback));
    return { { "directory", wstring_to_utf8(directory) } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsDialogNative(const nlohmann::json& params)
{
    if (WebDesktopBridge::getInstance()->getWebLaunchMode() != "webview") {
        return { { "available", false }, { "reason", "not_webview" } };
    }

    const std::string mode = params.value("mode", std::string("open"));
    if (mode != "open" && mode != "save" && mode != "dir") {
        throw std::runtime_error("invalid native dialog mode");
    }
    const std::string defaultKey = "desktop." + (mode == "dir" ? std::string("directory") : mode);
    const std::string historyKey = params.value("historyKey", defaultKey);
    if (historyKey.rfind("desktop.", 0) != 0) {
        throw std::runtime_error("invalid dialog history key");
    }

    RunFileDialogFn runDialog = resolveRunFileDialog();
    if (runDialog == nullptr) {
        return { { "available", false }, { "reason", "qt_bridge_unavailable" } };
    }

    GuiDialogRequest request;
    request.kind = L"file";
    request.fileMode = utf8_to_wstring(mode);
    request.title = utf8_to_wstring(params.value("title", std::string()));
    request.defaultPath = utf8_to_wstring(params.value("defaultPath", std::string()));
    request.suggestedName = utf8_to_wstring(params.value("suggestedName", std::string()));
    request.historyKey = utf8_to_wstring(historyKey);
    request.fileFilters.push_back(fileDialogFilter(params));

    GuiDialogResponse response;
    if (!runDialog(nullptr, request, response)) {
        return { { "available", false }, { "reason", "qt_dialog_unavailable" } };
    }
    if (!response.ok || response.path.empty()) {
        return { { "available", true }, { "canceled", true } };
    }
    return { { "available", true }, { "path", wstring_to_utf8(response.path) } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsDialogAccept(const nlohmann::json& params)
{
    const std::string key = params.value("historyKey", std::string());
    const std::string mode = params.value("mode", std::string());
    const std::string value = params.value("path", std::string());
    if (key.rfind("desktop.", 0) != 0 || value.empty()) {
        throw std::runtime_error("invalid dialog history update");
    }
    const std::filesystem::path selected = pathFromUtf8(value);
    const std::filesystem::path directory = mode == "dir" ? selected : selected.parent_path();
    return { { "ok",
        !directory.empty()
            && setDialogDirectory(utf8_to_wstring(key), utf8_to_wstring(pathToUtf8(directory))) } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsList(const nlohmann::json& params)
{
    std::string pathStr = params.value("path", std::string());
    std::error_code ec;
    std::filesystem::path dir
        = pathStr.empty() ? std::filesystem::current_path(ec) : std::filesystem::path(pathStr);

    nlohmann::json entries = nlohmann::json::array();
    for (std::filesystem::directory_iterator it(dir, ec), end; !ec && it != end; it.increment(ec)) {
        const std::filesystem::directory_entry& e = *it;
        std::error_code ec2;
        const bool isDir = e.is_directory(ec2);
        std::uintmax_t size = 0;
        if (!isDir) {
            size = e.file_size(ec2);
            if (ec2) {
                size = 0;
            }
        }
        entries.push_back({
            { "name", e.path().filename().generic_string() },
            { "path", e.path().generic_string() },
            { "isDir", isDir },
            { "size", (long long)size },
        });
    }
    std::sort(entries.begin(), entries.end(), [](const nlohmann::json& a, const nlohmann::json& b) {
        const bool da = a.value("isDir", false), db = b.value("isDir", false);
        if (da != db) {
            return da;
        }
        return a.value("name", std::string()) < b.value("name", std::string());
    });
    return { { "cwd", dir.generic_string() }, { "entries", entries } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsCreate(const nlohmann::json& params)
{
    const std::string pathStr = params.value("path", std::string());
    const std::string kind = params.value("kind", std::string("file")); // file|dir
    const std::string content = params.value("content", std::string());
    if (pathStr.empty()) {
        return { { "error", { { "message", "path required" } } } };
    }
    std::filesystem::path target(pathStr);
    std::error_code ec;
    if (std::filesystem::exists(target, ec)) {
        return { { "error", { { "message", "already exists" } } } };
    }
    if (kind == "dir") {
        std::filesystem::create_directories(target, ec);
    } else {
        std::filesystem::create_directories(target.parent_path(), ec);
        std::ofstream out(target.string(), std::ios::binary);
        if (!out) {
            return { { "error", { { "message", "cannot create file" } } } };
        }
        out << content;
    }
    return { { "ok", !ec }, { "path", target.generic_string() } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsRename(const nlohmann::json& params)
{
    const std::string from = params.value("from", std::string());
    const std::string to = params.value("to", std::string());
    if (from.empty() || to.empty()) {
        return { { "error", { { "message", "from and to required" } } } };
    }
    std::error_code ec;
    if (std::filesystem::exists(std::filesystem::path(to), ec)) {
        return { { "error", { { "message", "target already exists" } } } };
    }
    std::filesystem::rename(std::filesystem::path(from), std::filesystem::path(to), ec);
    return { { "ok", !ec }, { "path", std::filesystem::path(to).generic_string() } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsDelete(const nlohmann::json& params)
{
    const std::string pathStr = params.value("path", std::string());
    if (pathStr.empty()) {
        return { { "error", { { "message", "path required" } } } };
    }
    std::error_code ec;
    const std::uintmax_t removed = std::filesystem::remove_all(std::filesystem::path(pathStr), ec);
    return { { "ok", !ec && removed > 0 } };
}
//=============================================================================
namespace {
    static const char* const kB64
        = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    static std::string
    base64Encode(const std::string& in)
    {
        std::string out;
        out.reserve(((in.size() + 2) / 3) * 4);
        size_t i = 0;
        for (; i + 2 < in.size(); i += 3) {
            const unsigned n = (static_cast<unsigned char>(in[i]) << 16)
                | (static_cast<unsigned char>(in[i + 1]) << 8)
                | static_cast<unsigned char>(in[i + 2]);
            out.push_back(kB64[(n >> 18) & 63]);
            out.push_back(kB64[(n >> 12) & 63]);
            out.push_back(kB64[(n >> 6) & 63]);
            out.push_back(kB64[n & 63]);
        }
        if (i < in.size()) {
            unsigned n = static_cast<unsigned char>(in[i]) << 16;
            const bool two = (i + 1 < in.size());
            if (two) {
                n |= static_cast<unsigned char>(in[i + 1]) << 8;
            }
            out.push_back(kB64[(n >> 18) & 63]);
            out.push_back(kB64[(n >> 12) & 63]);
            out.push_back(two ? kB64[(n >> 6) & 63] : '=');
            out.push_back('=');
        }
        return out;
    }
    //=============================================================================
    static std::string
    base64Decode(const std::string& in)
    {
        int rev[256];
        for (int k = 0; k < 256; ++k) {
            rev[k] = -1;
        }
        for (int k = 0; k < 64; ++k) {
            rev[static_cast<unsigned char>(kB64[k])] = k;
        }
        std::string out;
        out.reserve((in.size() / 4) * 3);
        int val = 0, bits = -8;
        for (unsigned char c : in) {
            if (c == '=' || rev[c] == -1) {
                continue; // skip padding, whitespace, or a data: URI prefix's stray chars
            }
            val = (val << 6) | rev[c];
            bits += 6;
            if (bits >= 0) {
                out.push_back(static_cast<char>((val >> bits) & 0xFF));
                bits -= 8;
            }
        }
        return out;
    }
    //=============================================================================
} // namespace
//=============================================================================
// The ordered search path. The frontend changes it through the
// addpath/rmpath/savepath/restoredefaultpath builtins.
nlohmann::json
WebGuiRpc::pathList()
{
    nlohmann::json paths = nlohmann::json::array();
    runOnInterpreterThread([&] {
        // Built-in module paths under the Nelson root are hidden.
        auto slashes = [](std::wstring s) {
            for (wchar_t& c : s) {
                if (c == L'\\') {
                    c = L'/';
                }
            }
            return s;
        };
        const std::wstring root
            = slashes(NelsonConfiguration::getInstance()->getNelsonRootDirectory());
        for (const std::wstring& p :
            PathFunctionIndexerManager::getInstance()->getPathNameVector(false)) {
            if (!root.empty() && slashes(p).rfind(root, 0) == 0) {
                continue;
            }
            paths.push_back(wstring_to_utf8(p));
        }
    });
    return { { "paths", paths } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::toolDispatch(const nlohmann::json& params)
{
    // The tool answers {"result": ...} or {"error": {"message": ...}}.
    const std::string tool = params.value("tool", std::string());
    const std::string method = params.value("method", std::string());
    const nlohmann::json toolParams = params.value("params", nlohmann::json::object());
    const nlohmann::json response = nlohmann::json::parse(
        Nelson::DispatchWebTool(tool, method, toolParams.dump()), nullptr, false);
    if (!response.is_object()) {
        throw std::runtime_error("web tool " + tool + " returned an invalid response");
    }
    if (response.contains("error")) {
        throw std::runtime_error(response["error"].value("message", "web tool request failed"));
    }
    return response.value("result", nlohmann::json::object());
}
//=============================================================================
nlohmann::json
WebGuiRpc::examplesList()
{
    nlohmann::json response
        = nlohmann::json::parse(Nelson::BuildLoadedModulesExamplesCatalog().json);
    response["locale"] = Nelson::CurrentExamplesLocale();
    return response;
}
//=============================================================================
nlohmann::json
WebGuiRpc::examplesRun(const nlohmann::json& params)
{
    if (m_terminal == nullptr) {
        return { { "accepted", false } };
    }
    if (!m_terminal->isAtPrompt()) {
        throw std::runtime_error(
            "Nelson is busy. Stop the current execution before starting another demo.");
    }

    std::wstring command;
    std::string error;
    const Nelson::ExamplesCatalog catalog = Nelson::BuildLoadedModulesExamplesCatalog();
    if (params.contains("path") && params.at("path").is_string()) {
        if (!BuildExamplesCommand("run", utf8_to_wstring(params.at("path").get<std::string>()),
                catalog.allowedPaths, command, error)) {
            throw std::runtime_error(error);
        }
    } else if (params.contains("command") && params.at("command").is_string()) {
        if (!BuildExamplesCommandFromCatalogCommand("run",
                utf8_to_wstring(params.at("command").get<std::string>()), catalog.allowedCommands,
                command, error)) {
            throw std::runtime_error(error);
        }
    } else {
        throw std::runtime_error("example run request requires a path or command");
    }

    m_server->pushJson("term.busy", nlohmann::json::object());
    m_terminal->pushInputLine(command);
    return { { "accepted", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::examplesOpen(const nlohmann::json& params)
{
    if (m_terminal == nullptr) {
        return { { "accepted", false } };
    }
    if (!m_terminal->isAtPrompt()) {
        throw std::runtime_error(
            "Nelson is busy. Stop the current execution before opening another demo.");
    }
    if (!params.contains("path") || !params.at("path").is_string()) {
        throw std::runtime_error("example open request requires a path");
    }

    std::wstring command;
    std::string error;
    const Nelson::ExamplesCatalog catalog = Nelson::BuildLoadedModulesExamplesCatalog();
    if (!BuildExamplesCommand("open", utf8_to_wstring(params.at("path").get<std::string>()),
            catalog.allowedPaths, command, error)) {
        throw std::runtime_error(error);
    }

    m_server->pushJson("term.busy", nlohmann::json::object());
    m_terminal->pushInputLine(command);
    return { { "accepted", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::helpText(const nlohmann::json& params)
{
    const std::string name = params.value("name", std::string());
    std::string text;
    runOnInterpreterThread([&] {
        if (m_evaluator == nullptr) {
            return;
        }
        std::wstring wname = utf8_to_wstring(name);
        // Quotes are doubled so the topic cannot break out of the help() call.
        std::wstring safe;
        for (wchar_t c : wname) {
            if (c == L'\'') {
                safe += L"''";
            } else {
                safe += c;
            }
        }
        std::wstring out;
        try {
            EvaluateConsoleCommandToString(m_evaluator, L"help('" + safe + L"');", out);
        } catch (...) {
            out.clear();
        }
        text = wstring_to_utf8(out);
    });
    return { { "name", name }, { "text", text } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::profileInfo()
{
    nlohmann::json rows = nlohmann::json::array();
    const bool completed = m_terminal->runOnInterpreterThread([&] {
        if (m_evaluator == nullptr) {
            return;
        }
        EvaluateCommand(m_evaluator, L"__nlsprof__ = profile('info');", true);
        Context* ctx = m_evaluator->getContext();
        ArrayOf* v = ctx ? ctx->lookupVariable("__nlsprof__") : nullptr;
        if (v != nullptr && v->isStruct()) {
            stringVector fns = v->fieldNames();
            const size_t fc = fns.size();
            const size_t nel = v->dimensions().elementCount();
            const ArrayOf* ap = static_cast<const ArrayOf*>(v->dataPointer());
            auto idxOf = [&](const char* n) -> int {
                for (size_t k = 0; k < fc; ++k) {
                    if (fns[k] == n) {
                        return (int)k;
                    }
                }
                return -1;
            };
            const int iName = idxOf("FunctionName");
            const int iFile = idxOf("Filename");
            const int iLine = idxOf("LinePosition");
            const int iCalls = idxOf("NumCalls");
            const int iTot = idxOf("TotalTime");
            const int iPer = idxOf("PerCall");
            for (size_t i = 0; i < nel && ap != nullptr; ++i) {
                auto getStr = [&](int f) -> std::string {
                    if (f < 0) {
                        return std::string();
                    }
                    const ArrayOf& e = ap[i * fc + (size_t)f];
                    return e.isCharacterArray() ? wstring_to_utf8(e.asCharacterArrayWideString())
                                                : std::string();
                };
                auto getNum = [&](int f) -> double {
                    if (f < 0) {
                        return 0.;
                    }
                    try {
                        ArrayOf e = ap[i * fc + (size_t)f];
                        return e.isEmpty() ? 0. : e.asDoubleScalar(true);
                    } catch (...) {
                        return 0.;
                    }
                };
                rows.push_back({ { "function", getStr(iName) }, { "file", getStr(iFile) },
                    { "line", getNum(iLine) }, { "calls", getNum(iCalls) },
                    { "total", getNum(iTot) }, { "perCall", getNum(iPer) } });
            }
        }
        EvaluateCommand(m_evaluator, L"clear __nlsprof__;", true);
    });
    if (!completed) {
        throw std::runtime_error("interpreter is busy");
    }
    // Live state published by the profiler: the panel toggle follows profiling
    // switched from the console.
    const bool on = NelsonConfiguration::getInstance()->isProfilerActive();
    return { { "rows", rows }, { "on", on } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::profileControl(const nlohmann::json& params)
{
    const std::string action = params.value("action", std::string());
    if (action != "on" && action != "off" && action != "clear") {
        throw std::runtime_error("invalid profiler action");
    }
    if (m_evaluator == nullptr) {
        throw std::runtime_error("interpreter is not ready");
    }

    const bool completed = m_terminal->runOnInterpreterThread(
        [this, action] {
            EvaluateCommand(m_evaluator, L"profile('" + utf8_to_wstring(action) + L"');", true);
        },
        60000);
    if (!completed) {
        throw std::runtime_error("interpreter is busy");
    }

    return { { "ok", true }, { "on", NelsonConfiguration::getInstance()->isProfilerActive() } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::profileExport(const nlohmann::json& params)
{
    if (NelsonConfiguration::getInstance()->isProfilerActive()) {
        throw std::runtime_error("profiler is running");
    }
    if (m_evaluator == nullptr) {
        throw std::runtime_error("interpreter is not ready");
    }

    const std::string pathParam = params.value("path", std::string());
    std::error_code ec;
    std::filesystem::path destination = pathParam.empty()
        ? std::filesystem::current_path(ec) / "profile_result"
        : std::filesystem::path(pathParam);
    if (ec) {
        throw std::runtime_error("cannot resolve current directory");
    }
    destination = std::filesystem::absolute(destination, ec);
    if (ec) {
        throw std::runtime_error("cannot resolve export path");
    }

    std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>> profileInfo
        = Profiler::getInstance()->info(Profiler::Profile_Sort_Type::SORT_BY_NAMEFILELINE);
    if (profileInfo.empty()) {
        throw std::runtime_error("no profile data to export");
    }

    std::wstring errorMessage;
    std::wstring moduleProfilerPath
        = NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/modules/profiler";
    Profiler::getInstance()->save(
        profileInfo, destination.generic_wstring(), moduleProfilerPath, errorMessage);
    if (!errorMessage.empty()) {
        throw std::runtime_error(wstring_to_utf8(errorMessage));
    }

    const std::filesystem::path index = destination / "index.html";
    return { { "ok", true }, { "path", destination.generic_string() },
        { "index", index.generic_string() }, { "url", fileUrlFromPath(index) },
        { "on", NelsonConfiguration::getInstance()->isProfilerActive() } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::newsfeedList(const nlohmann::json& params)
{
    const std::string url = params.value("url", std::string());
    nlohmann::json feed = nlohmann::json::object();
    std::string errorMessage;
    const bool completed = m_terminal->runOnInterpreterThread([&] {
        if (m_evaluator == nullptr) {
            return;
        }
        // Escape single quotes so the url is a safe Nelson string literal.
        std::wstring escaped;
        for (wchar_t c : utf8_to_wstring(url)) {
            escaped.push_back(c);
            if (c == L'\'') {
                escaped.push_back(L'\'');
            }
        }
        // The .m newsfeed pipeline does the fetch and parse; an empty url selects
        // the default feed.
        const std::wstring cmd
            = L"__nlsnewsfeed__ = jsonencode(newsfeed('" + escaped + L"', '-tostruct'));";
        try {
            EvaluateCommand(m_evaluator, cmd, true);
            Context* ctx = m_evaluator->getContext();
            ArrayOf* v = ctx ? ctx->lookupVariable("__nlsnewsfeed__") : nullptr;
            if (v != nullptr && v->isCharacterArray()) {
                const std::string js = wstring_to_utf8(v->asCharacterArrayWideString());
                nlohmann::json parsed = nlohmann::json::parse(js, nullptr, false);
                if (!parsed.is_discarded()) {
                    feed = parsed;
                }
            }
        } catch (const std::exception& e) {
            errorMessage = e.what();
        } catch (...) {
            errorMessage = "unable to load the feed";
        }
        EvaluateCommand(m_evaluator, L"clear __nlsnewsfeed__;", true);
    });
    if (!completed) {
        throw std::runtime_error("interpreter is busy");
    }
    if (!errorMessage.empty()) {
        throw std::runtime_error(errorMessage);
    }
    return { { "feed", feed } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsUpload(const nlohmann::json& params)
{
    const std::string pathStr = params.value("path", std::string());
    const std::string dataB64 = params.value("data", std::string());
    if (pathStr.empty()) {
        return { { "error", { { "message", "path required" } } } };
    }
    std::filesystem::path target(utf8_to_wstring(pathStr));
    std::error_code ec;
    std::filesystem::create_directories(target.parent_path(), ec);
    std::ofstream out(target, std::ios::binary);
    if (!out) {
        return { { "error", { { "message", "cannot write file" } } } };
    }
    const std::string bytes = base64Decode(dataB64);
    out.write(bytes.data(), static_cast<std::streamsize>(bytes.size()));
    return { { "ok", true }, { "path", target.generic_string() }, { "size", bytes.size() } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::fsDownload(const nlohmann::json& params)
{
    const std::string pathStr = params.value("path", std::string());
    if (pathStr.empty()) {
        return { { "error", { { "message", "path required" } } } };
    }
    std::ifstream in(std::filesystem::path(utf8_to_wstring(pathStr)), std::ios::binary);
    if (!in) {
        return { { "error", { { "message", "cannot read file" } } } };
    }
    std::ostringstream ss;
    ss << in.rdbuf();
    const std::string bytes = ss.str();
    return { { "ok", true }, { "name", std::filesystem::path(pathStr).filename().generic_string() },
        { "data", base64Encode(bytes) }, { "size", bytes.size() } };
}
//=============================================================================
namespace {
    bool
    readNumericElement(const ArrayOf* v, size_t index, double& out)
    {
        const void* p = v->dataPointer();
        if (p == nullptr) {
            return false;
        }
        switch (v->dataClass()) {
        case NLS_LOGICAL:
            out = static_cast<const logical*>(p)[index];
            return true;
        case NLS_DOUBLE:
            out = static_cast<const double*>(p)[index];
            return true;
        case NLS_SINGLE:
            out = static_cast<const single*>(p)[index];
            return true;
        case NLS_INT8:
            out = static_cast<const int8*>(p)[index];
            return true;
        case NLS_UINT8:
            out = static_cast<const uint8*>(p)[index];
            return true;
        case NLS_INT16:
            out = static_cast<const int16*>(p)[index];
            return true;
        case NLS_UINT16:
            out = static_cast<const uint16*>(p)[index];
            return true;
        case NLS_INT32:
            out = static_cast<const int32*>(p)[index];
            return true;
        case NLS_UINT32:
            out = static_cast<const uint32*>(p)[index];
            return true;
        case NLS_INT64:
            out = static_cast<double>(static_cast<const int64*>(p)[index]);
            return true;
        case NLS_UINT64:
            out = static_cast<double>(static_cast<const uint64*>(p)[index]);
            return true;
        default:
            return false;
        }
    }
    //=============================================================================
    bool
    writeNumericElement(ArrayOf* v, size_t index, double value)
    {
        void* p = v->readWriteDataPointer();
        if (p == nullptr) {
            return false;
        }
        switch (v->dataClass()) {
        case NLS_LOGICAL:
            static_cast<logical*>(p)[index] = (value != 0.) ? 1 : 0;
            return true;
        case NLS_DOUBLE:
            static_cast<double*>(p)[index] = value;
            return true;
        case NLS_SINGLE:
            static_cast<single*>(p)[index] = static_cast<single>(value);
            return true;
        case NLS_INT8:
            static_cast<int8*>(p)[index] = static_cast<int8>(value);
            return true;
        case NLS_UINT8:
            static_cast<uint8*>(p)[index] = static_cast<uint8>(value);
            return true;
        case NLS_INT16:
            static_cast<int16*>(p)[index] = static_cast<int16>(value);
            return true;
        case NLS_UINT16:
            static_cast<uint16*>(p)[index] = static_cast<uint16>(value);
            return true;
        case NLS_INT32:
            static_cast<int32*>(p)[index] = static_cast<int32>(value);
            return true;
        case NLS_UINT32:
            static_cast<uint32*>(p)[index] = static_cast<uint32>(value);
            return true;
        case NLS_INT64:
            static_cast<int64*>(p)[index] = static_cast<int64>(value);
            return true;
        case NLS_UINT64:
            static_cast<uint64*>(p)[index] = static_cast<uint64>(value);
            return true;
        default:
            return false;
        }
    }
    //=============================================================================
    bool
    isEditableNumeric(NelsonType c)
    {
        switch (c) {
        case NLS_LOGICAL:
        case NLS_DOUBLE:
        case NLS_SINGLE:
        case NLS_INT8:
        case NLS_UINT8:
        case NLS_INT16:
        case NLS_UINT16:
        case NLS_INT32:
        case NLS_UINT32:
        case NLS_INT64:
        case NLS_UINT64:
            return true;
        default:
            return false;
        }
    }
    //=============================================================================
    static std::string
    summarizeArrayOf(const ArrayOf& v)
    {
        if (v.isEmpty()) {
            return "[]";
        }
        if (v.isCharacterArray()) {
            return "'" + wstring_to_utf8(v.asCharacterArrayWideString()) + "'";
        }
        if (v.isStringArray() && v.isScalar()) {
            return "\"" + wstring_to_utf8(v.asWideString()) + "\"";
        }
        if (v.isScalar() && isEditableNumeric(v.dataClass())) {
            double d = 0.;
            if (readNumericElement(&v, 0, d)) {
                std::ostringstream os;
                os << d;
                return os.str();
            }
        }
        Dimensions dm = v.dimensions();
        return std::to_string(dm.rows()) + "x" + std::to_string(dm.columns()) + " "
            + wstring_to_utf8(ClassToStringW(v.dataClass()));
    }
    //=============================================================================
    static std::string
    summarizeColumnElement(const ArrayOf& col, size_t r)
    {
        if (col.isStringArray()) {
            wstringVector vals = col.asWideStringVector(false);
            return r < vals.size() ? wstring_to_utf8(vals[r]) : std::string();
        }
        if (col.isCell()) {
            const ArrayOf* ap = static_cast<const ArrayOf*>(col.dataPointer());
            return ap ? summarizeArrayOf(ap[r]) : std::string();
        }
        if (isEditableNumeric(col.dataClass()) && !col.isSparse()) {
            double d = 0.;
            if (readNumericElement(&col, r, d)) {
                std::ostringstream os;
                os << d;
                return os.str();
            }
        }
        return summarizeArrayOf(col);
    }
    //=============================================================================
    static std::string
    variableTextSummary(const ArrayOf& v)
    {
        return v.dimensions().toString() + " " + wstring_to_utf8(ClassToStringW(v.dataClass()))
            + (v.isSparse() ? " (sparse)" : "");
    }
    //=============================================================================
} // namespace
//=============================================================================
nlohmann::json
WebGuiRpc::varOpen(const nlohmann::json& params)
{
    const std::string name = params.value("name", std::string());
    nlohmann::json result;
    std::string err;
    runOnInterpreterThread([&] {
        Context* ctx = m_evaluator ? m_evaluator->getContext() : nullptr;
        ArrayOf* v = ctx ? ctx->lookupVariable(name) : nullptr;
        if (v == nullptr) {
            err = "Undefined variable '" + name + "'.";
            return;
        }
        Dimensions dims = v->dimensions();
        const NelsonType cls = v->dataClass();
        const bool numeric = isEditableNumeric(cls);
        const bool isString = v->isStringArray();
        const bool isChar = v->isCharacterArray();
        const bool isStruct = v->isStruct();
        const bool isCell = v->isCell();
        size_t rows = dims.rows();
        size_t cols = dims.columns();
        std::string kind = "matrix";
        bool editable = numeric && !v->isSparse();
        nlohmann::json columns = nlohmann::json::array();
        if (isString) {
            kind = "string";
            editable = true;
        } else if (isChar) {
            // Edited as a single string cell; writing back gives a char row vector.
            kind = "char";
            editable = true;
            rows = 1;
            cols = 1;
        } else if (isStruct) {
            // Read-only grid, elements x fields; a scalar struct is one row.
            kind = "struct";
            editable = false;
            rows = dims.elementCount();
            cols = v->fieldNames().size();
            for (const std::string& fn : v->fieldNames()) {
                columns.push_back(fn);
            }
        } else if (isCell) {
            kind = "cell";
            editable = false;
        } else if (v->isTable()) {
            kind = "table";
            editable = false;
            stringVector vn = v->tableVariableNames();
            rows = (size_t)v->tableHeight();
            cols = vn.size();
            for (const std::string& n : vn) {
                columns.push_back(n);
            }
        } else if (!numeric || v->isSparse() || !v->is2D()) {
            // No grid representation: show the full display() output as text.
            kind = "text";
            editable = false;
        }
        if (columns.empty()) {
            for (size_t c = 0; c < cols; ++c) {
                columns.push_back(std::to_string(c + 1));
            }
        }
        std::wstring classLabel = (v->isSparse() ? L"sparse " : L"") + ClassToStringW(cls);
        std::string textContent;
        if (kind == "text") {
            // Full value dump, captured from display(name, 'val').
            std::wstring out;
            EvaluateConsoleCommandToString(
                m_evaluator, L"display(" + utf8_to_wstring(name) + L", 'val');", out);
            textContent = wstring_to_utf8(out);
        }
        result = {
            { "name", name },
            { "className", wstring_to_utf8(classLabel) },
            { "rows", (long long)rows },
            { "cols", (long long)cols },
            { "columns", columns },
            { "editable", editable },
            { "kind", kind },
            { "text", textContent },
        };
    });
    if (!err.empty()) {
        throw std::runtime_error(err);
    }
    return result;
}
//=============================================================================
nlohmann::json
WebGuiRpc::varGetBlock(const nlohmann::json& params)
{
    const std::string name = params.value("name", std::string());
    const long long row0 = params.value("row0", 0LL);
    const long long col0 = params.value("col0", 0LL);
    const long long nrows = params.value("rows", 0LL);
    const long long ncols = params.value("cols", 0LL);
    nlohmann::json cells = nlohmann::json::array();
    std::string err;
    runOnInterpreterThread([&] {
        Context* ctx = m_evaluator ? m_evaluator->getContext() : nullptr;
        ArrayOf* v = ctx ? ctx->lookupVariable(name) : nullptr;
        if (v == nullptr) {
            err = "Undefined variable '" + name + "'.";
            return;
        }
        if (v->isCharacterArray()) {
            cells.push_back(
                nlohmann::json::array({ wstring_to_utf8(v->asCharacterArrayWideString()) }));
            return;
        }
        if (v->isStruct()) {
            // Element-major storage: ap[i * fieldCount + field].
            stringVector fns = v->fieldNames();
            const size_t fc = fns.size();
            const size_t nel = v->dimensions().elementCount();
            const ArrayOf* ap = static_cast<const ArrayOf*>(v->dataPointer());
            for (long long r = row0; r < row0 + nrows && r < (long long)nel; ++r) {
                nlohmann::json row = nlohmann::json::array();
                for (long long c = col0; c < col0 + ncols && c < (long long)fc; ++c) {
                    size_t idx = (size_t)r * fc + (size_t)c;
                    row.push_back(ap ? summarizeArrayOf(ap[idx]) : "");
                }
                cells.push_back(row);
            }
            return;
        }
        if (v->isTable()) {
            stringVector vn = v->tableVariableNames();
            const size_t H = (size_t)v->tableHeight();
            const size_t W = vn.size();
            std::vector<ArrayOf> cols;
            cols.reserve(W);
            for (size_t c = 0; c < W; ++c) {
                cols.push_back(v->tableColumn((indexType)c));
            }
            for (long long r = row0; r < row0 + nrows && r < (long long)H; ++r) {
                nlohmann::json row = nlohmann::json::array();
                for (long long c = col0; c < col0 + ncols && c < (long long)W; ++c) {
                    row.push_back(summarizeColumnElement(cols[(size_t)c], (size_t)r));
                }
                cells.push_back(row);
            }
            return;
        }
        if (v->isCell()) {
            Dimensions cdims = v->dimensions();
            const size_t R = cdims.rows();
            const size_t C = cdims.columns();
            const ArrayOf* ap = static_cast<const ArrayOf*>(v->dataPointer());
            for (long long r = row0; r < row0 + nrows && r < (long long)R; ++r) {
                nlohmann::json row = nlohmann::json::array();
                for (long long c = col0; c < col0 + ncols && c < (long long)C; ++c) {
                    size_t idx = (size_t)(r + c * (long long)R);
                    row.push_back(ap ? summarizeArrayOf(ap[idx]) : "");
                }
                cells.push_back(row);
            }
            return;
        }
        if (v->isStringArray()) {
            Dimensions sdims = v->dimensions();
            const size_t R = sdims.rows();
            const size_t C = sdims.columns();
            // Read the element storage directly so a <missing> element can be
            // told apart from an empty string "" (asWideStringVector collapses
            // both to ""). The frontend renders "<missing>" in italic.
            const ArrayOf* ap = static_cast<const ArrayOf*>(v->dataPointer());
            const size_t nel = sdims.elementCount();
            for (long long r = row0; r < row0 + nrows && r < (long long)R; ++r) {
                nlohmann::json row = nlohmann::json::array();
                for (long long c = col0; c < col0 + ncols && c < (long long)C; ++c) {
                    size_t idx = (size_t)(r + c * (long long)R);
                    if (ap == nullptr || idx >= nel) {
                        row.push_back("");
                    } else if (!ap[idx].isCharacterArray()) {
                        row.push_back("<missing>");
                    } else {
                        row.push_back(wstring_to_utf8(ap[idx].asWideString()));
                    }
                }
                cells.push_back(row);
            }
            return;
        }
        if (v->isSparse() || !v->is2D() || !isEditableNumeric(v->dataClass())) {
            cells.push_back(nlohmann::json::array({ variableTextSummary(*v) }));
            return;
        }
        Dimensions dims = v->dimensions();
        const size_t R = dims.rows();
        const size_t C = dims.columns();
        for (long long r = row0; r < row0 + nrows && r < (long long)R; ++r) {
            nlohmann::json row = nlohmann::json::array();
            for (long long c = col0; c < col0 + ncols && c < (long long)C; ++c) {
                double value = 0.;
                if (readNumericElement(v, (size_t)(r + c * (long long)R), value)) {
                    row.push_back(value);
                } else {
                    row.push_back(nullptr);
                }
            }
            cells.push_back(row);
        }
    });
    if (!err.empty()) {
        throw std::runtime_error(err);
    }
    return { { "row0", row0 }, { "col0", col0 }, { "cells", cells } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::varSetCell(const nlohmann::json& params)
{
    const std::string name = params.value("name", std::string());
    const long long row = params.value("row", 0LL);
    const long long col = params.value("col", 0LL);
    const double value = params.value("value", 0.0);
    std::string err;
    runOnInterpreterThread([&] {
        Context* ctx = m_evaluator ? m_evaluator->getContext() : nullptr;
        ArrayOf* v = ctx ? ctx->lookupVariable(name) : nullptr;
        if (v == nullptr) {
            err = "Undefined variable '" + name + "'.";
            return;
        }
        if (!isEditableNumeric(v->dataClass()) || v->isSparse()) {
            err = "'" + name + "' is not editable.";
            return;
        }
        Dimensions dims = v->dimensions();
        const size_t R = dims.rows();
        if (row < 0 || col < 0 || (size_t)row >= R || (size_t)col >= dims.columns()) {
            err = "index out of range";
            return;
        }
        if (!writeNumericElement(v, (size_t)(row + col * (long long)R), value)) {
            err = "write failed";
        }
    });
    if (!err.empty()) {
        throw std::runtime_error(err);
    }
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::varReplace(const nlohmann::json& params)
{
    // The browser editor computes insert/delete, block paste and undo/redo on
    // the grid and hands back the whole matrix: one code path for every edit.
    const std::string name = params.value("name", std::string());
    const nlohmann::json values
        = params.contains("values") ? params["values"] : nlohmann::json::array();
    std::string err;
    runOnInterpreterThread([&] {
        Context* ctx = m_evaluator ? m_evaluator->getContext() : nullptr;
        ArrayOf* v = ctx ? ctx->lookupVariable(name) : nullptr;
        if (v == nullptr) {
            err = "Undefined variable '" + name + "'.";
            return;
        }
        const NelsonType cls = v->dataClass();
        if (v->isCharacterArray()) {
            std::wstring text;
            if (!values.empty() && values[0].is_array() && !values[0].empty()
                && values[0][0].is_string()) {
                text = utf8_to_wstring(values[0][0].get<std::string>());
            }
            ctx->insertVariable(name, ArrayOf::characterArray(text));
            return;
        }
        if (v->isStringArray()) {
            const size_t sR = values.size();
            size_t sC = 0;
            for (const auto& row : values) {
                if (row.is_array() && row.size() > sC) {
                    sC = row.size();
                }
            }
            // Column-major element vector expected by stringArray().
            wstringVector vals(sR * sC);
            for (size_t r = 0; r < sR; ++r) {
                if (!values[r].is_array()) {
                    continue;
                }
                for (size_t c = 0; c < sC && c < values[r].size(); ++c) {
                    const auto& cell = values[r][c];
                    std::string s = cell.is_string() ? cell.get<std::string>() : std::string();
                    // The <missing> sentinel becomes empty: string arrays cannot
                    // carry a per-element missing mask through this API.
                    vals[r + c * sR] = (s == "<missing>") ? std::wstring() : utf8_to_wstring(s);
                }
            }
            ctx->insertVariable(name, ArrayOf::stringArray(vals, Dimensions(sR, sC)));
            return;
        }
        if (!isEditableNumeric(cls) || v->isSparse()) {
            err = "'" + name + "' is not editable.";
            return;
        }
        const size_t newR = values.size();
        size_t newC = 0;
        for (const auto& row : values) {
            if (row.is_array() && row.size() > newC) {
                newC = row.size();
            }
        }
        Dimensions dims(newR, newC);
        void* data = ArrayOf::allocateArrayOf(cls, newR * newC, stringVector(), true);
        ArrayOf next(cls, dims, data);
        for (size_t r = 0; r < newR; ++r) {
            if (!values[r].is_array()) {
                continue;
            }
            for (size_t c = 0; c < newC && c < values[r].size(); ++c) {
                const auto& cell = values[r][c];
                double d = cell.is_number() ? cell.get<double>() : 0.0;
                writeNumericElement(&next, r + c * newR, d);
            }
        }
        ctx->insertVariable(name, next);
    });
    if (!err.empty()) {
        throw std::runtime_error(err);
    }
    return { { "ok", true } };
}
//=============================================================================
namespace {
    // Scalar ArrayOf of `proto`'s type from an edited text value: editing a
    // nested cell keeps char as char, string as string and numeric in its class.
    // Only scalar/char/string elements are editable.
    static bool
    makeScalarLike(const ArrayOf& proto, const std::string& text, ArrayOf& out, std::string& why)
    {
        if (proto.isCharacterArray()) {
            out = ArrayOf::characterArray(utf8_to_wstring(text));
            return true;
        }
        if (proto.isStringArray() && proto.isScalar()) {
            out = ArrayOf::stringArray(wstringVector { utf8_to_wstring(text) }, Dimensions(1, 1));
            return true;
        }
        if (proto.isScalar() && isEditableNumeric(proto.dataClass())) {
            double d = 0.;
            try {
                d = std::stod(text);
            } catch (...) {
                d = 0.;
            }
            const NelsonType cls = proto.dataClass();
            void* data = ArrayOf::allocateArrayOf(cls, 1, stringVector(), true);
            ArrayOf scalar(cls, Dimensions(1, 1), data);
            writeNumericElement(&scalar, 0, d);
            out = scalar;
            return true;
        }
        why = "only scalar (numeric / char / string) cells are editable";
        return false;
    }
    //=============================================================================
} // namespace
//=============================================================================
// Edit one scalar element of a cell array or struct in place. The frontend
// routes these edits here and whole numeric/char/string edits to var.replace.
nlohmann::json
WebGuiRpc::varSetNested(const nlohmann::json& params)
{
    const std::string name = params.value("name", std::string());
    const std::string kind = params.value("kind", std::string());
    const long long row = params.value("row", 0LL);
    const long long col = params.value("col", 0LL);
    const std::string text = params.value("value", std::string());
    std::string err;
    runOnInterpreterThread([&] {
        Context* ctx = m_evaluator ? m_evaluator->getContext() : nullptr;
        ArrayOf* v = ctx ? ctx->lookupVariable(name) : nullptr;
        if (v == nullptr) {
            err = "Undefined variable '" + name + "'.";
            return;
        }
        size_t idx = 0;
        if (kind == "struct" && v->isStruct()) {
            const size_t fc = v->fieldNames().size();
            idx = (size_t)row * fc + (size_t)col; // element-major
        } else if (kind == "cell" && v->isCell()) {
            const size_t R = v->dimensions().rows();
            idx = (size_t)(row + col * (long long)R); // column-major
        } else {
            err = "'" + name + "' cell is not editable.";
            return;
        }
        ArrayOf* ap = static_cast<ArrayOf*>(v->readWriteDataPointer());
        if (ap == nullptr) {
            err = "cannot access '" + name + "'.";
            return;
        }
        ArrayOf next;
        std::string why;
        if (!makeScalarLike(ap[idx], text, next, why)) {
            err = why;
            return;
        }
        ap[idx] = next;
    });
    if (!err.empty()) {
        throw std::runtime_error(err);
    }
    return { { "ok", true } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::shellRun(const nlohmann::json& params)
{
    const std::string command = params.value("command", std::string());
    std::string cwdStr = params.value("cwd", std::string());

    std::error_code ec;
    std::filesystem::path cwd
        = cwdStr.empty() ? std::filesystem::current_path(ec) : std::filesystem::path(cwdStr);

    // `cd` is handled here: each SystemCommand runs in its own child process, so
    // the working directory would not persist between commands.
    std::string trimmed = command;
    trimmed.erase(0, trimmed.find_first_not_of(" \t"));
    while (!trimmed.empty()
        && (trimmed.back() == ' ' || trimmed.back() == '\t' || trimmed.back() == '\r'
            || trimmed.back() == '\n')) {
        trimmed.pop_back();
    }
    if (trimmed == "cd" || trimmed.rfind("cd ", 0) == 0) {
        std::string target = (trimmed.size() > 2) ? trimmed.substr(3) : std::string();
        target.erase(0, target.find_first_not_of(" \t"));
        if (target.size() >= 2 && target.front() == '"' && target.back() == '"') {
            target = target.substr(1, target.size() - 2);
        }
        std::filesystem::path next = target.empty()
            ? cwd
            : (std::filesystem::path(target).is_absolute() ? std::filesystem::path(target)
                                                           : cwd / target);
        std::filesystem::path canon = std::filesystem::weakly_canonical(next, ec);
        if (ec || !std::filesystem::is_directory(canon, ec)) {
            return { { "output", "cd: no such directory: " + target + "\n" },
                { "cwd", cwd.generic_string() }, { "exitCode", 1 } };
        }
        return { { "output", "" }, { "cwd", canon.generic_string() }, { "exitCode", 0 } };
    }

    if (command.empty()) {
        return { { "output", "" }, { "cwd", cwd.generic_string() }, { "exitCode", 0 } };
    }

    const size_t evalID = m_evaluator ? m_evaluator->getID() : 0;
    std::string output;
    int exitCode = 0;
    // Nelson is idle at the prompt, so the temporary chdir into the terminal's
    // directory is race-free; the cwd is restored before returning.
    runOnInterpreterThread([&] {
        std::error_code e2;
        std::filesystem::path saved = std::filesystem::current_path(e2);
        std::filesystem::current_path(cwd, e2);
        auto result = SystemCommand(utf8_to_wstring(command), 0, false, evalID);
        exitCode = std::get<0>(result);
        output = wstring_to_utf8(std::get<1>(result));
        std::filesystem::current_path(saved, e2);
    });
    return { { "output", output }, { "cwd", cwd.generic_string() }, { "exitCode", exitCode } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::editorOpen(const nlohmann::json& params)
{
    const std::string path = params.value("path", std::string());
    std::ifstream in(std::filesystem::path(utf8_to_wstring(path)), std::ios::binary);
    if (!in) {
        throw std::runtime_error("cannot open '" + path + "'");
    }
    std::string content((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
    const bool isM = path.size() >= 2 && path.substr(path.size() - 2) == ".m";
    return {
        { "path", path },
        { "content", content },
        { "language", isM ? "nelson" : "plaintext" },
    };
}
//=============================================================================
nlohmann::json
WebGuiRpc::editorSave(const nlohmann::json& params)
{
    const std::string path = params.value("path", std::string());
    const std::string content = params.value("content", std::string());
    std::ofstream out(std::filesystem::path(utf8_to_wstring(path)), std::ios::binary);
    if (!out) {
        throw std::runtime_error("cannot write '" + path + "'");
    }
    out.write(content.data(), (std::streamsize)content.size());
    return { { "ok", true } };
}
//=============================================================================
// Analyzes the editor buffer without saving it; diagnostics are shaped for
// Monaco's setModelMarkers.
nlohmann::json
WebGuiRpc::codeAnalyze(const nlohmann::json& params)
{
    const std::string path = params.value("path", std::string());
    const std::string source = params.value("source", std::string());

    std::vector<CodeAnalyzerDiagnostic> diagnostics;
    // Parsing touches global lexer/parser state; run it on the interpreter
    // thread (drained even while parked at the prompt) to avoid racing an eval.
    runOnInterpreterThread([&] {
        CodeAnalyzerOptions options;
        try {
            diagnostics = analyzeCodeBuffer(utf8_to_wstring(path), source, options);
        } catch (...) {
            // A malformed buffer must not take the RPC down; report no issues.
            diagnostics.clear();
        }
    });

    nlohmann::json items = nlohmann::json::array();
    for (const CodeAnalyzerDiagnostic& d : diagnostics) {
        if (d.suppressed) {
            continue;
        }
        nlohmann::json fixes = nlohmann::json::array();
        for (const CodeAnalyzerFix& fx : d.fixes) {
            nlohmann::json edits = nlohmann::json::array();
            for (const CodeAnalyzerTextEdit& e : fx.edits) {
                edits.push_back({ { "lineStart", e.lineStart }, { "lineEnd", e.lineEnd },
                    { "columnStart", e.columnStart }, { "columnEnd", e.columnEnd },
                    { "replacementText", e.replacementText } });
            }
            fixes.push_back({ { "title", fx.title }, { "safe", fx.safe }, { "edits", edits } });
        }
        items.push_back({
            { "severity", d.severity }, // "error" | "warning" | "info"
            { "message", d.message },
            { "ruleName", d.ruleName },
            { "checkId", d.checkId },
            { "category", d.category },
            { "helpUri", d.helpUri },
            { "lineStart", d.lineStart },
            { "lineEnd", d.lineEnd },
            { "columnStart", d.columnStart },
            { "columnEnd", d.columnEnd },
            { "fixes", fixes },
        });
    }
    return { { "diagnostics", items } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::debuggerGetBreakpoints(const nlohmann::json& params)
{
    const std::wstring file = utf8_to_wstring(params.value("file", std::string()));
    nlohmann::json lines = nlohmann::json::array();
    runOnInterpreterThread([&] {
        if (m_evaluator == nullptr) {
            return;
        }
        for (size_t l : m_evaluator->getBreakpointLines(file)) {
            lines.push_back((long long)l);
        }
    });
    return { { "file", params.value("file", std::string()) }, { "lines", lines } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::debuggerToggleBreakpoint(const nlohmann::json& params)
{
    const std::string fileUtf8 = params.value("file", std::string());
    const std::wstring file = utf8_to_wstring(fileUtf8);
    const size_t line = params.value("line", 0LL);
    nlohmann::json lines = nlohmann::json::array();
    runOnInterpreterThread([&] {
        if (m_evaluator == nullptr) {
            return;
        }
        if (m_evaluator->hasBreakpoint(file, line)) {
            m_evaluator->removeBreakpoint(file, line);
        } else {
            Breakpoint bp;
            bp.filename = file;
            bp.line = line;
            bp.enabled = true;
            m_evaluator->addBreakpoint(bp);
        }
        for (size_t l : m_evaluator->getBreakpointLines(file)) {
            lines.push_back((long long)l);
        }
    });
    return { { "file", fileUtf8 }, { "lines", lines } };
}
//=============================================================================
nlohmann::json
WebGuiRpc::debuggerState()
{
    // The interpreter thread drains these tasks even while parked at a debug
    // (K>>) prompt, so there is no deadlock.
    nlohmann::json result = { { "running", false }, { "currentLine", nullptr },
        { "currentFile", nullptr }, { "stack", nlohmann::json::array() } };
    if (m_evaluator == nullptr) {
        return result;
    }
    runOnInterpreterThread([&] {
        if (m_evaluator->getDebugDepth() <= 0) {
            return;
        }
        stackTrace trace;
        DebugStack(m_evaluator->callstack, 0, trace);
        nlohmann::json stack = nlohmann::json::array();
        for (const PositionScript& pos : trace) {
            stack.push_back(
                { { "file", wstring_to_utf8(pos.getFilename()) }, { "line", pos.getLine() },
                    { "function", wstring_to_utf8(pos.getFunctionName()) } });
        }
        result["running"] = true;
        result["stack"] = stack;
        if (!trace.empty()) {
            // DebugStack orders innermost-first: the current frame is the top.
            const PositionScript& top = trace.front();
            result["currentFile"] = wstring_to_utf8(top.getFilename());
            result["currentLine"] = top.getLine();
        }
    });
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
