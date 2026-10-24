//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nmm_guiBuiltin.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "MainThreadTaskQueue.hpp"
#include "ModulePath.hpp"
#include "NelsonConfiguration.hpp"
#include "NmmGuiLocaleJson.hpp"
#include "NmmGuiPrefs.hpp"
#include "PackageUpdatesCount.hpp"
#include "WebUiConfigScope.hpp"
#include "WebToolsHost.hpp"
#include "WebUiHelpers.hpp"
#include "nlsNmm_gui_builtin_exports.h"
#include "characters_encoding.hpp"
#include <nlohmann/json.hpp>
#include <filesystem>
#include <mutex>
#include <regex>
#include <string>
//=============================================================================
extern "C"
{
#include "webui.h"
}
#ifdef _WIN32
#include <windows.h>
#endif
//=============================================================================
// Singleton package manager window:
//   - one WebUI window, re-focused on every nmm_gui() call;
//   - the frontend calls the bound "nmmGuiRpc" function with a JSON envelope
//     { id, method, params } and receives { id, result } | { id, error };
//   - read-only methods are evaluated synchronously on the interpreter thread
//     through MainThreadTaskQueue (they call the nmm_gui_rpc.m facade, which
//     wraps nmm(...) and returns JSON);
//   - mutating methods (install / remove / update) are posted to the same
//     interpreter task queue without waiting for them: they run in the console
//     like a typed nmm(...) call and end with __nmm_gui_notify__ so the window
//     refreshes.
//=============================================================================
namespace Nelson::NmmGuiGateway {
//=============================================================================
namespace {
    //=============================================================================
    using json = nlohmann::json;
    constexpr int RPC_TIMEOUT_MS = 60000;
    constexpr const char* RPC_BINDING = "nmmGuiRpc";
    //=============================================================================
    struct WindowState
    {
        size_t window = 0;
        size_t retiredWindow = 0;
        // Deep link requested by nmm_gui(action, name[, version]). The page
        // fetches it once it is ready, so a window opened by the same call
        // never races with the notification that wakes an existing one.
        std::string pendingAction;
        std::string pendingName;
        std::string pendingVersion;
    };
    //=============================================================================
    std::recursive_mutex&
    stateMutex()
    {
        // WebUI can finish a close callback during static teardown.
        static auto* mutex = new std::recursive_mutex;
        return *mutex;
    }
    //=============================================================================
    WindowState&
    windowState()
    {
        // Keep callback state alive until the process has completely stopped.
        static auto* value = new WindowState;
        return *value;
    }
    //=============================================================================
    bool
    isSafeIdentifier(const std::string& text)
    {
        static const std::regex pattern("^[A-Za-z][A-Za-z0-9_]*$");
        return std::regex_match(text, pattern);
    }
    //=============================================================================
    bool
    isSafeVersion(const std::string& text)
    {
        // semver or a constraint (>=1.2.3, ^1.0.0, ~1.2, 1.x)
        static const std::regex pattern("^[<>=^~]*[0-9A-Za-z.\\-+ ]+$");
        return text.empty() || std::regex_match(text, pattern);
    }
    //=============================================================================
    std::string
    escapeSingleQuotes(const std::string& value)
    {
        std::string out;
        out.reserve(value.size() + 8);
        for (char c : value) {
            if (c == '\'') {
                out += "''";
            } else {
                out += c;
            }
        }
        return out;
    }
    //=============================================================================
    bool
    isSafeFreeText(const std::string& value)
    {
        // A path, URL or registry location: any single line without control chars.
        return value.find('\n') == std::string::npos && value.find('\r') == std::string::npos;
    }
    //=============================================================================
    bool
    isSafeExternalUrl(const std::string& value)
    {
        // webui_open_url delegates to the operating system. Accept only web
        // URLs and reject characters that can be interpreted by its Unix shell
        // fallback even when the URL is enclosed in double quotes.
        static const std::regex scheme("^https?://", std::regex_constants::icase);
        if (!std::regex_search(value, scheme)) {
            return false;
        }
        for (const unsigned char c : value) {
            if (c <= 0x20 || c == 0x7f || c == '"' || c == '`' || c == '$' || c == '\\') {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    std::string
    uiCatalog(const std::string& locale)
    {
        // Embedded window strings (locale/nmm-ui-<locale>.json), empty for a
        // language without a catalog.
        for (const UiCatalog& catalog : UI_CATALOGS) {
            if (locale == catalog.locale) {
                return catalog.json;
            }
        }
        return std::string();
    }
    //=============================================================================
    std::string
    stringParam(const json& params, const char* name)
    {
        if (params.contains(name) && params.at(name).is_string()) {
            return params.at(name).get<std::string>();
        }
        return std::string();
    }
    //=============================================================================
    // Evaluate nmm_gui_rpc(method, paramsJson) on the interpreter thread and
    // return its JSON text. Blocks the WebUI thread until the interpreter
    // drains the task queue (prompt, statement boundary, pause).
    std::string
    callFacade(const std::string& method, const std::string& paramsJson)
    {
        std::string result;
        std::string failure;
        const bool completed = MainThreadTaskQueue::getInstance()->runOnInterpreterThread(
            [&](Evaluator* eval) {
                try {
                    if (eval == nullptr || eval->getContext() == nullptr) {
                        failure = "interpreter is not available";
                        return;
                    }
                    FunctionDef* funcDef = nullptr;
                    eval->getContext()->lookupFunction("nmm_gui_rpc", funcDef);
                    if (funcDef == nullptr) {
                        failure = "nmm_gui_rpc is not available";
                        return;
                    }
                    ArrayOfVector args;
                    args << ArrayOf::characterArray(method);
                    args << ArrayOf::characterArray(paramsJson);
                    ArrayOfVector out = funcDef->evaluateFunction(eval, args, 1);
                    if (out.empty()) {
                        failure = "nmm_gui_rpc returned nothing";
                        return;
                    }
                    result = out[0].asUtf8String();
                } catch (const std::exception& exception) {
                    failure = exception.what();
                } catch (...) {
                    failure = "nmm_gui_rpc failed";
                }
            },
            RPC_TIMEOUT_MS);
        if (!completed) {
            throw std::runtime_error("the interpreter is busy; retry when the prompt is back");
        }
        if (!failure.empty()) {
            throw std::runtime_error(failure);
        }
        return result;
    }
    //=============================================================================
    // Run a console command on the interpreter thread without waiting for it.
    //
    // Deliberately not postCommand(): the CLI prompt consults the command queue
    // only once, before blocking in getLine(), so a command posted while the
    // prompt waits would sit there until the user pressed Enter (the window then
    // stays on "waiting for the console", and Cancel has nothing to interrupt).
    // The interpreter task queue is drained by processCliCallbacks() while the
    // prompt is blocked, which is what the read methods already rely on.
    void
    runOnConsole(const std::string& command)
    {
        const std::wstring line = utf8_to_wstring(command);
        MainThreadTaskQueue::getInstance()->postToInterpreterThread(
            [line](Evaluator* eval) { eval->evaluateString(line, false); });
    }
    //=============================================================================
    // Queue a package command in the console. The trailing notify refreshes
    // the window whether the command succeeded or failed.
    json
    postPackageCommand(const std::string& call)
    {
        runOnConsole("try; " + call
            + "; catch e; disp(e.message); __nmm_gui_notify__('failed', e.message); end; "
              "__nmm_gui_notify__('changed');");
        return { { "accepted", true }, { "command", call } };
    }
    //=============================================================================
    json
    dispatch(const std::string& method, const json& params)
    {
        if (method == "interrupt") {
            // Raise the interrupt flag directly from the WebUI thread: the
            // interpreter is busy running the queued command, so a request
            // marshalled onto it would only run after the command finishes.
            NelsonConfiguration::getInstance()->setInterruptPending(true);
            return { { "interrupted", true } };
        }
        if (method == "i18n") {
            // The page falls back to English for an entry missing or left
            // empty in the catalog of the current language.
            const std::string locale
                = wstring_to_utf8(NelsonConfiguration::getInstance()->getCurrentLocale());
            return { { "ok", true }, { "locale", locale }, { "en_US", uiCatalog("en_US") },
                { "strings", locale == "en_US" ? std::string() : uiCatalog(locale) } };
        }
        if (method == "prefs") {
            return readNmmGuiPrefs();
        }
        if (method == "savePrefs") {
            // Only the keys the window owns: a page bug cannot write anything
            // else into the preferences file.
            json changes = json::object();
            const json prefs = params.value("prefs", json::object());
            // On Windows the window size is measured natively when the window
            // closes (rememberWindowSize): the page only knows CSS pixels, not
            // the physical pixels webui_set_size expects.
#ifdef _WIN32
            const auto keys = { "filter", "sort", "selected", "category", "license" };
#else
            const auto keys
                = { "filter", "sort", "selected", "category", "license", "width", "height" };
#endif
            for (const char* key : keys) {
                if (prefs.is_object() && prefs.contains(key)) {
                    changes[key] = prefs.at(key);
                }
            }
            return mergeNmmGuiPrefs(changes);
        }
        if (method == "updates") {
            // The page reports how many installed packages have a newer
            // registry version after each load: the desktops show it as a
            // badge on their Package Manager action.
            const int count = params.contains("count") && params.at("count").is_number_integer()
                ? params.at("count").get<int>()
                : 0;
            setPackageUpdatesCount(count);
            return { { "count", getPackageUpdatesCount() } };
        }
        if (method == "openExternal") {
            const std::string url = stringParam(params, "url");
            if (!isSafeExternalUrl(url)) {
                throw std::runtime_error("invalid external URL");
            }
            webui_open_url(url.c_str());
            return { { "opened", true } };
        }
        if (method == "installSource") {
            // Install from a free path or URL typed by the user (local .nmz,
            // a module directory, or a git URL). Not a registry name.
            const std::string source = stringParam(params, "source");
            if (source.empty() || !isSafeFreeText(source)) {
                throw std::runtime_error("invalid install source");
            }
            return postPackageCommand("nmm('install', '" + escapeSingleQuotes(source) + "')");
        }
        if (method == "setRegistry") {
            // Point NELSON_NMM_REGISTRY at a URL or file (empty clears it),
            // then refresh the window.
            const std::string url = stringParam(params, "url");
            if (!isSafeFreeText(url)) {
                throw std::runtime_error("invalid registry location");
            }
            const std::string setenvCall = url.empty()
                ? "setenv('NELSON_NMM_REGISTRY')"
                : "setenv('NELSON_NMM_REGISTRY', '" + escapeSingleQuotes(url) + "')";
            runOnConsole(setenvCall + "; __nmm_gui_notify__('changed');");
            return { { "accepted", true } };
        }
        if (method == "install" || method == "update" || method == "remove" || method == "pin"
            || method == "unpin" || method == "load" || method == "unload") {
            const std::string name = stringParam(params, "name");
            const std::string version = stringParam(params, "version");
            if (!isSafeIdentifier(name)) {
                throw std::runtime_error("invalid package name");
            }
            if (!isSafeVersion(version)) {
                throw std::runtime_error("invalid package version");
            }
            if (method == "remove") {
                return postPackageCommand("nmm('remove', '" + name + "')");
            }
            if (method == "unpin") {
                return postPackageCommand("nmm('unpin', '" + name + "')");
            }
            if (method == "unload") {
                return postPackageCommand("removemodule('" + name + "')");
            }
            if (method == "pin") {
                return postPackageCommand(version.empty()
                        ? "nmm('pin', '" + name + "')"
                        : "nmm('pin', '" + name + "', '" + version + "')");
            }
            if (method == "load") {
                return postPackageCommand(version.empty()
                        ? "nmm('load', '" + name + "')"
                        : "nmm('load', '" + name + "', '" + version + "')");
            }
            // Optional opt-in to the package tests of a source install.
            const bool runTests = params.contains("tests") && params.at("tests").is_boolean()
                && params.at("tests").get<bool>();
            const std::string testsOption = runTests ? ", '-tests'" : "";
            if (version.empty()) {
                return postPackageCommand("nmm('install', '" + name + "'" + testsOption + ")");
            }
            return postPackageCommand(
                "nmm('install', '" + name + "', '" + version + "'" + testsOption + ")");
        }
        if (method == "pendingDeepLink") {
            // Consumed exactly once, whoever asks first: the page on startup,
            // or the 'deeplink' notification when the window was already open.
            std::lock_guard<std::recursive_mutex> lock(stateMutex());
            WindowState& state = windowState();
            json out = { { "action", state.pendingAction }, { "name", state.pendingName },
                { "version", state.pendingVersion } };
            state.pendingAction.clear();
            state.pendingName.clear();
            state.pendingVersion.clear();
            return out;
        }
        // Everything else is read-only and answered by the .m facade.
        const std::string text = callFacade(method, params.dump());
        return json::parse(text.empty() ? "{}" : text);
    }
    //=============================================================================
    void
    rpcCallback(webui_event_t* event)
    {
        int id = 0;
        try {
            const char* text = webui_get_string(event);
            const json request = json::parse(text == nullptr ? "{}" : text);
            id = request.value("id", 0);
            const std::string method = request.value("method", "");
            const json params = request.value("params", json::object());
            const std::string response
                = json { { "id", id }, { "result", dispatch(method, params) } }.dump();
            webui_return_string(event, response.c_str());
        } catch (const std::exception& exception) {
            const std::string response
                = json { { "id", id }, { "error", { { "message", exception.what() } } } }.dump();
            webui_return_string(event, response.c_str());
        }
    }
    //=============================================================================
    // Stores the restored (normal) window size, in the physical pixels that
    // webui_set_size uses on Windows. Elsewhere the page reports it.
    void
    rememberWindowSize(size_t window)
    {
#ifdef _WIN32
        HWND hwnd = static_cast<HWND>(webui_win32_get_hwnd(window));
        WINDOWPLACEMENT placement {};
        placement.length = sizeof(placement);
        if (hwnd != nullptr && GetWindowPlacement(hwnd, &placement)) {
            const RECT& r = placement.rcNormalPosition;
            mergeNmmGuiPrefs({ { "width", r.right - r.left }, { "height", r.bottom - r.top } });
        }
#else
        (void)window;
#endif
    }
    //=============================================================================
    bool
    closeCallback(size_t window)
    {
        rememberWindowSize(window);
        std::lock_guard<std::recursive_mutex> lock(stateMutex());
        WindowState& state = windowState();
        if (state.window == window) {
            state.window = 0;
            state.retiredWindow = window;
        }
        return true;
    }
    //=============================================================================
    std::wstring
    frontendRoot()
    {
        return ModulePath(L"nmm_gui", GET_ROOT_PATH) + L"/frontend/dist";
    }
    //=============================================================================
    bool
    showNewWindow()
    {
        WindowState& state = windowState();
        state.window = webui_new_window();
        if (state.window == 0) {
            return false;
        }
        const std::string rootUtf8 = wstring_to_utf8(frontendRoot());
        webui_set_root_folder(state.window, rootUtf8.c_str());
        webui_bind(state.window, RPC_BINDING, rpcCallback);
        webui_set_close_handler_wv(state.window, closeCallback);
        int width = 0;
        int height = 0;
        nmmGuiWindowSize(width, height);
        webui_set_size(state.window, width, height);
        // Process-global: restore it once this window is shown, so the package
        // manager does not leave later windows unable to wait for their
        // connection.
        WebUiConfigScope waitForConnection(show_wait_connection, false);
        // Window icon before show: Linux/GTK applies it as the native window
        // icon, elsewhere it is the served favicon. The Windows title-bar icon
        // is applied with WM_SETICON after show (same as the desktop windows).
        const std::wstring nelsonRoot
            = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
        const std::string iconFile = findNelsonApplicationIconPath(nelsonRoot);
        if (!iconFile.empty()) {
            webui_set_icon_file(state.window, iconFile.c_str());
        }
        if (webui_show_wv(state.window, "index.html")) {
            applyNelsonApplicationWindowIcon(state.window, nelsonRoot);
            return true;
        }
        if (webui_show_browser(state.window, "index.html", Edge)
            || webui_show_browser(state.window, "index.html", Chrome)
            || webui_show_browser(state.window, "index.html", ChromiumBased)) {
            applyNelsonApplicationWindowIcon(state.window, nelsonRoot);
            return true;
        }
        webui_destroy(state.window);
        state.window = 0;
        return false;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
nmm_guiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 3);
    nargoutcheck(nLhs, 0, 0);
    // nmm_gui() opens or focuses the window; nmm_gui(action, name[, version])
    // also asks the page to select that package, and to install it when the
    // action is 'install'. Arguments are validated before anything is opened.
    std::string action;
    std::string name;
    std::string version;
    if (!argIn.empty()) {
        action = argIn[0].asUtf8String();
        if (action != "show" && action != "install") {
            Error(L"Nelson:nmm_gui:invalidAction", L"Valid actions are 'show' and 'install'.");
        }
        if (argIn.size() < 2) {
            Error(L"Nelson:nmm_gui:nameExpected", L"A package name is expected.");
        }
        name = argIn[1].asUtf8String();
        if (!isSafeIdentifier(name)) {
            Error(L"Nelson:nmm_gui:invalidName", L"Invalid package name.");
        }
        if (argIn.size() == 3) {
            version = argIn[2].asUtf8String();
            if (!isSafeVersion(version)) {
                Error(L"Nelson:nmm_gui:invalidVersion", L"Invalid package version.");
            }
        }
    }
    // A web desktop hosts the package manager as a panel: the deep link goes
    // with the request and no WebUI window is opened.
    if (ShowWebToolInHost("nmm",
            json { { "action", action }, { "name", name }, { "version", version } }.dump())) {
        return {};
    }
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    WindowState& state = windowState();
    state.pendingAction = action;
    state.pendingName = name;
    state.pendingVersion = version;
    if (state.retiredWindow != 0) {
        webui_destroy(state.retiredWindow);
        state.retiredWindow = 0;
    }
    if (state.window != 0) {
        webui_run(state.window, "window.nmmGuiReload && window.nmmGuiReload();");
        webui_focus(state.window);
    } else if (!std::filesystem::exists(std::filesystem::path(frontendRoot()) / L"index.html")) {
        // Fail with the reason instead of opening an empty window.
        Error(L"Nelson:nmm_gui:frontendNotBuilt",
            L"The package manager frontend is not built (npm run build in "
            L"modules/nmm_gui/frontend).");
    } else if (!showNewWindow()) {
        Error(L"Nelson:nmm_gui:cannotOpenWindow", L"Cannot open the package manager window.");
    }
    if (!name.empty() && state.window != 0) {
        // Wake a page that is already loaded; a freshly opened one instead
        // picks the pending link up itself once its first load completes.
        webui_run(state.window, "window.nmmGuiPush && window.nmmGuiPush('deeplink', '');");
    }
    return {};
}
//=============================================================================
ArrayOfVector
nmm_gui_notifyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 0);
    std::string channel = "changed";
    if (argIn.size() >= 1) {
        channel = argIn[0].asUtf8String();
    }
    if (!isSafeIdentifier(channel)) {
        Error(L"Nelson:nmm_gui:invalidChannel", L"Invalid notification channel.");
    }
    // Optional payload (progress JSON, an error message): delivered as a JS
    // string literal, so any content is safe to inject into the script.
    std::string payload;
    if (argIn.size() == 2) {
        payload = argIn[1].asUtf8String();
    }
    // The web desktop panel and the window both follow the operations run
    // from the console.
    PushWebToolEvent("nmm", channel, json(payload).dump());
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    WindowState& state = windowState();
    if (state.window != 0) {
        const std::string script = "window.nmmGuiPush && window.nmmGuiPush('" + channel + "', "
            + json(payload).dump() + ");";
        webui_run(state.window, script.c_str());
    }
    return {};
}
//=============================================================================
ArrayOfVector
nmm_gui_i18nBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // __nmm_gui_i18n__() lists the embedded catalogs, __nmm_gui_i18n__(locale)
    // returns one as JSON text ('' when there is none).
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        stringVector locales;
        for (const UiCatalog& catalog : UI_CATALOGS) {
            locales.push_back(catalog.locale);
        }
        return { ArrayOf::cellArrayFromArrayOfCharacterRowVectors(locales) };
    }
    return { ArrayOf::characterArray(uiCatalog(argIn[0].asUtf8String())) };
}
//=============================================================================
ArrayOfVector
nmm_gui_updatesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // __nmm_gui_updates__() returns the update count shown by the desktops,
    // __nmm_gui_updates__(count) sets it (as the window does after a load).
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        setPackageUpdatesCount(static_cast<int>(argIn[0].asDoubleScalar()));
    }
    return { ArrayOf::doubleScalar(static_cast<double>(getPackageUpdatesCount())) };
}
//=============================================================================
ArrayOfVector
nmm_gui_prefsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // __nmm_gui_prefs__() returns the stored window preferences as JSON text,
    // __nmm_gui_prefs__(jsonText) merges keys into them.
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        const json changes = json::parse(argIn[0].asUtf8String(), nullptr, false);
        if (!changes.is_object()) {
            Error(L"Nelson:nmm_gui:invalidPrefs", L"Preferences must be a JSON object.");
        }
        return { ArrayOf::characterArray(mergeNmmGuiPrefs(changes).dump()) };
    }
    return { ArrayOf::characterArray(readNmmGuiPrefs().dump()) };
}
//=============================================================================
void
closeNmmGuiWindow()
{
    size_t window = 0;
    size_t retiredWindow = 0;
    {
        std::lock_guard<std::recursive_mutex> lock(stateMutex());
        WindowState& state = windowState();
        window = state.window;
        retiredWindow = state.retiredWindow;
        state.window = 0;
        state.retiredWindow = 0;
    }
    if (window != 0) {
        rememberWindowSize(window);
        webui_set_close_handler_wv(window, nullptr);
        webui_close(window);
        webui_destroy(window);
    }
    if (retiredWindow != 0 && retiredWindow != window) {
        webui_set_close_handler_wv(retiredWindow, nullptr);
        webui_destroy(retiredWindow);
    }
}
//=============================================================================
} // namespace Nelson::NmmGuiGateway
//=============================================================================
// Web tool dispatcher registered by etc/startup.m (WebToolsHost.hpp): the web
// desktop panel sends the same requests as the window page.
extern "C" NLSNMM_GUI_BUILTIN_IMPEXP int
nlsNmmGuiWebToolDispatch(const char* method, const char* paramsJson,
    void (*sink)(void* context, const char* responseJson), void* context)
{
    if (sink == nullptr) {
        return 0;
    }
    std::string response;
    try {
        const nlohmann::json params
            = nlohmann::json::parse(paramsJson == nullptr ? "{}" : paramsJson, nullptr, false);
        response = nlohmann::json {
            { "result",
                Nelson::NmmGuiGateway::dispatch(method == nullptr ? "" : method,
                    params.is_object() ? params : nlohmann::json::object()) }
        }.dump();
    } catch (const std::exception& exception) {
        response = nlohmann::json { { "error", { { "message", exception.what() } } } }.dump();
    } catch (...) {
        response = nlohmann::json {
            { "error", { { "message", "package manager request failed" } } }
        }.dump();
    }
    sink(context, response.c_str());
    return 1;
}
//=============================================================================
