//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "examplesBuiltin.hpp"
#include "ExamplesBridge.hpp"
#include "ExamplesCatalogHost.hpp"
#include "WebToolsHost.hpp"
#include "WebUiConfigScope.hpp"
#include "ExamplesCatalog.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ModulePath.hpp"
#include "PostCommand.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <nlohmann/json.hpp>
#include <algorithm>
#include <mutex>
//=============================================================================
extern "C"
{
#include "webui.h"
}
//=============================================================================
namespace Nelson::WebviewGateway {
//=============================================================================
namespace {
    //=============================================================================
    using json = nlohmann::json;
    struct ExamplesWindowState
    {
        size_t window = 0;
        size_t retiredWindow = 0;
        size_t generation = 0;
        ExamplesCatalog catalog;
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
    ExamplesWindowState&
    windowState()
    {
        // Keep callback state alive until the process has completely stopped.
        static auto* value = new ExamplesWindowState;
        return *value;
    }
    //=============================================================================
    void
    refreshCatalog()
    {
        windowState().catalog = BuildLoadedModulesExamplesCatalog();
    }
    //=============================================================================
    json
    catalogResponse()
    {
        json response = json::parse(windowState().catalog.json);
        response["locale"] = CurrentExamplesLocale();
        return response;
    }
    //=============================================================================
    json
    dispatch(const std::string& method, const json& params)
    {
        if (method == "list") {
            return catalogResponse();
        }
        if (method == "refresh") {
            refreshCatalog();
            return catalogResponse();
        }
        if ((method == "open" || method == "run") && params.contains("path")
            && params.at("path").is_string()) {
            std::wstring command;
            std::string error;
            const std::wstring path = utf8_to_wstring(params.at("path").get<std::string>());
            if (!BuildExamplesCommand(
                    method, path, windowState().catalog.allowedPaths, command, error)) {
                throw std::runtime_error(error);
            }
            if (!postCommand(command)) {
                throw std::runtime_error("cannot post command to the interpreter");
            }
            return { { "accepted", true } };
        }
        if (method == "run" && params.contains("command") && params.at("command").is_string()) {
            std::wstring command;
            std::string error;
            const std::wstring requested = utf8_to_wstring(params.at("command").get<std::string>());
            if (!BuildExamplesCommandFromCatalogCommand(
                    method, requested, windowState().catalog.allowedCommands, command, error)) {
                throw std::runtime_error(error);
            }
            if (!postCommand(command)) {
                throw std::runtime_error("cannot post command to the interpreter");
            }
            return { { "accepted", true } };
        }
        throw std::runtime_error("unsupported examples RPC method");
    }
    //=============================================================================
    void
    rpcCallback(webui_event_t* event)
    {
        int id = 0;
        try {
            std::lock_guard<std::recursive_mutex> lock(stateMutex());
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
    bool
    closeCallback(size_t window)
    {
        std::lock_guard<std::recursive_mutex> lock(stateMutex());
        ExamplesWindowState& state = windowState();
        if (state.window == window) {
            state.window = 0;
            state.retiredWindow = window;
            state.catalog = {};
        }
        return true;
    }
    //=============================================================================
    bool
    showNewWindow()
    {
        ExamplesWindowState& state = windowState();
        state.window = webui_new_window();
        if (state.window == 0) {
            return false;
        }
        const std::wstring root = ModulePath(L"webview", GET_ROOT_PATH) + L"/frontend/examples";
        const std::string rootUtf8 = wstring_to_utf8(root);
        webui_set_root_folder(state.window, rootUtf8.c_str());
        webui_bind(state.window, "examplesRpc", rpcCallback);
        webui_set_close_handler_wv(state.window, closeCallback);
        webui_set_size(state.window, 1080, 760);
        // Process-global: restore it once this window is shown, so the gallery
        // does not leave later windows unable to wait for their connection.
        WebUiConfigScope waitForConnection(show_wait_connection, false);
        if (webui_show_wv(state.window, "index.html")) {
            ++state.generation;
            return true;
        }
        if (webui_show_browser(state.window, "index.html", Edge)
            || webui_show_browser(state.window, "index.html", Chrome)
            || webui_show_browser(state.window, "index.html", ChromiumBased)) {
            ++state.generation;
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
examplesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 0);
    // A web desktop hosts the gallery as a panel: no WebUI window there.
    if (ShowWebToolInHost("examples")) {
        return {};
    }
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    ExamplesWindowState& state = windowState();
    refreshCatalog();
    if (state.retiredWindow != 0) {
        webui_destroy(state.retiredWindow);
        state.retiredWindow = 0;
    }
    if (state.window != 0) {
        webui_run(state.window, "window.reloadExamples && window.reloadExamples();");
        webui_focus(state.window);
    } else {
        if (!showNewWindow()) {
            Error(L"Nelson:examples:cannotOpenGallery", L"Cannot open examples gallery.");
        }
    }
    return {};
}
//=============================================================================
void
closeExamplesWindow()
{
    size_t window = 0;
    size_t retiredWindow = 0;
    {
        std::lock_guard<std::recursive_mutex> lock(stateMutex());
        ExamplesWindowState& state = windowState();
        window = state.window;
        retiredWindow = state.retiredWindow;
        state.window = 0;
        state.retiredWindow = 0;
        state.catalog = {};
    }
    if (window != 0) {
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
void
getExamplesWindowState(size_t& window, size_t& generation, std::string& catalogJson)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    ExamplesWindowState& state = windowState();
    window = state.window;
    generation = state.generation;
    catalogJson = state.catalog.json;
}
//=============================================================================
} // namespace Nelson::WebviewGateway
//=============================================================================
