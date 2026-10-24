//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebViewer.hpp"
#include "WebUiConfigScope.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
extern "C"
{
#include "webui.h"
}
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <map>
#include <mutex>
#include <string>
#include <vector>
#include "ErrorID.hpp"
//=============================================================================
namespace {
//=============================================================================
constexpr size_t HTML_BUFFER_SIZE = 64 * 1024 * 1024;
constexpr size_t SCRIPT_TIMEOUT_SECONDS = 2;
//=============================================================================
struct ViewerState
{
    size_t window = 0;
    std::string inputUtf8;
    bool visible = true;
    bool showToolbar = true;
    bool showAddressBox = true;
};
//=============================================================================
// Embedded HTML shown by web() is served by an in-memory file handler keyed by
// the webui window id: an embedded HTML string leaves the window unconnected.
// A dedicated mutex (not the viewer mutex) guards it because the handler runs
// on WebUI's webview thread.
std::mutex&
servedHtmlMutex()
{
    static std::mutex mutex;
    return mutex;
}
//=============================================================================
std::map<size_t, std::string>&
servedHtmlByWindow()
{
    static std::map<size_t, std::string> content;
    return content;
}
//=============================================================================
void
dropServedHtml(size_t window)
{
    std::lock_guard<std::mutex> lock(servedHtmlMutex());
    servedHtmlByWindow().erase(window);
}
//=============================================================================
std::mutex&
viewerMutex()
{
    static std::mutex mutex;
    return mutex;
}
//=============================================================================
std::map<size_t, ViewerState>&
viewerStates()
{
    static std::map<size_t, ViewerState> states;
    return states;
}
//=============================================================================
size_t&
lastViewerId()
{
    static size_t id = 0;
    return id;
}
//=============================================================================
std::string
blankHtml()
{
    return "<!doctype html><html><head><meta charset=\"utf-8\"></head><body></body></html>";
}
//=============================================================================
bool
startsWith(const std::string& value, const std::string& prefix)
{
    return value.rfind(prefix, 0) == 0;
}
//=============================================================================
// A file handler bypasses WebUI's automatic webui.js injection. Without the
// <script> reference the page never connects back and the native window is
// treated as never shown. WebUI still serves /webui.js from its core.
std::string
injectWebuiBridge(const std::string& content)
{
    static const std::string bridge("<script src=\"/webui.js\"></script>");
    std::string lowered(content);
    std::transform(lowered.begin(), lowered.end(), lowered.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    if (lowered.find("/webui.js") != std::string::npos) {
        return content;
    }
    std::string result(content);
    size_t headClose = lowered.find("</head>");
    if (headClose != std::string::npos) {
        result.insert(headClose, bridge);
        return result;
    }
    size_t htmlOpen = lowered.find("<html");
    if (htmlOpen != std::string::npos) {
        size_t tagEnd = content.find('>', htmlOpen);
        if (tagEnd != std::string::npos) {
            result.insert(tagEnd + 1, "<head>" + bridge + "</head>");
            return result;
        }
    }
    return "<head>" + bridge + "</head>" + content;
}
//=============================================================================
// The file handler must return a full HTTP response, not the raw body, or the
// WebView reports ERR_INVALID_HTTP_RESPONSE.
std::string
htmlHttpResponse(const std::string& body)
{
    std::string response;
    response.reserve(body.size() + 160);
    response += "HTTP/1.1 200 OK\r\n";
    response += "Content-Type: text/html; charset=utf-8\r\n";
    response += "Content-Length: " + std::to_string(body.size()) + "\r\n";
    response += "Cache-Control: no-cache, no-store, must-revalidate\r\n";
    response += "\r\n";
    response += body;
    return response;
}
//=============================================================================
std::string
contentFromInput(const std::string& input)
{
    if (input.empty()) {
        return blankHtml();
    }
    if (startsWith(input, "text://")) {
        return injectWebuiBridge(input.substr(7));
    }
    return input;
}
//=============================================================================
bool
isEmbeddedHtml(const std::string& content)
{
    std::string lowered(content);
    std::transform(lowered.begin(), lowered.end(), lowered.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return lowered.find("<html") != std::string::npos
        || lowered.find("<!doctype") != std::string::npos;
}
//=============================================================================
extern "C"
{
    static const void*
    webViewerFileHandler(size_t window, const char* filename, int* length)
    {
        if (filename == nullptr || length == nullptr) {
            return nullptr;
        }
        std::lock_guard<std::mutex> lock(servedHtmlMutex());
        auto it = servedHtmlByWindow().find(window);
        if (it == servedHtmlByWindow().end()) {
            return nullptr;
        }
        const std::string path(filename);
        if (path == "/" || path == "/index.html") {
            *length = static_cast<int>(it->second.size());
            return it->second.data();
        }
        return nullptr;
    }
}
//=============================================================================
bool
showContent(ViewerState& state, const std::string& content)
{
    // Wait for the connection only on the first show, so web() returns a ready
    // viewer. A window already shown is connected: waiting again would stall a
    // live viewer for the whole timeout on every visibility change.
    const bool firstShow = !webui_is_shown(state.window);
    webui_set_timeout(firstShow ? 10 : 1);
    Nelson::WebUiConfigScope waitForConnection(show_wait_connection, firstShow);
    // Per-window: `webui_set_config(ui_event_blocking, ...)` would have applied
    // to every other live WebUI window as well.
    webui_set_event_blocking(state.window, true);
    webui_set_size(state.window, 960, 720);
    if (isEmbeddedHtml(content)) {
        {
            std::lock_guard<std::mutex> lock(servedHtmlMutex());
            servedHtmlByWindow()[state.window] = htmlHttpResponse(content);
        }
        webui_set_file_handler_window(state.window, webViewerFileHandler);
        return webui_show_wv(state.window, "index.html");
    }
    dropServedHtml(state.window);
    // webui resolves a shown path against the root folder, so an absolute path
    // gives "Resource Not Available" (seen on Linux): root at the parent, show
    // the basename.
    std::error_code pathError;
    const std::filesystem::path candidate(content);
    if (std::filesystem::is_regular_file(candidate, pathError)) {
        const std::string parent = candidate.parent_path().string();
        if (!parent.empty()) {
            webui_set_root_folder(state.window, parent.c_str());
        }
        return webui_show_wv(state.window, candidate.filename().string().c_str());
    }
    return webui_show_wv(state.window, content.c_str());
}
//=============================================================================
bool
stateIsOpen(const ViewerState& state)
{
    return state.window != 0 && webui_is_shown(state.window);
}
//=============================================================================
const wchar_t*
backendName(size_t browser)
{
    switch (browser) {
    case Webview:
        return L"webview";
    case Chrome:
        return L"chrome";
    case Edge:
        return L"edge";
    case Chromium:
        return L"chromium";
    case ChromiumBased:
        return L"chromium-based";
    case Firefox:
        return L"firefox";
    case Safari:
        return L"safari";
    case Opera:
        return L"opera";
    case Brave:
        return L"brave";
    case Vivaldi:
        return L"vivaldi";
    case Epic:
        return L"epic";
    case Yandex:
        return L"yandex";
    case AnyBrowser:
        return L"browser";
    default:
        return L"none";
    }
}
//=============================================================================
bool
findState(size_t viewerId, ViewerState*& state)
{
    auto& states = viewerStates();
    auto it = states.find(viewerId);
    if (it == states.end() || !stateIsOpen(it->second)) {
        if (it != states.end()) {
            dropServedHtml(it->second.window);
            states.erase(it);
        }
        state = nullptr;
        return false;
    }
    state = &it->second;
    return true;
}
//=============================================================================
} // namespace
//=============================================================================
namespace Nelson {
//=============================================================================
bool
OpenWebViewer(const std::wstring& input, const WebViewerOptions& options, size_t& viewerId,
    std::wstring& currentInput, std::wstring& error)
{
    std::lock_guard<std::mutex> lock(viewerMutex());
    viewerId = 0;
    currentInput.clear();
    error.clear();

    ViewerState* state = nullptr;
    if (!options.newWindow && lastViewerId() != 0) {
        findState(lastViewerId(), state);
    }

    if (state == nullptr) {
        size_t window = webui_new_window();
        if (window == 0) {
            error = Nelson::resolveMessageTemplate(
                L"Nelson:webview:cannotCreateHtmlViewer", L"Cannot create HTML viewer.");
            return false;
        }
        ViewerState newState;
        newState.window = window;
        newState.showToolbar = options.showToolbar;
        newState.showAddressBox = options.showAddressBox;
        auto inserted = viewerStates().emplace(window, newState);
        state = &inserted.first->second;
        lastViewerId() = window;
    }

    state->inputUtf8 = wstring_to_utf8(input);
    state->visible = true;
    if (!showContent(*state, contentFromInput(state->inputUtf8))) {
        error = Nelson::resolveMessageTemplate(
            L"Nelson:webview:cannotOpenHtmlViewer", L"Cannot open HTML viewer.");
        viewerStates().erase(state->window);
        return false;
    }

    viewerId = state->window;
    currentInput = utf8_to_wstring(state->inputUtf8);
    return true;
}
//=============================================================================
bool
SetWebViewerInput(
    size_t viewerId, const std::wstring& input, std::wstring& currentInput, std::wstring& error)
{
    std::lock_guard<std::mutex> lock(viewerMutex());
    currentInput.clear();
    error.clear();

    ViewerState* state = nullptr;
    if (!findState(viewerId, state)) {
        error = _W("HTML viewer is closed.");
        return false;
    }

    state->inputUtf8 = wstring_to_utf8(input);
    if (state->visible && !showContent(*state, contentFromInput(state->inputUtf8))) {
        error = Nelson::resolveMessageTemplate(
            L"Nelson:webview:cannotUpdateHtmlViewer2", L"Cannot update HTML viewer.");
        return false;
    }
    currentInput = utf8_to_wstring(state->inputUtf8);
    return true;
}
//=============================================================================
bool
SetWebViewerVisible(size_t viewerId, bool visible, std::wstring& error)
{
    std::lock_guard<std::mutex> lock(viewerMutex());
    error.clear();

    ViewerState* state = nullptr;
    if (!findState(viewerId, state)) {
        error = _W("HTML viewer is closed.");
        return false;
    }

    const std::string content = visible ? contentFromInput(state->inputUtf8) : blankHtml();
    if (!showContent(*state, content)) {
        error = Nelson::resolveMessageTemplate(
            L"Nelson:webview:cannotUpdateHtmlViewer", L"Cannot update HTML viewer visibility.");
        return false;
    }
    state->visible = visible;
    return true;
}
//=============================================================================
bool
GetWebViewerHtmlText(size_t viewerId, std::wstring& htmlText, std::wstring& error)
{
    std::lock_guard<std::mutex> lock(viewerMutex());
    htmlText.clear();
    error.clear();

    ViewerState* state = nullptr;
    if (!findState(viewerId, state)) {
        error = _W("HTML viewer is closed.");
        return false;
    }

    std::vector<char> htmlBuffer(HTML_BUFFER_SIZE);
    htmlBuffer[0] = '\0';
    const char* script = "return document.documentElement.outerHTML;";
    if (!webui_script(
            state->window, script, SCRIPT_TIMEOUT_SECONDS, htmlBuffer.data(), htmlBuffer.size())) {
        error = Nelson::resolveMessageTemplate(
            L"Nelson:webview:cannotReadHtmlViewer", L"Cannot read HTML viewer content.");
        return false;
    }

    htmlText = utf8_to_wstring(std::string(htmlBuffer.data()));
    return true;
}
//=============================================================================
bool
CloseWebViewer(size_t viewerId)
{
    std::lock_guard<std::mutex> lock(viewerMutex());
    auto& states = viewerStates();
    auto it = states.find(viewerId);
    if (it == states.end()) {
        return false;
    }
    webui_close(it->second.window);
    webui_destroy(it->second.window);
    dropServedHtml(it->second.window);
    states.erase(it);
    if (lastViewerId() == viewerId) {
        lastViewerId() = states.empty() ? 0 : states.rbegin()->first;
    }
    return true;
}
//=============================================================================
void
CloseAllWebViewers()
{
    std::lock_guard<std::mutex> lock(viewerMutex());
    auto& states = viewerStates();
    for (auto& item : states) {
        webui_close(item.second.window);
        webui_destroy(item.second.window);
        dropServedHtml(item.second.window);
    }
    states.clear();
    lastViewerId() = 0;
}
//=============================================================================
bool
IsWebViewerOpen(size_t viewerId)
{
    std::lock_guard<std::mutex> lock(viewerMutex());
    ViewerState* state = nullptr;
    return findState(viewerId, state);
}
//=============================================================================
bool
GetWebViewerBackend(size_t viewerId, std::wstring& backend, std::wstring& error)
{
    std::lock_guard<std::mutex> lock(viewerMutex());
    backend.clear();
    error.clear();

    ViewerState* state = nullptr;
    if (!findState(viewerId, state)) {
        error = _W("HTML viewer is closed.");
        return false;
    }
    backend = backendName(webui_get_current_browser(state->window));
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
