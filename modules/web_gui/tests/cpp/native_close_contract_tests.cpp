//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cctype>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
//=============================================================================
namespace {
//=============================================================================
std::string
withoutWhitespace(const std::string& source)
{
    std::string result;
    for (const unsigned char character : source) {
        if (!std::isspace(character)) {
            result += static_cast<char>(character);
        }
    }
    return result;
}
//=============================================================================
std::string
readSource(const std::filesystem::path& path)
{
    std::ifstream stream(path);
    if (!stream) {
        std::cerr << "Cannot read native-close contract source: " << path << '\n';
        return {};
    }
    std::ostringstream contents;
    contents << stream.rdbuf();
    return withoutWhitespace(contents.str());
}
//=============================================================================
bool
contains(const std::string& source, const std::string& expected, const char* description)
{
    if (source.find(withoutWhitespace(expected)) != std::string::npos) {
        return true;
    }
    std::cerr << "Missing native-close contract: " << description << '\n';
    return false;
}
//=============================================================================
std::string
sourceSection(const std::string& source, const std::string& begin, const std::string& end,
    std::ostream& diagnostics)
{
    const auto start = source.find(withoutWhitespace(begin));
    if (start == std::string::npos) {
        diagnostics << "Missing native-close section start: " << begin << '\n';
        return {};
    }
    const auto finish
        = source.find(withoutWhitespace(end), start + withoutWhitespace(begin).size());
    if (finish == std::string::npos) {
        diagnostics << "Missing native-close section end: " << end << '\n';
        return {};
    }
    return source.substr(start, finish - start);
}
//=============================================================================
bool
testSourceSections()
{
    const auto fixture = withoutWhitespace(
        "void\nwebui_close(size_t window)\n{\n lock();\n if (win->webView) { close(); }\n}\n"
        "void\nwebui_destroy(size_t window) { unrelated(); }");
    std::ostringstream diagnostics;
    const auto section = sourceSection(fixture, "void webui_close(size_t window)",
        "void webui_destroy(size_t window)", diagnostics);
    bool valid = contains(section, "lock(); if (win->webView)", "formatting-independent sections");
    valid &= !section.empty() && section.find("unrelated") == std::string::npos;
    valid &= diagnostics.str().empty();
    valid &= sourceSection(fixture, "absent()", "webui_destroy", diagnostics).empty();
    valid &= sourceSection(fixture, "webui_close", "absent()", diagnostics).empty();
    valid &= sourceSection({}, "webui_close", "webui_destroy", diagnostics).empty();
    valid &= !diagnostics.str().empty();
    if (!valid) {
        std::cerr << "Native-close section extraction regression\n";
    }
    return valid;
}
//=============================================================================
} // namespace
//=============================================================================
int
main()
{
    const std::filesystem::path root(NELSON_SOURCE_ROOT);
    const std::string server = readSource(root / "modules/web_gui/src/cpp/WebGuiServer.cpp");
    const std::string figures = readSource(root / "modules/web_gui/src/cpp/WebGuiRpcFigures.cpp");
    const std::string watchdog = readSource(root / "modules/web_gui/src/cpp/WebGuiWatchdog.cpp");
    const std::string terminal = readSource(root / "modules/web_gui/src/cpp/WebGuiTerminal.cpp");
    const std::string webui = readSource(root / "modules/webview/src/webui/src/webui.c");
    const std::string appkit
        = readSource(root / "modules/webview/src/webui/src/webview/wkwebview.m");

    const auto shutdown = sourceSection(
        server, "WebGuiServer::shutdown()", "WebGuiServer::activeServer()", std::cerr);
    const auto macClose = sourceSection(
        appkit, "bool _webui_macos_wv_close(int index)", "bool _webui_macos_wv_focus", std::cerr);
    const auto webuiClose = sourceSection(
        webui, "void webui_close(size_t window)", "void webui_destroy(size_t window)", std::cerr);

    bool valid = testSourceSections();
    valid &= contains(server,
        "webui_set_close_handler_wv(m_window, WebGuiServer::webViewCloseHandler)",
        "Nelson Web registers its window-specific handler");
    valid &= contains(
        server, "server->requestExit()", "the Nelson Web close handler requests interpreter exit");
    valid &= contains(webui, "return win->close_handler_wv(win->num)",
        "the macOS WebUI bridge forwards the configured handler");
    valid &= contains(webuiClose,
        "_webui_mutex_lock(&win->mutex_webview_update);\n            if (win->webView)",
        "native shutdown rechecks the WebView while holding its cleanup mutex");
    valid &= contains(
        appkit, "should_close_callback(index)", "AppKit asks the WebUI handler before closing");
    valid &= contains(appkit, "applicationShouldTerminate:(NSApplication*)sender",
        "AppKit termination is intercepted");
    valid &= contains(appkit, "[windows[i] performClose:nil]",
        "application termination closes managed windows through their handlers");
    valid &= contains(appkit, "return NSTerminateCancel",
        "AppKit waits for the interpreter when a managed window is closing");
    valid &= contains(
        appkit, "[closingWebView removeFromSuperview]", "the closed native WebView is detached");
    valid &= contains(shutdown, "closeAllFigureWindows()",
        "application shutdown closes every separate Figure window");
    valid &= contains(
        shutdown, "stopWatchdog()", "application shutdown stops its heartbeat watchdog");
    valid &= contains(
        shutdown, "webui_destroy(window)", "application shutdown destroys the main WebUI window");
    valid &= contains(watchdog, "m_watchdogThread.join()",
        "the heartbeat watchdog is joined before server teardown");
    valid &= contains(
        macClose, "[window close]", "a programmatic macOS close runs the normal NSWindow teardown");
    valid &= contains(macClose, "if ([NSThread isMainThread])",
        "a macOS close runs immediately when initiated by the AppKit thread");
    valid &= contains(macClose, "dispatch_sync(dispatch_get_main_queue(), closeWindow)",
        "a worker-thread macOS close completes before its WebUI slot can be reused");
    valid &= contains(webui, "if (_webui_macos_wv_is_main_thread())",
        "webui_close tears down an AppKit window synchronously before destroying its slot");
    valid &= contains(webui, "if (!win->is_closed)",
        "the macOS WebView worker does not close a replacement window twice");
    valid &= contains(terminal, "if (m_interrupt.exchange(false))",
        "the Web terminal handles a posted-command wake-up");
    valid &= contains(terminal, "return std::wstring(1, L'\\n');",
        "a posted-command wake-up is a newline rather than end-of-input");

    // Notebook inline figures: figures a cell keeps alive are owned by the
    // notebook and never surfaced to the desktop.
    valid &= contains(server, "if (server->isNotebookFigure(handle)) {",
        "the figure lifecycle hook never surfaces a Notebook-owned figure");
    valid &= contains(figures, "m_server->isNotebookFigure(handles[i])",
        "figure.list hides Notebook-owned figures");
    valid &= contains(figures, "m_server->registerNotebookFigure(handle)",
        "interactive notebook.run keeps its figures alive and owned");
    valid &= contains(figures, "m_server->registerNotebookFigure(handle);",
        "a figure is owned before the capture guard is released");
    valid &= contains(figures, "WebGuiRpc::notebookCloseFigures",
        "a Notebook can release the figures it kept alive");
    valid &= contains(figures, "m_server->unregisterNotebookFigure(handle)",
        "closing a Notebook figure unregisters its ownership");
    valid &= contains(figures, "m_clearCurrentFigureFn",
        "an interactive cell resets the current figure so cells do not share one");
    valid &= contains(figures, "WebGuiRpc::notebookReleaseFigure",
        "a Notebook figure can be handed to the desktop (open in window)");
    return valid ? 0 : 1;
}
//=============================================================================
