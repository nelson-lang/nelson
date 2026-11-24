//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
//=============================================================================
namespace {
//=============================================================================
std::string
readSource(const std::filesystem::path& path)
{
    std::ifstream stream(path);
    std::ostringstream contents;
    contents << stream.rdbuf();
    return contents.str();
}
//=============================================================================
} // namespace
//=============================================================================
int
main()
{
    const std::filesystem::path root(NELSON_SOURCE_ROOT);
    const std::string source = readSource(root / "modules/gui/src/cpp/GuiServer.cpp");
    const std::string webui = readSource(root / "modules/web_engine/src/webui/src/webui.c");
    const std::string appkit
        = readSource(root / "modules/web_engine/src/webui/src/webview/wkwebview.m");
    const size_t start = source.find("GuiServer::startListening()");
    const size_t finish = source.find("GuiServer::wait()", start);
    if (start == std::string::npos || finish == std::string::npos) {
        std::cerr << "Could not locate the main web launch implementation.\n";
        return 1;
    }

    const std::string launch = source.substr(start, finish - start);
    if (launch.find("webui_show_wv(m_window, \"index.html\")") == std::string::npos) {
        std::cerr << "The main desktop no longer attempts a native WebView launch.\n";
        return 1;
    }
    if (source.find("webui_show_browser") != std::string::npos) {
        std::cerr << "Native desktop windows must not launch an external browser.\n";
        return 1;
    }
    if (launch.find("\"No WebView is available.\"") == std::string::npos) {
        std::cerr << "An unavailable native WebView must report a launch error.\n";
        return 1;
    }
    if (webui.find("(void)_webui_wv_navigate(win->webView, (char*)url)") == std::string::npos) {
        std::cerr << "macOS navigation must stay in the existing native WebView.\n";
        return 1;
    }
    if (webui.find("_webui_macos_wv_new_thread_safe(win->num") != std::string::npos) {
        std::cerr << "macOS native windows must be created synchronously before they are shown.\n";
        return 1;
    }
    if (appkit.find("dispatch_sync(dispatch_get_main_queue(), navigate)") == std::string::npos) {
        std::cerr << "AppKit navigation must complete on the main thread.\n";
        return 1;
    }
    // Without a desktop toolkit there is no native OS file chooser; the web front
    // end draws its own picker, so the bridge reports the native dialog unavailable.
    const std::string rpcSource = readSource(root / "modules/gui/src/cpp/GuiRpc.cpp");
    const size_t dialogStart = rpcSource.find("GuiRpc::fsDialogNative(");
    if (dialogStart == std::string::npos
        || rpcSource.find("native_dialog_unavailable", dialogStart) == std::string::npos) {
        std::cerr << "The file dialog bridge must report that no native chooser exists.\n";
        return 1;
    }
    // Closing the Notebook window must not quit Nelson: no exit handler, watchdog or launch mode.
    const size_t hostStart = source.find("GuiServer::showNotebookWindow()");
    const size_t hostFinish = source.find("GuiServer::openNotebookDocument(", hostStart);
    if (hostStart == std::string::npos || hostFinish == std::string::npos) {
        std::cerr << "Could not locate the notebook host window implementation.\n";
        return 1;
    }
    const std::string host = source.substr(hostStart, hostFinish - hostStart);
    if (host.find("notebookWindowCloseHandler") == std::string::npos
        || host.find("webViewCloseHandler") != std::string::npos) {
        std::cerr << "The notebook host window must use its own close handler.\n";
        return 1;
    }
    if (launch.find("if (!figuresOnly && !notebookHost) {") == std::string::npos
        || launch.find("if (!notebookHost) {") == std::string::npos) {
        std::cerr << "The notebook host must skip the watchdog and the global launch mode.\n";
        return 1;
    }
    return 0;
}
//=============================================================================
