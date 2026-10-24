//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
#include "RuntimeDispatch.hpp"
#include "UiHtmlHost.hpp"
#include "WebViewer.hpp"
#include "examplesBuiltin.hpp"
#include "render_htmlBuiltin.hpp"
#include "uihtml_spikeBuiltin.hpp"
#include "webviewBuiltin.hpp"
//=============================================================================
extern "C"
{
#include "webui.h"
}
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"webview";
//=============================================================================
static const nlsGateway gateway[] = {
    { "demo", (ptrBuiltin)Nelson::WebviewGateway::examplesBuiltin, 0, 0 },
    { "__webview__", (ptrBuiltin)Nelson::WebviewGateway::webviewBuiltin, 2, -1 },
    { "__webview_render_html__", (ptrBuiltin)Nelson::WebviewGateway::render_htmlBuiltin, 1, 1 },
    { "__uihtml_spike__", (ptrBuiltin)Nelson::WebviewGateway::uihtml_spikeBuiltin, 2, -1 },
};
//=============================================================================
// WebUI expects to own the process' UI thread: `webui_wait()` is where it builds
// its GTK/WebKit widgets on Linux and drives the macOS run loop. In Qt desktop
// mode Nelson never calls it (the Qt event loop owns that thread), so WebUI
// would create the native window of `web()` on the interpreter's worker thread,
// where nothing services it. This hands WebUI a way back to the UI thread.
//
// Not installed in WEB_GUI mode: there WebUI does own the main loop through
// `WebGuiServer`'s `webui_wait_async()`, and must keep doing its own pumping.
extern "C"
{
    static bool
    runOnGuiThread(webui_ui_task_func task, void* arg, bool blocking)
    {
        if (task == nullptr || !Nelson::hasQtRuntime()) {
            return false;
        }
        if (blocking) {
            Nelson::callGuiSync([task, arg]() { task(arg); });
        } else {
            Nelson::postToGui([task, arg]() { task(arg); });
        }
        return true;
    }
    //=============================================================================
    static bool
    isCurrentGuiThread(void)
    {
        return Nelson::isGuiThread();
    }
}
//=============================================================================
static bool
initializeWebviewModule(Nelson::Evaluator*)
{
#ifndef _WIN32
    // Windows needs none of this: its WebView2 backend already owns a dedicated
    // STA host thread with its own message loop, and WebUI ignores the
    // dispatcher there.
    if (Nelson::NelsonConfiguration::getInstance()->getNelsonEngineMode()
        == NELSON_ENGINE_MODE::GUI) {
        webui_set_ui_thread_dispatcher(runOnGuiThread, isCurrentGuiThread);
    }
#endif
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED_LAZY(gateway, (void*)initializeWebviewModule)
//=============================================================================
static bool
finishWebviewModule(Nelson::Evaluator*)
{
    Nelson::UiHtmlCloseAll();
    Nelson::CloseAllWebViewers();
    Nelson::WebviewGateway::closeExamplesWindow();
    // In WEB_GUI mode the whole application runs on WebUI: the main thread is
    // still pumping webui_wait_async() while this module unloads on the
    // evaluator thread. Calling webui_clean() here frees WebUI buffers and
    // destroys its mutexes under the main thread (use-after-free, then AV on a
    // destroyed critical section during _webui_wait_clean). The main loop owns
    // the WebUI teardown in that mode, so skip the library-level cleanup.
    if (Nelson::NelsonConfiguration::getInstance()->getNelsonEngineMode()
        != NELSON_ENGINE_MODE::WEB_GUI) {
        webui_clean();
    }
    webui_set_ui_thread_dispatcher(nullptr, nullptr);
    return true;
}
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishWebviewModule)
//=============================================================================
