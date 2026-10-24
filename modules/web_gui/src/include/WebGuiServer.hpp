//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <algorithm>
#include <atomic>
#include <condition_variable>
#include <deque>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <thread>
#include <vector>
#include <nlohmann/json.hpp>
#include "nlsWeb_gui_exports.h"
#include "WebUiHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class WebGuiRpc;
class WebGuiTerminal;
//=============================================================================
// Owns the WebUI window: serves the bundled frontend (frontend/dist), binds the
// single `rpc` function, and pushes events to the browser. Single-window model.
//=============================================================================
class NLSWEB_GUI_IMPEXP WebGuiServer
{
public:
    // Installs the /uihtml/ file handler the first time a uihtml page needs to
    // be served. Kept lazy on purpose: installing a WebUI file handler turns
    // off cookie based client identification for the window, and a desktop that
    // never shows a uihtml should not pay that.
    void
    ensureUiHtmlFileHandler();

    using ProcessQtEventsFn = void (*)(void);
    WebGuiServer();
    ~WebGuiServer();

    bool
    start(const std::wstring& rootFolder, const WebUiLaunchOptions& options);

    void
    wait();

    // Called from the process main thread. It performs the deferred launch and
    // pumps native WebView events while the interpreter runs in its worker.
    bool
    processMainThreadEvents();
    bool
    waitUntilLaunched();
    void
    shutdown();
    static WebGuiServer*
    activeServer();
    std::string
    launchError() const;
    // Figures-only desktop: no browser console window; each figure opens its own
    // native WebView window (nelson-adv-cli --webview).
    bool
    figuresOnly() const
    {
        return m_launchOptions.figuresOnly;
    }

    // Push an event to the frontend: window.__nelsonPush(channel, payloadJson).
    // pushJson targets the main desktop window; pushJsonToWindow targets a
    // specific WebUI window (a separate figure window); frame pushes follow the
    // window hosting the figure.
    void
    pushJson(const std::string& channel, const nlohmann::json& payload);
    void
    pushJsonToWindow(size_t window, const std::string& channel, const nlohmann::json& payload);
    void
    pushFigureFrameJson(long long handle, const char* displayListJson, int len);

    void
    setDispatcher(WebGuiRpc* dispatcher);
    WebGuiRpc*
    dispatcher() const;
    // The terminal is registered so the server can inject `exit` when the web
    // window is closed.
    void
    setTerminal(WebGuiTerminal* terminal);

    // Internal: called by the bound C callback with the request envelope and the
    // WebUI window that issued it (so a standalone figure window can learn which
    // figure it hosts, and dock/undock requests can be routed).
    std::string
    handleRpc(const std::string& envelope, size_t window);

    // Each opens its own WebUI/WebView window on the same served frontend and RPC channel;
    // the standalone page learns its figure through `client.role`. Both are safe
    // to call from any thread: the WebUI work is marshalled to the main thread.
    // width/height (pixels) size the new window; <= 0 uses a default.
    void
    openFigureWindow(long long handle, int width = 0, int height = 0);
    void
    closeFigureWindow(long long handle);
    // Close a figure's window because the figure itself is being destroyed by
    // the engine: no re-dock notification (the figure is gone).
    void
    discardFigureWindow(long long handle);
    // Hide/show a figure's separate native window to match its Visible property.
    void
    setFigureWindowVisible(long long handle, bool visible);
    // Resize a figure's separate native window so its client (drawable) area is
    // clientWidth x clientHeight physical pixels. Only the frontend knows the device
    // pixel ratio and its toolbar/menu chrome, so it computes the target size.
    void
    setFigureWindowClientSize(long long handle, int clientWidth, int clientHeight);
    bool
    isFigureWindowOpen(long long handle) const;
    // The figure a WebUI window hosts, or -1 for the main desktop window.
    long long
    figureForWindow(size_t window) const;
    void
    closeAllFigureWindows();
    // Post a task to run on the process main thread (drained by
    // processMainThreadEvents). WebUI window creation must not run from a WebUI
    // event callback, so it is deferred here.
    void
    postMainThreadTask(std::function<void()> task);

    // The frontend sends a heartbeat every second. A watchdog quits nelson-webview
    // when no heartbeat has arrived for a few seconds (the browser window was
    // closed); a page reload resumes heartbeats in time, so it survives reloads.
    void
    heartbeat();

    // RPC gate. The `rpc` binding is live as soon as the server starts, the engine
    // is not: a browser tab left open from a previous run would drive interpreter
    // and graphics work during initialisation and crash. Requests are refused until
    // the engine reaches its first prompt.
    void
    setEngineReady();
    bool
    engineReady() const
    {
        return m_engineReady.load();
    }

    // Notebook inline-figure capture window. While active, the engine's
    // figure lifecycle/frame hooks do not push to the desktop, so a cell's
    // figures are rendered inline only and never flash as a panel or window.
    // Nestable and thread-safe; always pair begin/end (RAII in notebookRun).
    void
    beginInlineFigureCapture()
    {
        m_inlineFigureCaptureDepth.fetch_add(1);
    }
    void
    endInlineFigureCapture()
    {
        if (m_inlineFigureCaptureDepth.fetch_sub(1) == 1) {
            std::lock_guard<std::mutex> lock(m_inlineBaselineMutex);
            m_inlineFigureBaseline.clear();
        }
    }
    bool
    inlineFigureCaptureActive() const
    {
        return m_inlineFigureCaptureDepth.load() > 0;
    }
    // The figures that already existed when the current capture began. Figures
    // created during the capture are inline-only, so figure.list hides them (a
    // figure.list reconcile must not surface a cell's figure as a panel).
    void
    setInlineFigureBaseline(std::vector<long long> handles)
    {
        std::lock_guard<std::mutex> lock(m_inlineBaselineMutex);
        m_inlineFigureBaseline = std::move(handles);
    }
    // Figures a Notebook cell keeps alive to render interactively inline. They
    // are owned by the notebook, not the desktop: figure.list hides them and the
    // lifecycle/frame hooks never push them, for their whole life (not only during
    // the capture window), so they never appear as a desktop panel or window.
    void
    registerNotebookFigure(long long handle)
    {
        std::lock_guard<std::mutex> lock(m_notebookFiguresMutex);
        m_notebookFigures.insert(handle);
    }
    void
    unregisterNotebookFigure(long long handle)
    {
        std::lock_guard<std::mutex> lock(m_notebookFiguresMutex);
        m_notebookFigures.erase(handle);
    }
    // Transfer ownership of all figures kept alive by a previous Notebook
    // frontend session. The browser can reload while the Wasm worker survives,
    // so the new frontend has no handles with which to close those figures.
    std::vector<long long>
    takeNotebookFigures()
    {
        std::lock_guard<std::mutex> lock(m_notebookFiguresMutex);
        std::vector<long long> handles(m_notebookFigures.begin(), m_notebookFigures.end());
        m_notebookFigures.clear();
        return handles;
    }
    bool
    isNotebookFigure(long long handle) const
    {
        std::lock_guard<std::mutex> lock(m_notebookFiguresMutex);
        return m_notebookFigures.count(handle) != 0;
    }
    bool
    isInlineBaselineFigure(long long handle) const
    {
        std::lock_guard<std::mutex> lock(m_inlineBaselineMutex);
        return std::find(m_inlineFigureBaseline.begin(), m_inlineFigureBaseline.end(), handle)
            != m_inlineFigureBaseline.end();
    }

private:
    static bool
    webViewCloseHandler(size_t window);
    // WebView close handler for a separate figure window: unregisters the window
    // and notifies the desktop so the figure can return to a docked panel.
    static bool
    figureWindowCloseHandler(size_t window);
    void
    drainMainThreadTasks();
    void
    openFigureWindowOnMainThread(long long handle, int width, int height);
    void
    requestExit();
    void
    startWatchdog();
    void
    stopWatchdog();
    void
    clearBridgeHooks();
    void
    startPushWorker();
    void
    stopPushWorker();
    // window == 0 targets the main desktop window (resolved at send time).
    void
    enqueueScript(std::string&& js, size_t window = 0);
    size_t
    windowForFigure(long long handle) const;
    // Open the WebSocket port (deferred until the engine is ready). Idempotent.
    bool
    startListening();
    size_t m_window { 0 };
    WebGuiRpc* m_dispatcher { nullptr };
    WebGuiTerminal* m_terminal { nullptr };
    std::atomic<long long> m_lastBeatMs { -1 };
    std::atomic<bool> m_exitRequested { false };
    std::atomic<bool> m_engineReady { false };
    // The WebSocket port is opened only once the engine is ready (see
    // startListening), so a client cannot connect and race the startup.
    std::atomic<bool> m_listening { false };
    std::atomic<bool> m_launchAttempted { false };
    std::atomic<bool> m_shuttingDown { false };
    // While > 0, figures created by the engine are not surfaced to the desktop (no
    // figure.created/frame pushes, figure.list hides them): a Notebook cell
    // captures its figures inline. Closures are always let through. See notebookRun.
    std::atomic<int> m_inlineFigureCaptureDepth { 0 };
    // Handles that existed when the capture began: the only ones figure.list may
    // report while capturing.
    mutable std::mutex m_inlineBaselineMutex;
    std::vector<long long> m_inlineFigureBaseline;
    // Figures owned by a Notebook cell (kept alive, rendered inline). Hidden
    // from the desktop for their whole life, until notebook.closeFigures.
    mutable std::mutex m_notebookFiguresMutex;
    std::set<long long> m_notebookFigures;
    WebUiLaunchOptions m_launchOptions;
    WebUiLaunchResult m_launchResult;
    std::string m_launchError;
    mutable std::mutex m_launchMutex;
    std::condition_variable m_launchCv;
    bool m_launchCompleted { false };
    bool m_clientReady { false };
    ProcessQtEventsFn m_processQtEvents { nullptr };
    // Push tasks carry their target WebUI window (0 = main desktop window), so
    // figure.frame reaches the separate native window hosting that figure.
    struct PushTask
    {
        size_t window;
        std::string js;
    };
    std::mutex m_pushMutex;
    std::condition_variable m_pushCv;
    std::deque<PushTask> m_pushQueue;
    std::thread m_pushThread;
    bool m_stopPushWorker { false };
    std::mutex m_watchdogMutex;
    std::condition_variable m_watchdogCv;
    std::thread m_watchdogThread;
    // The served frontend root (absolute, UTF-8), reused by figure windows.
    std::string m_rootUtf8;
    mutable std::mutex m_figureWindowsMutex;
    std::map<long long, size_t> m_figureWindows;
    std::map<size_t, long long> m_windowFigure;
    std::mutex m_mainTaskMutex;
    std::deque<std::function<void()>> m_mainTasks;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
