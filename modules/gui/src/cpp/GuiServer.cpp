//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GuiServer.hpp"
#include "UiHtmlFileHandler.hpp"
#include "GuiJson.hpp"
#include "GuiRpc.hpp"
#include "GuiTerminal.hpp"
#include "MainThreadTaskQueue.hpp"
#include "WebUiHelpers.hpp"
#include "GuiPreferences.hpp"
#include "WebUiWindowGeometry.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "DynamicLibrary.hpp"
#include <filesystem>
#include <cstdio>
#include <cmath>
#include <string>
#include <thread>
#include <chrono>
//=============================================================================
extern "C"
{
#include "webui.h"
}
//=============================================================================
// After "webui.h": this header needs its declarations, and pulling it in earlier
// would drag the Win32 headers ahead of <windows.h> below.
#include "WebUiConfigScope.hpp"
#ifdef _WIN32
#include <windows.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
// The bound C callback reaches the server through this pointer.
static std::atomic<GuiServer*> s_activeServer { nullptr };
//=============================================================================
static void
rpcCallback(webui_event_t* e)
{
    try {
        GuiServer* server = s_activeServer.load();
        if (server == nullptr) {
            webui_return_string(e, "{\"error\":{\"message\":\"server not ready\"}}");
            return;
        }
        const char* arg = webui_get_string(e);
        const std::string envelope = (arg != nullptr) ? std::string(arg) : std::string("{}");
        const std::string response = server->handleRpc(envelope, e->window);
        webui_return_string(e, response.c_str());
    } catch (...) {
        webui_return_string(e, "{\"error\":{\"message\":\"RPC dispatch failed\"}}");
    }
}
//=============================================================================
static bool
windowCloseCallback(size_t window)
{
    saveGuiWindowGeometry(window);
    return true;
}
//=============================================================================
static const char*
webFigureWindowStyleName(FigureWindowStyle style)
{
    switch (style) {
    case FigureWindowStyle::Docked:
        return "docked";
    case FigureWindowStyle::Modal:
        return "modal";
    case FigureWindowStyle::AlwaysOnTop:
        return "alwaysontop";
    case FigureWindowStyle::Normal:
    default:
        return "normal";
    }
}
//=============================================================================
// How long a figure window opened hidden waits for its page to size and place
// it before it is shown anyway.
static constexpr std::chrono::milliseconds FIGURE_REVEAL_TIMEOUT(3000);
// The desktop window opens hidden and is shown after this delay, the usual time
// its page needs to paint its splash screen, so no empty frame is seen first.
static constexpr std::chrono::milliseconds MAIN_REVEAL_DELAY(400);
// Time one pump spends creating or destroying windows before it goes back to
// the windows already open.
static constexpr std::chrono::milliseconds WINDOW_LIFETIME_TASK_BUDGET(10);
//=============================================================================
// webui_set_size sizes the whole window, but a figure Position is its client
// area. Grow the window by its chrome so the client area matches, consistent
// with the move/resize write-back. Windows only.
static void
matchFigureWindowClientSize(size_t win, int width, int height)
{
#ifdef _WIN32
    if (width <= 0 || height <= 0) {
        return;
    }
    HWND hwnd = static_cast<HWND>(webui_get_hwnd(win));
    if (hwnd == nullptr) {
        return;
    }
    RECT wr;
    RECT cr;
    if (!GetWindowRect(hwnd, &wr) || !GetClientRect(hwnd, &cr)) {
        return;
    }
    const int chromeW = (wr.right - wr.left) - (cr.right - cr.left);
    const int chromeH = (wr.bottom - wr.top) - (cr.bottom - cr.top);
    if (chromeW <= 0 && chromeH <= 0) {
        return;
    }
    SetWindowPos(hwnd, nullptr, 0, 0, width + chromeW, height + chromeH,
        SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);
#else
    (void)win;
    (void)width;
    (void)height;
#endif
}
//=============================================================================
GuiServer::GuiServer() { s_activeServer.store(this); }
//=============================================================================
GuiServer::~GuiServer() { shutdown(); }
//=============================================================================
void
GuiServer::clearBridgeHooks()
{
    if (m_launchOptions.figuresOnly) {
        MainThreadTaskQueue::getInstance()->setWakeupHook(nullptr);
    }
    WebDesktopBridge::getInstance()->setWebEventPumpHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureFrameHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureFrameAttachmentHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureLifecycleHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureNativeGeometryHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureWindowStyleHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureVisibleHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureTitleHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigurePositionHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebClipboardImageHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebPackageUpdatesHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebEditorOpener(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebNotebookOpener(nullptr, nullptr);
}
//=============================================================================
bool
GuiServer::start(const std::wstring& rootFolder, const WebUiLaunchOptions& options)
{
    WebDesktopBridge::getInstance()->setWebViewUnavailableAtLaunch(false);
    if (!validateWebUiLaunchOptions(options, m_launchOptions, m_launchError)) {
        return false;
    }
    // Headless figure RPC (notebook kernel): no WebUI window and no page. The
    // figure bridge hooks route to the push sink; dispatch is driven by the caller.
    if (m_launchOptions.headlessRpc) {
        installFigureRpcHooks();
        m_listening.store(true);
        return true;
    }
    m_window = webui_new_window();
    if (m_window == 0) {
        return false;
    }
    // Window icon before show: Linux/GTK applies it as the native window icon;
    // elsewhere it is the served favicon (the Windows title-bar icon comes from
    // fibonacci.ico, see prepareNelsonApplicationWindowIcon).
    const std::string applicationIcon = findNelsonApplicationIconPath(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory());
    if (!applicationIcon.empty()) {
        webui_set_icon_file(m_window, applicationIcon.c_str());
    }
    webui_set_close_handler_wv(m_window, windowCloseCallback);
    // The frontend dist lives in the module tree in both the dev build and the
    // installed layout, so resolve it against the Nelson modules directory (known
    // before the evaluator exists). A cwd-relative path is the fallback for unusual
    // layouts. rootFolder is a modules-relative subpath ("gui/dist").
    std::error_code ec;
    std::filesystem::path root;
    const std::wstring modulesDir = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
    if (!modulesDir.empty()) {
        std::filesystem::path fromModules
            = std::filesystem::path(modulesDir) / std::filesystem::path(rootFolder);
        if (std::filesystem::exists(fromModules / "index.html", ec)) {
            root = fromModules.lexically_normal();
        }
    }
    if (root.empty()) {
        root = std::filesystem::absolute(std::filesystem::path(rootFolder), ec);
        if (ec) {
            root = std::filesystem::path(rootFolder);
        }
    }
    const std::string rootUtf8 = wstring_to_utf8(root.wstring());
    m_rootUtf8 = rootUtf8; // reused when opening separate figure windows
    webui_set_root_folder(m_window, rootUtf8.c_str());
    webui_bind(m_window, "rpc", rpcCallback);
    webui_set_config(multi_client, true);
    webui_set_config(show_wait_connection, false);
    const bool publicAccess = m_launchOptions.hasHost && !isWebUiLoopbackHost(m_launchOptions.host);
    webui_set_public(m_window, publicAccess);
    if (m_launchOptions.hasPort && !webui_set_port(m_window, m_launchOptions.port)) {
        m_launchError = "The requested web port is unavailable.";
        return false;
    }
    m_launchResult.publicAccess = publicAccess;
    WebDesktopBridge::getInstance()->setWebNotebookOpener(
        [](void* ctx, const wchar_t* path) {
            static_cast<GuiServer*>(ctx)->openNotebookDocument(
                path != nullptr ? std::wstring(path) : std::wstring());
        },
        this);
    if (m_launchOptions.notebookHost) {
        // The notebook host installs no other desktop hook; the desktop keeps its own windows.
        return true;
    }
    WebDesktopBridge::getInstance()->setWebEditorOpener(
        [](void* ctx, const wchar_t* path, int line, int column) {
            const std::string p
                = (path != nullptr) ? wstring_to_utf8(std::wstring(path)) : std::string();
            nlohmann::json payload { { "path", p } };
            if (line > 0) {
                payload["line"] = line;
            }
            if (column > 0) {
                payload["column"] = column;
            }
            static_cast<GuiServer*>(ctx)->pushJson("editor.show", payload);
        },
        this);
    WebDesktopBridge::getInstance()->setWebFigureFrameHook(
        [](void* ctx, long long handle, const char* json, int len) {
            auto* server = static_cast<GuiServer*>(ctx);
            // Notebook figures render inline: streaming their drawnow frames
            // would open a panel mid-run.
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->pushFigureFrameJson(handle, json, len);
        },
        this);
    // Figures-only desktop (nelson-adv-cli web figures): the engine has no desktop
    // event loop, so it pumps the native figure windows from the interpreter's
    // event servicing through this hook. The full desktop has its own pump
    // loop and must not register it.
    if (m_launchOptions.figuresOnly) {
        WebDesktopBridge::getInstance()->setWebEventPumpHook(
            [](void* ctx) { static_cast<GuiServer*>(ctx)->processMainThreadEvents(); }, this);
        // Figure RPCs run in place while the terminal is idle and drain at
        // statement boundaries while a command runs, so no prompt wake is needed.
        // A no-op wakeup hook stops MainThreadTaskQueue from injecting an Enter,
        // which would print an empty prompt for every mouse event.
        MainThreadTaskQueue::getInstance()->setWakeupHook([]() { });
    }
    WebDesktopBridge::getInstance()->setWebFigureLifecycleHook(
        [](void* ctx, long long handle, bool created, FigureWindowStyle windowStyle) {
            auto* server = static_cast<GuiServer*>(ctx);
            if (!created) {
                std::lock_guard<std::mutex> lock(server->m_figureWindowsMutex);
                server->m_figureTitles.erase(handle);
                server->m_figureNativeGeometry.erase(handle);
                server->m_figureRequestedPosition.erase(handle);
                server->m_figureHiddenAt.erase(handle);
                server->m_figureEngineHidden.erase(handle);
            }
            // A Notebook figure stays inline for its whole life.
            if (server->isNotebookFigure(handle)) {
                return;
            }
            // During inline capture suppress only the CREATION. Always let the
            // CLOSURE through: if something else surfaced the figure (the startup
            // figure.list reconcile), its panel must still be torn down.
            if (created && server->inlineFigureCaptureActive()) {
                return;
            }
            if (server->figuresOnly()) {
                // No desktop console: each figure gets its own native window.
                if (created) {
                    // Open the window at the figure's own size AND place it where
                    // its Position asks, not a fixed default size at the OS default
                    // spot. This lifecycle hook already runs on the interpreter
                    // thread, so the figure property read is in place.
                    int x = 0;
                    int y = 0;
                    int width = 0;
                    int height = 0;
                    GuiRpc* dispatcher = server->dispatcher();
                    if (dispatcher != nullptr) {
                        dispatcher->getFigurePosition(handle, &x, &y, &width, &height);
                    }
                    server->openFigureWindow(handle, x, y, width, height);
                } else {
                    server->discardFigureWindow(handle);
                }
                return;
            }
            server->pushJson(created ? "figure.created" : "figure.closed",
                nlohmann::json { { "handle", handle },
                    // Figures are numbered from 1 for display, while the handle is
                    // the 0-based figure id used for RPCs.
                    { "title", "Figure " + std::to_string(handle + 1) },
                    { "windowStyle", webFigureWindowStyleName(windowStyle) } });
            if (!created) {
                // The engine destroyed the figure: close its native window too,
                // without a re-dock notification.
                server->discardFigureWindow(handle);
            }
        },
        this);
    // __figure_native_geometry__ (engine, interpreter thread) reads the real
    // native window the desktop owns. The value is the frontend's own
    // on-screen measurement, cached per figure, so one code path serves Windows,
    // Linux and macOS without per-OS window queries.
    WebDesktopBridge::getInstance()->setWebFigureNativeGeometryHook(
        [](void* ctx, long long handle, double* x, double* y, double* width,
            double* height) -> bool {
            return static_cast<GuiServer*>(ctx)->nativeFigureGeometry(handle, x, y, width, height);
        },
        this);
    WebDesktopBridge::getInstance()->setWebFigureWindowStyleHook(
        [](void* ctx, long long handle, FigureWindowStyle windowStyle) {
            auto* server = static_cast<GuiServer*>(ctx);
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->pushJson("figure.windowStyle",
                nlohmann::json { { "handle", handle },
                    { "windowStyle", webFigureWindowStyleName(windowStyle) } });
        },
        this);
    WebDesktopBridge::getInstance()->setWebFigureVisibleHook(
        [](void* ctx, long long handle, bool visible) {
            auto* server = static_cast<GuiServer*>(ctx);
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->setFigureWindowVisible(handle, visible);
        },
        this);
    WebDesktopBridge::getInstance()->setWebFigureTitleHook(
        [](void* ctx, long long handle, const char* title) {
            auto* server = static_cast<GuiServer*>(ctx);
            {
                std::lock_guard<std::mutex> lock(server->m_figureWindowsMutex);
                server->m_figureTitles[handle] = title != nullptr ? title : "";
            }
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            const nlohmann::json payload { { "handle", handle },
                { "title", title != nullptr ? title : "" } };
            server->pushJson("figure.title", payload);
            // A separate native window renames itself through the same push.
            const size_t figureWindow = server->windowForFigure(handle);
            if (figureWindow != 0) {
                webui_set_native_title(figureWindow, title != nullptr ? title : "");
                server->pushJsonToWindow(figureWindow, "figure.title", payload);
            }
        },
        this);
    // Programmatic Position change: slide the figure's separate native window so
    // its drawing area follows the new Position.
    WebDesktopBridge::getInstance()->setWebFigurePositionHook(
        [](void* ctx, long long handle, int x, int y, int /*width*/, int /*height*/) {
            auto* server = static_cast<GuiServer*>(ctx);
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->requestFigureWindowPosition(handle, x, y);
        },
        this);
    WebDesktopBridge::getInstance()->setWebClipboardImageHook(
        [](void* ctx, const char* base64, int length) {
            static_cast<GuiServer*>(ctx)->pushJson("clipboard.image",
                nlohmann::json { { "dataUrl",
                    "data:image/png;base64,"
                        + std::string(base64, static_cast<size_t>(length)) } });
        },
        this);
    WebDesktopBridge::getInstance()->setWebPackageUpdatesHook(
        [](void* ctx, int count) {
            static_cast<GuiServer*>(ctx)->pushJson(
                "packages.updates", nlohmann::json { { "count", count } });
        },
        this);
    // ipc(pid, 'minimize', ...) minimizes/restores the native main window.
    WebDesktopBridge::getInstance()->setWebMinimizeHook(
        [](void* ctx, bool minimize) {
            auto* server = static_cast<GuiServer*>(ctx);
            server->m_minimized = minimize;
            if (server->m_window == 0) {
                return;
            }
            if (minimize) {
                webui_minimize(server->m_window);
                return;
            }
#ifdef _WIN32
            HWND hwnd = static_cast<HWND>(webui_get_hwnd(server->m_window));
            if (hwnd != nullptr) {
                ShowWindow(hwnd, SW_RESTORE);
                SetForegroundWindow(hwnd);
            }
#endif
        },
        this);
    WebDesktopBridge::getInstance()->setWebIsMinimizedHook(
        [](void* ctx) -> bool {
            auto* server = static_cast<GuiServer*>(ctx);
#ifdef _WIN32
            if (server->m_window != 0) {
                HWND hwnd = static_cast<HWND>(webui_get_hwnd(server->m_window));
                if (hwnd != nullptr) {
                    return IsIconic(hwnd) != 0;
                }
            }
#endif
            return server->m_minimized;
        },
        this);

    // NOTE: the WebSocket port is NOT opened here but by startListening() once
    // the engine is ready. A browser already pointed at the URL would otherwise
    // send RPC and push traffic to a half-initialised engine and crash it.
    return true;
}
//=============================================================================
bool
GuiServer::startListening()
{
    // Idempotent: only the first call opens the port.
    bool expected = false;
    if (!m_launchAttempted.compare_exchange_strong(expected, true)) {
        return m_listening.load();
    }
    startPushWorker();
    bool ok = false;
    const bool serverOnly = m_launchOptions.serverMode;
    const bool figuresOnly = m_launchOptions.figuresOnly;
    const bool notebookHost = m_launchOptions.notebookHost;
    // Only the full desktop has a console window to close.
    if (!serverOnly && !figuresOnly && !notebookHost) {
        webui_set_close_handler_wv(m_window, GuiServer::webViewCloseHandler);
    }
    if (notebookHost) {
        // One native window on the Notebook editor; no desktop dialog bridge or geometry here.
        ok = showNotebookWindow();
        m_launchResult.mode = WebUiLaunchMode::WebView;
    } else if (figuresOnly) {
        // The WebUI server is started by the first figure window's webui_show_wv
        // (openFigureWindow), like an undocked figure on the full desktop.
        // Starting it here would block the main thread in some WebUI builds.
        ok = true;
        m_launchResult.mode = WebUiLaunchMode::WebView;
    } else if (serverOnly) {
        const char* url = webui_start_server(m_window, "index.html");
        ok = url != nullptr && url[0] != '\0';
        m_launchResult.mode = WebUiLaunchMode::Server;
    } else {
        applyGuiWindowGeometry(m_window);
        prepareNelsonWebUiWindow(m_window, "Nelson");
#ifdef _WIN32
        // Shown once its page has had time to paint (revealMainWindow), not as
        // an empty frame.
        webui_set_hide(m_window, true);
        m_mainWindowHiddenAt = std::chrono::steady_clock::now();
        m_mainWindowHidden.store(true);
#endif
        if (webui_show_wv(m_window, "index.html")) {
            ok = true;
            m_launchResult.mode = WebUiLaunchMode::WebView;
#ifndef _WIN32
            // Only the Win32 window takes its whole geometry at creation.
            applyGuiWindowGeometry(m_window);
#endif
        }
    }
    if (!ok) {
        stopPushWorker();
        {
            std::lock_guard<std::mutex> lock(m_launchMutex);
            m_launchError = serverOnly ? "The requested web port is unavailable."
                                       : "No WebView is available.";
            WebDesktopBridge::getInstance()->setWebViewUnavailableAtLaunch(!serverOnly);
            m_launchCompleted = true;
        }
        m_launchCv.notify_all();
        std::fprintf(stderr, "Nelson web launch failed: %s\n", m_launchError.c_str());
        std::fflush(stderr);
        if (m_terminal != nullptr) {
            m_terminal->pushInputLine(L"exit");
        }
        return false;
    }
    m_listening.store(true);
    m_launchResult.port = webui_get_port(m_window);
    const std::string host = m_launchOptions.host.empty() ? "localhost" : m_launchOptions.host;
    m_launchResult.url = "http://" + host + ":" + std::to_string(m_launchResult.port);
    if (!serverOnly && !figuresOnly && !notebookHost) {
        const std::string indexUrl = m_launchResult.url + "/index.html";
        webui_navigate(m_window, indexUrl.c_str());
    }
    // The notebook host leaves the global launch mode alone, so NFlow still sees the native mode.
    if (!notebookHost) {
        WebDesktopBridge::getInstance()->setWebLaunchMode(webUiLaunchModeName(m_launchResult.mode));
        WebDesktopBridge::getInstance()->setWebLaunchUrlAndPort(
            m_launchResult.url, static_cast<uint64_t>(m_launchResult.port));
    }
    {
        std::lock_guard<std::mutex> lock(m_launchMutex);
        m_launchCompleted = true;
    }
    m_launchCv.notify_all();
    if (m_launchResult.publicAccess) {
        std::fprintf(
            stderr, "Warning: Nelson Web is accessible from the network without authentication.\n");
    }
    const bool shouldPrintUrl
        = !figuresOnly && (serverOnly || m_launchOptions.hasHost || m_launchOptions.hasPort);
    if (shouldPrintUrl) {
        std::fprintf(stdout,
            "\n"
            "  Nelson WebView (preview) is running in %s mode.\n"
            "  URL: %s\n"
            "\n",
            webUiLaunchModeName(m_launchResult.mode), m_launchResult.url.c_str());
        std::fflush(stdout);
    }
    // Closing a figures-only or notebook host window must not quit Nelson, so no watchdog here.
    if (!figuresOnly && !notebookHost) {
        startWatchdog();
    }
    return true;
}
//=============================================================================
void
GuiServer::wait()
{
    webui_wait();
}
//=============================================================================
bool
GuiServer::processMainThreadEvents()
{
    // Headless RPC has no WebUI window to pump; the kernel drives its own I/O and
    // the dispatch drains through the interpreter task queue.
    if (m_launchOptions.headlessRpc) {
        return true;
    }
    if (m_engineReady.load() && !m_launchAttempted.load()) {
        startListening();
    }
    if (m_launchAttempted.load() && m_listening.load()) {
        // Deferred WebUI work (figure windows) runs on the main thread, before
        // the WebView event loop is pumped.
        drainMainThreadTasks();
        // Show any figure window opened hidden that its page never placed.
        revealFigureWindowsDue();
        // Show the desktop window once its page has had time to paint.
        if (m_mainWindowHidden.load()
            && std::chrono::steady_clock::now() - m_mainWindowHiddenAt >= MAIN_REVEAL_DELAY) {
            revealMainWindow();
        }
        // The figures-only desktop starts the WebUI event loop only once the first
        // figure window exists. webui_wait_async() with no window can block, which
        // would freeze the interpreter (this pump runs on the interpreter thread).
        if (m_launchOptions.figuresOnly) {
            bool hasWindow;
            {
                std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
                hasWindow = !m_figureWindows.empty();
            }
            if (!hasWindow) {
                return true;
            }
        }
        const bool result = webui_wait_async();
        return result;
    }
    return true;
}
//=============================================================================
bool
GuiServer::waitUntilLaunched()
{
    setEngineReady();
    std::unique_lock<std::mutex> lock(m_launchMutex);
    m_launchCv.wait(lock, [this]() { return m_launchCompleted; });
    // Figures-only: figure windows connect only when a figure is created.
    if (m_launchOptions.figuresOnly) {
        return m_listening.load();
    }
    if (m_listening.load() && !m_launchOptions.serverMode && !m_clientReady) {
        const bool ready = m_launchCv.wait_for(
            lock, std::chrono::seconds(10), [this]() { return m_clientReady; });
        if (!ready) {
            // Typical cause: a stale frontend dist that never sends client.ready.
            // The launch continues, but the timeout leaves a trace.
            std::fprintf(stderr,
                "Nelson web launch: client.ready not received after 10s, continuing without it.\n");
            std::fflush(stderr);
        }
    }
    return m_listening.load();
}
//=============================================================================
void
GuiServer::shutdown()
{
    if (m_shuttingDown.exchange(true)) {
        return;
    }
    m_listening.store(false);
    stopWatchdog();
    stopPushWorker();
    {
        std::lock_guard<std::mutex> lock(m_mainTaskMutex);
        m_mainTasks.clear();
    }
    closeAllFigureWindows();
    const size_t window = m_window;
    m_window = 0;
    if (window != 0) {
        // Only the desktop window has a geometry to remember: the figures-only server never
        // shows its main window.
        if (m_launchAttempted.load() && !m_launchOptions.notebookHost
            && !m_launchOptions.figuresOnly) {
            saveGuiWindowGeometry(window);
        }
        webui_close(window);
        webui_destroy(window);
    }
    // The retired window is the one the user closed; never destroy an id twice.
    if (m_retiredWindow != 0 && m_retiredWindow != window) {
        webui_set_close_handler_wv(m_retiredWindow, nullptr);
        webui_destroy(m_retiredWindow);
    }
    m_retiredWindow = 0;
    clearBridgeHooks();
    WebDesktopBridge::getInstance()->setWebLaunchMode("none");
    WebDesktopBridge::getInstance()->setWebLaunchUrlAndPort("", 0);
    GuiServer* expected = this;
    s_activeServer.compare_exchange_strong(expected, nullptr);
}
//=============================================================================
GuiServer*
GuiServer::activeServer()
{
    return s_activeServer.load();
}
//=============================================================================
std::string
GuiServer::launchError() const
{
    std::lock_guard<std::mutex> lock(m_launchMutex);
    return m_launchError;
}
//=============================================================================
static std::string
escapeForJsSingleQuote(const std::string& s)
{
    std::string out;
    out.reserve(s.size() + 8);
    for (char c : s) {
        switch (c) {
        case '\\':
            out += "\\\\";
            break;
        case '\'':
            out += "\\'";
            break;
        case '\n':
            out += "\\n";
            break;
        case '\r':
            out += "\\r";
            break;
        default:
            out += c;
        }
    }
    return out;
}
//=============================================================================
void
GuiServer::pushJson(const std::string& channel, const nlohmann::json& payload)
{
    pushJsonToWindow(0, channel, payload);
}
//=============================================================================
void
GuiServer::pushJsonToWindow(
    size_t window, const std::string& channel, const nlohmann::json& payload)
{
    // Headless RPC: hand the push to the sink (the kernel writes it as a JSON-RPC
    // notification) instead of a WebUI page.
    if (m_launchOptions.headlessRpc) {
        if (m_pushSink) {
            m_pushSink(channel, serializeGuiJson(payload));
        }
        return;
    }
    // Never push before the port is open and the engine is ready: startup
    // output must not be sent over a WebSocket to a client racing the init.
    if (m_window == 0 || !m_listening.load()) {
        return;
    }
    const std::string js = "window.__nelsonPush && window.__nelsonPush('" + channel + "', '"
        + escapeForJsSingleQuote(serializeGuiJson(payload)) + "');";
    enqueueScript(std::string(js), window);
}
//=============================================================================
void
GuiServer::pushFigureFrameJson(long long handle, const char* displayListJson, int len)
{
    if (displayListJson == nullptr || len <= 0) {
        return;
    }
    // Headless RPC: the frame payload is {handle,displayList}; the sink writes it
    // as a figure.frame notification. Reuse the display list string as is.
    if (m_launchOptions.headlessRpc) {
        if (m_pushSink) {
            std::string payload = "{\"handle\":" + std::to_string(handle) + ",\"displayList\":";
            payload.append(displayListJson, static_cast<size_t>(len));
            payload += "}";
            m_pushSink("figure.frame", payload);
        }
        return;
    }
    if (m_window == 0 || !m_listening.load()) {
        return;
    }
    std::string payload = "{\"handle\":" + std::to_string(handle) + ",\"displayList\":";
    payload.append(displayListJson, static_cast<size_t>(len));
    payload += "}";
    const std::string js = "window.__nelsonPush && window.__nelsonPush('figure.frame', '"
        + escapeForJsSingleQuote(payload) + "');";
    // Frames go to the window that actually hosts the figure: the separate
    // native window when undocked, the desktop (docked panel) otherwise.
    enqueueScript(std::string(js), windowForFigure(handle));
}
//=============================================================================
void
GuiServer::installFigureRpcHooks()
{
    // The desktop figure frame/lifecycle/title hooks, but every push is routed to
    // the sink (pushJson / pushFigureFrameJson) and no window ever opens.
    WebDesktopBridge::getInstance()->setWebFigureFrameHook(
        [](void* ctx, long long handle, const char* json, int len) {
            auto* server = static_cast<GuiServer*>(ctx);
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->pushFigureFrameJson(handle, json, len);
        },
        this);
    WebDesktopBridge::getInstance()->setWebFigureLifecycleHook(
        [](void* ctx, long long handle, bool created, FigureWindowStyle windowStyle) {
            auto* server = static_cast<GuiServer*>(ctx);
            if (!created) {
                std::lock_guard<std::mutex> lock(server->m_figureWindowsMutex);
                server->m_figureTitles.erase(handle);
            }
            if (server->isNotebookFigure(handle)) {
                return;
            }
            if (created && server->inlineFigureCaptureActive()) {
                return;
            }
            server->pushJson(created ? "figure.created" : "figure.closed",
                nlohmann::json { { "handle", handle },
                    { "title", "Figure " + std::to_string(handle + 1) },
                    { "windowStyle", webFigureWindowStyleName(windowStyle) } });
        },
        this);
    WebDesktopBridge::getInstance()->setWebFigureTitleHook(
        [](void* ctx, long long handle, const char* title) {
            auto* server = static_cast<GuiServer*>(ctx);
            {
                std::lock_guard<std::mutex> lock(server->m_figureWindowsMutex);
                server->m_figureTitles[handle] = title != nullptr ? title : "";
            }
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->pushJson("figure.title",
                nlohmann::json {
                    { "handle", handle }, { "title", title != nullptr ? title : "" } });
        },
        this);
    // Headless: no native window to hide, so report a figure's Visible change to
    // the RPC client, which shows or hides its figure panel accordingly.
    WebDesktopBridge::getInstance()->setWebFigureVisibleHook(
        [](void* ctx, long long handle, bool visible) {
            auto* server = static_cast<GuiServer*>(ctx);
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->pushJson(
                "figure.visible", nlohmann::json { { "handle", handle }, { "visible", visible } });
        },
        this);
}
//=============================================================================
void
GuiServer::startPushWorker()
{
    std::lock_guard<std::mutex> lock(m_pushMutex);
    if (m_pushThread.joinable()) {
        return;
    }
    m_stopPushWorker = false;
    m_pushThread = std::thread([this]() {
        for (;;) {
            PushTask task;
            {
                std::unique_lock<std::mutex> lock(m_pushMutex);
                m_pushCv.wait(lock, [this] { return m_stopPushWorker || !m_pushQueue.empty(); });
                if (m_stopPushWorker && m_pushQueue.empty()) {
                    return;
                }
                task = std::move(m_pushQueue.front());
                m_pushQueue.pop_front();
            }
            const size_t target = task.window != 0 ? task.window : m_window;
            if (target != 0) {
                webui_run(target, task.js.c_str());
            }
        }
    });
}
//=============================================================================
void
GuiServer::stopPushWorker()
{
    {
        std::lock_guard<std::mutex> lock(m_pushMutex);
        m_stopPushWorker = true;
        m_pushQueue.clear();
    }
    m_pushCv.notify_all();
    if (m_pushThread.joinable()) {
        m_pushThread.join();
    }
}
//=============================================================================
void
GuiServer::enqueueScript(std::string&& js, size_t window)
{
    {
        std::lock_guard<std::mutex> lock(m_pushMutex);
        if (m_stopPushWorker) {
            return;
        }
        m_pushQueue.push_back(PushTask { window, std::move(js) });
    }
    m_pushCv.notify_one();
}
//=============================================================================
size_t
GuiServer::windowForFigure(long long handle) const
{
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    auto it = m_figureWindows.find(handle);
    return it != m_figureWindows.end() ? it->second : 0;
}
//=============================================================================
void
GuiServer::setDispatcher(GuiRpc* dispatcher)
{
    m_dispatcher = dispatcher;
}
//=============================================================================
void
GuiServer::setTerminal(GuiTerminal* terminal)
{
    m_terminal = terminal;
}
//=============================================================================
bool
GuiServer::webViewCloseHandler(size_t window)
{
    GuiServer* server = s_activeServer.load();
    if (server != nullptr && server->m_window == window) {
        // The window is still there: remember its geometry now, shutdown() runs
        // once it is destroyed.
        saveGuiWindowGeometry(window);
        server->requestExit();
    }
    return true;
}
//=============================================================================
bool
GuiServer::notebookWindowCloseHandler(size_t window)
{
    GuiServer* server = s_activeServer.load();
    if (server != nullptr && server->m_window == window) {
        // No push reaches a closed window; the next open shows a fresh one.
        server->m_listening.store(false);
        server->m_notebookWindowOpen.store(false);
        {
            std::lock_guard<std::mutex> lock(server->m_launchMutex);
            server->m_clientReady = false;
            server->m_pendingNotebookOpens.clear();
        }
        // webui_destroy is unsafe inside the close callback, so the id is released later.
        server->m_retiredWindow = window;
        server->m_window = 0;
    }
    return true;
}
//=============================================================================
bool
GuiServer::showNotebookWindow()
{
    if (m_retiredWindow != 0) {
        webui_set_close_handler_wv(m_retiredWindow, nullptr);
        webui_destroy(m_retiredWindow);
        m_retiredWindow = 0;
    }
    const std::wstring nelsonRoot = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
    if (m_window == 0) {
        // After a close, a new WebUI window serves the same frontend and RPC.
        m_window = webui_new_window();
        if (m_window == 0) {
            return false;
        }
        webui_set_root_folder(m_window, m_rootUtf8.c_str());
        webui_bind(m_window, "rpc", rpcCallback);
        const std::string applicationIcon = findNelsonApplicationIconPath(nelsonRoot);
        if (!applicationIcon.empty()) {
            webui_set_icon_file(m_window, applicationIcon.c_str());
        }
    }
    webui_set_close_handler_wv(m_window, GuiServer::notebookWindowCloseHandler);
    webui_set_size(m_window, webUiScaledToDisplay(1080), webUiScaledToDisplay(760));
    // Process-global option restored after show; the interpreter must not wait for the page socket.
    WebUiConfigScope waitForConnection(show_wait_connection, false);
    // Fixed title: keep it while the page (titled "Nelson" until mounted) loads.
    prepareNelsonWebUiWindow(m_window, "Notebook");
    webui_set_native_title_owned(m_window, true);
    if (!webui_show_wv(m_window, "index.html")) {
        return false;
    }
    webui_focus(m_window);
    m_notebookWindowOpen.store(true);
    m_listening.store(true);
    return true;
}
//=============================================================================
bool
GuiServer::openNotebookDocument(const std::wstring& path)
{
    const std::string utf8Path = wstring_to_utf8(path);
    if (!m_launchOptions.notebookHost) {
        pushJson("notebook.open", nlohmann::json { { "path", utf8Path } });
        return true;
    }
    if (!m_notebookWindowOpen.load()) {
        const bool shown = m_launchAttempted.load() ? showNotebookWindow() : startListening();
        if (!shown) {
            return false;
        }
    } else if (m_window != 0) {
        webui_focus(m_window);
    }
    // client.ready flushes the documents requested while the page loads.
    std::lock_guard<std::mutex> lock(m_launchMutex);
    if (m_clientReady) {
        pushJson("notebook.open", nlohmann::json { { "path", utf8Path } });
    } else {
        m_pendingNotebookOpens.push_back(utf8Path);
    }
    return true;
}
//=============================================================================
void
GuiServer::requestExit()
{
    GuiTerminal* terminal = m_terminal;
    if (terminal == nullptr) {
        return;
    }
    bool expected = false;
    if (m_exitRequested.compare_exchange_strong(expected, true)) {
        terminal->pushInputLine(L"exit");
    }
}
//=============================================================================
GuiRpc*
GuiServer::dispatcher() const
{
    return m_dispatcher;
}
//=============================================================================
std::string
GuiServer::handleRpc(const std::string& envelope, size_t window)
{
    if (m_dispatcher == nullptr) {
        return "{\"error\":{\"message\":\"no dispatcher\"}}";
    }
    // after shutdown the interpreter drains no tasks: a request here would delay the teardown
    if (m_shuttingDown.load()) {
        return "{\"error\":{\"message\":\"Nelson is shutting down\"}}";
    }
    // The desktop page already runs: its window, opened hidden, can be shown.
    if (window == m_window && m_mainWindowHidden.load()) {
        postMainThreadTask([this]() { revealMainWindow(); });
    }
    // A window whose figure is gone is only waiting for its teardown: its page
    // must not act on a new figure that reuses the handle.
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        if (m_retiredWindows.count(window) != 0) {
            return "{\"error\":{\"message\":\"figure window closed\"}}";
        }
    }
    nlohmann::json request = nlohmann::json::parse(envelope, nullptr, false);
    const std::string method = (!request.is_discarded() && request.contains("method"))
        ? request.value("method", std::string())
        : std::string();
    auto withId = [&](nlohmann::json reply) {
        if (!request.is_discarded() && request.contains("id")) {
            reply["id"] = request["id"];
        }
        return serializeGuiJson(reply);
    };
    // client.role: which figure this window hosts, or the desktop. Answered
    // without the interpreter, so it works during startup.
    if (method == "client.role") {
        const long long handle = figureForWindow(window);
        // The launch mode is carried here because client.role is answered
        // reliably during startup, unlike the polled app.capabilities. The desktop
        // decides from it whether a new figure opens in a native window, so a
        // missing value would dock every figure.
        const char* launchMode = webUiLaunchModeName(m_launchResult.mode);
        nlohmann::json result;
        if (handle >= 0) {
            // With the interpreter at its prompt the page can fetch its first
            // frame at once, with no wait for a running animation to push one.
            const bool engineIdle = (m_terminal != nullptr && m_terminal->isAtPrompt())
                || MainThreadTaskQueue::getInstance()->isInterpreterIdle();
            result = { { "role", "figure" }, { "handle", handle }, { "launchMode", launchMode },
                { "engineIdle", engineIdle } };
        } else if (m_launchOptions.notebookHost) {
            result = { { "role", "notebook" }, { "launchMode", launchMode } };
        } else {
            result = { { "role", "desktop" }, { "launchMode", launchMode } };
        }
        return withId(nlohmann::json { { "result", result } });
    }
    if (method == "client.ready") {
        if (window == m_window) {
            std::vector<std::string> pending;
            {
                std::lock_guard<std::mutex> lock(m_launchMutex);
                m_clientReady = true;
                pending.swap(m_pendingNotebookOpens);
            }
            m_launchCv.notify_all();
            // Documents requested while the notebook host page was loading.
            for (const std::string& path : pending) {
                pushJson("notebook.open", nlohmann::json { { "path", path } });
            }
        }
        return withId(nlohmann::json { { "result", { { "ok", true } } } });
    }
    if (!m_engineReady.load()) {
        // Still starting up. Only the heartbeat is answered: it keeps the
        // watchdog fed without touching the interpreter or graphics bridge.
        if (method == "app.heartbeat" || method == "ping") {
            heartbeat();
            return withId(
                nlohmann::json { { "result", { { "ok", true }, { "starting", true } } } });
        }
        return withId(
            nlohmann::json { { "error", { { "message", "Nelson is still starting" } } } });
    }
    // figure.undock / figure.dock: the dispatcher owns the graphics bridge (the
    // figure size) and calls back to open/close the native window.
    return m_dispatcher->dispatch(envelope);
}
//=============================================================================
void
GuiServer::postMainThreadTask(std::function<void()> task)
{
    std::lock_guard<std::mutex> lock(m_mainTaskMutex);
    m_mainTasks.push_back(std::move(task));
}
//=============================================================================
void
GuiServer::postWindowLifetimeTask(std::function<void()> task)
{
    std::lock_guard<std::mutex> lock(m_mainTaskMutex);
    m_windowLifetimeTasks.push_back(std::move(task));
}
//=============================================================================
void
GuiServer::drainMainThreadTasks()
{
    // Quick window operations first, all of them.
    std::deque<std::function<void()>> tasks;
    {
        std::lock_guard<std::mutex> lock(m_mainTaskMutex);
        tasks.swap(m_mainTasks);
    }
    for (auto& task : tasks) {
        task();
    }
    // Then window creations and destructions, in order, within a short budget.
    // A creation blocks for tens of milliseconds: the rest waits for the next
    // pump, so the windows already open keep being sized, placed and shown.
    const auto start = std::chrono::steady_clock::now();
    for (;;) {
        std::function<void()> task;
        {
            std::lock_guard<std::mutex> lock(m_mainTaskMutex);
            if (m_windowLifetimeTasks.empty()) {
                break;
            }
            task = std::move(m_windowLifetimeTasks.front());
            m_windowLifetimeTasks.pop_front();
        }
        task();
        if (std::chrono::steady_clock::now() - start >= WINDOW_LIFETIME_TASK_BUDGET) {
            return;
        }
    }
    // Last, with nothing else waiting, the windows already taken off screen.
    while (std::chrono::steady_clock::now() - start < WINDOW_LIFETIME_TASK_BUDGET) {
        size_t win = 0;
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            if (m_windowTeardowns.empty()) {
                return;
            }
            win = m_windowTeardowns.front();
            m_windowTeardowns.pop_front();
        }
        destroyRetiredWindow(win);
    }
}
//=============================================================================
void
GuiServer::destroyRetiredWindow(size_t win)
{
    // webui_close alone leaves a WebView window on screen: webui_destroy is required.
    webui_close(win);
    webui_destroy(win);
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    m_retiredWindows.erase(win);
}
//=============================================================================
long long
GuiServer::figureForWindow(size_t window) const
{
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    auto it = m_windowFigure.find(window);
    return it != m_windowFigure.end() ? it->second : -1;
}
//=============================================================================
bool
GuiServer::isFigureWindowOpen(long long handle) const
{
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    return m_figureWindows.find(handle) != m_figureWindows.end();
}
//=============================================================================
void
GuiServer::setFigureNativeGeometry(
    long long handle, double x, double y, double width, double height)
{
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    m_figureNativeGeometry[handle] = { x, y, width, height };
}
//=============================================================================
bool
GuiServer::nativeFigureGeometry(
    long long handle, double* x, double* y, double* width, double* height) const
{
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    auto it = m_figureNativeGeometry.find(handle);
    if (it == m_figureNativeGeometry.end()) {
        return false;
    }
    if (x != nullptr) {
        *x = it->second[0];
    }
    if (y != nullptr) {
        *y = it->second[1];
    }
    if (width != nullptr) {
        *width = it->second[2];
    }
    if (height != nullptr) {
        *height = it->second[3];
    }
    return true;
}
//=============================================================================
void
GuiServer::openFigureWindow(long long handle, int x, int y, int width, int height)
{
    postWindowLifetimeTask([this, handle, x, y, width, height]() {
        openFigureWindowOnMainThread(handle, x, y, width, height);
    });
}
//=============================================================================
void
GuiServer::openFigureWindowOnMainThread(long long handle, int x, int y, int width, int height)
{
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        auto it = m_figureWindows.find(handle);
        if (it != m_figureWindows.end()) {
            webui_focus(it->second);
            return;
        }
    }
    const size_t win = webui_new_window();
    if (win == 0) {
        return;
    }
    webui_set_root_folder(win, m_rootUtf8.c_str());
    webui_bind(win, "rpc", rpcCallback);
    webui_set_close_handler_wv(win, figureWindowCloseHandler);
    // Window icon and title before show, so the window never appears untitled
    // with a generic icon: on Linux/GTK WebUI applies the favicon as the native
    // window icon, on Windows the .ico is set when the window is created. macOS
    // has no per-window icon.
    {
        const std::wstring modulesDir
            = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
        if (!modulesDir.empty()) {
            const std::wstring iconBase
                = modulesDir + L"/graphics/resources/icon-mathematical-plot";
            webui_set_icon_file(win, wstring_to_utf8(iconBase + L".png").c_str());
            prepareWebUiWindowIcon(win, iconBase + L".ico");
        }
    }
    {
        std::string title = "Figure " + std::to_string(handle + 1);
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            auto it = m_figureTitles.find(handle);
            if (it != m_figureTitles.end()) {
                title = it->second;
            }
        }
        // The title follows the figure properties, never the page (no flicker while it loads).
        webui_set_native_title(win, title.c_str());
        webui_set_native_title_owned(win, true);
    }
    // The figure size is in logical pixels, the window calls take display pixels.
    width = static_cast<int>(webUiScaledToDisplay(width));
    height = static_cast<int>(webUiScaledToDisplay(height));
    webui_set_size(win, width > 0 ? static_cast<unsigned int>(width) : webUiScaledToDisplay(840),
        height > 0 ? static_cast<unsigned int>(height) : webUiScaledToDisplay(680));
    // Open the window hidden. Only the page knows its toolbar height, so the host
    // cannot size and place the window before the page has loaded. The page then
    // drives the size and reports where the drawing area sits, the window is
    // moved to the figure Position, and only then shown (revealFigureWindow).
    // Only the Win32 WebView honors a hidden open.
#ifdef _WIN32
    const bool openHidden = width > 0 && height > 0;
#else
    const bool openHidden = false;
#endif
    if (openHidden) {
        webui_set_hide(win, true);
    }
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        m_figureWindows[handle] = win;
        m_windowFigure[win] = handle;
        m_newestFigureWindow = handle;
        if (openHidden) {
            m_figureHiddenAt[handle] = std::chrono::steady_clock::now();
        } else {
            m_figureHiddenAt.erase(handle);
        }
    }
    // A native figure window loads the slim figure page (figure.html), not the
    // full desktop index.html: it pulls in only the figure panel, not the Monaco
    // editor / xterm console / dockview shell, so the window appears far faster.
    // figure.html is deployed next to index.html in the same dist root, and still
    // reads its figure handle from client.role.
    //
    // show_wait_connection is false so webui_show_wv does not block the
    // interpreter waiting for the page's WebSocket. FigurePanel's refresh() on
    // mount recovers a figure.frame pushed before the socket is up.
    //
    // The option is process-global, so it is set explicitly here: an HTML viewer
    // or a Markdown render opened meanwhile sets it to true for its own window.
    WebUiConfigScope waitForConnection(show_wait_connection, false);
    const bool shown = webui_show_wv(win, "figure.html");
    if (!shown) {
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            m_figureWindows.erase(handle);
            m_windowFigure.erase(win);
            m_figureHiddenAt.erase(handle);
        }
        // No transient docked panel exists while the window launches: tell the
        // desktop to re-dock the figure.
        pushJson("figure.window",
            nlohmann::json { { "handle", handle }, { "open", false }, { "reason", "fallback" } });
        return;
    }
    // Showing a WebView window does not reliably make it the key window (notably
    // macOS when the command window started the figure). Focus it so the new
    // figure is visible and receives keyboard and mouse input. A window opened
    // hidden is focused when it is revealed.
    if (!openHidden) {
        webui_focus(win);
    }
    if (width > 0 && height > 0) {
        matchFigureWindowClientSize(win, width, height);
    }
    // Remember where the figure Position asks its drawing area to sit; once the
    // frontend reports where the window actually landed, nudgeFigureWindowToRequested
    // slides the window by the difference. A delta move needs no screen height,
    // chrome or toolbar height (they cancel), so one path serves every platform.
    if (width > 0 && height > 0) {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        m_figureRequestedPosition[handle] = { x, y };
    }
    pushJson("figure.window", nlohmann::json { { "handle", handle }, { "open", true } });
}
//=============================================================================
bool
GuiServer::nudgeFigureWindowToRequested(
    long long handle, double measuredX, double measuredBottom, int* settledX, int* settledBottom)
{
    int requestedX = 0;
    int requestedBottom = 0;
    size_t win = 0;
    bool hasRequest = false;
    bool pendingReveal = false;
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        pendingReveal = m_figureHiddenAt.count(handle) != 0;
        auto req = m_figureRequestedPosition.find(handle);
        if (req != m_figureRequestedPosition.end()) {
            auto w = m_figureWindows.find(handle);
            if (w == m_figureWindows.end()) {
                m_figureRequestedPosition.erase(req);
                return false;
            }
            requestedX = req->second.first;
            requestedBottom = req->second.second;
            win = w->second;
            hasRequest = true;
        }
    }
    const double dX = requestedX - measuredX;
    const double dBottom = requestedBottom - measuredBottom;
    // Converged (within a couple pixels): stop nudging and forget the request.
    const bool converged = hasRequest && std::fabs(dX) <= 2.0 && std::fabs(dBottom) <= 2.0;
    if (converged) {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        m_figureRequestedPosition.erase(handle);
        // In place, give or take the pixel rounding of the display scaling: the
        // Position to keep is the requested one, not a value a pixel off.
        if (settledX != nullptr) {
            *settledX = requestedX;
        }
        if (settledBottom != nullptr) {
            *settledBottom = requestedBottom;
        }
    }
    if (!hasRequest || converged) {
        // The page only reports once it is sized, so a window opened hidden can
        // be shown now.
        if (pendingReveal) {
            postMainThreadTask([this, handle]() { revealFigureWindow(handle); });
        }
        return false;
    }
    postMainThreadTask([this, handle, win, dX, dBottom, pendingReveal]() {
        int gx = 0;
        int gy = 0;
        unsigned int gw = 0;
        unsigned int gh = 0;
        bool maximized = false;
        if (webui_get_window_geometry(win, &gx, &gy, &gw, &gh, &maximized) && !maximized) {
#ifdef _WIN32
            // webui geometry/position is physical pixels on Windows; the figure
            // Position (and so the delta) is logical, so scale by the window DPI.
            HWND hwnd = static_cast<HWND>(webui_get_hwnd(win));
            const UINT dpi = hwnd != nullptr ? GetDpiForWindow(hwnd) : 96;
            const double scale = (dpi > 0 ? static_cast<double>(dpi) : 96.0) / 96.0;
            int nx = gx + static_cast<int>(std::lround(dX * scale));
            int ny = gy - static_cast<int>(std::lround(dBottom * scale));
#else
            // GTK/macOS: webui geometry/position is in logical units, so the delta
            // applies directly. Top-down origin assumed (verify on macOS).
            int nx = gx + static_cast<int>(std::lround(dX));
            int ny = gy - static_cast<int>(std::lround(dBottom));
#endif
            nx = nx < 0 ? 0 : nx;
            ny = ny < 0 ? 0 : ny;
            webui_set_position(win, static_cast<unsigned int>(nx), static_cast<unsigned int>(ny));
#ifdef _WIN32
            // webui_set_position is applied later by the WebView thread. Move the
            // window now, so it is in place before it is shown below.
            if (pendingReveal && hwnd != nullptr) {
                SetWindowPos(
                    hwnd, nullptr, nx, ny, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
            }
#endif
        }
        // The delta move puts the drawing area at its Position: show the window
        // only now, so it is never seen at its opening spot.
        if (pendingReveal) {
            revealFigureWindow(handle);
        }
    });
    // Not shown yet: this report is the opening spot, not a place to keep.
    return pendingReveal;
}
//=============================================================================
void
GuiServer::revealFigureWindow(long long handle)
{
    size_t win = 0;
    bool hidden = false;
    bool focus = false;
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        if (m_figureHiddenAt.erase(handle) == 0) {
            return;
        }
        auto w = m_figureWindows.find(handle);
        if (w == m_figureWindows.end()) {
            return;
        }
        win = w->second;
        hidden = m_figureEngineHidden.count(handle) != 0;
        // Pages do not get ready in opening order: only the newest figure takes
        // the focus, so an older one shown later does not cover it.
        focus = !hidden && handle == m_newestFigureWindow;
    }
    // A figure with Visible off keeps its window hidden.
    webui_set_hidden(win, hidden);
    if (focus) {
        webui_focus(win);
    }
}
//=============================================================================
void
GuiServer::revealMainWindow()
{
    if (!m_mainWindowHidden.exchange(false)) {
        return;
    }
    webui_set_hidden(m_window, false);
    webui_focus(m_window);
}
//=============================================================================
void
GuiServer::revealFigureWindowsDue()
{
    // Safety net for figure windows opened hidden: show any whose page has not
    // reported its geometry in time (busy interpreter, page failed to load), so
    // a figure is never left hidden. Runs on the main thread.
    std::vector<long long> due;
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        if (m_figureHiddenAt.empty()) {
            return;
        }
        const auto now = std::chrono::steady_clock::now();
        for (const auto& hidden : m_figureHiddenAt) {
            if (now - hidden.second >= FIGURE_REVEAL_TIMEOUT) {
                due.push_back(hidden.first);
            }
        }
    }
    for (long long handle : due) {
        revealFigureWindow(handle);
    }
}
//=============================================================================
void
GuiServer::requestFigureWindowPosition(long long handle, int x, int y)
{
    double measuredX = 0.0;
    double measuredBottom = 0.0;
    bool hasMeasurement = false;
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        if (m_figureWindows.find(handle) == m_figureWindows.end()) {
            return;
        }
        m_figureRequestedPosition[handle] = { x, y };
        auto geom = m_figureNativeGeometry.find(handle);
        if (geom != m_figureNativeGeometry.end()) {
            measuredX = geom->second[0];
            measuredBottom = geom->second[1];
            hasMeasurement = true;
        }
    }
    // Slide now from the last place the frontend reported; its next measurement
    // refines the move until the drawing area reaches the new Position.
    if (hasMeasurement) {
        nudgeFigureWindowToRequested(handle, measuredX, measuredBottom);
    }
}
//=============================================================================
void
GuiServer::closeFigureWindow(long long handle)
{
    // Dock action: unregister the window, tell the desktop to re-dock, and tear
    // the native window down (webui_close alone leaves a WebView window on
    // screen; webui_destroy is required).
    postWindowLifetimeTask([this, handle]() {
        size_t win = 0;
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            auto it = m_figureWindows.find(handle);
            if (it == m_figureWindows.end()) {
                return;
            }
            win = it->second;
            m_windowFigure.erase(win);
            m_figureWindows.erase(it);
        }
        pushJson("figure.window",
            nlohmann::json { { "handle", handle }, { "open", false }, { "reason", "dock" } });
        webui_close(win);
        webui_destroy(win);
    });
}
//=============================================================================
void
GuiServer::discardFigureWindow(long long handle)
{
    // The figure is gone: no re-dock notification. The window leaves the screen
    // at once; its teardown blocks for tens of milliseconds, so it runs later,
    // when nothing else waits (drainMainThreadTasks).
    size_t win = 0;
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        auto it = m_figureWindows.find(handle);
        if (it != m_figureWindows.end()) {
            win = it->second;
            m_windowFigure.erase(win);
            m_figureWindows.erase(it);
            m_figureHiddenAt.erase(handle);
            m_retiredWindows.insert(win);
        }
    }
    if (win != 0) {
        postMainThreadTask([this, win]() {
            webui_set_hidden(win, true);
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            m_windowTeardowns.push_back(win);
        });
        return;
    }
    // No window yet: its creation may still be queued, so the teardown is
    // queued behind it.
    postWindowLifetimeTask([this, handle]() {
        size_t queuedWin = 0;
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            auto it = m_figureWindows.find(handle);
            if (it == m_figureWindows.end()) {
                return;
            }
            queuedWin = it->second;
            m_windowFigure.erase(queuedWin);
            m_figureWindows.erase(it);
            m_figureHiddenAt.erase(handle);
        }
        webui_close(queuedWin);
        webui_destroy(queuedWin);
    });
}
//=============================================================================
void
GuiServer::setFigureWindowVisible(long long handle, bool visible)
{
    postMainThreadTask([this, handle, visible]() {
        size_t win = 0;
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            // Remembered even before the window exists: it then opens hidden.
            if (visible) {
                m_figureEngineHidden.erase(handle);
            } else {
                m_figureEngineHidden.insert(handle);
            }
            auto it = m_figureWindows.find(handle);
            if (it == m_figureWindows.end()) {
                return;
            }
            // Not shown yet, still being placed: revealFigureWindow applies it.
            if (m_figureHiddenAt.count(handle) != 0) {
                return;
            }
            win = it->second;
        }
        // Cross-platform via the WebUI hide/show patch (Win32 ShowWindow, GTK
        // gtk_widget_hide/show, macOS orderOut/makeKeyAndOrderFront).
        webui_set_hidden(win, !visible);
    });
}
//=============================================================================
void
GuiServer::setFigureWindowClientSize(long long handle, int clientWidth, int clientHeight)
{
    if (clientWidth <= 0 || clientHeight <= 0) {
        return;
    }
    postMainThreadTask([this, handle, clientWidth, clientHeight]() {
        size_t win = 0;
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            auto it = m_figureWindows.find(handle);
            if (it == m_figureWindows.end()) {
                return;
            }
            win = it->second;
        }
        // Windows: grow the outer window by its chrome. Elsewhere webui_set_size
        // sizes the client area (GTK resize, macOS setContentSize).
#ifdef _WIN32
        matchFigureWindowClientSize(win, clientWidth, clientHeight);
#else
        webui_set_size(
            win, static_cast<unsigned int>(clientWidth), static_cast<unsigned int>(clientHeight));
#endif
    });
}
//=============================================================================
void
GuiServer::closeAllFigureWindows()
{
    std::map<long long, size_t> snapshot;
    std::deque<size_t> retired;
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        snapshot.swap(m_figureWindows);
        m_windowFigure.clear();
        retired.swap(m_windowTeardowns);
    }
    for (const auto& kv : snapshot) {
        webui_close(kv.second);
        webui_destroy(kv.second);
    }
    for (size_t win : retired) {
        destroyRetiredWindow(win);
    }
}
//=============================================================================
bool
GuiServer::figureWindowCloseHandler(size_t window)
{
    // The user closed the native figure window: close the figure itself. The dock
    // path (closeFigureWindow) unregisters the window first, so it never gets here.
    GuiServer* server = s_activeServer.load();
    if (server == nullptr) {
        return true;
    }
    long long handle = -1;
    {
        std::lock_guard<std::mutex> lock(server->m_figureWindowsMutex);
        auto it = server->m_windowFigure.find(window);
        if (it != server->m_windowFigure.end()) {
            handle = it->second;
            server->m_figureWindows.erase(handle);
            server->m_windowFigure.erase(it);
        }
    }
    if (handle >= 0) {
        server->pushJson("figure.window",
            nlohmann::json { { "handle", handle }, { "open", false }, { "reason", "close" } });
    }
    return true;
}
//=============================================================================
void
GuiServer::setEngineReady()
{
    m_engineReady.store(true);
}
//=============================================================================
void
GuiServer::ensureUiHtmlFileHandler()
{
    static bool installed = false;
    if (installed || m_window == 0) {
        return;
    }
    // Serves the pages of uihtml components under /uihtml/ and declines every
    // other path, so the frontend keeps being served from the root folder.
    webui_set_file_handler_window(m_window, UiHtmlFileHandler);
    installed = true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
