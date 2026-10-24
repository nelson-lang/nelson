//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiServer.hpp"
#include "UiHtmlFileHandler.hpp"
#include "WebGuiJson.hpp"
#include "WebGuiRpc.hpp"
#include "WebGuiTerminal.hpp"
#include "MainThreadTaskQueue.hpp"
#include "WebUiHelpers.hpp"
#include "WebGuiPreferences.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "DynamicLibrary.hpp"
#include <filesystem>
#include <cstdio>
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
static std::atomic<WebGuiServer*> s_activeServer { nullptr };
//=============================================================================
static void
rpcCallback(webui_event_t* e)
{
    try {
        WebGuiServer* server = s_activeServer.load();
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
    saveWebGuiWindowGeometry(window);
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
// Nelson plot icon for a separate figure window (title bar and taskbar), instead
// of the generic WebView window icon. Windows only.
static void
applyFigureWindowIcon(size_t win)
{
#ifdef _WIN32
    HWND hwnd = static_cast<HWND>(webui_get_hwnd(win));
    if (hwnd == nullptr) {
        return;
    }
    const std::wstring modulesDir = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
    if (modulesDir.empty()) {
        return;
    }
    const std::wstring iconPath = modulesDir + L"/graphics/resources/icon-mathematical-plot.ico";
    HICON big = static_cast<HICON>(LoadImageW(nullptr, iconPath.c_str(), IMAGE_ICON,
        GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON), LR_LOADFROMFILE));
    HICON small = static_cast<HICON>(LoadImageW(nullptr, iconPath.c_str(), IMAGE_ICON,
        GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), LR_LOADFROMFILE));
    if (big != nullptr) {
        SendMessageW(hwnd, WM_SETICON, ICON_BIG, reinterpret_cast<LPARAM>(big));
    }
    if (small != nullptr) {
        SendMessageW(hwnd, WM_SETICON, ICON_SMALL, reinterpret_cast<LPARAM>(small));
    }
#else
    (void)win;
#endif
}
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
static WebGuiServer::ProcessQtEventsFn
ensureQtForWebView()
{
    const std::wstring libName = L"libnlsGui" + get_dynamic_library_extensionW();
    const std::wstring dir = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
    library_handle handle = load_dynamic_libraryW(dir + L"/" + libName);
    if (handle == nullptr) {
        std::fprintf(stderr, "Nelson WebView: Qt dialog bridge could not load libnlsGui.\n");
        return nullptr;
    }
    using EnsureApplicationFn = bool (*)(void);
    auto ensure
        = reinterpret_cast<EnsureApplicationFn>(get_function(handle, "nlsGuiEnsureApplication"));
    auto process = reinterpret_cast<WebGuiServer::ProcessQtEventsFn>(
        get_function(handle, "nlsGuiProcessEvents"));
    if (ensure == nullptr || process == nullptr || !ensure()) {
        std::fprintf(stderr, "Nelson WebView: Qt dialog bridge could not initialize Qt.\n");
        return nullptr;
    }
    return process;
}
//=============================================================================
WebGuiServer::WebGuiServer() { s_activeServer.store(this); }
//=============================================================================
WebGuiServer::~WebGuiServer() { shutdown(); }
//=============================================================================
void
WebGuiServer::clearBridgeHooks()
{
    if (m_launchOptions.figuresOnly) {
        MainThreadTaskQueue::getInstance()->setWakeupHook(nullptr);
    }
    WebDesktopBridge::getInstance()->setWebEventPumpHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureFrameHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureFrameAttachmentHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureLifecycleHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureWindowStyleHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureVisibleHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebFigureTitleHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebMessageBox(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebClipboardImageHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebPackageUpdatesHook(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebTerminalOpener(nullptr, nullptr);
    WebDesktopBridge::getInstance()->setWebEditorOpener(nullptr, nullptr);
}
//=============================================================================
bool
WebGuiServer::start(const std::wstring& rootFolder, const WebUiLaunchOptions& options)
{
    WebDesktopBridge::getInstance()->setWebViewUnavailableAtLaunch(false);
    if (!validateWebUiLaunchOptions(options, m_launchOptions, m_launchError)) {
        return false;
    }
    m_window = webui_new_window();
    if (m_window == 0) {
        return false;
    }
    // Window icon before show: Linux/GTK applies it as the native window icon;
    // elsewhere it is the served favicon (the Windows title-bar icon is set
    // with WM_SETICON after show, from fibonacci.ico).
    const std::string applicationIcon = findNelsonApplicationIconPath(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory());
    if (!applicationIcon.empty()) {
        webui_set_icon_file(m_window, applicationIcon.c_str());
    }
    webui_set_close_handler_wv(m_window, windowCloseCallback);
    // The frontend dist lives in the module tree in both the dev build and the
    // installed layout, so resolve it against the Nelson modules directory (known
    // before the evaluator exists). A cwd-relative path is the fallback for unusual
    // layouts. rootFolder is a modules-relative subpath ("web_gui/frontend/dist").
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
    WebDesktopBridge::getInstance()->setWebTerminalOpener(
        [](void* ctx) {
            static_cast<WebGuiServer*>(ctx)->pushJson("terminal.open", nlohmann::json::object());
        },
        this);
    WebDesktopBridge::getInstance()->setWebEditorOpener(
        [](void* ctx, const wchar_t* path, int line) {
            const std::string p
                = (path != nullptr) ? wstring_to_utf8(std::wstring(path)) : std::string();
            nlohmann::json payload { { "path", p } };
            if (line > 0) {
                payload["line"] = line;
            }
            static_cast<WebGuiServer*>(ctx)->pushJson("editor.show", payload);
        },
        this);
    WebDesktopBridge::getInstance()->setWebNotebookOpener(
        [](void* ctx, const wchar_t* path) {
            const std::string p
                = (path != nullptr) ? wstring_to_utf8(std::wstring(path)) : std::string();
            static_cast<WebGuiServer*>(ctx)->pushJson(
                "notebook.open", nlohmann::json { { "path", p } });
        },
        this);
    WebDesktopBridge::getInstance()->setWebFigureFrameHook(
        [](void* ctx, long long handle, const char* json, int len) {
            auto* server = static_cast<WebGuiServer*>(ctx);
            // Notebook figures render inline: streaming their drawnow frames
            // would open a panel mid-run.
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->pushFigureFrameJson(handle, json, len);
        },
        this);
    // Figures-only desktop (nelson-adv-cli --webview): the engine has no desktop
    // event loop, so it pumps the native figure windows from the interpreter's
    // event servicing through this hook. The full web desktop has its own pump
    // loop and must not register it.
    if (m_launchOptions.figuresOnly) {
        WebDesktopBridge::getInstance()->setWebEventPumpHook(
            [](void* ctx) { static_cast<WebGuiServer*>(ctx)->processMainThreadEvents(); }, this);
        // Figure RPCs run in place while the terminal is idle and drain at
        // statement boundaries while a command runs, so no prompt wake is needed.
        // A no-op wakeup hook stops MainThreadTaskQueue from injecting an Enter,
        // which would print an empty prompt for every mouse event.
        MainThreadTaskQueue::getInstance()->setWakeupHook([]() { });
    }
    WebDesktopBridge::getInstance()->setWebFigureLifecycleHook(
        [](void* ctx, long long handle, bool created, FigureWindowStyle windowStyle) {
            auto* server = static_cast<WebGuiServer*>(ctx);
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
                    server->openFigureWindow(handle);
                } else {
                    server->discardFigureWindow(handle);
                }
                return;
            }
            server->pushJson(created ? "figure.created" : "figure.closed",
                nlohmann::json { { "handle", handle },
                    // Figures are numbered from 1, as in the Qt desktop, while the handle is
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
    WebDesktopBridge::getInstance()->setWebFigureWindowStyleHook(
        [](void* ctx, long long handle, FigureWindowStyle windowStyle) {
            auto* server = static_cast<WebGuiServer*>(ctx);
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
            auto* server = static_cast<WebGuiServer*>(ctx);
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            server->setFigureWindowVisible(handle, visible);
        },
        this);
    WebDesktopBridge::getInstance()->setWebFigureTitleHook(
        [](void* ctx, long long handle, const char* title) {
            auto* server = static_cast<WebGuiServer*>(ctx);
            if (server->inlineFigureCaptureActive() || server->isNotebookFigure(handle)) {
                return;
            }
            const nlohmann::json payload { { "handle", handle },
                { "title", title != nullptr ? title : "" } };
            server->pushJson("figure.title", payload);
            // A separate native window renames itself through the same push.
            const size_t figureWindow = server->windowForFigure(handle);
            if (figureWindow != 0) {
                server->pushJsonToWindow(figureWindow, "figure.title", payload);
            }
        },
        this);
    // msgbox shows a clean message modal in the browser instead of a figure.
    WebDesktopBridge::getInstance()->setWebMessageBox(
        [](void* ctx, const wchar_t* title, const wchar_t* message, const wchar_t* icon) {
            auto w = [](const wchar_t* s) { return wstring_to_utf8(std::wstring(s ? s : L"")); };
            static_cast<WebGuiServer*>(ctx)->pushJson("dialog.message",
                nlohmann::json {
                    { "title", w(title) }, { "message", w(message) }, { "icon", w(icon) } });
        },
        this);
    WebDesktopBridge::getInstance()->setWebClipboardImageHook(
        [](void* ctx, const char* base64, int length) {
            static_cast<WebGuiServer*>(ctx)->pushJson("clipboard.image",
                nlohmann::json { { "dataUrl",
                    "data:image/png;base64,"
                        + std::string(base64, static_cast<size_t>(length)) } });
        },
        this);
    WebDesktopBridge::getInstance()->setWebPackageUpdatesHook(
        [](void* ctx, int count) {
            static_cast<WebGuiServer*>(ctx)->pushJson(
                "packages.updates", nlohmann::json { { "count", count } });
        },
        this);

    // NOTE: the WebSocket port is NOT opened here but by startListening() once
    // the engine is ready. A browser already pointed at the URL would otherwise
    // send RPC and push traffic to a half-initialised engine and crash it.
    return true;
}
//=============================================================================
bool
WebGuiServer::startListening()
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
    // Only the full web desktop has a console window to close.
    if (!serverOnly && !figuresOnly) {
        webui_set_close_handler_wv(m_window, WebGuiServer::webViewCloseHandler);
    }
    if (figuresOnly) {
        // The WebUI server is started by the first figure window's webui_show_wv
        // (openFigureWindow), like an undocked figure on the full desktop.
        // Starting it here would block the main thread in some WebUI builds.
        ok = true;
        m_launchResult.mode = WebUiLaunchMode::WebView;
        // NOTE: Qt for WebView dialogs is initialized lazily when the first
        // figure window opens (see openFigureWindowOnMainThread), so startup
        // never blocks on Qt in a non-interactive session.
    } else if (serverOnly) {
        const char* url = webui_start_server(m_window, "index.html");
        ok = url != nullptr && url[0] != '\0';
        m_launchResult.mode = WebUiLaunchMode::Server;
    } else {
        // Native WebView mode gets Qt-backed system dialogs. Initialize Qt
        // BEFORE the first WebView2 window: QApplication calls OleInitialize on
        // this (main) thread, and doing that after the shared WebView2
        // environment exists makes every later CreateCoreWebView2Controller fail
        // with 0x8007139F (ERROR_INVALID_STATE), so separate figure windows never
        // open. The server and external-browser modes do not initialize Qt.
        m_processQtEvents = ensureQtForWebView();
        applyWebGuiWindowGeometry(m_window);
        if (webui_show_wv(m_window, "index.html")) {
            ok = true;
            m_launchResult.mode = WebUiLaunchMode::WebView;
            applyWebGuiWindowGeometry(m_window);
            applyNelsonApplicationWindowIcon(
                m_window, NelsonConfiguration::getInstance()->getNelsonRootDirectory());
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
    if (!serverOnly && !figuresOnly) {
        const std::string indexUrl = m_launchResult.url + "/index.html";
        webui_navigate(m_window, indexUrl.c_str());
    }
    WebDesktopBridge::getInstance()->setWebLaunchMode(webUiLaunchModeName(m_launchResult.mode));
    WebDesktopBridge::getInstance()->setWebLaunchUrlAndPort(
        m_launchResult.url, static_cast<uint64_t>(m_launchResult.port));
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
    // The figures-only desktop is driven by the terminal: closing a figure must
    // never quit Nelson, so the heartbeat watchdog (which quits when the desktop
    // window disappears) is not started.
    if (!figuresOnly) {
        startWatchdog();
    }
    return true;
}
//=============================================================================
void
WebGuiServer::wait()
{
    webui_wait();
}
//=============================================================================
bool
WebGuiServer::processMainThreadEvents()
{
    if (m_engineReady.load() && !m_launchAttempted.load()) {
        startListening();
    }
    if (m_launchAttempted.load() && m_listening.load()) {
        // Deferred WebUI work (figure windows) runs on the main thread, before
        // the WebView event loop is pumped.
        drainMainThreadTasks();
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
        if (m_processQtEvents != nullptr) {
            m_processQtEvents();
        }
        return result;
    }
    return true;
}
//=============================================================================
bool
WebGuiServer::waitUntilLaunched()
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
WebGuiServer::shutdown()
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
        if (m_launchAttempted.load()) {
            saveWebGuiWindowGeometry(window);
        }
        webui_close(window);
        webui_destroy(window);
    }
    clearBridgeHooks();
    WebDesktopBridge::getInstance()->setWebLaunchMode("none");
    WebDesktopBridge::getInstance()->setWebLaunchUrlAndPort("", 0);
    WebGuiServer* expected = this;
    s_activeServer.compare_exchange_strong(expected, nullptr);
}
//=============================================================================
WebGuiServer*
WebGuiServer::activeServer()
{
    return s_activeServer.load();
}
//=============================================================================
std::string
WebGuiServer::launchError() const
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
WebGuiServer::pushJson(const std::string& channel, const nlohmann::json& payload)
{
    pushJsonToWindow(0, channel, payload);
}
//=============================================================================
void
WebGuiServer::pushJsonToWindow(
    size_t window, const std::string& channel, const nlohmann::json& payload)
{
    // Never push before the port is open and the engine is ready: startup
    // output must not be sent over a WebSocket to a client racing the init.
    if (m_window == 0 || !m_listening.load()) {
        return;
    }
    const std::string js = "window.__nelsonPush && window.__nelsonPush('" + channel + "', '"
        + escapeForJsSingleQuote(serializeWebGuiJson(payload)) + "');";
    enqueueScript(std::string(js), window);
}
//=============================================================================
void
WebGuiServer::pushFigureFrameJson(long long handle, const char* displayListJson, int len)
{
    if (m_window == 0 || !m_listening.load() || displayListJson == nullptr || len <= 0) {
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
WebGuiServer::startPushWorker()
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
WebGuiServer::stopPushWorker()
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
WebGuiServer::enqueueScript(std::string&& js, size_t window)
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
WebGuiServer::windowForFigure(long long handle) const
{
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    auto it = m_figureWindows.find(handle);
    return it != m_figureWindows.end() ? it->second : 0;
}
//=============================================================================
void
WebGuiServer::setDispatcher(WebGuiRpc* dispatcher)
{
    m_dispatcher = dispatcher;
}
//=============================================================================
void
WebGuiServer::setTerminal(WebGuiTerminal* terminal)
{
    m_terminal = terminal;
}
//=============================================================================
bool
WebGuiServer::webViewCloseHandler(size_t window)
{
    WebGuiServer* server = s_activeServer.load();
    if (server != nullptr && server->m_window == window) {
        server->requestExit();
    }
    return true;
}
//=============================================================================
void
WebGuiServer::requestExit()
{
    WebGuiTerminal* terminal = m_terminal;
    if (terminal == nullptr) {
        return;
    }
    bool expected = false;
    if (m_exitRequested.compare_exchange_strong(expected, true)) {
        terminal->pushInputLine(L"exit");
    }
}
//=============================================================================
WebGuiRpc*
WebGuiServer::dispatcher() const
{
    return m_dispatcher;
}
//=============================================================================
std::string
WebGuiServer::handleRpc(const std::string& envelope, size_t window)
{
    if (m_dispatcher == nullptr) {
        return "{\"error\":{\"message\":\"no dispatcher\"}}";
    }
    nlohmann::json request = nlohmann::json::parse(envelope, nullptr, false);
    const std::string method = (!request.is_discarded() && request.contains("method"))
        ? request.value("method", std::string())
        : std::string();
    auto withId = [&](nlohmann::json reply) {
        if (!request.is_discarded() && request.contains("id")) {
            reply["id"] = request["id"];
        }
        return serializeWebGuiJson(reply);
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
        nlohmann::json result = handle >= 0
            ? nlohmann::json { { "role", "figure" }, { "handle", handle },
                  { "launchMode", launchMode } }
            : nlohmann::json { { "role", "desktop" }, { "launchMode", launchMode } };
        return withId(nlohmann::json { { "result", result } });
    }
    if (method == "client.ready") {
        if (window == m_window) {
            {
                std::lock_guard<std::mutex> lock(m_launchMutex);
                m_clientReady = true;
            }
            m_launchCv.notify_all();
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
WebGuiServer::postMainThreadTask(std::function<void()> task)
{
    std::lock_guard<std::mutex> lock(m_mainTaskMutex);
    m_mainTasks.push_back(std::move(task));
}
//=============================================================================
void
WebGuiServer::drainMainThreadTasks()
{
    std::deque<std::function<void()>> tasks;
    {
        std::lock_guard<std::mutex> lock(m_mainTaskMutex);
        tasks.swap(m_mainTasks);
    }
    for (auto& task : tasks) {
        task();
    }
}
//=============================================================================
long long
WebGuiServer::figureForWindow(size_t window) const
{
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    auto it = m_windowFigure.find(window);
    return it != m_windowFigure.end() ? it->second : -1;
}
//=============================================================================
bool
WebGuiServer::isFigureWindowOpen(long long handle) const
{
    std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
    return m_figureWindows.find(handle) != m_figureWindows.end();
}
//=============================================================================
void
WebGuiServer::openFigureWindow(long long handle, int width, int height)
{
    postMainThreadTask(
        [this, handle, width, height]() { openFigureWindowOnMainThread(handle, width, height); });
}
//=============================================================================
void
WebGuiServer::openFigureWindowOnMainThread(long long handle, int width, int height)
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
    // Window icon before show: on Linux/GTK WebUI applies the favicon as the
    // native window icon; on Windows it is the served favicon (the title-bar
    // icon is set with WM_SETICON after show). macOS has no per-window icon.
    {
        const std::wstring modulesDir
            = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
        if (!modulesDir.empty()) {
            const std::string iconPng
                = wstring_to_utf8(modulesDir + L"/graphics/resources/icon-mathematical-plot.png");
            webui_set_icon_file(win, iconPng.c_str());
        }
    }
    webui_set_size(win, width > 0 ? width : 840, height > 0 ? height : 680);
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        m_figureWindows[handle] = win;
        m_windowFigure[win] = handle;
    }
    // The standalone page reads its figure from client.role, so one index.html
    // serves the desktop and every figure window.
    //
    // show_wait_connection is false so webui_show_wv does not block the
    // interpreter waiting for the page's WebSocket. FigurePanel's refresh() on
    // mount recovers a figure.frame pushed before the socket is up.
    //
    // The option is process-global, so it is set explicitly here: an HTML viewer
    // or a Markdown render opened meanwhile sets it to true for its own window.
    WebUiConfigScope waitForConnection(show_wait_connection, false);
    const bool shown = webui_show_wv(win, "index.html");
    if (!shown) {
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            m_figureWindows.erase(handle);
            m_windowFigure.erase(win);
        }
        // No transient docked panel exists while the window launches: tell the
        // desktop to re-dock the figure.
        pushJson("figure.window",
            nlohmann::json { { "handle", handle }, { "open", false }, { "reason", "fallback" } });
        return;
    }
    // Showing a WebView window does not reliably make it the key window (notably
    // macOS when the command window started the figure). Focus it so the new
    // figure is visible and receives keyboard and mouse input.
    webui_focus(win);
    applyFigureWindowIcon(win);
    if (width > 0 && height > 0) {
        matchFigureWindowClientSize(win, width, height);
    }
    pushJson("figure.window", nlohmann::json { { "handle", handle }, { "open", true } });
}
//=============================================================================
void
WebGuiServer::closeFigureWindow(long long handle)
{
    // Dock action: unregister the window, tell the desktop to re-dock, and tear
    // the native window down (webui_close alone leaves a WebView window on
    // screen; webui_destroy is required).
    postMainThreadTask([this, handle]() {
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
WebGuiServer::discardFigureWindow(long long handle)
{
    // The figure is gone: tear the window down with no re-dock notification.
    postMainThreadTask([this, handle]() {
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
        webui_close(win);
        webui_destroy(win);
    });
}
//=============================================================================
void
WebGuiServer::setFigureWindowVisible(long long handle, bool visible)
{
    postMainThreadTask([this, handle, visible]() {
        size_t win = 0;
        {
            std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
            auto it = m_figureWindows.find(handle);
            if (it == m_figureWindows.end()) {
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
WebGuiServer::setFigureWindowClientSize(long long handle, int clientWidth, int clientHeight)
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
        // already sizes the client area natively (GTK gtk_window_resize, macOS
        // setContentSize).
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
WebGuiServer::closeAllFigureWindows()
{
    std::map<long long, size_t> snapshot;
    {
        std::lock_guard<std::mutex> lock(m_figureWindowsMutex);
        snapshot.swap(m_figureWindows);
        m_windowFigure.clear();
    }
    for (const auto& kv : snapshot) {
        webui_close(kv.second);
        webui_destroy(kv.second);
    }
}
//=============================================================================
bool
WebGuiServer::figureWindowCloseHandler(size_t window)
{
    // The user closed the native figure window: close the figure itself. The dock
    // path (closeFigureWindow) unregisters the window first, so it never gets here.
    WebGuiServer* server = s_activeServer.load();
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
WebGuiServer::setEngineReady()
{
    m_engineReady.store(true);
}
//=============================================================================
void
WebGuiServer::ensureUiHtmlFileHandler()
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
