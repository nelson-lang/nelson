//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "nflow_guiBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "webui_bindings.hpp"
#include "NFlowHelpers.hpp"
#include "NFlowBootstrapHtml.hpp"
#include "WebUiHelpers.hpp"
#include "NFlowToolchainCache.hpp"
#include <webui.hpp>
#include "WebUiWindowGeometry.hpp"
#include <thread>
#include <mutex>
#include <queue>
#include <deque>
#include <memory>
#include <functional>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <filesystem>
#include <atomic>
#include "SimEngine.hpp"
#include "BlockRegistry.hpp"
#include <nlohmann/json.hpp>
#include <algorithm>
#include <iomanip>
#include <chrono>
#ifdef _WIN32
#include <windows.h>
#include <commdlg.h>
#else
#include <unistd.h>
#endif
#include <ctime>
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "SimEventBridge.hpp"
#include "NFlowNelsonBridge.hpp"
#include "NFlowWorkspaceIO.hpp"
#include "NFlowModelDocument.hpp"
#include "NFlowModelRegistry.hpp"
#include "BuildCodeHelpers.hpp"
#include "BackgroundWorkerPool.hpp"
#include "MainThreadTaskQueue.hpp"
#include "Warning.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
// Any value > 0 enables interim progress reporting; the engine publishes on a
// wall-clock cadence (~100 ms), not per step count.
constexpr int WEB_SIM_PROGRESS_CHUNK_SIZE = 250;
//=============================================================================
struct HTMLEvent
{
    std::string name;
    std::string data;
};
//=============================================================================
struct WindowEntry
{
    std::unique_ptr<webui::window> win;
    // The WebView serves "/" from its own thread AFTER websim() returns, so the
    // bootstrap HTML must live here for the window's lifetime, not on the stack.
    std::string bootstrapHtml;
    // Creation order (0 = first nflow() of the process); tags the window in the
    // diagnostic log.
    size_t creationIndex = 0;
    std::queue<HTMLEvent> events;
    std::mutex eventsMutex;
    std::function<void(const HTMLEvent&)> eventCallback;
    std::atomic<bool> simCancel { false };
    std::atomic<bool> simRunning { false };
    std::atomic<bool> simPaused { false };
    std::atomic<bool> embeddedWebView { false };
    std::mutex simResultMutex;
    std::string simResultRunId;
    std::string simResultJson;
    std::deque<std::string> simPartialResults;
    bool simResultReady = false;
};
//=============================================================================
std::vector<std::shared_ptr<WindowEntry>> openWindows;
std::mutex windowsMutex;
std::mutex windowPreferencesMutex;
Nelson::NFlowGateway::BackgroundWorkerPool simulationWorkers;
// Separate pool for the (memoized) C/Rust toolchain probe so it never contends
// with, or is gated by, the simulation pool. Joined on shutdown.
Nelson::NFlowGateway::BackgroundWorkerPool toolchainWorkers;
std::string
runSimulationForEntry(std::weak_ptr<WindowEntry> entryWeak, const std::string& diagramJson,
    bool pushProgress, bool prepareState = true);
std::string
startSimulationForEntry(std::weak_ptr<WindowEntry> entryWeak, const std::string& diagramJson);
std::string
pollSimulationForEntry(std::weak_ptr<WindowEntry> entryWeak, const std::string& runId);
//=============================================================================
// Diagnostic log for the intermittent "already_running" rejection. It is appended
// next to the nflow preferences so a recurrence can be attributed to cross-window
// RPC routing or to a stranded per-window running flag. The rejection is always
// recorded; the per-transition trace needs NFLOW_SIM_DIAG=1.
//=============================================================================
bool
simDiagVerbose()
{
    static const bool verbose = []() {
        const char* value = getenv("NFLOW_SIM_DIAG");
        return value != nullptr && value[0] != '\0' && value[0] != '0';
    }();
    return verbose;
}
//=============================================================================
void
simDiagLog(const std::string& message)
{
    std::wstring dir;
    try {
        dir = getNFlowPreferencesDirectory();
    } catch (...) {
    }
    if (dir.empty()) {
        return;
    }
    std::time_t now = std::time(nullptr);
    std::tm tmv {};
#ifdef _MSC_VER
    localtime_s(&tmv, &now);
#else
    localtime_r(&now, &tmv);
#endif
    char stamp[16];
    if (std::strftime(stamp, sizeof(stamp), "%H:%M:%S", &tmv) == 0) {
        stamp[0] = '\0';
    }
    try {
#ifdef _MSC_VER
        const std::filesystem::path path = std::filesystem::path(dir) / L"nflow_sim_diag.log";
#else
        const std::filesystem::path path
            = std::filesystem::path(wstring_to_utf8(dir)) / "nflow_sim_diag.log";
#endif
        std::ofstream out(path, std::ios::app | std::ios::binary);
        if (out) {
            out << stamp << " " << message << "\r\n";
        }
    } catch (...) {
    }
}
//=============================================================================
// Window tag for the diagnostic log; the suffix is the creation index.
//=============================================================================
std::string
simDiagWindowTag(const std::shared_ptr<WindowEntry>& entry)
{
    if (!entry) {
        return "<expired>";
    }
    return "nflow_win_" + std::to_string(entry->creationIndex);
}
//=============================================================================
WebUiWindowGeometry
loadWindowGeometry(const std::string& preferencesDirectory)
{
    WebUiWindowGeometry geometry;
    try {
#ifdef _MSC_VER
        std::filesystem::path path(utf8_to_wstring(preferencesDirectory));
#else
        std::filesystem::path path(preferencesDirectory);
#endif
        path /= "nflow-ui-prefs.json";
        std::ifstream stream(path, std::ios::binary);
        if (!stream) {
            return geometry;
        }
        const auto preferences = nlohmann::json::parse(stream);
        if (!preferences.contains("nf-window-geometry")) {
            return geometry;
        }
        const auto& raw = preferences.at("nf-window-geometry");
        const auto value = raw.is_string() ? nlohmann::json::parse(raw.get<std::string>()) : raw;
        if (value.value("coordinateSpace", "") != "native-window") {
            return geometry;
        }
        geometry.x = value.value("x", 0);
        geometry.y = value.value("y", 0);
        geometry.width = value.value("width", 0);
        geometry.height = value.value("height", 0);
        geometry.maximized = value.value("maximized", false);
        validateWebUiWindowGeometry(geometry);
    } catch (...) {
    }
    return geometry;
}
//=============================================================================
void
saveWindowGeometry(const std::string& preferencesDirectory, size_t window)
{
    const WebUiWindowGeometry geometry = captureWebUiWindowGeometry(window);
    if (!geometry.valid) {
        return;
    }
    try {
        std::lock_guard<std::mutex> lock(windowPreferencesMutex);
#ifdef _MSC_VER
        std::filesystem::path path(utf8_to_wstring(preferencesDirectory));
#else
        std::filesystem::path path(preferencesDirectory);
#endif
        path /= "nflow-ui-prefs.json";
        nlohmann::json preferences = nlohmann::json::object();
        {
            std::ifstream input(path, std::ios::binary);
            if (input) {
                const nlohmann::json current = nlohmann::json::parse(input, nullptr, false);
                if (!current.is_discarded() && current.is_object()) {
                    preferences = current;
                }
            }
        }
        const nlohmann::json value = { { "coordinateSpace", "native-window" }, { "x", geometry.x },
            { "y", geometry.y }, { "width", geometry.width }, { "height", geometry.height },
            { "maximized", geometry.maximized } };
        preferences["nf-window-geometry"] = value.dump();
        std::filesystem::path temporary = path;
        temporary += ".window.tmp";
        {
            std::ofstream output(temporary, std::ios::binary | std::ios::trunc);
            output << preferences.dump(2);
            if (!output) {
                return;
            }
        }
        std::error_code ec;
        std::filesystem::remove(path, ec);
        ec.clear();
        std::filesystem::rename(temporary, path, ec);
        if (ec) {
            std::filesystem::remove(temporary, ec);
        }
    } catch (...) {
    }
}
//=============================================================================
bool
nflowWindowCloseCallback(size_t window)
{
    const std::string preferencesDirectory
        = wstring_to_utf8(NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory());
    if (!preferencesDirectory.empty()) {
        saveWindowGeometry(preferencesDirectory, window);
    }
    return true;
}
//=============================================================================
} // anonymous namespace
//=============================================================================
ArrayOfVector
Nelson::NFlowGateway::nflow_guiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 2);

    // Simulations launched from this window run on a detached thread and marshal their
    // workspace accesses (fromWorkspace / toWorkspace / nelsonFunction) back here.
    ensureNelsonWorkspaceBridge(eval);

    Nelson::NFlow::BootstrapVars bootstrapVars;
    bootstrapVars.uiPath = wstring_to_utf8(getNFlowDistributionDirectory());
    bootstrapVars.nflowPreferencesPath = wstring_to_utf8(getNFlowPreferencesDirectory());
    // Toolchain detection runs a full CMake compiler-detection configure (seconds)
    // and must not block the return of nflow(). Seed the flags from the persisted
    // cache (false on a fresh profile, so the build option starts disabled) and
    // refresh them from a background probe once the window is shown.
    const Nelson::NFlow::ToolchainCache toolchainCache
        = Nelson::NFlow::loadToolchainCache(bootstrapVars.nflowPreferencesPath);
    bootstrapVars.haveCToolChain = toolchainCache.haveC;
    bootstrapVars.haveRustToolChain = toolchainCache.haveRust;

    if (argIn.size() > 0) {
        if (!argIn[0].isRowVectorCharacterArray() && !argIn[0].isScalarStringArray()) {
            Error(L"Nelson:nflow_gui:invalidModelFilename",
                L"nflow: first argument (model filename) must be a string.");
        }
        bootstrapVars.modelFilename = argIn[0].asUtf8String();
    }
    // Second argument: name of the NFlowModelRegistry model this window is bound
    // to. The editor then loads, commits and saves through the live document, so
    // scripted edits and GUI edits stay in sync. open_system passes it.
    if (argIn.size() > 1) {
        if (!argIn[1].isRowVectorCharacterArray() && !argIn[1].isScalarStringArray()) {
            Error(L"Nelson:nflow_gui:invalidModelName",
                L"nflow: second argument (model name) must be a string.");
        }
        bootstrapVars.modelName = argIn[1].asUtf8String();
    }

    auto entry = std::make_shared<WindowEntry>();
    entry->win = std::make_unique<webui::window>();
    // Window icon before show: Linux/GTK uses it as the native icon, elsewhere it
    // is the served favicon (the Windows title-bar icon is set with WM_SETICON
    // after show). Falls back to the Nelson icon when nflow.png is missing.
    {
        std::string windowIconFile;
        const std::wstring modulesDir
            = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
        if (!modulesDir.empty()) {
            std::error_code fsError;
            const std::filesystem::path candidate
                = std::filesystem::path(modulesDir) / "nflow_gui" / "resources" / "nflow.png";
            if (std::filesystem::is_regular_file(candidate, fsError)) {
                windowIconFile = candidate.u8string();
            }
        }
        if (windowIconFile.empty()) {
            windowIconFile = findNelsonApplicationIconPath(
                NelsonConfiguration::getInstance()->getNelsonRootDirectory());
        }
        if (!windowIconFile.empty()) {
            entry->win->set_icon_file(windowIconFile);
        }
    }
    entry->win->set_close_handler_wv(nflowWindowCloseCallback);
    auto entryWeak = std::weak_ptr<WindowEntry>(entry);
    auto sendHtmlEvent = [entryWeak](const std::string& name, const std::string& data) {
        auto entry = entryWeak.lock();
        if (!entry) {
            return;
        }
        HTMLEvent ev;
        ev.name = name;
        ev.data = data;
        std::lock_guard<std::mutex> lock(entry->eventsMutex);
        if (entry->eventCallback) {
            entry->eventCallback(ev);
        } else {
            entry->events.push(ev);
        }
    };

    if (!entry->win->set_root_folder(bootstrapVars.uiPath)) {
        Error("Nelson:nflow_gui:invalidPath", "websim: cannot set root folder: %s",
            bootstrapVars.uiPath);
    }

    size_t winIdx = 0;
    {
        static std::atomic<size_t> creationCounter { 0 };
        entry->creationIndex = creationCounter.fetch_add(1);
        std::lock_guard<std::mutex> lock(windowsMutex);
        // Drop entries of closed windows so the list stays bounded.
        openWindows.erase(std::remove_if(openWindows.begin(), openWindows.end(),
                              [](const std::shared_ptr<WindowEntry>& e) {
                                  return !e || !e->win
                                      || (!e->simRunning.load() && !e->win->is_shown());
                              }),
            openWindows.end());
        openWindows.push_back(entry);
        winIdx = openWindows.size() - 1;
    }

    bootstrapVars.darkMode = NelsonConfiguration::getInstance()->isDarkMode();
    bootstrapVars.locale = wstring_to_utf8(NelsonConfiguration::getInstance()->getCurrentLocale());

    // Stored on the persistent entry (NOT the stack): WebUI keeps only a pointer.
    entry->bootstrapHtml = Nelson::NFlow::buildNFlowBootstrapHtml(bootstrapVars);

    NFlowUiBindings::NFlowRpcCallbacks rpcCallbacks;
    rpcCallbacks.runSimulation = [entryWeak](const std::string& diagramJson) {
        return runSimulationForEntry(entryWeak, diagramJson, true);
    };
    rpcCallbacks.startSimulation
        = std::bind(&startSimulationForEntry, entryWeak, std::placeholders::_1);
    rpcCallbacks.pollSimulation
        = std::bind(&pollSimulationForEntry, entryWeak, std::placeholders::_1);
    rpcCallbacks.pauseSimulation = [entryWeak](bool wantPause) {
        auto entry = entryWeak.lock();
        if (!entry) {
            return;
        }
        entry->simPaused.store(wantPause);
    };
    rpcCallbacks.stopSimulation = [entryWeak]() {
        auto entry = entryWeak.lock();
        if (entry) {
            entry->simCancel.store(true);
        }
    };
    rpcCallbacks.isSimulationRunning = [entryWeak]() -> bool {
        auto entry = entryWeak.lock();
        return entry ? entry->simRunning.load() : false;
    };
    rpcCallbacks.sendEvent = sendHtmlEvent;
    rpcCallbacks.importSsp = [](const std::string& sspPath) -> std::string {
        std::string errorMessage;
        const std::string nflowPath
            = nflowSspImportViaInterpreter(sspPath, std::string {}, errorMessage);
        nlohmann::json out;
        out["success"] = errorMessage.empty() && !nflowPath.empty();
        out["path"] = nflowPath;
        if (!errorMessage.empty()) {
            out["error"] = errorMessage;
        }
        return out.dump();
    };
    rpcCallbacks.exportSsp
        = [](const std::string& nflowContent, const std::string& sspPath) -> std::string {
        std::string errorMessage;
        const std::string produced
            = nflowSspExportDiagramViaInterpreter(nflowContent, sspPath, errorMessage);
        nlohmann::json out;
        out["success"] = errorMessage.empty() && !produced.empty();
        out["path"] = produced;
        if (!errorMessage.empty()) {
            out["error"] = errorMessage;
        }
        return out.dump();
    };
    rpcCallbacks.exportFmu
        = [](const std::string& nflowContent, const std::string& fmuPath) -> std::string {
        std::string errorMessage;
        const std::string produced
            = nflowExportFmuViaInterpreter(nflowContent, fmuPath, errorMessage);
        nlohmann::json out;
        out["success"] = errorMessage.empty() && !produced.empty();
        out["path"] = produced;
        if (!errorMessage.empty()) {
            out["error"] = errorMessage;
        }
        return out.dump();
    };
    rpcCallbacks.modelicaInfo = []() -> std::string {
        std::string errorMessage;
        const std::string info = nflowModelicaInfoViaInterpreter(errorMessage);
        if (!errorMessage.empty()) {
            throw std::runtime_error(errorMessage);
        }
        return info;
    };
    rpcCallbacks.evaluateCallback = [](const std::string& code, const std::string& blockId,
                                        std::string& errorMessage) -> bool {
        return nflowEvaluateCallbackViaInterpreter(code, blockId, errorMessage);
    };
    rpcCallbacks.openHelp
        = [](const std::string& topic, const std::string& page, std::string& errorMessage) -> bool {
        return nflowOpenHelpViaInterpreter(topic, page, errorMessage);
    };
    rpcCallbacks.isEmbeddedWebView = [entryWeak]() -> bool {
        auto entry = entryWeak.lock();
        if (!entry) {
            return false;
        }
        if (WebDesktopBridge::getInstance()->getWebLaunchMode() == "webview") {
            return true;
        }
        return entry->embeddedWebView.load() || entry->win->get_current_browser() == Webview;
    };
    NFlowUiBindings::bindRpc(
        entry->win.get(), bootstrapVars.modelFilename, bootstrapVars.modelName, rpcCallbacks);

    // Serve every request statelessly. WebUI's single-client mode gates requests
    // behind a session cookie. The React/Vite app fires ~15 near-simultaneous
    // subresource requests and the embedded WebView (ephemeral profile,
    // SameSite=Strict) does not echo the cookie on all of them, so some get a 403
    // and the page renders as garbage. multi_client removes the gate.
    webui::set_config(multi_client, true);

    const char* portStr = getenv("NFLOW_GUI_DEBUGGING_PORT");
    if (portStr) {
        try {
            int port = std::stoi(portStr);
            if (port > 0 && port <= 65535) {
                if (!entry->win->set_port(static_cast<size_t>(port))) {
                    // A stale host or unrelated process owns the port: WebUI silently falls back
                    // to a random port, so a test harness pinned to this one would drive the
                    // wrong server. Warn instead of failing later with a refused connection.
                    RaiseWarning("Nelson:nflow:debuggingPortInUse",
                        "nflow: NFLOW_GUI_DEBUGGING_PORT " + std::string(portStr)
                            + " is already in use; the editor is served on a random port.");
                }
            }
        } catch (...) {
        }
        // Debugging/test host: by default WebUI stops a window's HTTP server once every
        // websocket client has been gone for ~1.5 s, and the Playwright harness then
        // gets ERR_CONNECTION_REFUSED. A zero startup timeout selects serve-forever
        // mode, even if the auto-opened window is closed. Process-global, only set
        // on an explicit debugging host.
        webui::set_timeout(0);
    }

    const int MIN_WINDOW_WIDTH = 800;
    const int MIN_WINDOW_HEIGHT = 600;
    const int DEFAULT_WINDOW_WIDTH = 1280;
    const int DEFAULT_WINDOW_HEIGHT = 890;
    const WebUiWindowGeometry savedGeometry
        = loadWindowGeometry(bootstrapVars.nflowPreferencesPath);
    const int windowWidth = savedGeometry.valid ? savedGeometry.width : DEFAULT_WINDOW_WIDTH;
    const int windowHeight = savedGeometry.valid ? savedGeometry.height : DEFAULT_WINDOW_HEIGHT;

    entry->win->set_minimum_size(MIN_WINDOW_WIDTH, MIN_WINDOW_HEIGHT);
    entry->win->set_size(windowWidth - 1, windowHeight - 1);
    if (savedGeometry.valid) {
        entry->win->set_position(
            static_cast<unsigned int>(savedGeometry.x), static_cast<unsigned int>(savedGeometry.y));
    } else {
        entry->win->set_center();
    }

    // Show as an embedded WebView so the editor owns a real top-level native window
    // (system file dialogs can be modal to it). The bootstrap is handed over as
    // embedded HTML, not written into the served root: modules/nflow_gui/dist is
    // read-only on a normal install. It then redirects to ./nflow.html, which is
    // served from that root over HTTP.
    bool shown = entry->win->show_wv(entry->bootstrapHtml.c_str());
    // show_wv() may return false when the window has already been created but its
    // startup handshake did not complete before the timeout. The current-browser
    // state identifies the embedded mode in that case.
    {
        const bool embedded = shown || entry->win->get_current_browser() == Webview;
        entry->embeddedWebView.store(embedded);
    }
    if (!shown && !entry->embeddedWebView.load()) {
        entry->embeddedWebView.store(false);
        // show_wv() also reports false when the window did open but its socket
        // did not validate in time, so only fall back if nothing is on screen.
        // The same embedded HTML works in an external browser: WebUI serves it
        // over the local HTTP server and the redirect resolves against dist.
        entry->win->show_browser(entry->bootstrapHtml.c_str(), AnyBrowser);
    }

    // WebView2 on Windows does not always trigger its initial resize.
    entry->win->set_size(windowWidth, windowHeight);
    if (savedGeometry.valid) {
        restoreWebUiWindowGeometry(entry->win->get_window_id(), savedGeometry);
    }
    if (entry->embeddedWebView.load()) {
        const std::wstring modulesDir
            = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
        if (!modulesDir.empty()) {
            applyWebUiWindowIcon(
                entry->win->get_window_id(), modulesDir + L"/nflow_gui/resources/nflow.ico");
        }
    }

    // Detect the C/Rust toolchains OFF the launch path. The probe is memoized
    // (BuildCodeHelpers), so only the first launch spawns cmake/cargo/rustc. It runs
    // on a worker joined at shutdown; if the window is gone, entryWeak is empty and
    // the task returns. The result updates the persisted cache and is pushed to the
    // codegen panel, which listens for `storage` events; a same-window setItem fires
    // none, so a synthetic StorageEvent is dispatched.
    {
        const std::string toolchainPrefsDir = bootstrapVars.nflowPreferencesPath;
        try {
            toolchainWorkers.start([entryWeak, toolchainPrefsDir]() {
                std::wstring detectError;
                const bool haveC = nflow::haveCToolChain(detectError);
                const bool haveRust = nflow::haveRustToolChain(detectError);
                Nelson::NFlow::saveToolchainCache(toolchainPrefsDir, haveC, haveRust);
                auto liveEntry = entryWeak.lock();
                if (!liveEntry || !liveEntry->win) {
                    return;
                }
                try {
                    liveEntry->win->run(
                        Nelson::NFlow::buildToolchainStoragePushJs(haveC, haveRust));
                } catch (...) {
                }
            });
        } catch (...) {
            // Pool is shutting down or a thread could not be created: the seeded
            // cache value stands and codegen availability refreshes next launch.
        }
    }

    // When this window is bound to a registry model, subscribe to its change listener:
    // a scripted mutation (interpreter thread) pushes a "model changed" event so the
    // editor pulls the fresh diagram. GUI-origin changes are skipped to avoid an
    // echo loop.
    if (!bootstrapVars.modelName.empty()) {
        Nelson::NFlow::NFlowModelDocument* doc
            = Nelson::NFlow::NFlowModelRegistry::instance().resolveByName(bootstrapVars.modelName);
        if (doc != nullptr) {
            doc->setChangeListener([entryWeak](
                                       std::uint64_t revision, Nelson::NFlow::ChangeOrigin origin) {
                if (origin != Nelson::NFlow::ChangeOrigin::Script) {
                    return;
                }
                auto liveEntry = entryWeak.lock();
                if (!liveEntry || !liveEntry->win) {
                    return;
                }
                const std::string js = std::string("if(typeof window.__nelsonPush==='function')")
                    + "window.__nelsonPush('nflow.model.changed',"
                    + nlohmann::json({ { "revision", revision }, { "origin", "script" } }).dump()
                    + ");";
                try {
                    liveEntry->win->run(js);
                } catch (...) {
                }
            });
        }
    }

    ArrayOfVector retval;
    if (nLhs == 1) {
        retval << ArrayOf::doubleScalar(static_cast<double>(winIdx));
    }
    return retval;
}
//=============================================================================
void
Nelson::NFlowGateway::closeAllNFlowWindows(Evaluator* eval)
{
    std::vector<std::shared_ptr<WindowEntry>> entries;
    {
        std::lock_guard<std::mutex> lock(windowsMutex);
        entries.swap(openWindows);
    }

    // Close the admission gate before setting cancellation flags. A concurrent
    // start then either already belongs to the pool and observes cancellation,
    // or is rejected without launching a thread.
    simulationWorkers.beginStopping();
    toolchainWorkers.beginStopping();

    // Ask every window to close first: close() only signals, so the editors all
    // start closing at once instead of serialising each destroy()'s wait.
    for (const auto& e : entries) {
        if (e && e->win) {
            e->simCancel.store(true);
            e->win->close();
        }
    }

    // A simulation may be waiting for a workspace operation posted to the
    // interpreter thread. Keep draining those operations until every worker is
    // finished, then join before the gateway and its dependencies are unloaded.
    simulationWorkers.stopAndJoin([eval]() {
        if (eval != nullptr) {
            MainThreadTaskQueue::getInstance()->processTasks(eval);
        }
    });
    // The toolchain probe touches no workspace/interpreter state, so it needs no
    // drain; join it (the windows are signalled-closed but not yet destroyed, so
    // any in-flight UI push targets a still-valid window or is caught).
    toolchainWorkers.stopAndJoin({});

    for (const auto& e : entries) {
        if (!e) {
            continue;
        }
        if (e->win) {
            e->win->destroy();
        }
    }
}
//=============================================================================
void
Nelson::NFlowGateway::broadcastNFlowPush(const std::string& channel, const std::string& payloadJson)
{
    std::vector<std::shared_ptr<WindowEntry>> targets;
    {
        std::lock_guard<std::mutex> lock(windowsMutex);
        targets = openWindows;
    }
    const std::string js = std::string("if(typeof window.__nelsonPush==='function')")
        + "window.__nelsonPush(" + nlohmann::json(channel).dump() + ","
        + nlohmann::json(payloadJson).dump() + ");";
    for (const auto& e : targets) {
        if (e && e->win) {
            try {
                e->win->run(js);
            } catch (...) {
            }
        }
    }
}
//=============================================================================
namespace {
//=============================================================================
std::string
runSimulationForEntry(std::weak_ptr<WindowEntry> entryWeak, const std::string& diagramJson,
    bool pushProgress, bool prepareState)
{
    auto entry = entryWeak.lock();
    if (!entry) {
        return std::string("{\"status\":\"error\",\"reason\":\"no_window\",\"scopes\":[]}");
    }

    // Always detach callbacks and clear the running state, including on failure.
    struct RunTeardown
    {
        std::shared_ptr<WindowEntry> entry;
        ~RunTeardown()
        {
            {
                std::lock_guard<std::mutex> lock(entry->eventsMutex);
                entry->eventCallback = nullptr;
            }
            Nelson::NFlow::setSimEventFetcher(
                std::function<std::vector<Nelson::NFlow::SimEvent>()>());
            entry->simRunning.store(false);
            entry->simCancel.store(false);
            if (simDiagVerbose()) {
                simDiagLog("CLEAR running " + simDiagWindowTag(entry));
            }
        }
    } runTeardown { entry };

    if (prepareState) {
        entry->simCancel.store(false);
        entry->simPaused.store(false);
        entry->simRunning.store(true);
    }

    // Shared queue keeps callbacks independent from this function's stack.
    struct SimEventQueue
    {
        std::deque<HTMLEvent> q;
        std::mutex m;
    };
    auto simEv = std::make_shared<SimEventQueue>();

    {
        std::lock_guard<std::mutex> lock(entry->eventsMutex);
        entry->eventCallback = [simEv](const HTMLEvent& ev) {
            std::lock_guard<std::mutex> lock(simEv->m);
            simEv->q.push_back(ev);
        };
    }

    Nelson::NFlow::setSimEventFetcher([simEv]() -> std::vector<Nelson::NFlow::SimEvent> {
        std::vector<Nelson::NFlow::SimEvent> out;
        std::lock_guard<std::mutex> lock(simEv->m);
        while (!simEv->q.empty()) {
            const HTMLEvent e = simEv->q.front();
            out.emplace_back(e.name, e.data);
            simEv->q.pop_front();
        }
        return out;
    });

    std::string result;
    try {
        auto progressCb = [entry, pushProgress](const std::string& partialJson) {
            if (!pushProgress) {
                // Poll deltas must accumulate; bound the queue while the UI stalls.
                constexpr std::size_t MAX_PENDING_PARTIALS = 64;
                std::lock_guard<std::mutex> lock(entry->simResultMutex);
                if (entry->simPartialResults.size() >= MAX_PENDING_PARTIALS) {
                    entry->simPartialResults.pop_front();
                }
                entry->simPartialResults.push_back(partialJson);
                return;
            }
            std::string js = std::string("if(typeof window.__nelsonPush==='function')")
                + std::string("window.__nelsonPush('nflow.sim.partial',")
                + nlohmann::json(partialJson).dump() + std::string(");");
            entry->win->run(js);
        };
        auto isCancelled = [entry]() -> bool { return entry->simCancel.load(); };
        auto isPaused = [entry]() -> bool { return entry->simPaused.load(); };

        // modelica blocks: compile each to an FMU (OpenModelica) and rewrite the block,
        // since the engine knows nothing about Modelica. sim() does this in .m but the
        // GUI Run button drives the engine directly. No thread hop without a modelica
        // block; a build failure blocks the run with the typed reason.
        std::string resolvedJson = diagramJson;
        std::string errorJson;
        std::string modelicaError;
        resolvedJson
            = Nelson::NFlowGateway::nflowExpandModelicaViaInterpreter(resolvedJson, modelicaError);
        // acausal / planar blocks: lower each physical-pin (physicalWires) net into the
        // engine's native island blocks, since the engine knows nothing about catalog
        // block types (Resistor, ConstantVoltage, ...). sim() does this in .m but the
        // GUI Run button drives the engine directly. No thread hop without physical
        // wires; a lowering failure blocks the run with the typed reason.
        std::string acausalError;
        if (modelicaError.empty()) {
            resolvedJson = Nelson::NFlowGateway::nflowExpandAcausalViaInterpreter(
                resolvedJson, acausalError);
        }
        if (!modelicaError.empty() || !acausalError.empty()) {
            nlohmann::json res;
            res["status"] = "error";
            res["reason"] = modelicaError.empty() ? acausalError : modelicaError;
            res["scopes"] = nlohmann::json::array();
            result = res.dump();
        } else if (!Nelson::NFlow::resolveWorkspaceInputs(resolvedJson, errorJson)) {
            result = errorJson;
        } else {
            result = Nelson::NFlow::runSimulation(
                resolvedJson, WEB_SIM_PROGRESS_CHUNK_SIZE, progressCb, isCancelled, isPaused);
            std::vector<std::string> writeErrors = Nelson::NFlow::flushWorkspaceOutputs(result);
            if (!writeErrors.empty()) {
                try {
                    nlohmann::json res = nlohmann::json::parse(result);
                    res["workspaceWriteErrors"] = writeErrors;
                    result = res.dump();
                } catch (...) {
                }
            }
            // Async delivery runs StopFcn only after publishing the final result.
            if (prepareState) {
                std::string stopFcnError;
                Nelson::NFlowGateway::nflowRunStopFcnViaInterpreter(
                    resolvedJson, result, stopFcnError);
                if (!stopFcnError.empty()) {
                    try {
                        nlohmann::json res = nlohmann::json::parse(result);
                        res["stopFcnError"] = stopFcnError;
                        result = res.dump();
                    } catch (...) {
                    }
                }
            }
        }
    } catch (const std::exception& ex) {
        nlohmann::json res;
        res["status"] = "error";
        res["reason"] = std::string("C++ exception: ") + ex.what();
        res["scopes"] = nlohmann::json::array();
        result = res.dump();
    } catch (...) {
        result = std::string(
            "{\"status\":\"error\",\"reason\":\"Unknown C++ exception\",\"scopes\":[]}");
    }

    return result;
}
//=============================================================================
std::string
startSimulationForEntry(std::weak_ptr<WindowEntry> entryWeak, const std::string& diagramJson)
{
    static std::atomic<unsigned long long> nextRunId { 1 };
    const std::string runId = "sim-" + std::to_string(nextRunId.fetch_add(1));
    auto entry = entryWeak.lock();
    if (!entry) {
        return nlohmann::json { { "status", "error" }, { "reason", "no_window" } }.dump();
    }
    bool expected = false;
    if (!entry->simRunning.compare_exchange_strong(expected, true)) {
        // Record which window's binding fired and the running state of every open
        // window, to tell cross-window RPC routing from a stranded running flag.
        std::string snapshot;
        {
            std::lock_guard<std::mutex> lock(windowsMutex);
            for (const auto& other : openWindows) {
                if (!other) {
                    continue;
                }
                snapshot += " {" + simDiagWindowTag(other)
                    + " running=" + (other->simRunning.load() ? "1" : "0") + "}";
            }
        }
        simDiagLog("REJECT already_running firing=" + simDiagWindowTag(entry)
            + " winId=" + std::to_string(entry->win ? entry->win->get_window_id() : 0)
            + " runId=" + runId + " open:" + snapshot);
        return nlohmann::json { { "status", "error" }, { "reason", "already_running" } }.dump();
    }
    if (simDiagVerbose()) {
        simDiagLog("SET running " + simDiagWindowTag(entry) + " runId=" + runId);
    }
    entry->simCancel.store(false);
    entry->simPaused.store(false);
    const bool pollDelivery = entry
        && (WebDesktopBridge::getInstance()->getWebLaunchMode() == "webview"
            || entry->embeddedWebView.load() || entry->win->get_current_browser() == Webview);

    if (pollDelivery) {
        std::lock_guard<std::mutex> lock(entry->simResultMutex);
        entry->simResultRunId = runId;
        entry->simResultJson.clear();
        entry->simPartialResults.clear();
        entry->simResultReady = false;
    }

    try {
        if (!simulationWorkers.start([entryWeak, diagramJson, runId, pollDelivery]() {
                const std::string result
                    = runSimulationForEntry(entryWeak, diagramJson, !pollDelivery, false);
                auto entry = entryWeak.lock();
                if (!entry || !entry->win) {
                    return;
                }

                if (pollDelivery) {
                    std::lock_guard<std::mutex> lock(entry->simResultMutex);
                    if (entry->simResultRunId == runId) {
                        entry->simResultJson = result;
                        entry->simResultReady = true;
                    }
                } else {
                    nlohmann::json payload;
                    payload["runId"] = runId;
                    try {
                        payload["result"] = nlohmann::json::parse(result);
                    } catch (...) {
                        payload["resultJson"] = result;
                    }

                    const std::string js = std::string("if(typeof window.__nelsonPush==='function')"
                                                       "window.__nelsonPush('nflow.sim.final',")
                        + nlohmann::json(payload.dump()).dump() + std::string(");");
                    try {
                        entry->win->run(js);
                    } catch (...) {
                    }
                }

                // The integrated scopes now own the final result. Run the model
                // callback afterwards so a figure cannot delay their final frame.
                std::string stopFcnError;
                Nelson::NFlowGateway::nflowRunStopFcnViaInterpreter(
                    diagramJson, result, stopFcnError);
                if (!stopFcnError.empty() && simDiagVerbose()) {
                    simDiagLog("StopFcn failed: " + stopFcnError);
                }
            })) {
            entry->simRunning.store(false);
            return nlohmann::json { { "status", "error" }, { "reason", "shutting_down" } }.dump();
        }
    } catch (...) {
        entry->simRunning.store(false);
        throw;
    }

    nlohmann::json started;
    started["status"] = "started";
    started["runId"] = runId;
    started["delivery"] = pollDelivery ? "poll" : "push";
    return started.dump();
}
//=============================================================================
std::string
pollSimulationForEntry(std::weak_ptr<WindowEntry> entryWeak, const std::string& runId)
{
    auto entry = entryWeak.lock();
    if (!entry) {
        return nlohmann::json { { "status", "error" }, { "reason", "no_window" } }.dump();
    }

    std::lock_guard<std::mutex> lock(entry->simResultMutex);
    const bool matchesRun = entry->simResultRunId == runId;
    const bool complete = matchesRun && entry->simResultReady;
    // Hand-assembled response: the queued partials and the final result are
    // engine-produced JSON already, so splice them in verbatim instead of
    // parsing and re-serializing every payload on each poll.
    std::string response = "{\"status\":\"";
    response += complete ? "complete" : "pending";
    response += "\",\"runId\":";
    response += nlohmann::json(runId).dump();
    response += ",\"partials\":[";
    bool firstPartial = true;
    while (matchesRun && !entry->simPartialResults.empty()) {
        const std::string partialJson = std::move(entry->simPartialResults.front());
        entry->simPartialResults.pop_front();
        if (!partialJson.empty() && (partialJson[0] == '{' || partialJson[0] == '[')) {
            if (!firstPartial) {
                response += ",";
            }
            firstPartial = false;
            response += partialJson;
        }
    }
    response += "]";
    if (complete) {
        if (!entry->simResultJson.empty()
            && (entry->simResultJson[0] == '{' || entry->simResultJson[0] == '[')) {
            response += ",\"result\":";
            response += entry->simResultJson;
        } else {
            response += ",\"resultJson\":";
            response += nlohmann::json(entry->simResultJson).dump();
        }
    }
    response += "}";
    return response;
}
//=============================================================================
} // anonymous namespace
//=============================================================================
