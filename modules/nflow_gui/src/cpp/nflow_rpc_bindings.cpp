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
#include "webui_bindings.hpp"
#include "characters_encoding.hpp"
#include "BlockRegistry.hpp"
#include "RouterEngine.hpp"
#include "ExprEngine.hpp"
#include "NFlowCodegenC.hpp"
#include "NFlowCodegenFlatten.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "NFlowCodegenPipeline.hpp"
#include "NFlowCodegenRust.hpp"
#include "NFlowNelsonBridge.hpp"
#include "ExamplesCatalogHost.hpp"
#include "NFlowModelDocument.hpp"
#include "NFlowModelRegistry.hpp"
#include "SimEngine.hpp"
#include "FmiInspect.hpp"
#include "nflow_rpc_codegen.hpp"
#include "FileSystemWrapper.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include "DialogDirectories.hpp"
#include "GuiDialogProvider.hpp"
#include "i18n.hpp"
#include "WebUiHelpers.hpp"
#include <webui.hpp>
#include <nlohmann/json.hpp>
#include <algorithm>
#include <atomic>
#include <cctype>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#ifdef _MSC_VER
#include <windows.h>
#endif
#include <mutex>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#ifndef _MSC_VER
#include <dlfcn.h>
#endif
//=============================================================================
namespace Nelson::NFlowUiBindings {
//=============================================================================
namespace {
    //=============================================================================
    constexpr size_t LARGE_STRING_THRESHOLD = 1024 * 1024;
    constexpr size_t POLL_RESULT_INLINE_THRESHOLD = 32 * 1024;
    constexpr size_t BLOB_CHUNK_SIZE = 32 * 1024;
    std::mutex blobMutex;
    std::unordered_map<std::string, std::string> blobs;
    std::atomic<unsigned long long> nextBlobId { 1 };
    //=============================================================================
    static bool
    readTextFileUtf8(const std::string& path, std::string& out)
    {
#ifdef _MSC_VER
        std::ifstream file(utf8_to_wstring(path), std::ios::binary);
#else
        std::ifstream file(path, std::ios::binary);
#endif
        if (!file.is_open()) {
            return false;
        }
        std::ostringstream ss;
        ss << file.rdbuf();
        out = ss.str();
        return true;
    }
    //=============================================================================
    static bool
    writeTextFileUtf8(const std::string& path, const std::string& content)
    {
#ifdef _MSC_VER
        std::ofstream ofs(utf8_to_wstring(path), std::ios::binary | std::ios::trunc);
#else
        std::ofstream ofs(path, std::ios::binary | std::ios::trunc);
#endif
        if (!ofs.is_open()) {
            return false;
        }
        ofs.write(content.data(), static_cast<std::streamsize>(content.size()));
        return true;
    }
    //=============================================================================
    static bool
    writeBytesFileUtf8(const std::string& path, const std::vector<unsigned char>& data)
    {
#ifdef _MSC_VER
        std::ofstream ofs(utf8_to_wstring(path), std::ios::binary | std::ios::trunc);
#else
        std::ofstream ofs(path, std::ios::binary | std::ios::trunc);
#endif
        if (!ofs.is_open()) {
            return false;
        }
        if (!data.empty()) {
            ofs.write(reinterpret_cast<const char*>(data.data()),
                static_cast<std::streamsize>(data.size()));
        }
        return true;
    }
    //=============================================================================
    static std::vector<unsigned char>
    decodeBase64(const std::string& encoded)
    {
        static const std::string chars
            = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        std::vector<int> table(256, -1);
        for (int i = 0; i < 64; i++) {
            table[static_cast<unsigned char>(chars[i])] = i;
        }
        std::vector<unsigned char> decoded;
        int val = 0;
        int valb = -8;
        for (unsigned char c : encoded) {
            if (table[c] == -1) {
                break;
            }
            val = (val << 6) + table[c];
            valb += 6;
            if (valb >= 0) {
                decoded.push_back(static_cast<unsigned char>((val >> valb) & 0xFF));
                valb -= 8;
            }
        }
        return decoded;
    }
    //=============================================================================
    static nlohmann::json
    stringResult(const std::string& value, const std::string& mime = "text/plain",
        size_t inlineThreshold = LARGE_STRING_THRESHOLD)
    {
        if (value.size() <= inlineThreshold) {
            return value;
        }
        const std::string blobId = "nflow-" + std::to_string(nextBlobId.fetch_add(1));
        {
            std::lock_guard<std::mutex> lock(blobMutex);
            blobs[blobId] = value;
        }
        return nlohmann::json { { "__nflowBlob", true }, { "blobId", blobId },
            { "size", value.size() }, { "mime", mime } };
    }
    //=============================================================================
    static nlohmann::json
    okEnvelope(int id, const nlohmann::json& result)
    {
        return nlohmann::json { { "id", id }, { "result", result } };
    }
    //=============================================================================
    static nlohmann::json
    errorEnvelope(int id, const std::string& message)
    {
        return nlohmann::json { { "id", id }, { "error", { { "message", message } } } };
    }
    //=============================================================================
    static std::string
    pathToUtf8(const std::filesystem::path& p)
    {
#ifdef _MSC_VER
        return wstring_to_utf8(p.wstring());
#else
        return p.string();
#endif
    }
    //=============================================================================
    static std::filesystem::path
    pathFromUtf8(const std::string& value)
    {
#ifdef _MSC_VER
        return std::filesystem::path(utf8_to_wstring(value));
#else
        return std::filesystem::u8path(value);
#endif
    }
    //=============================================================================
    static std::string
    dialogHistoryKey(const std::string& mode)
    {
        return "nflow." + (mode == "dir" ? std::string("directory") : mode);
    }
    //=============================================================================
    // Qt-free directory listing behind the in-window HTML file picker. Returns
    // {cwd, entries:[{name, path, isDir, size}]}, directories first, each group
    // sorted by name. An unreadable or missing path falls back to the working directory.
    static std::string
    fsList(const std::string& requestedPath)
    {
        namespace fs = std::filesystem;
        std::error_code ec;
        fs::path dir;
        if (requestedPath.empty() || requestedPath == ".") {
            dir = fs::current_path(ec);
        } else {
#ifdef _MSC_VER
            dir = fs::path(utf8_to_wstring(requestedPath));
#else
            dir = fs::path(requestedPath);
#endif
        }
        if (!fs::is_directory(dir, ec)) {
            dir = fs::current_path(ec);
        }
        dir = fs::absolute(dir, ec);

        struct Entry
        {
            std::string name;
            std::string path;
            bool isDir;
            unsigned long long size;
        };
        std::vector<Entry> items;
        for (fs::directory_iterator it(dir, fs::directory_options::skip_permission_denied, ec), end;
            !ec && it != end; it.increment(ec)) {
            std::error_code ecEntry;
            const bool isDir = it->is_directory(ecEntry);
            unsigned long long size = 0;
            if (!isDir) {
                const auto bytes = it->file_size(ecEntry);
                if (!ecEntry) {
                    size = static_cast<unsigned long long>(bytes);
                }
            }
            items.push_back(
                { pathToUtf8(it->path().filename()), pathToUtf8(it->path()), isDir, size });
        }
        std::sort(items.begin(), items.end(), [](const Entry& a, const Entry& b) {
            if (a.isDir != b.isDir) {
                return a.isDir;
            }
            return a.name < b.name;
        });

        nlohmann::json entries = nlohmann::json::array();
        for (const Entry& e : items) {
            entries.push_back({ { "name", e.name }, { "path", e.path }, { "isDir", e.isDir },
                { "size", e.size } });
        }
        nlohmann::json res = { { "cwd", pathToUtf8(dir) }, { "entries", entries } };
        return res.dump();
    }
    //=============================================================================
    // System (OS) file dialog, served by the Qt implementation in libnlsGui, which
    // is resolved DYNAMICALLY so nflow_gui never links Qt. Returns nullptr when
    // libnlsGui cannot be loaded/resolved (e.g. a Qt-free build); the caller then
    // reports the dialog as unavailable to the front end.
    using RunFileDialogFn = bool (*)(void*, const GuiDialogRequest&, GuiDialogResponse&);
    static std::string s_guiDialogBridgeError;
    static RunFileDialogFn
    resolveRunFileDialog()
    {
        static RunFileDialogFn fn = nullptr;
        if (fn != nullptr) {
            return fn;
        }
#ifndef _MSC_VER
        fn = reinterpret_cast<RunFileDialogFn>(dlsym(RTLD_DEFAULT, "nlsGuiRunFileDialog"));
        if (fn != nullptr) {
            s_guiDialogBridgeError.clear();
            return fn;
        }
#endif
        const std::wstring libName = L"libnlsGui" + get_dynamic_library_extensionW();
        const std::wstring dir = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        library_handle handle = load_dynamic_libraryW(dir + L"/" + libName);
        if (handle == nullptr) {
            s_guiDialogBridgeError = get_dynamic_library_error();
            return nullptr;
        }
        fn = reinterpret_cast<RunFileDialogFn>(get_function(handle, "nlsGuiRunFileDialog"));
        if (fn == nullptr) {
            s_guiDialogBridgeError = "nlsGuiRunFileDialog symbol not found";
        } else {
            s_guiDialogBridgeError.clear();
        }
        return fn;
    }
    //=============================================================================
    // Builds a Qt filter string ("Files (*.a *.b);;All Files (*)") from the JSON
    // `filters` array of extensions (e.g. [".nflow"]).
    static std::wstring
    buildQtFilterFromParams(const nlohmann::json& params)
    {
        std::wstring pattern;
        if (params.contains("filters") && params["filters"].is_array()) {
            for (const auto& f : params["filters"]) {
                if (!f.is_string()) {
                    continue;
                }
                std::string ext = f.get<std::string>();
                if (ext.empty()) {
                    continue;
                }
                if (ext[0] == '.') {
                    ext = "*" + ext;
                } else if (ext[0] != '*') {
                    ext = "*." + ext;
                }
                if (!pattern.empty()) {
                    pattern += L" ";
                }
                pattern += utf8_to_wstring(ext);
            }
        }
        if (pattern.empty()) {
            return L"All Files (*)";
        }
        return L"Files (" + pattern + L");;All Files (*)";
    }
    //=============================================================================
    static std::string
    generateCode(const nlohmann::json& params)
    {
        std::string diagramJson = params.value("diagramJson", "");
        std::string optionsJson = params.value("optionsJson", "{}");
        auto diagram = nlohmann::json::parse(diagramJson);

        nflow::CodegenOptions opts;
        std::string lang = "c";
        if (!optionsJson.empty()) {
            auto optObj = nlohmann::json::parse(optionsJson);
            if (optObj.contains("sampleTime") && optObj["sampleTime"].is_number()) {
                opts.sampleTime = optObj["sampleTime"].get<double>();
            }
            if (optObj.contains("includeMain") && optObj["includeMain"].is_boolean()) {
                opts.includeMain = optObj["includeMain"].get<bool>();
            }
            if (optObj.contains("lang") && optObj["lang"].is_string()) {
                lang = optObj["lang"].get<std::string>();
                std::transform(lang.begin(), lang.end(), lang.begin(),
                    [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
            }
        }

        // The preview goes through the shared codegen pipeline, so it applies the same
        // gates as every other entry point. An error shows as a "/* codegen error: ... */"
        // marker in the code view. Packaged user.* library blocks are inlined first, as
        // in the directory generators.
        Nelson::NFlowUiBindings::inlineUserBlocksForCodegen(diagram);
        opts.target = (lang == "rust") ? "rust" : "C";
        nflow::CodegenPipelineRequest pipelineRequest;
        pipelineRequest.diagramJson = diagram.dump();
        pipelineRequest.options = opts;
        nflow::CodegenPipelineResult pipelineResult = nflow::runCodegenPipeline(
            pipelineRequest, Nelson::NFlowGateway::makeInterpreterCodegenHooks());
        if (const nflow::CodegenDiagnostic* e = pipelineResult.firstError()) {
            return "/* codegen error: " + nflow::composeCodegenMessageEnglish(*e) + " */";
        }
        if (lang == "rust") {
            const nflow::CodegenRustResult& rres = pipelineResult.rust;
            std::string out = rres.rustSource;
            if (opts.includeMain && !rres.mainSource.empty()) {
                out += "\n\n// --- src/main.rs ---\n\n";
                out += rres.mainSource;
            }
            out += "\n\n// --- Cargo.toml ---\n\n";
            out += rres.cargoToml;
            return out;
        }
        return pipelineResult.c.combined();
    }
    //=============================================================================
    // Registry document the window is bound to; params may override the name.
    // Returns nullptr when unbound or not loaded.
    static Nelson::NFlow::NFlowModelDocument*
    resolveBoundModel(const std::string& modelName, const nlohmann::json& params)
    {
        std::string name = params.value("name", modelName);
        if (name.empty()) {
            return nullptr;
        }
        return Nelson::NFlow::NFlowModelRegistry::instance().resolveByName(name);
    }
    //=============================================================================
    static nlohmann::json
    dispatchRpc(webui::window::event* e, const std::string& modelFilename,
        const std::string& modelName, const NFlowRpcCallbacks& callbacks, const std::string& method,
        const nlohmann::json& params)
    {
        if (method == "nflow.ping") {
            return nlohmann::json { { "pong", true }, { "backend", "nflow_gui" } };
        }
        if (method == "nflow.host.mode") {
            const bool embeddedWebView
                = callbacks.isEmbeddedWebView ? callbacks.isEmbeddedWebView() : false;
            return nlohmann::json { { "mode", embeddedWebView ? "webview" : "web" } };
        }
        if (method == "nflow.host.user") {
#ifdef _MSC_VER
            const char* user = std::getenv("USERNAME");
#else
            const char* user = std::getenv("USER");
            if (!user || !*user) {
                user = std::getenv("LOGNAME");
            }
#endif
            return nlohmann::json { { "user", user ? user : "" } };
        }
        if (method == "nflow.fs.list") {
            const std::string path = params.value("path", params.value("dir", ""));
            return stringResult(fsList(path), "application/json");
        }
        if (method == "nflow.fs.dialogStart") {
            const std::string mode = params.value("mode", "open");
            const std::string key = dialogHistoryKey(mode);
            std::error_code ec;
            const std::filesystem::path current = std::filesystem::current_path(ec);
            const std::wstring directory = resolveDialogDirectory(utf8_to_wstring(key),
                utf8_to_wstring(params.value("defaultPath", std::string())),
                ec ? std::wstring() : utf8_to_wstring(pathToUtf8(current)));
            return stringResult(
                nlohmann::json { { "directory", wstring_to_utf8(directory) } }.dump(),
                "application/json");
        }
        if (method == "nflow.fs.dialogAccept") {
            const std::string mode = params.value("mode", "open");
            const std::filesystem::path selected
                = pathFromUtf8(params.value("path", std::string()));
            const std::filesystem::path directory
                = mode == "dir" ? selected : selected.parent_path();
            const bool ok = !directory.empty()
                && setDialogDirectory(utf8_to_wstring(dialogHistoryKey(mode)),
                    utf8_to_wstring(pathToUtf8(directory)));
            return stringResult(nlohmann::json { { "ok", ok } }.dump(), "application/json");
        }
        if (method == "nflow.fs.nativeDialog") {
            // Debugging/test host (Playwright): never open an OS dialog. It would show
            // on the developer's desktop and block a webui worker thread until a human
            // answers, while the driven browser only sees the HTML picker.
            if (std::getenv("NFLOW_GUI_DEBUGGING_PORT") != nullptr) {
                return stringResult(
                    "{\"available\":false,\"reason\":\"debug_host\"}", "application/json");
            }
            // Native file/directory dialogs are reserved for the embedded desktop
            // WebView. Server and external-browser clients use the HTML picker.
            if (!callbacks.isEmbeddedWebView || !callbacks.isEmbeddedWebView()) {
                return stringResult(
                    "{\"available\":false,\"reason\":\"not_webview\"}", "application/json");
            }
            // Try the system (Qt) chooser through libnlsGui, resolved dynamically so
            // nflow_gui stays Qt-free. When it is unavailable, report available=false and
            // let the front end show the reason.
            RunFileDialogFn runDialog = resolveRunFileDialog();
            if (runDialog == nullptr) {
                nlohmann::json res;
                res["available"] = false;
                res["reason"] = s_guiDialogBridgeError.empty()
                    ? std::string("qt_bridge_unavailable")
                    : std::string("qt_bridge_unavailable: ") + s_guiDialogBridgeError;
                return stringResult(res.dump(), "application/json");
            }
            void* parent = nullptr;
#ifdef _MSC_VER
            try {
                parent = e->get_window().win32_get_hwnd();
            } catch (...) {
                parent = nullptr;
            }
#endif
            GuiDialogRequest request;
            request.kind = L"file";
            request.fileMode = utf8_to_wstring(params.value("mode", "open"));
            request.title = utf8_to_wstring(params.value("title", ""));
            request.defaultPath = utf8_to_wstring(params.value("defaultPath", ""));
            request.historyKey = utf8_to_wstring(dialogHistoryKey(params.value("mode", "open")));
            request.fileFilters.push_back(buildQtFilterFromParams(params));
            GuiDialogResponse response;
            const bool ran = runDialog(parent, request, response);
            if (!ran) {
                return stringResult("{\"available\":false,\"reason\":\"qt_dialog_unavailable\"}",
                    "application/json");
            }
            nlohmann::json res;
            res["available"] = true;
            if (!response.ok || response.path.empty()) {
                res["canceled"] = true;
            } else {
                res["path"] = wstring_to_utf8(response.path);
            }
            return stringResult(res.dump(), "application/json");
        }
        if (method == "nflow.file.readText") {
            std::string content;
            const std::string path = params.value("path", params.value("filePath", ""));
            if (!readTextFileUtf8(path, content)) {
                return stringResult(
                    "{\"success\":false,\"error\":\"Cannot open file\"}", "application/json");
            }
            return stringResult(
                nlohmann::json { { "success", true }, { "content", content } }.dump(),
                "application/json");
        }
        if (method == "nflow.file.writeText") {
            const std::string path = params.value("path", params.value("filePath", ""));
            const std::string content = params.value("content", "");
            std::filesystem::path p(std::filesystem::u8path(path));
            if (p.has_parent_path()) {
                std::filesystem::create_directories(p.parent_path());
            }
            // A "base64:" prefix carries binary payloads (e.g. rasterized exports);
            // everything else is written verbatim as UTF-8 text.
            bool writeOk = false;
            if (content.rfind("base64:", 0) == 0) {
                writeOk = writeBytesFileUtf8(path, decodeBase64(content.substr(7)));
            } else {
                writeOk = writeTextFileUtf8(path, content);
            }
            if (!writeOk) {
                return stringResult(
                    "{\"success\":false,\"error\":\"Cannot write file\"}", "application/json");
            }
            return stringResult("{\"success\":true}", "application/json");
        }
        if (method == "nflow.file.loadInitial") {
            if (modelFilename.empty()) {
                return stringResult(
                    "{\"success\":false,\"error\":\"no_file\"}", "application/json");
            }
            std::string content;
            if (!readTextFileUtf8(modelFilename, content)) {
                return stringResult(
                    "{\"success\":false,\"error\":\"Cannot open file\"}", "application/json");
            }
            return stringResult(nlohmann::json { { "success", true }, { "filePath", modelFilename },
                                    { "content", content } }
                                    .dump(),
                "application/json");
        }
        // Live registry-backed model channel. The editor and the scripted API share one
        // NFlowModelDocument: the GUI reads it (getDiagram), writes edits back (commit)
        // and saves it (save). Scripted mutations reach the window through
        // nflow.model.changed (see nflow_guiBuiltin), which triggers a getDiagram.
        if (method == "nflow.model.getDiagram") {
            Nelson::NFlow::NFlowModelDocument* doc = resolveBoundModel(modelName, params);
            if (doc == nullptr) {
                return stringResult(
                    "{\"success\":false,\"error\":\"model_not_loaded\"}", "application/json");
            }
            return stringResult(
                nlohmann::json { { "success", true }, { "name", doc->name() },
                    { "revision", doc->revision() }, { "filePath", doc->filePath() },
                    { "content", doc->toJsonString() } }
                    .dump(),
                "application/json");
        }
        if (method == "nflow.model.commit") {
            Nelson::NFlow::NFlowModelDocument* doc = resolveBoundModel(modelName, params);
            if (doc == nullptr) {
                return stringResult(
                    "{\"success\":false,\"error\":\"model_not_loaded\"}", "application/json");
            }
            // Optimistic concurrency (conflict policy A): reject a commit built on
            // a stale revision so a concurrent scripted change is not clobbered;
            // the editor then pulls and merges/reloads.
            if (params.contains("baseRevision")) {
                const std::uint64_t base
                    = params.value("baseRevision", static_cast<std::uint64_t>(0));
                if (base != doc->revision()) {
                    return stringResult(nlohmann::json { { "success", false }, { "conflict", true },
                                            { "revision", doc->revision() } }
                                            .dump(),
                        "application/json");
                }
            }
            nlohmann::json next;
            try {
                next = nlohmann::json::parse(params.value("content", std::string()));
            } catch (const std::exception&) {
                return stringResult(
                    "{\"success\":false,\"error\":\"invalid_content\"}", "application/json");
            }
            std::string err;
            if (!doc->replaceDiagram(next, Nelson::NFlow::ChangeOrigin::Gui, err)) {
                return stringResult(
                    nlohmann::json { { "success", false }, { "error", err } }.dump(),
                    "application/json");
            }
            return stringResult(
                nlohmann::json { { "success", true }, { "revision", doc->revision() } }.dump(),
                "application/json");
        }
        if (method == "nflow.model.setPath") {
            Nelson::NFlow::NFlowModelDocument* doc = resolveBoundModel(modelName, params);
            if (doc == nullptr) {
                return stringResult(
                    "{\"success\":false,\"error\":\"model_not_loaded\"}", "application/json");
            }
            doc->setFilePath(params.value("path", std::string()));
            return stringResult(
                nlohmann::json { { "success", true }, { "filePath", doc->filePath() } }.dump(),
                "application/json");
        }
        if (method == "nflow.model.save") {
            Nelson::NFlow::NFlowModelDocument* doc = resolveBoundModel(modelName, params);
            if (doc == nullptr) {
                return stringResult(
                    "{\"success\":false,\"error\":\"model_not_loaded\"}", "application/json");
            }
            // Flush the editor's latest edits into the document first, so the
            // saved file always matches what is on screen.
            if (params.contains("content")) {
                nlohmann::json next;
                try {
                    next = nlohmann::json::parse(params.value("content", std::string()));
                    std::string err;
                    doc->replaceDiagram(next, Nelson::NFlow::ChangeOrigin::Gui, err);
                } catch (const std::exception&) {
                }
            }
            if (params.contains("path") && !params.value("path", std::string()).empty()) {
                doc->setFilePath(params.value("path", std::string()));
            }
            if (doc->filePath().empty()) {
                // No target yet: the editor must run Save As and call again.
                return stringResult("{\"success\":false,\"needPath\":true}", "application/json");
            }
            if (!writeTextFileUtf8(doc->filePath(), doc->toJsonString())) {
                return stringResult(
                    "{\"success\":false,\"error\":\"Cannot write file\"}", "application/json");
            }
            doc->setDirty(false);
            return stringResult(
                nlohmann::json { { "success", true }, { "filePath", doc->filePath() },
                    { "revision", doc->revision() } }
                    .dump(),
                "application/json");
        }
        if (method == "nflow.context.set") {
            // The editor reports its active system and block selection so gcs/gcb
            // reflect the GUI. Absent keys are left unchanged.
            auto& reg = Nelson::NFlow::NFlowModelRegistry::instance();
            if (params.contains("system")) {
                reg.setCurrentSystem(params.value("system", std::string()));
            }
            if (params.contains("block")) {
                reg.setCurrentBlock(params.value("block", std::string()));
            }
            return stringResult("{\"success\":true}", "application/json");
        }
        if (method == "nflow.modelica.info") {
            if (!callbacks.modelicaInfo) {
                throw std::runtime_error("OpenModelica detection is not available");
            }
            return stringResult(callbacks.modelicaInfo(), "application/json");
        }
        if (method == "nflow.sim.run") {
            const bool asyncRun = params.value("async", false);
            if (asyncRun && callbacks.startSimulation) {
                return stringResult(
                    callbacks.startSimulation(params.value("diagramJson", "")), "application/json");
            }
            if (!callbacks.runSimulation) {
                throw std::runtime_error("simulation callback not available");
            }
            return stringResult(
                callbacks.runSimulation(params.value("diagramJson", "")), "application/json");
        }
        if (method == "nflow.sim.poll") {
            if (!callbacks.pollSimulation) {
                throw std::runtime_error("simulation polling callback not available");
            }
            return stringResult(callbacks.pollSimulation(params.value("runId", "")),
                "application/json", POLL_RESULT_INLINE_THRESHOLD);
        }
        if (method == "nflow.sim.pause") {
            if (!callbacks.pauseSimulation) {
                return "false";
            }
            const bool wantPause = params.value("pause", params.value("wantPause", false));
            callbacks.pauseSimulation(wantPause);
            if (!wantPause && callbacks.isSimulationRunning) {
                return callbacks.isSimulationRunning() ? "true" : "false";
            }
            return "true";
        }
        if (method == "nflow.sim.stop") {
            if (!callbacks.stopSimulation || !callbacks.isSimulationRunning
                || !callbacks.isSimulationRunning()) {
                return "false";
            }
            callbacks.stopSimulation();
            return "true";
        }
        if (method == "nflow.sim.event") {
            if (callbacks.sendEvent) {
                callbacks.sendEvent(params.value("name", ""), params.value("data", ""));
            }
            return nlohmann::json { { "success", true } };
        }
        if (method == "nflow.callback.invoke") {
            if (!callbacks.evaluateCallback) {
                throw std::runtime_error("callback evaluation is not available");
            }
            std::string errorMessage;
            const bool success = callbacks.evaluateCallback(
                params.value("code", ""), params.value("blockId", ""), errorMessage);
            if (!success) {
                throw std::runtime_error(
                    errorMessage.empty() ? "callback evaluation failed" : errorMessage);
            }
            return nlohmann::json { { "success", true } };
        }
        if (method == "nflow.help.open") {
            const std::string topic = params.value("topic", "");
            const std::string prefix = "block:";
            const auto validIdentifier = [](const std::string& identifier) {
                return !identifier.empty()
                    && std::isalpha(static_cast<unsigned char>(identifier.front()))
                    && std::all_of(
                        identifier.begin(), identifier.end(), [](unsigned char character) {
                            return std::isalnum(character) || character == '_' || character == '-'
                                || character == '.';
                        });
            };
            const bool isBlockTopic = topic.compare(0, prefix.size(), prefix) == 0;
            const bool validTopic = isBlockTopic ? validIdentifier(topic.substr(prefix.size()))
                                                 : validIdentifier(topic);
            if (!validTopic) {
                throw std::runtime_error("invalid documentation topic");
            }
            if (!callbacks.openHelp) {
                throw std::runtime_error("documentation is not available");
            }
            std::string errorMessage;
            const std::string page = params.value("page", "");
            if (!callbacks.openHelp(topic, page, errorMessage)) {
                throw std::runtime_error(
                    errorMessage.empty() ? "opening documentation failed" : errorMessage);
            }
            return nlohmann::json { { "success", true }, { "topic", topic } };
        }
        if (method == "nflow.router.routeWires") {
            return stringResult(
                Nelson::NFlow::routeWires(params.value("payload", "")), "application/json");
        }
        if (method == "nflow.expr.eval") {
            return stringResult(
                Nelson::NFlow::evalExpressionJson(params.value("payload", "")), "application/json");
        }
        if (method == "nflow.examples.list") {
            // The unified examples catalog: Nelson module manifests plus the
            // NFlow provider registered by etc/startup.m (models and scripts).
            try {
                nlohmann::json response
                    = nlohmann::json::parse(Nelson::BuildLoadedModulesExamplesCatalog().json);
                response["locale"] = Nelson::CurrentExamplesLocale();
                return response;
            } catch (...) {
                return nlohmann::json { { "groups", nlohmann::json::array() },
                    { "diagnostics", nlohmann::json::array() }, { "locale", "en_US" } };
            }
        }
        if (method == "nflow.library.list") {
            nlohmann::json arr = nlohmann::json::array();
            for (const auto& manifest :
                Nelson::NFlow::BlockRegistry::instance().discoveredManifests()) {
                try {
                    arr.push_back(nlohmann::json::parse(manifest));
                } catch (...) {
                    arr.push_back(manifest);
                }
            }
            return arr;
        }
        if (method == "nflow.library.load") {
            const std::string dirPath = params.value("path", params.value("dirPath", ""));
            if (dirPath.empty()) {
                return nlohmann::json { { "success", false }, { "path", dirPath },
                    { "message", "A library location is required." } };
            }
            auto& reg = Nelson::NFlow::BlockRegistry::instance();
            bool ok = false;
            std::filesystem::path base(dirPath);
            if (std::filesystem::exists(base / "library.json")) {
                ok = reg.scanDirectory(base.parent_path().string());
            } else {
                ok = reg.scanDirectory(dirPath);
            }
            nlohmann::json manifests = nlohmann::json::array();
            for (const auto& manifest : reg.discoveredManifests()) {
                try {
                    manifests.push_back(nlohmann::json::parse(manifest));
                } catch (...) {
                    manifests.push_back(manifest);
                }
            }
            std::string js
                = "if(typeof "
                  "window.__nelsonPush==='function')window.__nelsonPush('nflow.library.loaded','"
                + escapeWebUiJsonString(manifests.dump()) + "');";
            try {
                e->get_window().run(js);
            } catch (...) {
            }
            return nlohmann::json { { "success", ok }, { "path", dirPath },
                { "manifests", manifests },
                { "message", ok ? "Library location scanned." : "No library was found." } };
        }
        if (method == "nflow.library.unload") {
            const std::string id = params.value("id", "");
            if (id.empty()) {
                return nlohmann::json { { "success", false }, { "id", id },
                    { "message", "A library identifier is required." } };
            }
            Nelson::NFlow::BlockRegistry::instance().unregisterLibrary(id);
            return nlohmann::json { { "success", true }, { "id", id },
                { "message", "Library disabled." } };
        }
        if (method == "nflow.library.getSearchPaths") {
            return Nelson::NFlow::BlockRegistry::instance().searchPaths();
        }
        if (method == "nflow.library.setSearchPaths") {
            std::vector<std::string> paths;
            nlohmann::json arr = nlohmann::json::array();
            if (params.contains("paths") && params["paths"].is_array()) {
                arr = params["paths"];
            } else if (params.contains("pathsJson") && params["pathsJson"].is_string()) {
                arr = nlohmann::json::parse(params["pathsJson"].get<std::string>());
            }
            for (const auto& item : arr) {
                if (item.is_string()) {
                    paths.push_back(item.get<std::string>());
                }
            }
            auto& registry = Nelson::NFlow::BlockRegistry::instance();
            registry.setSearchPaths(paths);
            nlohmann::json manifests = nlohmann::json::array();
            nlohmann::json locations = nlohmann::json::array();
            const auto normalizedPaths = registry.searchPaths();
            const auto rawManifests = registry.discoveredManifests();
            for (const auto& manifest : rawManifests) {
                try {
                    manifests.push_back(nlohmann::json::parse(manifest));
                } catch (...) {
                    manifests.push_back(manifest);
                }
            }
            for (const auto& path : normalizedPaths) {
                std::size_t count = 0;
                const std::filesystem::path root(path);
                for (const auto& manifest : manifests) {
                    if (!manifest.is_object() || !manifest.contains("_dir")) {
                        continue;
                    }
                    std::error_code ec;
                    const auto relative = std::filesystem::relative(
                        std::filesystem::path(manifest.value("_dir", "")), root, ec);
                    if (!ec && !relative.empty() && *relative.begin() != "..") {
                        ++count;
                    }
                }
                locations.push_back(nlohmann::json { { "path", path },
                    { "available", std::filesystem::is_directory(root) },
                    { "libraryCount", count } });
            }
            return nlohmann::json { { "success", true }, { "paths", normalizedPaths },
                { "manifests", manifests }, { "locations", locations },
                { "diagnostics", registry.discoveryMessages() },
                { "message", "Library locations scanned." } };
        }
        if (method == "nflow.codegen.generate") {
            return stringResult(generateCode(params), "text/plain");
        }
        if (method == "nflow.codegen.generateToDir") {
            return stringResult(generateCodeToDirRpc(params), "application/json");
        }
        if (method == "nflow.codegen.generateBlockToDir") {
            return stringResult(generateBlockToDirRpc(params), "application/json");
        }
        if (method == "nflow.fmi3.inspect") {
            // Extracts + reads an FMU (a .fmu archive or an already-extracted
            // directory) and returns its model description as JSON so the editor
            // can populate FMU block ports. Non-idempotent: it unpacks native code.
            const std::string fmuPath = params.value("path", params.value("fmu", std::string {}));
            std::string errorMessage;
            return stringResult(
                Nelson::NFlow::Fmi::inspectFmuJson(fmuPath, errorMessage), "application/json");
        }
        if (method == "nflow.ssp.import") {
            // Import an SSP archive into a runnable .nflow and return the model path so
            // the editor can open it. The callback (wired in nflow_guiBuiltin, which links
            // the bridge) runs the Nelson code on the interpreter thread.
            if (!callbacks.importSsp) {
                return stringResult(
                    "{\"success\":false,\"error\":\"SSP import is not available.\"}",
                    "application/json");
            }
            return stringResult(
                callbacks.importSsp(params.value("path", params.value("ssp", std::string {}))),
                "application/json");
        }
        if (method == "nflow.ssp.export") {
            // Package the diagram (its .nflow JSON content) into an SSP at the target
            // path, the inverse of nflow.ssp.import. The callback runs
            // NFlow.sspExportDiagram on the interpreter thread. Returns {success, path, error}.
            if (!callbacks.exportSsp) {
                return stringResult(
                    "{\"success\":false,\"error\":\"SSP export is not available.\"}",
                    "application/json");
            }
            return stringResult(callbacks.exportSsp(params.value("content", std::string {}),
                                    params.value("path", std::string {})),
                "application/json");
        }
        if (method == "nflow.fmu.export") {
            // Export the diagram (its .nflow JSON content) to an FMI 3.0 CS FMU at the
            // target path. The callback runs NFlow.exportfmu on the interpreter thread.
            // Returns {success, path, error}.
            if (!callbacks.exportFmu) {
                return stringResult(
                    "{\"success\":false,\"error\":\"FMU export is not available.\"}",
                    "application/json");
            }
            return stringResult(callbacks.exportFmu(params.value("content", std::string {}),
                                    params.value("path", std::string {})),
                "application/json");
        }
        if (method == "nflow.clipboard.set") {
            const std::filesystem::path path
                = std::filesystem::temp_directory_path() / "nflow_clipboard.json";
            writeTextFileUtf8(path.string(), params.value("data", ""));
            return stringResult("{\"success\":true}", "application/json");
        }
        if (method == "nflow.clipboard.get") {
            const std::filesystem::path path
                = std::filesystem::temp_directory_path() / "nflow_clipboard.json";
            std::string content;
            if (readTextFileUtf8(path.string(), content)) {
                return stringResult(content, "application/json");
            }
            return "";
        }
        if (method == "nflow.blob.readChunk") {
            const std::string blobId = params.value("blobId", "");
            const size_t offset = params.value("offset", static_cast<size_t>(0));
            std::lock_guard<std::mutex> lock(blobMutex);
            auto it = blobs.find(blobId);
            if (it == blobs.end()) {
                throw std::runtime_error("blob not found");
            }
            const size_t safeOffset = std::min(offset, it->second.size());
            const size_t n = std::min(BLOB_CHUNK_SIZE, it->second.size() - safeOffset);
            return nlohmann::json { { "data", it->second.substr(safeOffset, n) },
                { "nextOffset", safeOffset + n } };
        }
        if (method == "nflow.blob.release") {
            std::lock_guard<std::mutex> lock(blobMutex);
            blobs.erase(params.value("blobId", ""));
            return nlohmann::json { { "success", true } };
        }
        throw std::runtime_error("unknown nflow RPC method: " + method);
    }
    //=============================================================================
} // namespace
//=============================================================================
void
bindRpc(webui::window* win, const std::string& modelFilename, const std::string& modelName,
    NFlowRpcCallbacks callbacks)
{
    auto rpcHandler = [modelFilename, modelName, callbacks](webui::window::event* e) {
        int id = 0;
        try {
            const std::string envelope = e->get_string(0);
            const nlohmann::json request = nlohmann::json::parse(envelope);
            id = request.value("id", 0);
            const std::string method = request.value("method", "");
            const nlohmann::json params = request.value("params", nlohmann::json::object());
            e->return_string(
                okEnvelope(id, dispatchRpc(e, modelFilename, modelName, callbacks, method, params))
                    .dump());
        } catch (const std::exception& ex) {
            e->return_string(errorEnvelope(id, ex.what()).dump());
        } catch (...) {
            e->return_string(errorEnvelope(id, "unknown native RPC error").dump());
        }
    };
    win->bind("rpc", rpcHandler);
    win->bind("nflow_rpc", rpcHandler);
}
//=============================================================================
} // namespace Nelson::NFlowUiBindings
//=============================================================================
