//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UiHtmlHost.hpp"
#include "UiHtmlBootstrap.hpp"
#include "WebUiConfigScope.hpp"
#include "characters_encoding.hpp"
//=============================================================================
extern "C"
{
#include "webui.h"
}
#include <nlohmann/json.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <mutex>
#include <sstream>
#include <cctype>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    using json = nlohmann::json;
    //=============================================================================
    constexpr size_t MAX_RECORDED_EVENTS = 4096;
    constexpr unsigned int DEFAULT_WIDTH = 900;
    constexpr unsigned int DEFAULT_HEIGHT = 640;
    //=============================================================================
    enum class SourceKind
    {
        LiteralHtml,
        LocalFile,
        Url
    };
    //=============================================================================
    struct UiHtmlWindow
    {
        size_t id = 0;
        size_t window = 0;
        SourceKind kind = SourceKind::LiteralHtml;
        std::wstring source;
        std::string url; // Url kind only
        std::string rootFolder; // LocalFile kind only
        // Built once per source and never mutated while the window is shown:
        // WebUI keeps the pointer until the response is written out.
        std::string entryResponse;
        std::string bootstrapResponse;
        std::string dataJson = "null";
        // Sequence of the last page call, to detect any ordering violation.
        uint64_t lastSequence = 0;
        UiHtmlStats stats;
        std::vector<UiHtmlEventRecord> events;
        std::vector<std::string> requestedPaths;
        // Responses for the page's own relative assets, cached so the pointer
        // handed to WebUI stays valid and stable (std::map never moves nodes).
        std::map<std::string, std::string> assetResponses;
    };
    //=============================================================================
    std::recursive_mutex&
    stateMutex()
    {
        // WebUI can run a close callback during static teardown.
        static auto* mutex = new std::recursive_mutex;
        return *mutex;
    }
    //=============================================================================
    std::map<size_t, UiHtmlWindow>&
    windows()
    {
        static auto* value = new std::map<size_t, UiHtmlWindow>;
        return *value;
    }
    //=============================================================================
    size_t&
    nextId()
    {
        static size_t value = 0;
        return value;
    }
    //=============================================================================
} // namespace
//=============================================================================
static UiHtmlWindow*
findById(size_t id);
//=============================================================================
static UiHtmlWindow*
findByWindow(size_t window);
//=============================================================================
static const void*
fileHandler(size_t window, const char* filename, int* length);
//=============================================================================
static void
rpcCallback(webui_event_t* event);
//=============================================================================
static bool
closeCallback(size_t window);
//=============================================================================
static std::string
httpResponse(const std::string& contentType, const std::string& body);
//=============================================================================
static bool
resolveSource(const std::wstring& source, UiHtmlWindow& target, std::wstring& error);
//=============================================================================
static bool
showWindow(UiHtmlWindow& target, std::wstring& error);
//=============================================================================
static void
destroyWindow(UiHtmlWindow& target);
//=============================================================================
static void
pushData(const UiHtmlWindow& target);
//=============================================================================
static const std::string*
assetResponseFor(UiHtmlWindow& target, const std::string& path);
//=============================================================================
static std::string
contentTypeForExtension(const std::string& extension);
//=============================================================================
bool
UiHtmlOpen(const std::wstring& source, size_t& id, std::wstring& error)
{
    UiHtmlWindow candidate;
    {
        std::lock_guard<std::recursive_mutex> lock(stateMutex());
        candidate.id = ++nextId();
        if (!resolveSource(source, candidate, error)) {
            return false;
        }
        windows()[candidate.id] = candidate;
    }
    bool shown = false;
    {
        std::lock_guard<std::recursive_mutex> lock(stateMutex());
        UiHtmlWindow* target = findById(candidate.id);
        if (target != nullptr) {
            shown = showWindow(*target, error);
            if (!shown) {
                windows().erase(candidate.id);
            }
        }
    }
    if (!shown) {
        return false;
    }
    id = candidate.id;
    return true;
}
//=============================================================================
bool
UiHtmlSetSource(size_t id, const std::wstring& source, std::wstring& error)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    if (target == nullptr) {
        error = L"Invalid uihtml identifier.";
        return false;
    }
    UiHtmlWindow updated = *target;
    if (!resolveSource(source, updated, error)) {
        return false;
    }
    // The window is rebuilt: in single-client mode WebUI hands out its JavaScript
    // bridge once per client (_webui_generate_js_bridge returns NULL the second
    // time), so an in-place reload can come back with an empty `webui.js`.
    destroyWindow(*target);
    updated.window = 0;
    *target = updated;
    return showWindow(*target, error);
}
//=============================================================================
bool
UiHtmlSetData(size_t id, const std::string& dataJson, std::wstring& error)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    if (target == nullptr) {
        error = L"Invalid uihtml identifier.";
        return false;
    }
    if (!json::accept(dataJson)) {
        error = L"Data must be valid JSON text.";
        return false;
    }
    target->dataJson = dataJson;
    ++target->stats.revision;
    ++target->stats.dataToPage;
    pushData(*target);
    return true;
}
//=============================================================================
bool
UiHtmlGetData(size_t id, std::string& dataJson, std::wstring& error)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    if (target == nullptr) {
        error = L"Invalid uihtml identifier.";
        return false;
    }
    dataJson = target->dataJson;
    return true;
}
//=============================================================================
bool
UiHtmlSendEvent(
    size_t id, const std::string& name, const std::string& dataJson, std::wstring& error)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    if (target == nullptr) {
        error = L"Invalid uihtml identifier.";
        return false;
    }
    if (!json::accept(dataJson)) {
        error = L"Event data must be valid JSON text.";
        return false;
    }
    std::string script = "window.__nelsonUiHtml && window.__nelsonUiHtml.applyEvent(";
    script += json(name).dump();
    script += ",";
    script += json(dataJson).dump();
    script += ");";
    webui_run(target->window, script.c_str());
    ++target->stats.eventsToPage;
    return true;
}
//=============================================================================
bool
UiHtmlDrainEvents(size_t id, std::vector<UiHtmlEventRecord>& events, std::wstring& error)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    if (target == nullptr) {
        error = L"Invalid uihtml identifier.";
        return false;
    }
    events = target->events;
    target->events.clear();
    return true;
}
//=============================================================================
bool
UiHtmlGetRequestedPaths(size_t id, std::vector<std::string>& paths, std::wstring& error)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    if (target == nullptr) {
        error = L"Invalid uihtml identifier.";
        return false;
    }
    paths = target->requestedPaths;
    return true;
}
//=============================================================================
bool
UiHtmlGetStats(size_t id, UiHtmlStats& stats, std::wstring& error)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    if (target == nullptr) {
        error = L"Invalid uihtml identifier.";
        return false;
    }
    stats = target->stats;
    return true;
}
//=============================================================================
bool
UiHtmlIsOpen(size_t id)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    return target != nullptr && target->window != 0 && webui_is_shown(target->window);
}
//=============================================================================
bool
UiHtmlClose(size_t id)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findById(id);
    if (target == nullptr) {
        return false;
    }
    destroyWindow(*target);
    windows().erase(id);
    return true;
}
//=============================================================================
void
UiHtmlCloseAll()
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    for (auto& entry : windows()) {
        destroyWindow(entry.second);
    }
    windows().clear();
}
//=============================================================================
static UiHtmlWindow*
findById(size_t id)
{
    auto found = windows().find(id);
    return found == windows().end() ? nullptr : &found->second;
}
//=============================================================================
static UiHtmlWindow*
findByWindow(size_t window)
{
    for (auto& entry : windows()) {
        if (entry.second.window == window && window != 0) {
            return &entry.second;
        }
    }
    return nullptr;
}
//=============================================================================
static const void*
fileHandler(size_t window, const char* filename, int* length)
{
    if (filename == nullptr || length == nullptr) {
        return nullptr;
    }
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findByWindow(window);
    if (target == nullptr) {
        return nullptr;
    }
    const std::string path(filename);
    target->requestedPaths.push_back(path);
    if (path == "/" || path == "/index.html") {
        *length = static_cast<int>(target->entryResponse.size());
        return target->entryResponse.data();
    }
    if (path == UIHTML_BOOTSTRAP_PATH) {
        *length = static_cast<int>(target->bootstrapResponse.size());
        return target->bootstrapResponse.data();
    }
    // Installing a file handler REPLACES root folder serving: WebUI never
    // falls back to it, it only probes the path as a virtual directory. So the
    // page's own relative assets have to be served from here.
    const std::string* asset = assetResponseFor(*target, path);
    if (asset != nullptr) {
        *length = static_cast<int>(asset->size());
        return asset->data();
    }
    return nullptr;
}
//=============================================================================
static void
rpcCallback(webui_event_t* event)
{
    std::string response = "{}";
    try {
        std::lock_guard<std::recursive_mutex> lock(stateMutex());
        UiHtmlWindow* target = findByWindow(event->window);
        const char* text = webui_get_string(event);
        const json request = json::parse(text == nullptr ? "{}" : text);
        const std::string kind = request.value("kind", "");
        const json payload = request.value("payload", json::object());
        if (target == nullptr) {
            webui_return_string(event, response.c_str());
            return;
        }
        // The page serializes its calls and stamps them, so a gap or a swap
        // means the ordering guarantee broke.
        const uint64_t sequence = request.value("sequence", static_cast<uint64_t>(0));
        if (sequence != 0) {
            if (sequence != target->lastSequence + 1) {
                ++target->stats.outOfOrder;
            }
            target->lastSequence = sequence;
        }
        if (kind == "ready") {
            ++target->stats.setupCount;
            target->stats.bridgeAvailable = true;
            json state;
            state["revision"] = target->stats.revision;
            state["data"] = json::parse(target->dataJson);
            response = state.dump();
        } else if (kind == "data") {
            target->dataJson
                = payload.contains("value") ? payload.at("value").dump() : std::string("null");
            ++target->stats.revision;
            ++target->stats.dataFromPage;
        } else if (kind == "event") {
            if (target->events.size() < MAX_RECORDED_EVENTS) {
                UiHtmlEventRecord record;
                record.name = payload.value("name", "");
                record.dataJson
                    = payload.contains("data") ? payload.at("data").dump() : std::string("null");
                target->events.push_back(record);
            }
            ++target->stats.eventsFromPage;
        }
    } catch (const std::exception&) {
        response = "{}";
    }
    webui_return_string(event, response.c_str());
}
//=============================================================================
static bool
closeCallback(size_t window)
{
    std::lock_guard<std::recursive_mutex> lock(stateMutex());
    UiHtmlWindow* target = findByWindow(window);
    if (target != nullptr) {
        target->stats.bridgeAvailable = false;
    }
    return true;
}
//=============================================================================
static std::string
httpResponse(const std::string& contentType, const std::string& body)
{
    std::ostringstream stream;
    stream << "HTTP/1.1 200 OK\r\n";
    stream << "Content-Type: " << contentType << "\r\n";
    stream << "Content-Length: " << body.size() << "\r\n";
    stream << "Cache-Control: no-cache, no-store, must-revalidate\r\n";
    stream << "\r\n";
    stream << body;
    return stream.str();
}
//=============================================================================
static bool
resolveSource(const std::wstring& source, UiHtmlWindow& target, std::wstring& error)
{
    std::wstring trimmed = source;
    const size_t first = trimmed.find_first_not_of(L" \t\r\n");
    if (first == std::wstring::npos) {
        error = L"HTMLSource is empty.";
        return false;
    }
    trimmed = trimmed.substr(first);
    target.source = source;
    target.rootFolder.clear();
    target.url.clear();

    if (trimmed[0] == L'<') {
        target.kind = SourceKind::LiteralHtml;
        target.entryResponse = httpResponse(
            "text/html; charset=utf-8", UiHtmlInjectBootstrap(wstring_to_utf8(trimmed)));
        target.bootstrapResponse
            = httpResponse("application/javascript; charset=utf-8", UiHtmlBootstrapScript());
        return true;
    }
    if (trimmed.rfind(L"http://", 0) == 0 || trimmed.rfind(L"https://", 0) == 0) {
        target.kind = SourceKind::Url;
        target.url = wstring_to_utf8(trimmed);
        // A foreign origin cannot be injected, so this page gets no bridge.
        target.entryResponse.clear();
        target.bootstrapResponse.clear();
        return true;
    }
    std::error_code filesystemError;
    const std::filesystem::path path(trimmed);
    if (!std::filesystem::is_regular_file(path, filesystemError)) {
        error = L"HTMLSource must be HTML text, an existing file, or an http(s) URL.";
        return false;
    }
    std::ifstream file(path, std::ios::binary);
    if (!file.is_open()) {
        error = L"Cannot read the HTMLSource file.";
        return false;
    }
    std::ostringstream contents;
    contents << file.rdbuf();
    target.kind = SourceKind::LocalFile;
    target.entryResponse
        = httpResponse("text/html; charset=utf-8", UiHtmlInjectBootstrap(contents.str()));
    target.bootstrapResponse
        = httpResponse("application/javascript; charset=utf-8", UiHtmlBootstrapScript());
    const std::filesystem::path parent = path.parent_path();
    target.rootFolder = parent.empty() ? std::string() : parent.string();
    return true;
}
//=============================================================================
static bool
showWindow(UiHtmlWindow& target, std::wstring& error)
{
    target.window = webui_new_window();
    if (target.window == 0) {
        error = L"Cannot create the uihtml window.";
        return false;
    }
    webui_set_event_blocking(target.window, true);
    webui_bind(target.window, "uihtmlRpc", rpcCallback);
    webui_set_close_handler_wv(target.window, closeCallback);
    webui_set_size(target.window, DEFAULT_WIDTH, DEFAULT_HEIGHT);
    if (target.kind != SourceKind::Url) {
        if (!target.rootFolder.empty()) {
            webui_set_root_folder(target.window, target.rootFolder.c_str());
        }
        webui_set_file_handler_window(target.window, fileHandler);
    }
    const std::string content = target.kind == SourceKind::Url ? target.url : "index.html";
    // Showing must not block the caller: with show_wait_connection left on, the
    // call spins until the page connects or the timeout expires.
    WebUiConfigScope waitForConnection(show_wait_connection, false);
    if (webui_show_wv(target.window, content.c_str())) {
        return true;
    }
    // No silent fallback to a web browser here: the spike exists to exercise
    // the native WebView, and a browser window would look like a pass.
    webui_destroy(target.window);
    target.window = 0;
    error = L"Cannot open the native WebView window.";
    return false;
}
//=============================================================================
static void
destroyWindow(UiHtmlWindow& target)
{
    if (target.window == 0) {
        return;
    }
    webui_set_close_handler_wv(target.window, nullptr);
    webui_close(target.window);
    webui_destroy(target.window);
    target.window = 0;
    target.stats.bridgeAvailable = false;
}
//=============================================================================
static const std::string*
assetResponseFor(UiHtmlWindow& target, const std::string& path)
{
    if (target.rootFolder.empty() || path.size() < 2 || path[0] != '/') {
        return nullptr;
    }
    const auto cached = target.assetResponses.find(path);
    if (cached != target.assetResponses.end()) {
        return &cached->second;
    }
    const std::string relative = path.substr(1);
    if (relative.find("..") != std::string::npos) {
        return nullptr;
    }
    std::error_code filesystemError;
    const std::filesystem::path root = std::filesystem::weakly_canonical(
        std::filesystem::path(target.rootFolder), filesystemError);
    if (filesystemError) {
        return nullptr;
    }
    const std::filesystem::path candidate
        = std::filesystem::weakly_canonical(root / relative, filesystemError);
    if (filesystemError || !std::filesystem::is_regular_file(candidate, filesystemError)) {
        return nullptr;
    }
    // Never serve outside the page's own directory.
    const std::string rootText = root.string();
    const std::string candidateText = candidate.string();
    if (candidateText.rfind(rootText, 0) != 0) {
        return nullptr;
    }
    std::ifstream file(candidate, std::ios::binary);
    if (!file.is_open()) {
        return nullptr;
    }
    std::ostringstream contents;
    contents << file.rdbuf();
    const std::string type = contentTypeForExtension(candidate.extension().string());
    const auto inserted = target.assetResponses.emplace(path, httpResponse(type, contents.str()));
    return &inserted.first->second;
}
//=============================================================================
static std::string
contentTypeForExtension(const std::string& extension)
{
    std::string lowered;
    for (char character : extension) {
        lowered += static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
    }
    if (lowered == ".html" || lowered == ".htm") {
        return "text/html; charset=utf-8";
    }
    if (lowered == ".css") {
        return "text/css; charset=utf-8";
    }
    if (lowered == ".js" || lowered == ".mjs") {
        return "application/javascript; charset=utf-8";
    }
    if (lowered == ".json") {
        return "application/json; charset=utf-8";
    }
    if (lowered == ".svg") {
        return "image/svg+xml";
    }
    if (lowered == ".png") {
        return "image/png";
    }
    if (lowered == ".jpg" || lowered == ".jpeg") {
        return "image/jpeg";
    }
    if (lowered == ".gif") {
        return "image/gif";
    }
    if (lowered == ".ico") {
        return "image/x-icon";
    }
    if (lowered == ".woff2") {
        return "font/woff2";
    }
    if (lowered == ".woff") {
        return "font/woff";
    }
    return "application/octet-stream";
}
//=============================================================================
static void
pushData(const UiHtmlWindow& target)
{
    if (target.window == 0) {
        return;
    }
    std::string script = "window.__nelsonUiHtml && window.__nelsonUiHtml.applyData(";
    script += json(target.dataJson).dump();
    script += ",";
    script += std::to_string(target.stats.revision);
    script += ");";
    webui_run(target.window, script.c_str());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
