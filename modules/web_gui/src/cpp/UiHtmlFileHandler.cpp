//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UiHtmlFileHandler.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <map>
#include <mutex>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    struct UiHtmlRoute
    {
        std::filesystem::path entry;
        std::filesystem::path directory;
    };
    //=============================================================================
    std::mutex&
    routesMutex()
    {
        static auto* mutex = new std::mutex;
        return *mutex;
    }
    //=============================================================================
    std::map<long long, UiHtmlRoute>&
    routes()
    {
        static auto* value = new std::map<long long, UiHtmlRoute>;
        return *value;
    }
    //=============================================================================
    // Responses stay alive and immutable once built: WebUI keeps the pointer
    // until it has written the body out, and std::map never moves its nodes.
    std::map<std::string, std::string>&
    responses()
    {
        static auto* value = new std::map<std::string, std::string>;
        return *value;
    }
    //=============================================================================
} // namespace
//=============================================================================
static const char* UIHTML_PREFIX = "/uihtml/";
//=============================================================================
static std::string
httpResponse(const std::string& contentType, const std::string& body);
//=============================================================================
static std::string
contentTypeFor(const std::string& extension);
//=============================================================================
static std::string
injectBootstrap(const std::string& document);
//=============================================================================
static const std::string*
cacheResponse(const std::string& key, std::string&& response);
//=============================================================================
static const void*
uiHtmlServe(size_t window, const char* filename, int* length);
//=============================================================================
void
UiHtmlRegisterSource(long long handle, const std::string& source)
{
    std::string trimmed = source;
    const size_t first = trimmed.find_first_not_of(" \t\r\n");
    if (first == std::string::npos) {
        UiHtmlForgetSource(handle);
        return;
    }
    trimmed = trimmed.substr(first);
    if (trimmed[0] == '<') {
        // Markup: the frontend inlines it, there is nothing to serve.
        UiHtmlForgetSource(handle);
        return;
    }
    std::error_code filesystemError;
    const std::filesystem::path path = std::filesystem::weakly_canonical(
        std::filesystem::path(utf8_to_wstring(trimmed)), filesystemError);
    if (filesystemError || !std::filesystem::is_regular_file(path, filesystemError)) {
        UiHtmlForgetSource(handle);
        return;
    }
    std::lock_guard<std::mutex> lock(routesMutex());
    routes()[handle] = UiHtmlRoute { path, path.parent_path() };
}
//=============================================================================
void
UiHtmlForgetSource(long long handle)
{
    std::lock_guard<std::mutex> lock(routesMutex());
    routes().erase(handle);
}
//=============================================================================
const void*
UiHtmlFileHandler(size_t window, const char* filename, int* length)
{
    // Called from a server thread across a C callback: an escaping exception
    // would terminate the process. Declining is always a safe answer.
    try {
        return uiHtmlServe(window, filename, length);
    } catch (...) {
        return nullptr;
    }
}
//=============================================================================
static const void*
uiHtmlServe(size_t window, const char* filename, int* length)
{
    (void)window;
    if (filename == nullptr || length == nullptr) {
        return nullptr;
    }
    const std::string path(filename);
    if (path.rfind(UIHTML_PREFIX, 0) != 0) {
        // Not ours. Declining lets WebUI serve the frontend from the root
        // folder exactly as it does without any handler installed.
        return nullptr;
    }
    const std::string rest = path.substr(std::string(UIHTML_PREFIX).size());

    const size_t slash = rest.find('/');
    const std::string handleText = rest.substr(0, slash);
    if (handleText.empty() || handleText.find_first_not_of("0123456789") != std::string::npos) {
        return nullptr;
    }
    const long long handle = std::stoll(handleText);

    UiHtmlRoute route;
    {
        std::lock_guard<std::mutex> lock(routesMutex());
        const auto found = routes().find(handle);
        if (found == routes().end()) {
            return nullptr;
        }
        route = found->second;
    }

    std::filesystem::path target = route.entry;
    if (slash != std::string::npos && slash + 1 < rest.size()) {
        const std::string relative = rest.substr(slash + 1);
        if (relative.find("..") != std::string::npos) {
            return nullptr;
        }
        std::error_code filesystemError;
        target = std::filesystem::weakly_canonical(
            route.directory / utf8_to_wstring(relative), filesystemError);
        if (filesystemError || !std::filesystem::is_regular_file(target, filesystemError)) {
            return nullptr;
        }
        // Never serve outside the page's own directory.
        if (target.string().rfind(route.directory.string(), 0) != 0) {
            return nullptr;
        }
    }

    std::ifstream file(target, std::ios::binary);
    if (!file.is_open()) {
        return nullptr;
    }
    std::ostringstream contents;
    contents << file.rdbuf();
    const bool isEntry = (target == route.entry);
    std::string body = contents.str();
    if (isEntry) {
        body = injectBootstrap(body);
    }
    const std::string* cached = cacheResponse(path + "#" + std::to_string(body.size()),
        httpResponse(contentTypeFor(target.extension().string()), body));
    *length = static_cast<int>(cached->size());
    return cached->data();
}
//=============================================================================
static const std::string*
cacheResponse(const std::string& key, std::string&& response)
{
    std::lock_guard<std::mutex> lock(routesMutex());
    auto& store = responses();
    auto found = store.find(key);
    if (found == store.end()) {
        found = store.emplace(key, std::move(response)).first;
    }
    return &found->second;
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
static std::string
contentTypeFor(const std::string& extension)
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
static size_t
findCaseInsensitive(const std::string& haystack, const std::string& needle)
{
    const auto equalsIgnoringCase = [](char left, char right) {
        return std::tolower(static_cast<unsigned char>(left))
            == std::tolower(static_cast<unsigned char>(right));
    };
    const auto found = std::search(
        haystack.begin(), haystack.end(), needle.begin(), needle.end(), equalsIgnoringCase);
    return found == haystack.end() ? std::string::npos
                                   : static_cast<size_t>(std::distance(haystack.begin(), found));
}
//=============================================================================
static std::string
injectBootstrap(const std::string& document)
{
    const std::string tag = "<script src=\"/uihtml-bootstrap.js\"></script>\n";
    const size_t headEnd = findCaseInsensitive(document, "</head>");
    if (headEnd != std::string::npos) {
        return document.substr(0, headEnd) + tag + document.substr(headEnd);
    }
    const size_t bodyTag = findCaseInsensitive(document, "<body");
    if (bodyTag != std::string::npos) {
        const size_t close = document.find('>', bodyTag);
        if (close != std::string::npos) {
            return document.substr(0, close + 1) + tag + document.substr(close + 1);
        }
    }
    return tag + document;
}
//=============================================================================
//=============================================================================
} // namespace Nelson
//=============================================================================
