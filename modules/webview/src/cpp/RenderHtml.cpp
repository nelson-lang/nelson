//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RenderHtml.hpp"
#include "RenderHtmlHelpers.hpp"
#include "WebUiConfigScope.hpp"
#ifdef __APPLE__
#include "MacHtmlRenderer.h"
#endif
#include "characters_encoding.hpp"
#include "ErrorID.hpp"
//=============================================================================
extern "C"
{
#include "webui.h"
}
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <vector>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winhttp.h>
#include "ErrorID.hpp"
#endif
//=============================================================================
namespace {
//=============================================================================
using Nelson::WebviewRender::extractRenderedRoot;
using Nelson::WebviewRender::fileUrlFromPath;
using Nelson::WebviewRender::replaceMathJaxContainersWithImages;
//=============================================================================
constexpr size_t READY_BUFFER_SIZE = 64;
constexpr size_t HTML_BUFFER_SIZE = 64 * 1024 * 1024;
constexpr int MAX_POLLS = 60;
constexpr int POLL_DELAY_MS = 50;
constexpr size_t SCRIPT_TIMEOUT_SECONDS = 1;
#ifdef _WIN32
constexpr DWORD RENDERER_IO_TIMEOUT_MS = 3000;
constexpr DWORD DUMP_DOM_TIMEOUT_MS = 20000;
#endif
//=============================================================================
bool
runScript(size_t window, const char* script, std::vector<char>& buffer)
{
    if (buffer.empty()) {
        return false;
    }
    buffer[0] = '\0';
    return webui_script(window, script, SCRIPT_TIMEOUT_SECONDS, buffer.data(), buffer.size());
}
//=============================================================================
void
closeWindow(size_t window)
{
    webui_close(window);
    webui_destroy(window);
}
//=============================================================================
bool
isReadyValue(const std::vector<char>& buffer)
{
    const std::string value(buffer.data());
    return value == "1" || value == "\"1\"" || value == "true";
}
//=============================================================================
std::wstring
quoteCommandArgument(const std::wstring& value)
{
    return L"\"" + value + L"\"";
}
//=============================================================================
#ifdef _WIN32
std::vector<std::wstring>
findChromiumBrowsers();
//=============================================================================
void
setWinHttpTimeouts(HINTERNET handle)
{
    if (handle != nullptr) {
        WinHttpSetTimeouts(handle, RENDERER_IO_TIMEOUT_MS, RENDERER_IO_TIMEOUT_MS,
            RENDERER_IO_TIMEOUT_MS, RENDERER_IO_TIMEOUT_MS);
    }
}
//=============================================================================
bool
runProcessCaptureOutput(const std::wstring& command, std::string& output)
{
    SECURITY_ATTRIBUTES securityAttributes;
    securityAttributes.nLength = sizeof(SECURITY_ATTRIBUTES);
    securityAttributes.bInheritHandle = TRUE;
    securityAttributes.lpSecurityDescriptor = nullptr;

    HANDLE readPipe = nullptr;
    HANDLE writePipe = nullptr;
    if (!CreatePipe(&readPipe, &writePipe, &securityAttributes, 0)) {
        return false;
    }
    if (!SetHandleInformation(readPipe, HANDLE_FLAG_INHERIT, 0)) {
        CloseHandle(readPipe);
        CloseHandle(writePipe);
        return false;
    }

    STARTUPINFOW startupInfo;
    ZeroMemory(&startupInfo, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    startupInfo.dwFlags = STARTF_USESTDHANDLES;
    startupInfo.hStdOutput = writePipe;
    startupInfo.hStdError = writePipe;
    startupInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);

    PROCESS_INFORMATION processInfo;
    ZeroMemory(&processInfo, sizeof(processInfo));

    std::wstring commandLine = command;
    BOOL created = CreateProcessW(nullptr, commandLine.data(), nullptr, nullptr, TRUE,
        CREATE_NO_WINDOW, nullptr, nullptr, &startupInfo, &processInfo);
    CloseHandle(writePipe);
    if (!created) {
        CloseHandle(readPipe);
        return false;
    }

    const ULONGLONG startTick = GetTickCount64();
    bool ok = true;
    bool running = true;
    char buffer[8192];
    while (running) {
        DWORD available = 0;
        if (!PeekNamedPipe(readPipe, nullptr, 0, nullptr, &available, nullptr)) {
            ok = false;
            break;
        }
        while (available > 0) {
            DWORD bytesToRead = available < sizeof(buffer) ? available : sizeof(buffer);
            DWORD bytesRead = 0;
            if (!ReadFile(readPipe, buffer, bytesToRead, &bytesRead, nullptr)) {
                ok = false;
                break;
            }
            if (bytesRead == 0) {
                break;
            }
            output.append(buffer, bytesRead);
            available -= bytesRead;
        }
        if (!ok) {
            break;
        }

        DWORD waitStatus = WaitForSingleObject(processInfo.hProcess, 25);
        if (waitStatus == WAIT_OBJECT_0) {
            running = false;
        } else if (waitStatus == WAIT_FAILED) {
            ok = false;
            break;
        } else if (GetTickCount64() - startTick > DUMP_DOM_TIMEOUT_MS) {
            TerminateProcess(processInfo.hProcess, 1);
            WaitForSingleObject(processInfo.hProcess, 1000);
            ok = false;
            break;
        }
    }

    while (ok) {
        DWORD available = 0;
        if (!PeekNamedPipe(readPipe, nullptr, 0, nullptr, &available, nullptr) || available == 0) {
            break;
        }
        DWORD bytesToRead = available < sizeof(buffer) ? available : sizeof(buffer);
        DWORD bytesRead = 0;
        if (!ReadFile(readPipe, buffer, bytesToRead, &bytesRead, nullptr) || bytesRead == 0) {
            break;
        }
        output.append(buffer, bytesRead);
    }

    CloseHandle(processInfo.hThread);
    CloseHandle(processInfo.hProcess);
    CloseHandle(readPipe);
    return ok;
}
//=============================================================================
#endif
//=============================================================================
std::wstring
findChromiumBrowser()
{
#ifdef _WIN32
    const std::vector<std::wstring> candidates = findChromiumBrowsers();
    if (!candidates.empty()) {
        return candidates.front();
    }
#endif
    return L"";
}
//=============================================================================
std::vector<std::wstring>
findChromiumBrowsers()
{
    std::vector<std::wstring> browsers;
#ifdef _WIN32
    const std::vector<std::wstring> candidates
        = { L"C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe",
              L"C:\\Program Files\\Microsoft\\Edge\\Application\\msedge.exe",
              L"C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe",
              L"C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe" };
    for (const auto& candidate : candidates) {
        std::error_code ec;
        if (std::filesystem::exists(candidate, ec)) {
            browsers.push_back(candidate);
        }
    }
#endif
    return browsers;
}
//=============================================================================
#ifdef _WIN32
std::wstring
stringToWide(const std::string& value)
{
    return Nelson::utf8_to_wstring(value);
}
//=============================================================================
std::string
jsonEscape(const std::string& value)
{
    std::string escaped;
    escaped.reserve(value.size() + 16);
    for (unsigned char c : value) {
        switch (c) {
        case '\\':
            escaped += "\\\\";
            break;
        case '"':
            escaped += "\\\"";
            break;
        case '\b':
            escaped += "\\b";
            break;
        case '\f':
            escaped += "\\f";
            break;
        case '\n':
            escaped += "\\n";
            break;
        case '\r':
            escaped += "\\r";
            break;
        case '\t':
            escaped += "\\t";
            break;
        default:
            if (c < 0x20) {
                escaped += " ";
            } else {
                escaped += static_cast<char>(c);
            }
            break;
        }
    }
    return escaped;
}
//=============================================================================
void
appendUtf8Codepoint(std::string& value, unsigned int codepoint)
{
    if (codepoint <= 0x7F) {
        value += static_cast<char>(codepoint);
    } else if (codepoint <= 0x7FF) {
        value += static_cast<char>(0xC0 | (codepoint >> 6));
        value += static_cast<char>(0x80 | (codepoint & 0x3F));
    } else {
        value += static_cast<char>(0xE0 | (codepoint >> 12));
        value += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
        value += static_cast<char>(0x80 | (codepoint & 0x3F));
    }
}
//=============================================================================
int
hexValue(char c)
{
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'a' && c <= 'f') {
        return 10 + c - 'a';
    }
    if (c >= 'A' && c <= 'F') {
        return 10 + c - 'A';
    }
    return -1;
}
//=============================================================================
bool
jsonUnescapeString(const std::string& input, std::string& output)
{
    output.clear();
    for (size_t k = 0; k < input.size(); ++k) {
        if (input[k] != '\\') {
            output += input[k];
            continue;
        }
        if (++k >= input.size()) {
            return false;
        }
        switch (input[k]) {
        case '"':
        case '\\':
        case '/':
            output += input[k];
            break;
        case 'b':
            output += '\b';
            break;
        case 'f':
            output += '\f';
            break;
        case 'n':
            output += '\n';
            break;
        case 'r':
            output += '\r';
            break;
        case 't':
            output += '\t';
            break;
        case 'u': {
            if (k + 4 >= input.size()) {
                return false;
            }
            unsigned int codepoint = 0;
            for (int j = 0; j < 4; ++j) {
                int v = hexValue(input[++k]);
                if (v < 0) {
                    return false;
                }
                codepoint = (codepoint << 4) | static_cast<unsigned int>(v);
            }
            appendUtf8Codepoint(output, codepoint);
            break;
        }
        default:
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
extractJsonStringAfter(const std::string& json, size_t valueStart, std::string& value)
{
    std::string raw;
    bool escape = false;
    for (size_t k = valueStart; k < json.size(); ++k) {
        const char c = json[k];
        if (escape) {
            raw += '\\';
            raw += c;
            escape = false;
            continue;
        }
        if (c == '\\') {
            escape = true;
            continue;
        }
        if (c == '"') {
            return jsonUnescapeString(raw, value);
        }
        raw += c;
    }
    return false;
}
//=============================================================================
bool
extractJsonStringValue(const std::string& json, const std::string& key, std::string& value)
{
    const std::string marker = "\"" + key + "\":";
    size_t markerPos = json.find(marker);
    if (markerPos == std::string::npos) {
        return false;
    }
    size_t valueStart = markerPos + marker.size();
    while (valueStart < json.size() && std::isspace(static_cast<unsigned char>(json[valueStart]))) {
        ++valueStart;
    }
    if (valueStart >= json.size() || json[valueStart] != '"') {
        return false;
    }
    return extractJsonStringAfter(json, valueStart + 1, value);
}
//=============================================================================
bool
hasJsonId(const std::string& json, int id)
{
    const std::string marker = "\"id\":";
    size_t position = json.find(marker);
    while (position != std::string::npos) {
        size_t valueStart = position + marker.size();
        while (valueStart < json.size()
            && std::isspace(static_cast<unsigned char>(json[valueStart]))) {
            ++valueStart;
        }
        char* end = nullptr;
        long value = std::strtol(json.c_str() + valueStart, &end, 10);
        if (end != json.c_str() + valueStart && value == id) {
            return true;
        }
        position = json.find(marker, position + marker.size());
    }
    return false;
}
//=============================================================================
bool
winHttpRequest(int port, const std::wstring& method, const std::wstring& path, std::string& body)
{
    body.clear();
    HINTERNET session = WinHttpOpen(L"Nelson Webview", WINHTTP_ACCESS_TYPE_NO_PROXY,
        WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
    if (session == nullptr) {
        return false;
    }
    setWinHttpTimeouts(session);
    HINTERNET connect = WinHttpConnect(session, L"127.0.0.1", static_cast<INTERNET_PORT>(port), 0);
    if (connect == nullptr) {
        WinHttpCloseHandle(session);
        return false;
    }
    HINTERNET request = WinHttpOpenRequest(connect, method.c_str(), path.c_str(), nullptr,
        WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, 0);
    bool ok = false;
    if (request != nullptr
        && WinHttpSendRequest(
            request, WINHTTP_NO_ADDITIONAL_HEADERS, 0, WINHTTP_NO_REQUEST_DATA, 0, 0, 0)
        && WinHttpReceiveResponse(request, nullptr)) {
        DWORD size = 0;
        ok = true;
        while (WinHttpQueryDataAvailable(request, &size) && size > 0) {
            std::vector<char> buffer(size);
            DWORD read = 0;
            if (!WinHttpReadData(request, buffer.data(), size, &read)) {
                ok = false;
                break;
            }
            body.append(buffer.data(), read);
        }
    }
    if (request != nullptr) {
        WinHttpCloseHandle(request);
    }
    WinHttpCloseHandle(connect);
    WinHttpCloseHandle(session);
    return ok;
}
//=============================================================================
bool
extractWebSocketPath(const std::string& json, std::wstring& path)
{
    const std::string marker = "\"webSocketDebuggerUrl\":";
    size_t markerPos = json.find(marker);
    std::wstring fallbackPath;
    std::wstring pagePath;
    while (markerPos != std::string::npos) {
        size_t valueStart = markerPos + marker.size();
        while (valueStart < json.size()
            && std::isspace(static_cast<unsigned char>(json[valueStart]))) {
            ++valueStart;
        }
        std::string url;
        if (valueStart < json.size() && json[valueStart] == '"'
            && extractJsonStringAfter(json, valueStart + 1, url)) {
            const std::string host = "ws://127.0.0.1";
            size_t pathStart = url.find("/devtools/");
            if (url.rfind(host, 0) == 0 && pathStart != std::string::npos) {
                std::wstring candidatePath = stringToWide(url.substr(pathStart));
                size_t objectStart = json.rfind('{', markerPos);
                size_t objectEnd = json.find('}', markerPos);
                std::string objectJson;
                if (objectStart != std::string::npos && objectEnd != std::string::npos
                    && objectEnd > objectStart) {
                    objectJson = json.substr(objectStart, objectEnd - objectStart + 1);
                }
                if (objectJson.find("about:blank") != std::string::npos) {
                    path = candidatePath;
                    return true;
                }
                if (pagePath.empty() && objectJson.find("service_worker") == std::string::npos
                    && objectJson.find("edge://") == std::string::npos
                    && objectJson.find("chrome-extension://") == std::string::npos) {
                    pagePath = candidatePath;
                }
                if (fallbackPath.empty()) {
                    fallbackPath = candidatePath;
                }
            }
        }
        markerPos = json.find(marker, markerPos + marker.size());
    }
    if (!pagePath.empty()) {
        path = pagePath;
        return true;
    }
    if (!fallbackPath.empty()) {
        path = fallbackPath;
        return true;
    }
    return false;
}
//=============================================================================
std::filesystem::path
temporaryBasePath()
{
    std::error_code ec;
    std::filesystem::path tempBase = std::filesystem::temp_directory_path(ec);
    if (ec || tempBase.empty()) {
        tempBase = std::filesystem::current_path();
    }
    return tempBase;
}
//=============================================================================
bool
writeTemporaryHtml(const std::string& html, std::filesystem::path& htmlPath)
{
    const auto stamp = std::chrono::high_resolution_clock::now().time_since_epoch().count();
    htmlPath = temporaryBasePath() / ("nelson_webview_cdp_" + std::to_string(stamp) + ".html");
    std::ofstream htmlFile(htmlPath, std::ios::binary);
    if (!htmlFile.is_open()) {
        return false;
    }
    htmlFile.write(html.data(), static_cast<std::streamsize>(html.size()));
    return true;
} //=============================================================================
//=============================================================================
class PersistentHeadlessRenderer
{
public:
    ~PersistentHeadlessRenderer() { stop(); }
    bool
    render(const std::string& utf8Html, std::wstring& renderedHtml, std::wstring& error);

private:
    bool
    start(std::wstring& error);
    bool
    waitForDevToolsPort();
    bool
    createPage();
    bool
    openWebSocket(const std::wstring& path);
    bool
    sendCommand(const std::string& method, const std::string& params, std::string& response);
    bool
    receiveMessage(std::string& message);
    bool
    navigate(const std::string& url);
    bool
    waitReady();
    bool
    evaluateRoot(std::string& root);
    void
    stop();

    HINTERNET webSocket = nullptr;
    HINTERNET webSocketSession = nullptr;
    HINTERNET webSocketConnect = nullptr;
    PROCESS_INFORMATION processInfo = {};
    std::filesystem::path profilePath;
    int port = 0;
    int nextId = 1;
};
//=============================================================================
bool
PersistentHeadlessRenderer::waitForDevToolsPort()
{
    std::filesystem::path portPath = profilePath / "DevToolsActivePort";
    for (int k = 0; k < 100; ++k) {
        std::ifstream portFile(portPath);
        if (portFile.is_open()) {
            std::string line;
            std::getline(portFile, line);
            port = std::atoi(line.c_str());
            return port > 0;
        }
        if (processInfo.hProcess != nullptr
            && WaitForSingleObject(processInfo.hProcess, 0) != WAIT_TIMEOUT) {
            return false;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
    return false;
}
//=============================================================================
bool
PersistentHeadlessRenderer::openWebSocket(const std::wstring& path)
{
    webSocketSession = WinHttpOpen(L"Nelson Webview", WINHTTP_ACCESS_TYPE_NO_PROXY,
        WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
    if (webSocketSession == nullptr) {
        return false;
    }
    setWinHttpTimeouts(webSocketSession);
    webSocketConnect = WinHttpConnect(webSocketSession, L"127.0.0.1", port, 0);
    if (webSocketConnect == nullptr) {
        return false;
    }
    HINTERNET request = WinHttpOpenRequest(webSocketConnect, L"GET", path.c_str(), nullptr,
        WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, 0);
    if (request == nullptr) {
        return false;
    }
    BOOL upgraded = WinHttpSetOption(request, WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET, nullptr, 0);
    upgraded = upgraded
        && WinHttpSendRequest(
            request, WINHTTP_NO_ADDITIONAL_HEADERS, 0, WINHTTP_NO_REQUEST_DATA, 0, 0, 0);
    upgraded = upgraded && WinHttpReceiveResponse(request, nullptr);
    if (upgraded) {
        webSocket = WinHttpWebSocketCompleteUpgrade(request, 0);
    }
    WinHttpCloseHandle(request);
    return webSocket != nullptr;
}
//=============================================================================
bool
PersistentHeadlessRenderer::createPage()
{
    std::string json;
    if (!winHttpRequest(port, L"PUT", L"/json/new?about:blank", json)) {
        winHttpRequest(port, L"GET", L"/json/new?about:blank", json);
    }
    std::wstring path;
    if (!extractWebSocketPath(json, path)) {
        if (!winHttpRequest(port, L"GET", L"/json", json) || !extractWebSocketPath(json, path)) {
            return false;
        }
    }
    return openWebSocket(path);
}
//=============================================================================
bool
PersistentHeadlessRenderer::start(std::wstring& error)
{
    if (webSocket != nullptr) {
        return true;
    }
    const std::wstring browser = findChromiumBrowser();
    if (browser.empty()) {
        error = Nelson::resolveMessageTemplate(L"Nelson:webview:cannotFindChromiumBrowser",
            L"Cannot find a Chromium browser for HTML rendering.");
        return false;
    }
    const auto stamp = std::chrono::high_resolution_clock::now().time_since_epoch().count();
    profilePath = temporaryBasePath() / ("nelson_webview_persistent_" + std::to_string(stamp));
    std::error_code ec;
    std::filesystem::create_directories(profilePath, ec);

    std::wstring command = quoteCommandArgument(browser)
        + L" --headless=new --disable-gpu --no-first-run --disable-extensions"
          L" --disable-background-networking --disable-sync --no-default-browser-check"
          L" --allow-file-access-from-files"
          L" --remote-debugging-port=0 --user-data-dir="
        + quoteCommandArgument(profilePath.wstring()) + L" about:blank";

    STARTUPINFOW startupInfo;
    ZeroMemory(&startupInfo, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    ZeroMemory(&processInfo, sizeof(processInfo));
    if (!CreateProcessW(nullptr, command.data(), nullptr, nullptr, FALSE, CREATE_NO_WINDOW, nullptr,
            nullptr, &startupInfo, &processInfo)) {
        error = Nelson::resolveMessageTemplate(
            L"Nelson:webview:cannotStartBrowserRenderer", L"Cannot start browser renderer.");
        stop();
        return false;
    }
    if (!waitForDevToolsPort() || !createPage()) {
        error = Nelson::resolveMessageTemplate(
            L"Nelson:webview:cannotConnectBrowserRenderer", L"Cannot connect to browser renderer.");
        stop();
        return false;
    }
    std::string response;
    sendCommand("Page.enable", "", response);
    sendCommand("Runtime.enable", "", response);
    return true;
}
//=============================================================================
bool
PersistentHeadlessRenderer::receiveMessage(std::string& message)
{
    message.clear();
    std::vector<char> buffer(32768);
    while (true) {
        DWORD read = 0;
        WINHTTP_WEB_SOCKET_BUFFER_TYPE type;
        DWORD err = WinHttpWebSocketReceive(
            webSocket, buffer.data(), static_cast<DWORD>(buffer.size()), &read, &type);
        if (err != NO_ERROR || type == WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE) {
            return false;
        }
        if (read > 0) {
            message.append(buffer.data(), read);
        }
        if (type == WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE
            || type == WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE) {
            return true;
        }
    }
}
//=============================================================================
bool
PersistentHeadlessRenderer::sendCommand(
    const std::string& method, const std::string& params, std::string& response)
{
    const int id = nextId++;
    std::string command = "{\"id\":" + std::to_string(id) + ",\"method\":\"" + method + "\"";
    if (!params.empty()) {
        command += ",\"params\":" + params;
    }
    command += "}";
    if (WinHttpWebSocketSend(webSocket, WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE,
            const_cast<char*>(command.data()), static_cast<DWORD>(command.size()))
        != NO_ERROR) {
        return false;
    }
    for (int k = 0; k < 200; ++k) {
        if (!receiveMessage(response)) {
            return false;
        }
        if (hasJsonId(response, id)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
PersistentHeadlessRenderer::navigate(const std::string& url)
{
    std::string response;
    const std::string params = "{\"url\":\"" + jsonEscape(url) + "\"}";
    return sendCommand("Page.navigate", params, response);
}
//=============================================================================
bool
PersistentHeadlessRenderer::waitReady()
{
    const std::string expression = "window.nelsonMarkdownReady ? '1' : '0'";
    const std::string params
        = "{\"expression\":\"" + jsonEscape(expression) + "\",\"returnByValue\":true}";
    for (int k = 0; k < 120; ++k) {
        std::string response;
        if (!sendCommand("Runtime.evaluate", params, response)) {
            return false;
        }
        std::string value;
        if (extractJsonStringValue(response, "value", value) && value == "1") {
            return true;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
    return false;
}
//=============================================================================
bool
PersistentHeadlessRenderer::evaluateRoot(std::string& root)
{
    const std::string expression
        = "(()=>{const n=document.getElementById('nelson-markdowndisp-root');"
          "return n?n.innerHTML:document.body.innerHTML;})()";
    const std::string params
        = "{\"expression\":\"" + jsonEscape(expression) + "\",\"returnByValue\":true}";
    std::string response;
    if (!sendCommand("Runtime.evaluate", params, response)) {
        return false;
    }
    return extractJsonStringValue(response, "value", root);
}
//=============================================================================
bool
PersistentHeadlessRenderer::render(
    const std::string& utf8Html, std::wstring& renderedHtml, std::wstring& error)
{
    if (!start(error)) {
        return false;
    }
    std::filesystem::path htmlPath;
    if (!writeTemporaryHtml(utf8Html, htmlPath)) {
        error = Nelson::resolveMessageTemplate(L"Nelson:webview:cannotCreateTemporaryHtml",
            L"Cannot create temporary HTML renderer file.");
        return false;
    }
    const std::string url = fileUrlFromPath(htmlPath);
    std::string root;
    bool ok = false;
    if (navigate(url)) {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        ok = waitReady() && evaluateRoot(root);
    }
    std::error_code ec;
    std::filesystem::remove(htmlPath, ec);
    if (!ok) {
        error = Nelson::resolveMessageTemplate(
            L"Nelson:webview:cannotRetrieveRenderedHtml", L"Cannot retrieve rendered HTML.");
        stop();
        return false;
    }
    replaceMathJaxContainersWithImages(root);
    renderedHtml = Nelson::utf8_to_wstring(root);
    return true;
}
//=============================================================================
void
PersistentHeadlessRenderer::stop()
{
    if (webSocket != nullptr) {
        WinHttpWebSocketClose(webSocket, WINHTTP_WEB_SOCKET_SUCCESS_CLOSE_STATUS, nullptr, 0);
        WinHttpCloseHandle(webSocket);
        webSocket = nullptr;
    }
    if (webSocketConnect != nullptr) {
        WinHttpCloseHandle(webSocketConnect);
        webSocketConnect = nullptr;
    }
    if (webSocketSession != nullptr) {
        WinHttpCloseHandle(webSocketSession);
        webSocketSession = nullptr;
    }
    if (processInfo.hProcess != nullptr) {
        TerminateProcess(processInfo.hProcess, 0);
        CloseHandle(processInfo.hProcess);
        processInfo.hProcess = nullptr;
    }
    if (processInfo.hThread != nullptr) {
        CloseHandle(processInfo.hThread);
        processInfo.hThread = nullptr;
    }
    std::error_code ec;
    if (!profilePath.empty()) {
        std::filesystem::remove_all(profilePath, ec);
    }
    port = 0;
    nextId = 1;
}
//=============================================================================
bool
renderHtmlWithPersistentHeadless(
    const std::string& utf8Html, std::wstring& renderedHtml, std::wstring& error)
{
    static std::mutex rendererMutex;
    static PersistentHeadlessRenderer renderer;
    std::lock_guard<std::mutex> lock(rendererMutex);
    return renderer.render(utf8Html, renderedHtml, error);
}
//=============================================================================
#endif
//=============================================================================
bool
renderHtmlWithDumpDom(const std::string& utf8Html, std::wstring& renderedHtml, std::wstring& error)
{
#ifdef _WIN32
    const std::vector<std::wstring> browsers = findChromiumBrowsers();
    if (browsers.empty()) {
        error = Nelson::resolveMessageTemplate(L"Nelson:webview:cannotFindChromiumBrowser",
            L"Cannot find a Chromium browser for HTML rendering.");
        return false;
    }

    const auto stamp = std::chrono::high_resolution_clock::now().time_since_epoch().count();
    std::filesystem::path tempBase = temporaryBasePath();
    std::error_code tempEc;
    std::filesystem::path htmlPath
        = tempBase / ("nelson_webview_render_" + std::to_string(stamp) + ".html");
    std::filesystem::path userDataPath
        = tempBase / ("nelson_webview_profile_" + std::to_string(stamp));
    std::filesystem::create_directories(userDataPath, tempEc);

    {
        std::ofstream htmlFile(htmlPath, std::ios::binary);
        if (!htmlFile.is_open()) {
            error = Nelson::resolveMessageTemplate(L"Nelson:webview:cannotCreateTemporaryHtml",
                L"Cannot create temporary HTML renderer file.");
            return false;
        }
        htmlFile.write(utf8Html.data(), static_cast<std::streamsize>(utf8Html.size()));
    }

    const std::wstring url = Nelson::utf8_to_wstring(fileUrlFromPath(htmlPath));
    std::string root;
    bool browserStarted = false;
    for (size_t k = 0; k < browsers.size(); ++k) {
        const std::filesystem::path candidateUserDataPath
            = userDataPath.wstring() + L"_" + std::to_wstring(k);
        std::filesystem::create_directories(candidateUserDataPath, tempEc);
        const std::wstring command = quoteCommandArgument(browsers[k])
            + L" --headless --disable-gpu --no-first-run --disable-extensions"
              L" --disable-background-networking --disable-sync --no-default-browser-check"
              L" --allow-file-access-from-files"
              L" --virtual-time-budget=750 --user-data-dir="
            + quoteCommandArgument(candidateUserDataPath.wstring()) + L" --dump-dom "
            + quoteCommandArgument(url);

        std::string dom;
        if (runProcessCaptureOutput(command, dom)) {
            browserStarted = true;
            if (extractRenderedRoot(dom, root)) {
                std::error_code ec;
                std::filesystem::remove(htmlPath, ec);
                std::filesystem::remove_all(userDataPath, ec);
                replaceMathJaxContainersWithImages(root);
                renderedHtml = Nelson::utf8_to_wstring(root);
                return true;
            }
        }
        std::error_code ec;
        std::filesystem::remove_all(candidateUserDataPath, ec);
    }

    std::error_code ec;
    std::filesystem::remove(htmlPath, ec);
    std::filesystem::remove_all(userDataPath, ec);
    error = browserStarted
        ? Nelson::resolveMessageTemplate(
              L"Nelson:webview:cannotRetrieveRenderedHtml", L"Cannot retrieve rendered HTML.")
        : Nelson::resolveMessageTemplate(
              L"Nelson:webview:cannotStartBrowserRenderer", L"Cannot start browser renderer.");
    return false;
#else
    error = Nelson::resolveMessageTemplate(L"Nelson:webview:htmlRenderingAvailablePlatform",
        L"HTML rendering is not available on this platform.");
    return false;
#endif
}
//=============================================================================
} // namespace
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RenderHtmlWithWebview(const std::wstring& html, std::wstring& renderedHtml, std::wstring& error)
{
    renderedHtml.clear();
    error.clear();

    const std::string utf8Html = wstring_to_utf8(html);
#ifdef __APPLE__
    std::vector<char> nativeBuffer(HTML_BUFFER_SIZE);
    if (NelsonRenderHtmlWithWebKit(utf8Html.c_str(), nativeBuffer.data(), nativeBuffer.size())) {
        std::string htmlResult(nativeBuffer.data());
        replaceMathJaxContainersWithImages(htmlResult);
        renderedHtml = utf8_to_wstring(htmlResult);
        return true;
    }
#endif
#ifdef _WIN32
    std::wstring rendererError;
    if (renderHtmlWithPersistentHeadless(utf8Html, renderedHtml, rendererError)) {
        return true;
    }
    if (renderHtmlWithDumpDom(utf8Html, renderedHtml, rendererError)) {
        return true;
    }
#endif

    size_t window = webui_new_window();
    if (window == 0) {
        return renderHtmlWithDumpDom(utf8Html, renderedHtml, error);
    }

    webui_set_size(window, 1, 1);
    webui_set_position(window, 32000, 32000);
    webui_set_timeout(5);
    // `show_wait_connection` is process-global: restore it once this offscreen
    // render window is shown, so the renderer does not force later windows to
    // block until their page connects.
    WebUiConfigScope waitForConnection(show_wait_connection, true);
    // Per-window: `webui_set_config(ui_event_blocking, ...)` would have applied
    // to every other live WebUI window as well.
    webui_set_event_blocking(window, true);

    bool shown = webui_show_wv(window, utf8Html.c_str());
    if (!shown) {
        shown = webui_show_browser(window, utf8Html.c_str(), Edge);
    }
    if (!shown) {
        shown = webui_show_browser(window, utf8Html.c_str(), Chrome);
    }
    if (!shown) {
        shown = webui_show_browser(window, utf8Html.c_str(), ChromiumBased);
    }
    if (!shown) {
        closeWindow(window);
        return renderHtmlWithDumpDom(utf8Html, renderedHtml, error);
    }

    std::vector<char> readyBuffer(READY_BUFFER_SIZE);
    bool ready = false;
    for (int k = 0; k < MAX_POLLS; ++k) {
        if (runScript(window, "return window.nelsonMarkdownReady ? '1' : '0';", readyBuffer)
            && isReadyValue(readyBuffer)) {
            ready = true;
            break;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(POLL_DELAY_MS));
    }

    if (!ready) {
        closeWindow(window);
        return renderHtmlWithDumpDom(utf8Html, renderedHtml, error);
    }

    std::vector<char> htmlBuffer(HTML_BUFFER_SIZE);
    const char* script = "const n=document.getElementById('nelson-markdowndisp-root');"
                         "return n ? n.innerHTML : document.body.innerHTML;";
    if (!runScript(window, script, htmlBuffer)) {
        closeWindow(window);
        return renderHtmlWithDumpDom(utf8Html, renderedHtml, error);
    }

    std::string htmlResult(htmlBuffer.data());
    replaceMathJaxContainersWithImages(htmlResult);
    renderedHtml = utf8_to_wstring(htmlResult);
    closeWindow(window);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
