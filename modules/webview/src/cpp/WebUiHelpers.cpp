//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebUiHelpers.hpp"
#include "webui.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <filesystem>
#ifdef _WIN32
#include <windows.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isValidHostName(const std::string& host)
{
    if (host.empty()) {
        return false;
    }
    if (host.front() == '[' && host.back() == ']') {
        if (host.size() < 4) {
            return false;
        }
        return std::all_of(host.begin() + 1, host.end() - 1,
            [](unsigned char c) { return std::isxdigit(c) || c == ':' || c == '.' || c == '%'; });
    }
    if (host.front() == '.' || host.back() == '.' || host.front() == '-' || host.back() == '-') {
        return false;
    }
    bool previousDot = false;
    for (unsigned char c : host) {
        if (c == '.') {
            if (previousDot) {
                return false;
            }
            previousDot = true;
            continue;
        }
        previousDot = false;
        if (!std::isalnum(c) && c != '-') {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
validateWebUiLaunchOptions(
    const WebUiLaunchOptions& input, WebUiLaunchOptions& normalized, std::string& error)
{
    normalized = input;
    error.clear();
    if (normalized.hasPort && (normalized.port < 1 || normalized.port > 65535)) {
        error = "Web port must be between 1 and 65535.";
        return false;
    }
    if (!normalized.hasHost) {
        normalized.host = "localhost";
        return true;
    }
    std::string host = normalized.host;
    const std::string httpPrefix = "http://";
    std::string lower = host;
    std::transform(lower.begin(), lower.end(), lower.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    if (lower.rfind("https://", 0) == 0) {
        error = "HTTPS is not supported by the Nelson web server.";
        return false;
    }
    if (lower.rfind(httpPrefix, 0) == 0) {
        host.erase(0, httpPrefix.size());
    } else if (host.find("://") != std::string::npos) {
        error = "Only HTTP URLs are supported.";
        return false;
    }
    if (host.find_first_of("/@?#") != std::string::npos) {
        error = "The web URL must not contain credentials, a path, query, or fragment.";
        return false;
    }
    const bool bracketedIpv6 = host.size() > 1 && host.front() == '[' && host.back() == ']';
    if (!bracketedIpv6 && host.find(':') != std::string::npos) {
        error = "The web URL must not contain a port; use --port instead.";
        return false;
    }
    if (!isValidHostName(host)) {
        error = "Invalid web host or HTTP URL.";
        return false;
    }
    normalized.host = host;
    return true;
}
//=============================================================================
bool
isWebUiLoopbackHost(const std::string& host)
{
    std::string lower = host;
    std::transform(lower.begin(), lower.end(), lower.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return lower == "localhost" || lower == "127.0.0.1" || lower == "[::1]";
}
//=============================================================================
const char*
webUiLaunchModeName(WebUiLaunchMode mode)
{
    switch (mode) {
    case WebUiLaunchMode::Server:
        return "server";
    case WebUiLaunchMode::WebView:
        return "webview";
    case WebUiLaunchMode::Browser:
        return "browser";
    case WebUiLaunchMode::None:
        return "none";
    }
    return "none";
}
//=============================================================================
std::string
findNelsonApplicationIconPath(const std::wstring& nelsonRootDirectory)
{
    if (nelsonRootDirectory.empty()) {
        return {};
    }

    const std::filesystem::path root(nelsonRootDirectory);
    const std::filesystem::path iconRelativePath
        = std::filesystem::path("icons") / "hicolor" / "512x512" / "apps" / "nelson.png";
    const std::filesystem::path candidates[] = {
        root / "desktop" / iconRelativePath,
        root / "Nelson.icns",
        root.parent_path() / iconRelativePath,
    };
    std::error_code error;
    for (const auto& candidate : candidates) {
        if (std::filesystem::is_regular_file(candidate, error)) {
            return candidate.u8string();
        }
        error.clear();
    }
    return {};
}
//=============================================================================
// Windows title-bar/taskbar icon for a native WebUI window: the WebView window
// class carries a generic icon, so the given .ico is applied with WM_SETICON
// once the window exists. A no-op on other platforms, where the favicon
// (Linux/GTK) or the app bundle (macOS) provides the icon.
void
applyWebUiWindowIcon(size_t window, const std::wstring& icoFilePath)
{
#ifdef _WIN32
    HWND hwnd = static_cast<HWND>(webui_get_hwnd(window));
    if (hwnd == nullptr || icoFilePath.empty()) {
        return;
    }
    HICON bigIcon = static_cast<HICON>(LoadImageW(nullptr, icoFilePath.c_str(), IMAGE_ICON,
        GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON), LR_LOADFROMFILE));
    HICON smallIcon = static_cast<HICON>(LoadImageW(nullptr, icoFilePath.c_str(), IMAGE_ICON,
        GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), LR_LOADFROMFILE));
    if (bigIcon != nullptr) {
        SendMessageW(hwnd, WM_SETICON, ICON_BIG, reinterpret_cast<LPARAM>(bigIcon));
    }
    if (smallIcon != nullptr) {
        SendMessageW(hwnd, WM_SETICON, ICON_SMALL, reinterpret_cast<LPARAM>(smallIcon));
    }
#else
    (void)window;
    (void)icoFilePath;
#endif
}
//=============================================================================
void
applyNelsonApplicationWindowIcon(size_t window, const std::wstring& nelsonRootDirectory)
{
    if (nelsonRootDirectory.empty()) {
        return;
    }
    applyWebUiWindowIcon(window, nelsonRootDirectory + L"/resources/fibonacci.ico");
}
//=============================================================================
std::string
escapeWebUiJsonString(const std::string& value)
{
    std::string result;
    result.reserve(value.size() + 16);
    for (unsigned char c : value) {
        if (c == '"') {
            result += "\\\"";
        } else if (c == '\\') {
            result += "\\\\";
        } else if (c == '\n') {
            result += "\\n";
        } else if (c == '\r') {
            result += "\\r";
        } else if (c == '\t') {
            result += "\\t";
        } else if (c < 0x20) {
            char buffer[7];
            std::snprintf(buffer, sizeof(buffer), "\\u%04x", c);
            result += buffer;
        } else {
            result += static_cast<char>(c);
        }
    }
    return result;
}
//=============================================================================
static std::string
webUiSetStorageItemScript(
    const std::string& storageObject, const std::string& key, const std::string& value)
{
    return std::string("try{") + storageObject + ".setItem('" + escapeWebUiJsonString(key) + "','"
        + escapeWebUiJsonString(value) + "');}catch(_){}\n";
}
//=============================================================================
std::string
webUiSetSessionStorageItemScript(const std::string& key, const std::string& value)
{
    return webUiSetStorageItemScript("sessionStorage", key, value);
}
//=============================================================================
std::string
webUiSetLocalStorageItemScript(const std::string& key, const std::string& value)
{
    return webUiSetStorageItemScript("localStorage", key, value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
