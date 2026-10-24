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
#include "nlsWebview_exports.h"
#include <cstddef>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class WebUiLaunchMode
{
    None,
    Server,
    WebView,
    Browser
};
//=============================================================================
struct WebUiLaunchOptions
{
    std::string host;
    size_t port { 0 };
    bool hasHost { false };
    bool hasPort { false };
    bool serverMode { false };
    // Figures-only desktop (nelson-adv-cli --webview): no browser console
    // window; each figure opens its own native WebView window. The interpreter
    // I/O stays on the real terminal.
    bool figuresOnly { false };
};
//=============================================================================
struct WebUiLaunchResult
{
    WebUiLaunchMode mode { WebUiLaunchMode::None };
    std::string url;
    size_t port { 0 };
    bool publicAccess { false };
};
//=============================================================================
NLSWEBVIEW_IMPEXP bool
validateWebUiLaunchOptions(
    const WebUiLaunchOptions& input, WebUiLaunchOptions& normalized, std::string& error);
NLSWEBVIEW_IMPEXP bool
isWebUiLoopbackHost(const std::string& host);
NLSWEBVIEW_IMPEXP const char*
webUiLaunchModeName(WebUiLaunchMode mode);
//=============================================================================
NLSWEBVIEW_IMPEXP std::string
findNelsonApplicationIconPath(const std::wstring& nelsonRootDirectory);
NLSWEBVIEW_IMPEXP void
applyWebUiWindowIcon(size_t window, const std::wstring& icoFilePath);
NLSWEBVIEW_IMPEXP void
applyNelsonApplicationWindowIcon(size_t window, const std::wstring& nelsonRootDirectory);
//=============================================================================
NLSWEBVIEW_IMPEXP std::string
escapeWebUiJsonString(const std::string& value);
NLSWEBVIEW_IMPEXP std::string
webUiSetSessionStorageItemScript(const std::string& key, const std::string& value);
NLSWEBVIEW_IMPEXP std::string
webUiSetLocalStorageItemScript(const std::string& key, const std::string& value);
//=============================================================================
} // namespace Nelson
//=============================================================================
