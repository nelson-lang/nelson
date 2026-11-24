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
#include "nlsWeb_engine_exports.h"
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
    // Figures-only desktop (nelson-adv-cli web figures): no browser console
    // window; each figure opens its own native WebView window. The interpreter
    // I/O stays on the real terminal.
    bool figuresOnly { false };
    // Notebook host of the Qt desktop: one native WebView window on the Notebook editor.
    bool notebookHost { false };
    // Headless figure RPC: no window of any kind. The figure RPC dispatch and the
    // pushes are routed to a caller-supplied sink instead of a WebUI page. Used by
    // the notebook kernel (figures carried over its stdio JSON-RPC channel).
    bool headlessRpc { false };
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
NLSWEB_ENGINE_IMPEXP bool
validateWebUiLaunchOptions(
    const WebUiLaunchOptions& input, WebUiLaunchOptions& normalized, std::string& error);
NLSWEB_ENGINE_IMPEXP bool
isWebUiLoopbackHost(const std::string& host);
NLSWEB_ENGINE_IMPEXP const char*
webUiLaunchModeName(WebUiLaunchMode mode);
//=============================================================================
NLSWEB_ENGINE_IMPEXP std::string
findNelsonApplicationIconPath(const std::wstring& nelsonRootDirectory);
NLSWEB_ENGINE_IMPEXP void
applyWebUiWindowIcon(size_t window, const std::wstring& icoFilePath);
NLSWEB_ENGINE_IMPEXP void
applyNelsonApplicationWindowIcon(size_t window, const std::wstring& nelsonRootDirectory);
NLSWEB_ENGINE_IMPEXP void
prepareWebUiWindowIcon(size_t window, const std::wstring& icoFilePath);
NLSWEB_ENGINE_IMPEXP void
prepareNelsonApplicationWindowIcon(size_t window, const std::wstring& nelsonRootDirectory);
NLSWEB_ENGINE_IMPEXP void
prepareNelsonWebUiWindow(size_t window, const std::string& initialTitle);
//=============================================================================
NLSWEB_ENGINE_IMPEXP std::string
escapeWebUiJsonString(const std::string& value);
NLSWEB_ENGINE_IMPEXP std::string
webUiSetSessionStorageItemScript(const std::string& key, const std::string& value);
NLSWEB_ENGINE_IMPEXP std::string
webUiSetLocalStorageItemScript(const std::string& key, const std::string& value);
//=============================================================================
} // namespace Nelson
//=============================================================================
