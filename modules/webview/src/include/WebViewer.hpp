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
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
struct WebViewerOptions
{
    bool newWindow = false;
    bool showToolbar = true;
    bool showAddressBox = true;
};
//=============================================================================
NLSWEBVIEW_IMPEXP bool
OpenWebViewer(const std::wstring& input, const WebViewerOptions& options, size_t& viewerId,
    std::wstring& currentInput, std::wstring& error);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
SetWebViewerInput(
    size_t viewerId, const std::wstring& input, std::wstring& currentInput, std::wstring& error);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
SetWebViewerVisible(size_t viewerId, bool visible, std::wstring& error);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
GetWebViewerHtmlText(size_t viewerId, std::wstring& htmlText, std::wstring& error);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
CloseWebViewer(size_t viewerId);
//=============================================================================
NLSWEBVIEW_IMPEXP void
CloseAllWebViewers();
//=============================================================================
NLSWEBVIEW_IMPEXP bool
IsWebViewerOpen(size_t viewerId);
//=============================================================================
// Which WebUI backend is actually showing the viewer: L"webview" for the native
// window, the browser name when `webui_show_wv` failed and the fallback opened
// it in a web browser, L"none" when it is not shown. Showing an HTML viewer is a
// two-step fallback, and both steps report success to the caller, so this is the
// only way to tell a native window from a browser tab.
NLSWEBVIEW_IMPEXP bool
GetWebViewerBackend(size_t viewerId, std::wstring& backend, std::wstring& error);
//=============================================================================
} // namespace Nelson
//=============================================================================
