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
#include <cstddef>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// Serves the pages of uihtml components to the desktop.
//
// A uihtml whose HTMLSource is a file cannot be shown with srcdoc: an
// about:srcdoc document inherits the parent's base URL, so the page's own
// relative css, js and images would resolve against the frontend bundle and
// 404. The file therefore has to be reachable over HTTP, next to its assets.
//
// The handler answers only the /uihtml/ prefix and returns NULL for everything
// else, which leaves the frontend served from the root folder: when the callback
// declines, WebUI falls back to looking for the file locally (see
// _webui_external_file_handler).
//
// Paths are never taken from the client. A component's file is served only
// after the engine itself reported that HTMLSource through uihtml.pull, and
// only files inside that document's own directory are reachable.
//=============================================================================
// Record what the engine says the component's HTMLSource is. Markup sources are
// ignored: they are inlined by the frontend and need no route.
void
UiHtmlRegisterSource(long long handle, const std::string& source);
//=============================================================================
// Drop a component's route (its figure closed, or it was deleted).
void
UiHtmlForgetSource(long long handle);
//=============================================================================
// WebUI file handler. Returns a full HTTP response, or NULL to decline.
const void*
UiHtmlFileHandler(size_t window, const char* filename, int* length);
//=============================================================================
} // namespace Nelson
//=============================================================================
