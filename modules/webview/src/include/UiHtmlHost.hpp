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
#include <cstdint>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
// Spike host for a future `uihtml` component: one native WebView window whose
// page is wired to Nelson through the uihtml page contract (`setup(htmlComponent)`,
// `htmlComponent.Data`, `addEventListener(...)`, `sendEventToNelson(...)`).
//
// It answers three questions before any of `uihtml` is written:
//   1. does the Data round trip close without an echo loop,
//   2. does a user HTML file with relative assets (css, js, images) render,
//   3. is `setup()` replayed when the source changes.
//
// Not covered: the graphics model (GOUIHTML, the builder, the builtin,
// describeForWeb), the Qt binding, geometry, export, parenting, and user
// callbacks through the interpreter. The test polls a counter/event log instead
// of function handles, so no interpreter marshalling is involved.
//=============================================================================
// One page-to-Nelson event, as recorded by the host.
struct UiHtmlEventRecord
{
    std::string name;
    std::string dataJson;
};
//=============================================================================
// Counters used to prove the round trip. `setupCount` rises every time the page
// completes its handshake, i.e. every time `setup(htmlComponent)` runs.
struct UiHtmlStats
{
    uint64_t revision = 0;
    uint64_t dataFromPage = 0;
    uint64_t dataToPage = 0;
    uint64_t eventsFromPage = 0;
    uint64_t eventsToPage = 0;
    uint64_t setupCount = 0;
    // Page calls that did not arrive in the sequence the page sent them in.
    uint64_t outOfOrder = 0;
    bool bridgeAvailable = false;
};
//=============================================================================
// `source` is HTML text (it starts with '<'), a local file path, or an URL.
// An URL is displayed but has no bridge: a foreign origin cannot be injected.
NLSWEBVIEW_IMPEXP bool
UiHtmlOpen(const std::wstring& source, size_t& id, std::wstring& error);
//=============================================================================
// Replace the content. The page reloads and `setup(htmlComponent)` runs again.
NLSWEBVIEW_IMPEXP bool
UiHtmlSetSource(size_t id, const std::wstring& source, std::wstring& error);
//=============================================================================
// Nelson -> page. `dataJson` is JSON text; the page receives the parsed value
// and its "DataChanged" listeners fire. Does not fire anything on this side.
NLSWEBVIEW_IMPEXP bool
UiHtmlSetData(size_t id, const std::string& dataJson, std::wstring& error);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
UiHtmlGetData(size_t id, std::string& dataJson, std::wstring& error);
//=============================================================================
// Nelson -> page, named event. The page receives it through
// `addEventListener(name, ...)` with the payload in `event.Data`.
NLSWEBVIEW_IMPEXP bool
UiHtmlSendEvent(
    size_t id, const std::string& name, const std::string& dataJson, std::wstring& error);
//=============================================================================
// Hand over everything the page sent since the last call, and clear the log.
NLSWEBVIEW_IMPEXP bool
UiHtmlDrainEvents(size_t id, std::vector<UiHtmlEventRecord>& events, std::wstring& error);
//=============================================================================
// Spike diagnostic: the paths WebUI asked the file handler to serve.
NLSWEBVIEW_IMPEXP bool
UiHtmlGetRequestedPaths(size_t id, std::vector<std::string>& paths, std::wstring& error);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
UiHtmlGetStats(size_t id, UiHtmlStats& stats, std::wstring& error);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
UiHtmlIsOpen(size_t id);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
UiHtmlClose(size_t id);
//=============================================================================
NLSWEBVIEW_IMPEXP void
UiHtmlCloseAll();
//=============================================================================
} // namespace Nelson
//=============================================================================
