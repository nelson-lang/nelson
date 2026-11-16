//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ai_notifyBuiltin.hpp"
#include "AssistantSink.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#ifndef NLSAI_WITHOUT_WEBVIEW
#include "WebToolsHost.hpp"
#endif
#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#include <string>
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
#ifdef __EMSCRIPTEN__
// Deliver an assistant event to the worker-client bridge, which re-emits it on the
// main thread as the 'tool.event' push the pane already consumes (subscribeToolEvents).
// A guarded no-op until that bridge registers the hook, mirroring the native path.
static void
pushWasmAssistantEvent(const std::string& channel, const std::string& payload)
{
    // clang-format off
    EM_ASM(
        {
            const callback = Module['onNelsonToolEvent'];
            if (typeof callback == 'function') {
                callback(UTF8ToString($0), UTF8ToString($1), UTF8ToString($2));
            }
        },
        "assistant", channel.c_str(), payload.c_str());
    // clang-format on
}
//=============================================================================
#endif
//=============================================================================
// __ai_notify__(channel, payloadJson[, surfaceId]): forward a live assistant
// event to ONE surface (surfaceId "web"/"notebook" -> the web desktop;
// "qt"/"nflow"/... -> that registered desktop sink). With no surfaceId it
// broadcasts to every surface (backward compatible). Silent no-op when no host
// is attached, so the agent can call it in any run mode.
ArrayOfVector
Nelson::AiGateway::ai_notifyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 0);
    std::string channel = argIn[0].asUtf8String();
    std::string payload;
    if (argIn.size() >= 2) {
        payload = argIn[1].asUtf8String();
    }
    std::string surfaceId;
    if (argIn.size() >= 3) {
        surfaceId = argIn[2].asUtf8String();
    }
    const bool toWeb = surfaceId.empty() || surfaceId == "web" || surfaceId == "notebook";
    if (toWeb) {
#ifndef NLSAI_WITHOUT_WEBVIEW
        PushWebToolEvent("assistant", channel, payload);
#elif defined(__EMSCRIPTEN__)
        pushWasmAssistantEvent(channel, payload);
#else
        (void)channel;
        (void)payload;
#endif
    }
    if (surfaceId.empty()) {
        // Backward-compatible broadcast to every registered desktop sink (legacy
        // single + NFlow), plus every surfaceId-registered sink, so surface-less
        // tool actions (editorInsert, notebookInsert) reach the host editor.
        Nelson::AssistantSinkPush(channel, payload);
        Nelson::NFlowAssistantSinkPush(channel, payload);
        Nelson::AssistantSinkBroadcast(channel, payload);
    } else if (!toWeb) {
        // A specific desktop surface (its pane registered under this surfaceId).
        Nelson::AssistantSinkPushTo(surfaceId, channel, payload);
    }
    return {};
}
//=============================================================================
