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
#include <functional>
#include <string>
#include <nlohmann/json.hpp>
#include "webui_bindings.hpp"
#include "nlsNflow_gui_builtin_exports.h"
//=============================================================================
namespace Nelson::NFlowUiBindings {
//=============================================================================
// Everything the nflow.* dispatch needs in place of a webui window event, so one
// core serves both the embedded webview and a headless stdio kernel. The webview
// binding fills push and nativeWindowHandle from its event; a headless host gives
// a push that relays a JSON-RPC notification and no native window handle.
struct NFlowDispatchContext
{
    // Emits a client push: a channel name plus a JSON payload string. The webview
    // host runs window.__nelsonPush; a headless host sends a JSON-RPC notification.
    std::function<void(const std::string& channel, const std::string& payloadJson)> push;
    // Native parent window handle for the system file dialog, or nullptr when none
    // is available (headless). Only nflow.fs.nativeDialog reads it.
    std::function<void*()> nativeWindowHandle;
    std::string modelFilename;
    std::string modelName;
    NFlowRpcCallbacks callbacks;
};
//=============================================================================
// Dispatch a single nflow.* method without a webui window. bindRpc routes the
// embedded webview through this via a context adapter, so the desktop host and a
// future stdio kernel share one dispatch core. Returns the RPC result payload;
// throws std::runtime_error on an unknown method.
NLSNFLOW_GUI_BUILTIN_IMPEXP nlohmann::json
dispatchNFlowRpc(
    const NFlowDispatchContext& context, const std::string& method, const nlohmann::json& params);
//=============================================================================
} // namespace Nelson::NFlowUiBindings
//=============================================================================
