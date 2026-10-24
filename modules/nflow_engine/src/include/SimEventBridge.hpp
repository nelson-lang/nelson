//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Simple bridge for providing UI-sent simulation events to the SimEngine.
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include <functional>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    using SimEvent = std::pair<std::string, std::string>;
    // The fetcher must return and drain the pending events.
    NLSNFLOW_ENGINE_IMPEXP void
    setSimEventFetcher(const std::function<std::vector<SimEvent>()>& f);
    // Returns an empty vector if no fetcher is registered.
    NLSNFLOW_ENGINE_IMPEXP std::vector<SimEvent>
    fetchSimEvents();
    // Invoker for 'invokeCallback' events (dashboardCallbackButton): runs a Nelson
    // code snippet mid-run. Returns true on success, false with an error message
    // otherwise. Only the single-threaded portable runtime uses it, evaluating the
    // callback between steps. The native build dispatches it on its own RPC thread.
    using SimCallbackInvoker = std::function<bool(
        const std::string& code, const std::string& blockId, std::string& err)>;
    NLSNFLOW_ENGINE_IMPEXP void
    setSimCallbackInvoker(const SimCallbackInvoker& invoker);
    // Returns false (without error) when no invoker is registered.
    NLSNFLOW_ENGINE_IMPEXP bool
    invokeSimCallback(const std::string& code, const std::string& blockId, std::string& err);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
