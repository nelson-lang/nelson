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
#include "nlsNflow_gui_builtin_exports.h"
//=============================================================================
namespace Nelson {
class Evaluator;
//=============================================================================
} // namespace Nelson
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// Run-state hooks the headless simulation core needs, with no window dependency.
// The window launcher binds these to its per-window state; a headless kernel binds
// them to its own run state. Empty hooks degrade safely (no progress reporting,
// and the run is never cancelled or paused).
struct NFlowSimRunHooks
{
    // Receives an engine progress delta (a partial-result JSON) during the run.
    std::function<void(const std::string& partialJson)> onProgress;
    // Returns true to abort the run at the next engine check.
    std::function<bool()> isCancelled;
    // Returns true to pause the run at the next engine check.
    std::function<bool()> isPaused;
    // Run the model StopFcn inline, on the expanded diagram, once the final result
    // is produced. Synchronous runs set this; async callers deliver the result
    // first and run StopFcn themselves afterwards.
    bool runStopFcnInline = false;
};
//=============================================================================
// Headless simulation core shared by the window launcher and a future stdio
// kernel. Expands modelica and acausal blocks on the interpreter thread, resolves
// From Workspace inputs against the evaluator base scope, runs the engine, flushes
// To Workspace outputs, and optionally runs StopFcn. Returns the engine result
// JSON. Pass the evaluator whose base workspace the model should read and write;
// ensureNelsonWorkspaceBridge is applied to it (a no-op when already installed for
// that evaluator, so call it on, or after registering, the interpreter thread).
// Install a sim event fetcher around the call when the model consumes injected
// events.
NLSNFLOW_GUI_BUILTIN_IMPEXP std::string
runNFlowSimulationCore(
    Evaluator* eval, const std::string& diagramJson, const NFlowSimRunHooks& hooks);
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
