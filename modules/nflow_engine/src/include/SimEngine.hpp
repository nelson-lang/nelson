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
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Callback invoked every chunkSize steps with a partial-result JSON string
    // (same schema as the final output, with status="running" + progress field).
    using SimProgressCallback = std::function<void(const std::string&)>;
    //=============================================================================
    // Run an entire NFlow simulation from a JSON diagram description.
    //
    // Input  JSON schema (top level object):
    //   {
    //     "blocks":      [ { "id": str, "type": str, "inputs": int, "outputs": int,
    //                        "params": { key: value, ... } }, ... ],
    //     "connections": [ { "from": str, "to": str,
    //                        "fromIndex": int, "toIndex": int }, ... ],
    //     "sampleTime":  double,    // simulation dt
    //     "duration":    double,    // total simulated time
    //     "variables":   { key: double, ... }  // optional user variables
    //   }
    //
    // Output JSON schema (top level object):
    //   {
    //     "status": "done" | "error",
    //     "reason": string,          // present when status == "error"
    //     "scopes": [
    //       { "id": str, "type": "scope",
    //         "times": [double, ...],
    //         "series": [[double|null, ...], ...],
    //         "connected": [bool, ...] },
    //       { "id": str, "type": "xyScope",
    //         "x": [double|null, ...], "y": [double|null, ...],
    //         "connected": [bool, bool] },
    //       { "id": str, "type": "xyzScope",
    //         "x": [double|null, ...], "y": [double|null, ...],
    //         "z": [double|null, ...],
    //         "connected": [bool, bool, bool] }
    //     ]
    //   }
    //
    // The function is thread-safe: all state is kept on the stack / in local
    // containers; no global mutable state is used.
    // chunkSize: > 0 enables interim progress reporting (0 = no callbacks).
    //   Publication cadence is wall-clock driven (100 ms default; diagram key
    //   "progressPeriodMs" overrides it): a run finishing within one period
    //   emits no interim frame at all.
    // onProgress: optional; receives incremental protocol-v2 frames
    //   {version,sequence,status,progress,currentTime,fromSample,toSample,
    //    scopes:[...Delta arrays...],dashboardUpdates} carrying only the
    //   samples recorded since the previous frame (see buildProgressJson).
    NLSNFLOW_ENGINE_IMPEXP std::string
    runSimulation(const std::string& diagramJson, int chunkSize = 250,
        const SimProgressCallback& onProgress = nullptr,
        const std::function<bool()>& isCancelled = nullptr,
        const std::function<bool()>& isPaused = nullptr);
    //=============================================================================
    // Register the built-in block handlers (and scan NFLOW_DEV_PLUGIN_SCAN_PATH)
    // exactly once. Called by runSimulation and by the code generators so block
    // codegen templates resolve even when no simulation ran first.
    NLSNFLOW_ENGINE_IMPEXP void
    ensureBuiltinsRegistered();
    //=============================================================================
    // Numerical linearization. Uses the same setup and global RHS as runSimulation
    // to central-difference the state-space Jacobians A = df/dx, B = df/du,
    // C = dy/dx, D = dy/du about the post-INIT operating point (external-port
    // label-source inputs injected). Returns a JSON string {status, nx, nu, ny, A,
    // B, C, D} (all row-major) or {status:"error", reason}.
    NLSNFLOW_ENGINE_IMPEXP std::string
    linearizeModel(const std::string& diagramJson);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
