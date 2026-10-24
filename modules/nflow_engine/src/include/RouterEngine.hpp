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
#include <string>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    /**
     * Route wires from a JSON snapshot.
     *
     * Input JSON: { "blocks":[...], "connections":[...], "width": N, "height": N,
     *               "timeLimitMs": N }
     * Snapshot may also be nested under a "snapshot" key.
     *
     * Returns JSON:
     *   { "ok": true,  "routes": [ [{x,y},...] | null, ... ] }
     *   { "ok": false, "error": "reason" }
     */
    NLSNFLOW_ENGINE_IMPEXP std::string
    routeWires(const std::string& snapshotJson);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
