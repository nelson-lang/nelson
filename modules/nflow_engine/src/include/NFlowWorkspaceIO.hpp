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
#include <vector>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Pre-simulation pass: resolves every fromWorkspace block by reading its
    // VariableName through the NelsonWorkspaceBridge and injecting the samples into
    // the block params ('times', 'values', 'width'). Runs on any thread (the bridge
    // marshals to the interpreter thread). Returns false when a variable cannot be
    // resolved; errorJson then holds a {"status":"error",...} document.
    NLSNFLOW_ENGINE_IMPEXP bool
    resolveWorkspaceInputs(std::string& diagramJson, std::string& errorJson);
    //=============================================================================
    // Post-simulation pass: writes every toWorkspace entry of resultJson into the
    // workspace. Returns one error message per failed write (empty = all good).
    NLSNFLOW_ENGINE_IMPEXP std::vector<std::string>
    flushWorkspaceOutputs(const std::string& resultJson);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
