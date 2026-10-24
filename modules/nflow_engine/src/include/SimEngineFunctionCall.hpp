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
#include <vector>
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Function-call wiring of SimEngine::runSimulation. For each functionCallGenerator,
// resolves the ordered callee subsystems by following its event output through any
// functionCallSplit routers (output-port order = execution order) and stores the
// list on the generator's state. Then installs ctx.invokeSubsystem, the hook that
// runs a callee's ALGEBRAIC + UPDATE outside the topological schedule. The hook
// captures the caller's ctx / blocks, which outlive the run.
// Returns whether the diagram contains any function-call subsystem (the solver
// path rejects those).
bool
wireFunctionCalls(SimCtx& ctx, std::vector<Block>& blocks, std::vector<BlockState>& blockState,
    const BlockRegistry& reg, const SignalLayout& sigLayout,
    const std::vector<std::vector<int>>& inputIds, int totalSlots);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
