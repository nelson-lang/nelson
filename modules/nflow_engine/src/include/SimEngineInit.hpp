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
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// INIT phase of SimEngine::runSimulation. Runs each block's INIT handler (seeding
// its state through ctx) and validates conditional subsystems: a triggered /
// enable-trigger subsystem may not carry continuous states, which cannot integrate
// between discrete edges. Aborts the run if any INIT reports a fatal diagnostic
// (e.g. an FMU that cannot be loaded). ctx, blockState and diagnostics are
// mutated in place.
// Returns a result-JSON error string when a triggered subsystem is invalid or an
// INIT diagnostic aborted the run; std::nullopt on success.
std::optional<std::string>
runInitPhase(SimCtx& ctx, const std::vector<Block>& blocks, std::vector<BlockState>& blockState,
    const BlockRegistry& reg, const std::unordered_map<std::string, double>& variables,
    const std::vector<int>& initIdx, SimDiagnostics& diagnostics,
    const std::vector<std::vector<int>>& inputIds);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
