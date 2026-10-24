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
#include <chrono>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>
#include <nlohmann/json.hpp>
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "ProgressPublishScheduler.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// The RHS-based (variable-step CVODES and native fixed-RK ode1/ode4) solver loop
// of SimEngine::runSimulation: builds the continuous state layout, creates the
// solver backend, integrates with zero-crossing detection and event handling,
// records outputs at each sample and advances purely discrete blocks.
//
// The closures are held by value (owned std::function): they capture
// runSimulation locals and are only invoked synchronously within this call.
// algLoopFailed / algLoopTime are written on non-convergence and read by the
// caller after the loop.
//
// Returns a result JSON when the run stops early (an error, or a cancellation),
// std::nullopt otherwise (also on an algebraic-loop failure, seen through
// algLoopFailed).
struct SolverLoopCtx
{
    SimCtx& ctx;
    std::vector<Block>& blocks;
    BlockRegistry& reg;
    std::vector<BlockState>& blockState;
    const std::unordered_map<std::string, double>& variables;
    const std::vector<std::vector<int>>& inputIds;
    const std::vector<int>& outputIdx;
    const std::vector<int>& algOrdered;
    const std::vector<int>& afterStepIdx;
    std::vector<double>& outputs;
    std::vector<double>& time;
    double dt;
    int samples;
    std::string solverName;
    bool solverIsFixedRk;
    bool hasFunctionCall;
    const nlohmann::json& diagram;
    bool& algLoopFailed;
    double& algLoopTime;
    // Wall-clock publish scheduler + sample cursor; nullptr disables progress.
    ProgressDeltaSession* progress;
    std::function<void(const std::string&)> onProgress;
    std::function<bool()> isCancelled;
    std::function<bool()> isPaused;
    bool useRealTime;
    std::chrono::steady_clock::time_point simWallStart;
    std::chrono::steady_clock::duration& pauseOffset;
    std::function<void()> applySimEvents;
    std::function<bool()> solveAlgebraicSystem;
    std::function<bool()> resolveLabelSources;
    std::function<std::string()> cancelledResult;
    // A discrete block runs its once-per-sample UPDATE only every updateRate[i]
    // output samples (>=1). 1 everywhere = single-rate.
    const std::vector<int>& updateRate;
};
//=============================================================================
std::optional<std::string>
runSolverLoop(SolverLoopCtx& c);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
