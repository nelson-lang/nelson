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
#include <chrono>
#include <functional>
#include <optional>
#include <string>
#include <vector>
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "ProgressPublishScheduler.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Default (non-solver) fixed-step loop of SimEngine::runSimulation: steps every
// block once per sample dt through the OUTPUT / algebraic / AFTER_STEP / UPDATE
// phases, with real-time pacing, pause/cancel and opt-in iterative coupling.
// solveAlgebraicSystem re-runs the SCC-scoped algebraic solve (false on
// non-convergence), applySimEvents drains queued UI parameter events and
// cancelledResult builds the "cancelled" result JSON. algLoopFailed /
// algLoopTime are written on non-convergence and read by the caller.
//
// Returns a result JSON when the run stops early (cancellation), std::nullopt
// otherwise (also on an algebraic-loop failure, seen through algLoopFailed).
struct FixedStepLoopCtx
{
    SimCtx& ctx;
    std::vector<Block>& blocks;
    BlockRegistry& reg;
    const SignalLayout& sigLayout;
    std::vector<double>& outputs;
    const std::vector<int>& outputBase;
    std::vector<BlockState>& blockState;
    const std::vector<std::vector<int>>& inputIds;
    const std::vector<int>& algOrdered;
    const std::vector<int>& outputIdx;
    const std::vector<int>& afterStepIdx;
    const std::vector<int>& updateIdx;
    std::vector<double>& time;
    double dt;
    int samples;
    bool needsAlgebraicSolve;
    bool& algLoopFailed;
    double& algLoopTime;
    // Wall-clock publish scheduler + sample cursor; nullptr disables progress.
    ProgressDeltaSession* progress;
    // Held by value: the lambdas capture by reference and are only invoked
    // synchronously within runSimulation, so the captured references stay valid.
    std::function<void(const std::string&)> onProgress;
    std::function<bool()> isCancelled;
    std::function<bool()> isPaused;
    bool useRealTime;
    std::chrono::steady_clock::time_point simWallStart;
    std::chrono::steady_clock::duration& pauseOffset;
    bool iterativeCoupling;
    int couplingMaxIter;
    double couplingTol;
    std::function<bool()> solveAlgebraicSystem;
    std::function<void()> applySimEvents;
    std::function<std::string()> cancelledResult;
    // Block i runs its UPDATE only every updateRate[i] samples (>=1), holding its
    // output between hits. 1 everywhere = single-rate.
    const std::vector<int>& updateRate;
    // Blocks reacting to discrete events (Phase::EVENT, e.g. an FMI Model Exchange
    // component), dispatched once per sample between AFTER_STEP and UPDATE.
    // Appended last to keep the aggregate-initializer call sites stable.
    const std::vector<int>& eventIdx;
};
//=============================================================================
std::optional<std::string>
runFixedStepLoop(FixedStepLoopCtx& c);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
