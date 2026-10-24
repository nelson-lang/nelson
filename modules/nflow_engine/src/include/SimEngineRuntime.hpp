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
#include <vector>
#include <nlohmann/json.hpp>
#include "SimEngineTypes.hpp"
#include "ProgressPublishScheduler.hpp"
//=============================================================================
namespace Nelson {
class BlockRegistry;
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Real-time pacing decision: whether the run must be paced to wall-clock (any
    // interactive block, or an explicit "realTime" flag). Progress publication is
    // paced by wall time (ProgressPublishScheduler); progressEnabled only carries
    // the caller's chunkSize > 0 switch.
    struct ProgressPacing
    {
        bool useRealTime = false;
        bool progressEnabled = false;
    };
    //=============================================================================
    ProgressPacing
    computeProgressPacing(const nlohmann::json& diagram, const std::vector<Block>& blocks,
        BlockRegistry& reg, int chunkSize);
    //=============================================================================
    // Opt-in progress instrumentation. Per-run counters, reset by
    // runSimulation on entry and readable afterwards; surfaced to tests through
    // the final payload's "progressStats" object when the diagram carries
    // "profileProgress": true (no production logging). thread_local: concurrent
    // runs on distinct worker threads never share an instance.
    struct SimProgressCounters
    {
        long long steps = 0; // samples recorded by the run
        long long publishChecks = 0; // publish-gate evaluations
        long long publishesSent = 0; // partial payloads actually emitted
        long long partialBytes = 0; // cumulative size of all partial payloads
        long long finalBytes = 0; // size of the final result payload
        long long partialBuildNs = 0; // time spent building partial payloads
        long long samplesSerialized = 0; // scope samples serialized into partials
    };
    //=============================================================================
    SimProgressCounters&
    simProgressCounters();
    //=============================================================================
    // Emit an incremental (v2) progress frame when the wall-clock scheduler says
    // one is due (or unconditionally with force = true): serializes only the
    // samples in [session.lastPublishedSample, time.size()), stamps the sequence,
    // advances the session cursor and feeds the instrumentation counters. The
    // publish instant is measured AFTER the payload is built and handed to
    // onProgress, so serialization/transport time is absorbed into the next
    // period instead of accumulating drift.
    void
    publishSimProgress(ProgressDeltaSession& session,
        const std::function<void(const std::string&)>& onProgress, double progressFrac,
        double currentTime, const std::vector<double>& time, const std::vector<Block>& blocks,
        const std::vector<BlockState>& blockState, const std::vector<std::vector<int>>& inputIds,
        bool force);
    //=============================================================================
    // Solver and coupling configuration parsed from the diagram: the solver name,
    // the derived fixed-RK / RHS-solver-loop flags, and the opt-in iterative
    // co-simulation coupling parameters (off by default: the update phase is a
    // single Jacobi pass).
    struct SimRunConfig
    {
        bool iterativeCoupling = false;
        int couplingMaxIter = 10;
        double couplingTol = 1e-6;
        std::string solverName;
        bool solverIsFixedRk = false;
        bool useSolverLoop = false;
    };
    //=============================================================================
    SimRunConfig
    buildRunConfig(const nlohmann::json& diagram);
    //=============================================================================
    // The diagram's solver name when the engine has no such solver, empty otherwise.
    std::string
    unknownSolverName(const nlohmann::json& diagram);
    //=============================================================================
    // A description of the first structural fault of the diagram (a wire naming a
    // block or a port that does not exist, two blocks sharing an id), empty when
    // there is none.
    std::string
    diagramStructureError(const nlohmann::json& diagram);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
