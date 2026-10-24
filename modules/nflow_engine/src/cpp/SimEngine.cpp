//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#define _USE_MATH_DEFINES
#include "SimEngine.hpp"
#include "SimEngineTypes.hpp"
#include "SimEngineBusTypes.hpp"
#include "SimEngineCoupling.hpp" // iterative co-simulation coupling (opt-in)
#include "SimEngineResult.hpp" // result-JSON assembly helpers
#include "SimEngineFixedStep.hpp" // legacy fixed-step macro loop
#include "SimEngineSolverLoop.hpp" // RHS-based (CVODES / fixed-RK) solver loop
#include "SimEngineSchedule.hpp" // per-phase block schedules + algebraic ordering
#include "SimEngineMultirate.hpp" // base-step refinement + multi-rate table
#include "SimEngineRuntime.hpp" // progress pacing + solver/coupling config
#include "SimEngineTopology.hpp" // model topology (blocks, layout, wiring)
#include "SimEngineInit.hpp" // INIT phase + conditional-subsystem validation
#include "SimEngineLabels.hpp" // goto/from label source resolution
#include "SimEngineAlgebraic.hpp" // algebraic-loop solve
#include "SimEngineFunctionCall.hpp" // function-call subsystem wiring
#include "SimEngineEvents.hpp" // queued UI parameter events
#include "SimEngineAssets.hpp" // packaged example roots for nflow-example:/ assets
#include "DashboardBindingManager.hpp"
#include "BlockRegistry.hpp"
#include "SolverBackend.hpp"
#include "StateLayout.hpp"
#include "FieldNames.hpp"
#include <iostream>
#include <nlohmann/json.hpp>
#include <cmath>
#include <algorithm>
#include <vector>
#include <string>
#include <map>
#include <unordered_map>
#include <functional>
#include <limits>
#include <mutex>
#include <cstdio>
#include <stdexcept>
#include <thread>
#include <chrono>
#include "SimEventBridge.hpp"
#include <filesystem>
#include <cstdlib>
//=============================================================================
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_E
#define M_E 2.71828182845904523536
#endif
//=============================================================================
using json = nlohmann::json;
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    static void
    applyCommentedBlocks(json& diagram);
    //=============================================================================
    static std::once_flag s_builtinsOnce;
    void
    ensureBuiltinsRegistered()
    {
        std::call_once(s_builtinsOnce, []() { registerAllBuiltinHandlers(); });
    }
    //=============================================================================
    std::string
    runSimulation(const std::string& diagramJson, int chunkSize,
        const SimProgressCallback& onProgress, const std::function<bool()>& isCancelled,
        const std::function<bool()>& isPaused)
    {
        json diagram;
        try {
            diagram = json::parse(diagramJson);
        } catch (const std::exception& ex) {
            return std::string("{\"status\":\"error\",\"reason\":\"JSON parse error: ") + ex.what()
                + "\",\"scopes\":[]}";
        }

        // "on" blocks are removed from the run; "through" blocks are bypassed.
        applyCommentedBlocks(diagram);

        // Builtins must be registered before the signal layout: dimension rules live in
        // the block metadata.
        ensureBuiltinsRegistered();
        auto& reg = BlockRegistry::instance();

        ModelTopology topo;
        if (auto early = buildModelTopology(diagram, topo)) {
            return *early;
        }
        std::vector<Block>& blocks = topo.blocks;
        std::unordered_map<std::string, BlockId>& nameToId = topo.nameToId;
        std::unordered_map<std::string, double>& variables = topo.variables;
        BusRegistry& busRegistry = topo.busRegistry;
        // Not const: refined below for non-integer sample times.
        double dt = topo.dt;
        int samples = topo.samples;
        std::unordered_map<std::string, std::string>& labelSinks = topo.labelSinks;
        SignalLayout& sigLayout = topo.sigLayout;
        std::vector<int>& outputBase = topo.outputBase;
        std::vector<std::vector<int>>& inputIds = topo.inputIds;
        const int totalSlots = topo.totalSlots;
        PhaseSchedule sched;
        if (auto early = buildPhaseSchedule(blocks, reg, inputIds, outputBase, sched)) {
            return *early;
        }
        std::vector<int>& initIdx = sched.initIdx;
        std::vector<int>& outputIdx = sched.outputIdx;
        std::vector<int>& algIdx = sched.algIdx;
        std::vector<int>& afterStepIdx = sched.afterStepIdx;
        std::vector<int>& updateIdx = sched.updateIdx;
        std::vector<int>& algOrdered = sched.algOrdered;
        std::vector<AlgebraicComponent>& algComponents = sched.algComponents;
        const bool hasLabelBlocks = sched.hasLabelBlocks;
        const bool needsAlgebraicSolve = sched.needsAlgebraicSolve;

        // Non-integer sample times: refine the base step to the GCD so every generic
        // SampleTime becomes an integer sub-rate.
        refineBaseStepForSampleTimes(blocks, dt, samples);

        std::vector<BlockState> blockState(blocks.size());
        // One slot past the layout: a block with no output port uses outputBase ==
        // totalSlots as its "nowhere" marker, and the phases write through it. Without
        // the extra cell that write corrupts the heap.
        const int slotsWithSink = totalSlots + 1;
        std::vector<double> outputs(slotsWithSink, 0.0);
        // Sibling lanes, allocated only when the layout needs them.
        std::vector<double> outputsImag(sigLayout.hasComplex ? slotsWithSink : 0, 0.0);
        std::vector<long long> outputsI64(sigLayout.hasExact64 ? slotsWithSink : 0, 0);

        SimCtx ctx { blocks, inputIds, outputBase, labelSinks, nameToId, blockState, outputs,
            variables, dt, 0.0 };
        // Final simulated time; handlers (e.g. an FMU block) must not advance past it.
        ctx.tEnd = samples * dt;
        ctx.sigs = &sigLayout.sigs;
        ctx.portBase = &sigLayout.portBase;
        ctx.inputSigs = &sigLayout.inputSigs;
        if (sigLayout.hasComplex) {
            ctx.outputsImag = &outputsImag;
        }
        if (sigLayout.hasExact64) {
            ctx.outputsI64 = &outputsI64;
        }
        ctx.busRegistry = &busRegistry;
        // Out-of-band warning/error channel for block handlers.
        SimDiagnostics diagnostics;
        ctx.diag = &diagnostics;
        // Diagram base directory, used to resolve relative asset paths (e.g. FMU packages).
        // Empty for an unsaved diagram.
        std::string baseDirStr;
        if (diagram.contains("baseDir") && diagram["baseDir"].is_string()) {
            baseDirStr = diagram["baseDir"].get<std::string>();
        }
        ctx.baseDir = &baseDirStr;
        const std::map<std::string, std::string> exampleRoots = packagedExampleRoots();
        ctx.exampleRoots = &exampleRoots;
        // Read the tolerance before INIT: a block whose init uses it (an FMU's
        // enterInitializationMode) must not get the FMU internal default.
        if (diagram.contains("relTol") && diagram["relTol"].is_number()) {
            ctx.relTol = diagram["relTol"].get<double>();
        }

        if (auto early = runInitPhase(
                ctx, blocks, blockState, reg, variables, initIdx, diagnostics, inputIds)) {
            return *early;
        }

        const std::vector<DashboardBindingDiagnostic> dashboardBindingErrors
            = validateDashboardBindings(blocks, blockState, variables);
        if (!dashboardBindingErrors.empty()) {
            std::string reason;
            std::string errorBlocks = "\"errorBlocks\":[";
            for (size_t i = 0; i < dashboardBindingErrors.size(); ++i) {
                if (i > 0) {
                    reason += "; ";
                    errorBlocks += ",";
                }
                reason += dashboardBindingErrors[i].message;
                errorBlocks += json(dashboardBindingErrors[i].blockId).dump();
            }
            errorBlocks += "]";
            const std::vector<double> noTime;
            return buildOutputJson(
                "error", reason, noTime, blocks, blockState, inputIds, errorBlocks);
        }

        // hasFunctionCall gates the solver path below.
        const bool hasFunctionCall
            = wireFunctionCalls(ctx, blocks, blockState, reg, sigLayout, inputIds, totalSlots);

        auto resolveLabelSources = [&]() -> bool {
            return resolveLabelSourcesOnce(
                blocks, labelSinks, nameToId, inputIds, outputBase, sigLayout, outputs);
        };

        const ProgressPacing pacing = computeProgressPacing(diagram, blocks, reg, chunkSize);
        const bool useRealTime = pacing.useRealTime;

        // Progress deltas are published on a wall-clock cadence (100 ms by default,
        // 'progressPeriodMs' overrides it, mainly for tests). Counters are reset per run.
        simProgressCounters() = SimProgressCounters {};
        long long progressPeriodNs = ProgressPublishScheduler::kDefaultPeriodNs;
        if (diagram.contains("progressPeriodMs") && diagram["progressPeriodMs"].is_number()) {
            const double ms = diagram["progressPeriodMs"].get<double>();
            if (ms > 0.0) {
                progressPeriodNs = (long long)(ms * 1e6);
            }
        }
        ProgressDeltaSession progressSession { ProgressPublishScheduler { progressPeriodNs } };
        const bool profileProgress = diagram.contains("profileProgress")
            && diagram["profileProgress"].is_boolean() && diagram["profileProgress"].get<bool>();
        // With profileProgress and no consumer, attach a no-op sink so the publish path
        // is exercised from headless tests.
        SimProgressCallback effectiveOnProgress = onProgress;
        if (!effectiveOnProgress && profileProgress && pacing.progressEnabled) {
            effectiveOnProgress = [](const std::string&) { };
        }
        const bool progressActive = pacing.progressEnabled && effectiveOnProgress;
        ProgressDeltaSession* progressPtr = progressActive ? &progressSession : nullptr;

        // Reject a configuration the engine cannot honour instead of replacing it: a
        // clamped sampleTime of 0 would give 300001 samples for a 0.3 s run. The error
        // names the wrong field.
        {
            const std::vector<double> noTime;
            const std::string badSolver = unknownSolverName(diagram);
            if (!badSolver.empty()) {
                return buildOutputJson("error",
                    "unknown solver '" + badSolver
                        + "'; expected one of discrete, ode1, ode4, ode45, variableStiff, "
                          "variableNonstiff, cvodesBdf, cvodesAdams, dae, idas",
                    noTime, blocks, blockState, inputIds, "");
            }
            auto nonPositive = [&](const char* key) {
                return diagram.contains(key) && diagram[key].is_number()
                    && !(diagram[key].get<double>() > 0.0);
            };
            for (const char* key : { nflow::kSampleTime, nflow::kDuration, nflow::kRuntime }) {
                if (nonPositive(key)) {
                    return buildOutputJson("error",
                        std::string("'") + key + "' must be greater than zero", noTime, blocks,
                        blockState, inputIds, "");
                }
            }
            const std::string structureError = diagramStructureError(diagram);
            if (!structureError.empty()) {
                return buildOutputJson(
                    "error", structureError, noTime, blocks, blockState, inputIds, "");
            }
        }

        const SimRunConfig runCfg = buildRunConfig(diagram);
        const bool iterativeCoupling = runCfg.iterativeCoupling;
        const int couplingMaxIter = runCfg.couplingMaxIter;
        const double couplingTol = runCfg.couplingTol;

        // Physical islands use the IDAS residual form only under 'dae'/'idas'. The other
        // loops run them in the embedded implicit-step mode: DERIVATIVE freezes the island
        // slice and OUTPUT advances it once per major step.
        ctx.islandImplicitStep = !(runCfg.solverName == "dae" || runCfg.solverName == "idas");

        using Clock = std::chrono::steady_clock;
        Clock::time_point simWallStart = Clock::now();
        Clock::duration pauseOffset {}; // accumulates time spent paused
        // The first interim frame is due one full period after the start, so a run shorter
        // than a period emits none.
        progressSession.scheduler.start(
            std::chrono::duration_cast<std::chrono::nanoseconds>(simWallStart.time_since_epoch())
                .count());

        auto applySimEvents
            = [&]() { applyQueuedSimEvents(blocks, blockState, variables, &diagnostics); };

        // An absent or "discrete" solver uses the fused fixed-step loop. The RHS-based
        // solver loop serves the native fixed-step RK steppers (ode1/ode4) and the
        // variable-step CVODES / IDAS backends.
        const std::string& solverName = runCfg.solverName;
        const bool solverIsFixedRk = runCfg.solverIsFixedRk;
        // A fixed-step RK stepper with nothing to integrate runs the fixed-step loop: with
        // an empty state vector every stage is a no-op, so the trajectory is the same.
        // A variable-step solver keeps the refusal: it locates event instants between
        // samples and needs something to integrate.
        const bool noContinuousState
            = (buildStateLayout(blocks, blockState, reg, variables).totalContinuous == 0);
        const bool useSolverLoop = runCfg.useSolverLoop && !(solverIsFixedRk && noContinuousState);

        std::vector<double> time;
        time.reserve(samples + 1);

        bool algLoopFailed = false;
        double algLoopTime = 0.0;
        std::vector<int> algFailBlocks; // blocks of the offending loop, for the diagnostic

        auto cancelledResult = [&]() -> std::string {
            return buildOutputJson("error", "cancelled", time, blocks, blockState, inputIds, "");
        };

        // Multi-rate: a block whose "SampleTime" is slower than the base step updates every
        // round(Ts/dt) samples and holds its output between hits. Absent or <= base step
        // gives rate 1. Shared by the fixed-step loop and the solver loop's discrete-update
        // pass.
        std::vector<int> updateRate(blocks.size(), 1);
        if (auto early = computeUpdateRates(
                blocks, dt, updateIdx, algIdx, updateRate, time, blockState, inputIds)) {
            return *early;
        }

        // Defined after updateRate so it captures the rate table (sub-rate feedthrough
        // sample-and-hold).
        auto solveAlgebraicSystem = [&]() -> bool {
            return solveAlgebraic(ctx, blocks, reg, algOrdered, algIdx, algComponents, outputBase,
                sigLayout, outputs, hasLabelBlocks, resolveLabelSources, algFailBlocks, updateRate);
        };

        if (useSolverLoop) {
            // algLoopFailed and stopRequested are set through the shared references and handled
            // after this block.
            SolverLoopCtx slc { ctx, blocks, reg, blockState, variables, inputIds, outputIdx,
                algOrdered, afterStepIdx, outputs, time, dt, samples, solverName, solverIsFixedRk,
                hasFunctionCall, diagram, algLoopFailed, algLoopTime, progressPtr,
                effectiveOnProgress, isCancelled, isPaused, useRealTime, simWallStart, pauseOffset,
                applySimEvents, solveAlgebraicSystem, resolveLabelSources, cancelledResult,
                updateRate };
            if (auto early = runSolverLoop(slc)) {
                return *early;
            }
        } else {
            FixedStepLoopCtx fsc { ctx, blocks, reg, sigLayout, outputs, outputBase, blockState,
                inputIds, algOrdered, outputIdx, afterStepIdx, updateIdx, time, dt, samples,
                needsAlgebraicSolve, algLoopFailed, algLoopTime, progressPtr, effectiveOnProgress,
                isCancelled, isPaused, useRealTime, simWallStart, pauseOffset, iterativeCoupling,
                couplingMaxIter, couplingTol, solveAlgebraicSystem, applySimEvents, cancelledResult,
                updateRate, sched.eventIdx };
            if (auto early = runFixedStepLoop(fsc)) {
                return *early;
            }
        } // end legacy fixed-step path (else of useSolverLoop)

        // A fatal structured diagnostic (e.g. a lookup with DiagnosticForOutOfRangeInput =
        // Error) ends the run with an error naming the block.
        if (diagnostics.aborted()) {
            for (const auto& d : diagnostics.all()) {
                if (!d.fatal) {
                    continue;
                }
                json err;
                err["status"] = "error";
                std::string reason = d.message;
                if (!d.blockId.empty()) {
                    reason += " (block '" + d.blockId + "')";
                }
                err["reason"] = reason;
                err["errorBlocks"] = json::array();
                if (!d.blockId.empty()) {
                    err["errorBlocks"].push_back(d.blockId);
                }
                return err.dump();
            }
        }

        std::string finalResult
            = buildFinalResult(algLoopFailed, algLoopTime, algFailBlocks, algIdx, blocks, time,
                blockState, inputIds, sigLayout, outputs, outputsImag, outputsI64, busRegistry);

        // With "profileProgress": true in the diagram, append the per-run counters to the
        // final payload so tests can assert the cadence and the partial volume.
        SimProgressCounters& pc = simProgressCounters();
        pc.steps = (long long)time.size();
        pc.finalBytes = (long long)finalResult.size();
        if (diagram.contains("profileProgress") && diagram["profileProgress"].is_boolean()
            && diagram["profileProgress"].get<bool>() && !finalResult.empty()
            && finalResult.back() == '}') {
            finalResult.pop_back();
            finalResult += ",\"progressStats\":{\"steps\":" + std::to_string(pc.steps)
                + ",\"publishChecks\":" + std::to_string(pc.publishChecks)
                + ",\"publishesSent\":" + std::to_string(pc.publishesSent)
                + ",\"partialBytes\":" + std::to_string(pc.partialBytes)
                + ",\"finalBytes\":" + std::to_string(pc.finalBytes)
                + ",\"partialBuildNs\":" + std::to_string(pc.partialBuildNs)
                + ",\"samplesSerialized\":" + std::to_string(pc.samplesSerialized) + "}}";
        }
        return finalResult;
    }
    //=============================================================================
    // "on": the block and every connection touching it are removed. "through": the block
    // is bypassed (the source of its input port 0 feeds what its output port 0 drove),
    // then removed. A missing or commented source drops the outgoing link.
    static void
    applyCommentedBlocks(json& diagram)
    {
        if (!diagram.contains("blocks") || !diagram["blocks"].is_array()) {
            return;
        }
        json& blocks = diagram["blocks"];
        if (!diagram.contains("connections") || !diagram["connections"].is_array()) {
            diagram["connections"] = json::array();
        }
        json& conns = diagram["connections"];
        for (auto& b : blocks) {
            if (b.contains("params") && b["params"].is_object() && b["params"].contains("subsystem")
                && b["params"]["subsystem"].is_object()) {
                applyCommentedBlocks(b["params"]["subsystem"]);
            }
        }
        std::unordered_map<std::string, std::string> mode; // id -> "on"|"through"
        for (auto& b : blocks) {
            if (b.contains("params") && b["params"].is_object() && b["params"].contains("Commented")
                && b["params"]["Commented"].is_string()) {
                const std::string c = b["params"]["Commented"].get<std::string>();
                if (c == "on" || c == "through") {
                    mode[b.value("id", std::string())] = c;
                }
            }
        }
        if (mode.empty()) {
            return;
        }
        for (const auto& kv : mode) {
            if (kv.second != "through") {
                continue;
            }
            std::string srcFrom;
            int srcIdx = 0;
            bool haveSrc = false;
            for (const auto& c : conns) {
                if (c.value("to", std::string()) == kv.first && c.value("toIndex", -1) == 0) {
                    srcFrom = c.value("from", std::string());
                    srcIdx = c.value("fromIndex", 0);
                    haveSrc = true;
                    break;
                }
            }
            for (auto& c : conns) {
                if (c.value("from", std::string()) == kv.first && c.value("fromIndex", -1) == 0) {
                    if (haveSrc && mode.find(srcFrom) == mode.end()) {
                        c["from"] = srcFrom;
                        c["fromIndex"] = srcIdx;
                    }
                }
            }
        }
        json keptConns = json::array();
        for (const auto& c : conns) {
            if (mode.count(c.value("from", std::string())) != 0
                || mode.count(c.value("to", std::string())) != 0) {
                continue;
            }
            keptConns.push_back(c);
        }
        conns = std::move(keptConns);
        json keptBlocks = json::array();
        for (auto& b : blocks) {
            if (mode.count(b.value("id", std::string())) == 0) {
                keptBlocks.push_back(b);
            }
        }
        blocks = std::move(keptBlocks);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
