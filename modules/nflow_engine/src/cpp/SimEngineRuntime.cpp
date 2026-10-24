//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineRuntime.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include <algorithm>
#include <map>
#include <chrono>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    ProgressPacing
    computeProgressPacing(const nlohmann::json& diagram, const std::vector<Block>& blocks,
        BlockRegistry& reg, int chunkSize)
    {
        ProgressPacing pacing;
        bool hasInteractiveBlocks = false;
        for (const Block& b : blocks) {
            if (reg.metadataOf(b.type).interactive) {
                hasInteractiveBlocks = true;
                break;
            }
        }
        pacing.useRealTime = hasInteractiveBlocks;
        if (diagram.contains("realTime") && diagram["realTime"].is_boolean()) {
            pacing.useRealTime = diagram["realTime"].get<bool>();
        }

        // Publication cadence is wall-clock driven (ProgressPublishScheduler,
        // 100 ms default): chunkSize only enables/disables progress reporting.
        pacing.progressEnabled = chunkSize > 0;
        return pacing;
    }
    //=============================================================================
    SimProgressCounters&
    simProgressCounters()
    {
        thread_local SimProgressCounters counters;
        return counters;
    }
    //=============================================================================
    void
    publishSimProgress(ProgressDeltaSession& session,
        const std::function<void(const std::string&)>& onProgress, double progressFrac,
        double currentTime, const std::vector<double>& time, const std::vector<Block>& blocks,
        const std::vector<BlockState>& blockState, const std::vector<std::vector<int>>& inputIds,
        bool force)
    {
        using Clock = std::chrono::steady_clock;
        SimProgressCounters& counters = simProgressCounters();
        ++counters.publishChecks;

        const long long nowNs
            = std::chrono::duration_cast<std::chrono::nanoseconds>(Clock::now().time_since_epoch())
                  .count();
        if (!force && !session.scheduler.due(nowNs)) {
            return;
        }

        const int fromSample = session.lastPublishedSample;
        const int toSample = (int)time.size();

        const auto buildBegin = Clock::now();
        const std::string payload
            = buildProgressJson(session.sequence, progressFrac, currentTime, fromSample, toSample,
                time, blocks, blockState, inputIds, &session.dashboardPublishedLen);
        counters.partialBuildNs
            += std::chrono::duration_cast<std::chrono::nanoseconds>(Clock::now() - buildBegin)
                   .count();
        counters.partialBytes += (long long)payload.size();
        counters.samplesSerialized += (long long)(toSample - fromSample);
        ++counters.publishesSent;

        onProgress(payload);

        session.lastPublishedSample = toSample;
        ++session.sequence;
        // Measure the next period from AFTER build + transport, so their cost
        // stretches the interval instead of causing catch-up bursts.
        session.scheduler.markPublished(
            std::chrono::duration_cast<std::chrono::nanoseconds>(Clock::now().time_since_epoch())
                .count());
    }
    //=============================================================================
    SimRunConfig
    buildRunConfig(const nlohmann::json& diagram)
    {
        SimRunConfig cfg;
        // Opt-in iterative (Gauss-Seidel) co-simulation coupling for FMU-CS blocks
        // in a feedback loop. Off = the update phase is a single Jacobi pass.
        if (diagram.contains("iterativeCoupling") && diagram["iterativeCoupling"].is_boolean()) {
            cfg.iterativeCoupling = diagram["iterativeCoupling"].get<bool>();
        }
        if (diagram.contains("couplingMaxIter") && diagram["couplingMaxIter"].is_number()) {
            cfg.couplingMaxIter = (std::max)(1, (int)diagram["couplingMaxIter"].get<double>());
        }
        if (diagram.contains("couplingTol") && diagram["couplingTol"].is_number()) {
            cfg.couplingTol = diagram["couplingTol"].get<double>();
        }

        // An absent/"discrete" solver uses the fused fixed-step loop. The RHS-based
        // solver loop serves ode1/ode4 (FixedStepBackend over the shared graph RHS)
        // and the variable-step CVODES backends.
        if (diagram.contains("solver") && diagram["solver"].is_string()) {
            cfg.solverName = diagram["solver"].get<std::string>();
        }
        cfg.solverIsFixedRk = (cfg.solverName == "ode1" || cfg.solverName == "ode4");
        cfg.useSolverLoop = cfg.solverIsFixedRk || cfg.solverName == "ode45"
            || cfg.solverName == "variableStiff" || cfg.solverName == "variableNonstiff"
            || cfg.solverName == "cvodesBdf"
            || cfg.solverName == "cvodesAdams"
            // The DAE (IDAS) backend also uses the solver loop, consuming
            // OdeProblem::residual instead of rhs.
            || cfg.solverName == "dae" || cfg.solverName == "idas";
        return cfg;
    }
    //=============================================================================
    std::string
    unknownSolverName(const nlohmann::json& diagram)
    {
        // An unknown solver would quietly run the fixed-step loop: report its name.
        if (!diagram.contains("solver") || !diagram["solver"].is_string()) {
            return std::string();
        }
        const std::string name = diagram["solver"].get<std::string>();
        // "auto" and "" let the engine pick the default (the fixed-step loop):
        // accept them.
        static const char* known[] = { "", "auto", "discrete", "ode1", "ode4", "ode45",
            "variableStiff", "variableNonstiff", "cvodesBdf", "cvodesAdams", "dae", "idas" };
        for (const char* k : known) {
            if (name == k) {
                return std::string();
            }
        }
        return name;
    }
    //=============================================================================
    // Structural integrity of one diagram level: a wire must name existing blocks
    // and ports, and block ids must be unique. Port counts are otherwise inferred
    // from the wires, so only a count the model states itself can be contradicted;
    // a negative index never makes sense.
    static std::string
    diagramStructureError(
        const nlohmann::json& blocks, const nlohmann::json& connections, const std::string& where)
    {
        if (!blocks.is_array()) {
            return std::string();
        }
        const std::string at = where.empty() ? std::string() : (" in subsystem '" + where + "'");
        std::map<std::string, const nlohmann::json*> byId;
        for (const auto& b : blocks) {
            if (!b.is_object()) {
                continue;
            }
            const std::string id = b.value(nflow::kId, std::string());
            if (!byId.emplace(id, &b).second) {
                return "two blocks share the id '" + id + "'" + at;
            }
        }
        if (!connections.is_array()) {
            return std::string();
        }
        for (const auto& c : connections) {
            if (!c.is_object()) {
                continue;
            }
            struct End
            {
                const char* nameKey;
                const char* indexKey;
                const char* countKey;
                const char* label;
            };
            const End ends[2] = { { nflow::kFrom, nflow::kFromIndex, nflow::kOutputs, "output" },
                { nflow::kTo, nflow::kToIndex, nflow::kInputs, "input" } };
            for (const End& e : ends) {
                const std::string name = c.value(e.nameKey, std::string());
                auto it = byId.find(name);
                if (it == byId.end()) {
                    return "a connection names the block '" + name + "', which does not exist" + at;
                }
                const int idx = c.value(e.indexKey, 0);
                if (idx < 0) {
                    return "a connection addresses " + std::string(e.label) + " port "
                        + std::to_string(idx) + " of block '" + name + "'" + at;
                }
                const nlohmann::json& b = *it->second;
                if (b.contains(e.countKey) && b[e.countKey].is_number_integer()) {
                    const int declared = b[e.countKey].get<int>();
                    if (idx >= declared) {
                        return "a connection addresses " + std::string(e.label) + " port "
                            + std::to_string(idx) + " of block '" + name + "', which declares "
                            + std::to_string(declared) + at;
                    }
                }
            }
        }
        for (const auto& b : blocks) {
            if (!b.is_object()) {
                continue;
            }
            const nlohmann::json params = b.value(nflow::kParams, nlohmann::json::object());
            if (!params.is_object() || !params.contains("subsystem")
                || !params["subsystem"].is_object()) {
                continue;
            }
            const nlohmann::json& inner = params["subsystem"];
            const std::string err
                = diagramStructureError(inner.value(nflow::kBlocks, nlohmann::json::array()),
                    inner.value(nflow::kConnections, nlohmann::json::array()),
                    b.value(nflow::kId, std::string()));
            if (!err.empty()) {
                return err;
            }
        }
        return std::string();
    }
    //=============================================================================
    std::string
    diagramStructureError(const nlohmann::json& diagram)
    {
        return diagramStructureError(diagram.value(nflow::kBlocks, nlohmann::json::array()),
            diagram.value(nflow::kConnections, nlohmann::json::array()), std::string());
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
