//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineSchedule.hpp"
#include "SimEngineResult.hpp"
#include "FieldNames.hpp"
#include <utility>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
std::optional<std::string>
buildPhaseSchedule(const std::vector<Block>& blocks, const BlockRegistry& reg,
    const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase,
    PhaseSchedule& out)
{
    std::vector<int> initIdx, outputIdx, algIdx, afterStepIdx, updateIdx, eventIdx;
    bool hasLabelBlocks = false;

    // Reject unknown block types: silently producing zeros hides real modeling
    // errors. Purely visual types (comments) are exempt.
    {
        std::vector<size_t> unknownIdx;
        for (size_t i = 0; i < blocks.size(); ++i) {
            const std::string& type = blocks[i].type;
            if (type == "comment") {
                continue;
            }
            if (reg.getHandler(type) == nullptr) {
                unknownIdx.push_back(i);
            }
        }
        if (!unknownIdx.empty()) {
            std::string errBlocks = "\"errorBlocks\":[";
            for (size_t k = 0; k < unknownIdx.size(); ++k) {
                if (k) {
                    errBlocks += ",";
                }
                errBlocks += "\"" + escapeJsonString(blocks[unknownIdx[k]].id) + "\"";
            }
            errBlocks += "]";
            const std::string reason = "unsupported_block: " + blocks[unknownIdx[0]].type;
            const std::vector<double> noTime;
            const std::vector<BlockState> noState(blocks.size());
            return buildOutputJson("error", reason, noTime, blocks, noState, inputIds, errBlocks);
        }
    }

    // A functionCall subsystem is never scheduled in topological order: it
    // runs only when a functionCallGenerator drives it through
    // SimCtx::invokeSubsystem. It keeps its INIT (its inner state must be
    // built) but is excluded from OUTPUT/ALGEBRAIC/AFTER_STEP/UPDATE.
    auto isFunctionCallSub = [](const Block& blk) -> bool {
        if (blk.type != "subsystem" || !blk.params.contains("controlPort")) {
            return false;
        }
        const auto& cp = blk.params["controlPort"];
        return cp.is_object() && cp.value("kind", std::string()) == "functionCall";
    };
    for (int i = 0; i < (int)blocks.size(); ++i) {
        const std::string& type = blocks[i].type;
        if (reg.getHandler(type) == nullptr) {
            continue; // unknown block
        }
        if (reg.hasPhase(type, Phase::INIT)) {
            initIdx.push_back(i);
        }
        if (isFunctionCallSub(blocks[i])) {
            continue; // driven on demand, not part of the topological schedule
        }
        if (reg.hasPhase(type, Phase::OUTPUT)) {
            outputIdx.push_back(i);
        }
        if (reg.hasPhase(type, Phase::ALGEBRAIC)) {
            algIdx.push_back(i);
        }
        if (reg.hasPhase(type, Phase::AFTER_STEP)) {
            afterStepIdx.push_back(i);
        }
        if (reg.hasPhase(type, Phase::UPDATE)) {
            updateIdx.push_back(i);
        }
        if (reg.hasPhase(type, Phase::EVENT)) {
            eventIdx.push_back(i);
        }
        if (type == nflow::kLabelSource || type == nflow::kLabelSink) {
            hasLabelBlocks = true;
        }
    }

    std::vector<int> algOrdered = topoSortAlgebraic(algIdx, blocks, inputIds, outputBase);
    bool algHasCycle = (algOrdered.size() != algIdx.size());
    if (algHasCycle) {
        algOrdered = algIdx; // fallback to original order
    }
    // Strongly-connected-component decomposition: acyclic blocks solve in a
    // single pass, and only the blocks of a genuine loop are iterated (never
    // the whole algebraic graph). Not used when goto/from labels are present
    // (their wires are virtual and outside this graph; see solveAlgebraicSystem).
    std::vector<AlgebraicComponent> algComponents
        = computeAlgebraicSCCs(algIdx, blocks, inputIds, outputBase);

    bool needsAlgebraicSolve = hasLabelBlocks || !algOrdered.empty();

    out.initIdx = std::move(initIdx);
    out.outputIdx = std::move(outputIdx);
    out.algIdx = std::move(algIdx);
    out.afterStepIdx = std::move(afterStepIdx);
    out.updateIdx = std::move(updateIdx);
    out.eventIdx = std::move(eventIdx);
    out.algOrdered = std::move(algOrdered);
    out.algComponents = std::move(algComponents);
    out.hasLabelBlocks = hasLabelBlocks;
    out.needsAlgebraicSolve = needsAlgebraicSolve;
    return std::nullopt;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
