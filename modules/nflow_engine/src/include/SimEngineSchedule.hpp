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
#include <vector>
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// The block-categorisation step of SimEngine::runSimulation. From the block list
// it derives the per-phase execution schedules (which blocks run INIT / OUTPUT /
// ALGEBRAIC / AFTER_STEP / UPDATE), the topologically ordered algebraic set and
// its strongly-connected components, and the flags that drive the algebraic
// solve. Unknown (unregistered, non-comment) block types are rejected.
struct PhaseSchedule
{
    std::vector<int> initIdx;
    std::vector<int> outputIdx;
    std::vector<int> algIdx;
    std::vector<int> afterStepIdx;
    std::vector<int> updateIdx;
    // Blocks reacting to discrete events (Phase::EVENT): dispatched by the
    // legacy fixed-step loop once per sample between AFTER_STEP and UPDATE,
    // and by the solver loop at every located zero-crossing.
    std::vector<int> eventIdx;
    std::vector<int> algOrdered;
    std::vector<AlgebraicComponent> algComponents;
    bool hasLabelBlocks = false;
    bool needsAlgebraicSolve = false;
};
//=============================================================================
// Fills `out` and returns std::nullopt on success; returns a result-JSON error
// string when the diagram contains an unsupported block type.
std::optional<std::string>
buildPhaseSchedule(const std::vector<Block>& blocks, const BlockRegistry& reg,
    const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase,
    PhaseSchedule& out);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
