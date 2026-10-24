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
#include <vector>
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// The algebraic-loop solve of SimEngine::runSimulation. Solves the algebraic
// subsystem in place: acyclic components in a single Gauss-Seidel pass, each
// strongly-connected loop iterated until every port-0 output settles within
// relTol*|x|+absTol (or, when goto/from labels are present, the whole algebraic
// set iterated to a numeric fixed point, re-resolving labels each sweep).
// On non-convergence it fills algFailBlocks with the blocks of the offending
// loop (read by the caller for the diagnostic) and returns false.
bool
solveAlgebraic(SimCtx& ctx, std::vector<Block>& blocks, BlockRegistry& reg,
    const std::vector<int>& algOrdered, const std::vector<int>& algIdx,
    const std::vector<AlgebraicComponent>& algComponents, const std::vector<int>& outputBase,
    const SignalLayout& sigLayout, std::vector<double>& outputs, bool hasLabelBlocks,
    const std::function<bool()>& resolveLabelSources, std::vector<int>& algFailBlocks,
    const std::vector<int>& updateRate);
//=============================================================================
// Evaluate one ALGEBRAIC-phase block with multi-rate sample-and-hold. With
// rate <= 1, or on a rate hit (ctx.stepIndex % rate == 0), it runs the block's
// ALGEBRAIC handler and latches the port-0 output into BlockState::heldOutput;
// between hits it replays the latched output without calling the handler.
// Shared by the fixed-step algebraic solve and the global-solver record path.
// All layout is read from ctx.
void
evalAlgebraicBlockMultirate(
    SimCtx& ctx, std::vector<Block>& blocks, BlockRegistry& reg, int blockIndex, int rate);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
