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
// Opt-in iterative (Gauss-Seidel) co-simulation coupling for the fixed-step UPDATE
// phase. When at least one FMU-CS block supports state get/set, the macro-step's
// FMU doSteps are iterated with state rollback until the coupling outputs
// converge, so a feedback loop of FMUs reaches a fixed point instead of a single
// Jacobi pass. Returns true when it handled the UPDATE phase. Returns false, doing
// nothing, when no FMU can roll back; the caller then runs the default single pass.
// solveAlgebraic re-runs the diagram's algebraic solve (may be a no-op).
bool
runIterativeCoupledUpdate(SimCtx& ctx, const std::vector<int>& updateIdx,
    const std::vector<int>& outputIdx, const BlockRegistry& reg, bool needsAlgebraicSolve,
    const std::function<void()>& solveAlgebraic, int couplingMaxIter, double couplingTol);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
