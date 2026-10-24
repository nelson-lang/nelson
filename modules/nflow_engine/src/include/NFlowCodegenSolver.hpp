//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
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
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace nflow {
// Shared emitters for the generated fixed-RK4 (ode4) and adaptive Dormand-Prince
// 5(4) (ode45) nflow_step bodies, used by the C and Rust generators. The
// __nflow_rhs function stays in each generator (its body is the block graph).
// The stepper loop is emitted from one place for both targets.
struct SolverEmitSpec
{
    bool rust = false;
    std::vector<std::string>* lines = nullptr;
    std::string stateCount; // packed state count, decimal text
    std::string dtLiteral; // formatted model step
    // ode45 only:
    std::string rtolLiteral;
    std::string atolLiteral;
    std::string hmaxLiteral;
    // Per-block seam emission (op is an Rk4Op value: gather / scatter / clamp;
    // arr names the packed array, "" for clamp).
    std::function<void(int op, const std::string& arr)> emitSeam;
    // Zero-crossing lowering: packed surface count, decimal text. Empty or "0"
    // means no surfaces. Otherwise the rhs carries a trailing nullable __g
    // argument and the stepper localizes each sign change by bisection with
    // fixed-RK4 trial steps (60 iters / 1e-12), restarting at the crossing.
    std::string crossingCount;
    // A reset gate re-seeds its members' state during the REPORTING evaluation,
    // after the packed state was gathered: the stepper must read it again, or the
    // stages integrate from the value the reset discarded.
    bool resetReseed = false;
};
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP void
emitRk4Stepper(const SolverEmitSpec& spec);
// Embedded adaptive Dormand-Prince 5(4): same tableau, error norm and step
// control as DormandPrinceBackend; k1 recomputed per attempt, no FSAL.
NLSNFLOW_ENGINE_IMPEXP void
emitDp45Stepper(const SolverEmitSpec& spec);
//=============================================================================
} // namespace nflow
//=============================================================================
