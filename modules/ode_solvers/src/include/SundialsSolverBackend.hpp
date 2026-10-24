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
#include "SolverBackend.hpp"
#include "nlsOde_solvers_exports.h"
//=============================================================================
// ode_solvers provides the SUNDIALS-backed variable-step (CVODES) and
// differential-algebraic (IDAS) implementations of NFlow's ISolverBackend. The
// seam is one-directional: ode_solvers is a low-level numeric module that must
// not link the NFlow simulation libraries, so it never touches NFlow's
// solver-backend registry. The NFlow side (nflow_engine) passes its own
// registrar callback here. NFlow consumes ode_solvers, and the ode_solvers
// builtin still loads in a headless install that has no NFlow.
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Register every SUNDIALS solver backend ode_solvers provides
    // (variableStiff / variableNonstiff / cvodesBdf / cvodesAdams via CVODES,
    // dae / idas via IDAS) through the supplied registrar, which receives each
    // solver name and a factory. `registrar` is typically NFlow's
    // registerSolverBackend. In a build without SUNDIALS this registers nothing
    // (the symbol still exists, so the NFlow side links unconditionally).
    NLSODE_SOLVERS_IMPEXP void
    registerSundialsSolverBackends(
        const std::function<void(const std::string&, SolverBackendFactory)>& registrar);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
