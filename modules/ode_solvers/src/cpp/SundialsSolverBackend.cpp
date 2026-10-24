//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// NFlow calls registerSundialsSolverBackends() at load and passes its own
// registerSolverBackend as the registrar. The dependency points NFlow ->
// ode_solvers only, so the builtin loads in a math-only install (no NFlow, Qt or
// webview).
//=============================================================================
#include "SundialsSolverBackend.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
#ifdef NELSON_WITH_SUNDIALS
    // Defined in CvodesBackend.cpp / IdasBackend.cpp (same library).
    void
    registerCvodesBackends(
        const std::function<void(const std::string&, SolverBackendFactory)>& registrar);
    void
    registerIdasBackends(
        const std::function<void(const std::string&, SolverBackendFactory)>& registrar);
#endif
    //=============================================================================
    void
    registerSundialsSolverBackends(
        const std::function<void(const std::string&, SolverBackendFactory)>& registrar)
    {
#ifdef NELSON_WITH_SUNDIALS
        if (!registrar) {
            return;
        }
        registerCvodesBackends(registrar);
        registerIdasBackends(registrar);
#else
        (void)registrar; // no SUNDIALS: nothing to provide, symbol still exists
#endif
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
