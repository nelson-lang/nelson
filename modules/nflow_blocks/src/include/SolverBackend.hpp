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
#include <vector>
#include <functional>
#include <memory>
#include <string>
#include "SimDiagnostic.hpp" // SimDiagnostic (no engine / json dependency)
#include "nlsNflow_blocks_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Solver-neutral integration interface. The native fixed-step steppers and a
    // CVODES backend both implement ISolverBackend. No SUNDIALS, N_Vector or
    // ArrayOf type appears here.
    //=============================================================================
    struct SolverOptions
    {
        std::string name = "ode1"; // ode1 (Euler), ode4 (RK4), variableStiff, ...
        double relTol = 1e-6;
        std::vector<double> absTol; // size 1 or nx (empty = default)
        double maxStep = 0.0; // 0 = unbounded
        double initStep = 0.0; // 0 = solver default
        double fixedStep = 0.0; // fixed-step size (0 = fall back to maxStep)
        int maxOrder = 0; // 0 = solver default
    };
    //=============================================================================
    struct SolverStats
    {
        long steps = 0;
        long rhsEvals = 0;
        long rootEvals = 0;
        long errTestFails = 0;
        long convFails = 0;
        long stepRejections = 0;
        long jacEvals = 0;
        double lastStepSize = 0.0;
        double tFinal = 0.0;
        std::string solverName;
    };
    //=============================================================================
    // The system to integrate. rhs / roots return false to signal a RECOVERABLE
    // failure (retry with a smaller step); they must never throw across the
    // callback boundary.
    struct OdeProblem
    {
        int nx = 0; // continuous states
        int ng = 0; // zero-crossing surfaces
        std::function<bool(double t, const double* x, double* xdot)> rhs;
        std::function<bool(double t, const double* x, double* g)> roots;
        std::vector<int> rootDirection; // per surface: -1 / 0 / +1 (0 = both)
        std::vector<double> x0; // initial state (size nx)
        // Differential-algebraic form (IDAS backend). When `residual` is set the
        // DAE backend integrates F(t, x, x') = 0 instead of `rhs`. `id` marks each
        // state 1 = differential / 0 = algebraic (empty = all differential);
        // `xp0` is an optional initial-derivative guess (IDACalcIC refines it).
        // Explicit-ODE backends ignore these fields.
        std::function<bool(double t, const double* x, const double* xdot, double* resid)> residual;
        std::vector<double> id;
        std::vector<double> xp0;
    };
    //=============================================================================
    enum class AdvanceStatus
    {
        ReachedStop, // integrated up to the requested stop time
        RootFound, // a zero-crossing was located before the stop time
        Failed // unrecoverable integration failure
    };
    //=============================================================================
    struct AdvanceResult
    {
        AdvanceStatus status = AdvanceStatus::ReachedStop;
        double t = 0.0; // time actually reached (crossing time on RootFound)
        std::vector<int> rootsFound; // surface indices that crossed
        SimDiagnostic diagnostic; // populated on Failed
    };
    //=============================================================================
    class ISolverBackend
    {
    public:
        virtual ~ISolverBackend() = default;
        virtual const char*
        name() const
            = 0;
        virtual bool
        init(const OdeProblem& problem, const SolverOptions& opts, SimDiagnostic& diag)
            = 0;
        // Integrate the in/out state x from the current time to tStop, stopping
        // early at a located root. x is updated in place to the reached time.
        virtual AdvanceResult
        advanceTo(double tStop, double* x)
            = 0;
        // Cold restart at (t, x) after a discontinuity (discards multistep history).
        virtual bool
        reinit(double t, const double* x, SimDiagnostic& diag)
            = 0;
        // Cap integration so the next advanceTo does not step past time t (a
        // scheduled source discontinuity). A variable-step implicit backend would
        // otherwise overshoot the jump and fail its corrector on the discontinuous
        // residual. Default: no-op (explicit backends step exactly to the stop).
        virtual void
        setStopTime(double t)
        {
            (void)t;
        }
        // After init(), copy the backend's consistent initial state into x (for a
        // DAE backend, the IDACalcIC-corrected algebraic components) so the t0
        // sample is not the raw guess. Default: no-op, x is left as is. Returns
        // true if x was written.
        virtual bool
        consistentInitialState(double* x)
        {
            (void)x;
            return false;
        }
        // Dense output at an interior time; false when unsupported.
        virtual bool
        interpolate(double t, double* x)
        {
            (void)t;
            (void)x;
            return false;
        }
        virtual SolverStats
        stats() const
            = 0;
    };
    //=============================================================================
    // Factory for externally provided backends (e.g. CVODES from ode_solvers).
    // Returns nullptr and fills the diagnostic on failure.
    using SolverBackendFactory
        = std::function<std::unique_ptr<ISolverBackend>(const SolverOptions&, SimDiagnostic&)>;
    // Register a backend factory under a solver name. Lets a SUNDIALS-linked
    // module plug in a variable-step backend without nflow_blocks depending on
    // SUNDIALS. Overrides any prior factory for the same name. Thread-safe.
    NLSNFLOW_BLOCKS_IMPEXP void
    registerSolverBackend(const std::string& name, SolverBackendFactory factory);
    // Remove a previously registered backend factory. Dynamic providers must
    // unregister their factories before their library is unloaded. Thread-safe.
    NLSNFLOW_BLOCKS_IMPEXP void
    unregisterSolverBackend(const std::string& name);
    // Factory. Returns a native fixed-step backend for ode1/ode4/discrete;
    // otherwise consults the registered factories (e.g. variableStiff via
    // CVODES); returns nullptr with a BackendUnavailable diagnostic when no
    // backend matches.
    NLSNFLOW_BLOCKS_IMPEXP std::unique_ptr<ISolverBackend>
    makeSolverBackend(const SolverOptions& opts, SimDiagnostic& diag);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
