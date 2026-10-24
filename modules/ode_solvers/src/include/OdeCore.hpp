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
// Native-callback CVODES core (interpreter-free): plain std::function
// callbacks over double buffers, no Evaluator, no ArrayOf, no SUNDIALS type in
// this surface. The implementation is guarded by NELSON_WITH_SUNDIALS.
//=============================================================================
#include "nlsOde_solvers_exports.h"
#include <functional>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace OdeCore {
    //=============================================================================
    using RhsFn = std::function<bool(double t, const double* y, double* ydot)>;
    using RootFn = std::function<bool(double t, const double* y, double* gout)>;
    // Differential-algebraic residual F(t, y, y') = r. Returns false on a
    // recoverable failure (retry with a smaller step). Used by IdasSession.
    using ResidualFn = std::function<bool(double t, const double* y, const double* yp, double* r)>;
    //=============================================================================
    struct CvodesOptions
    {
        std::string method = "bdf"; // "bdf" (stiff, Newton) or "adams" (nonstiff)
        double relTol = 1e-6;
        std::vector<double> absTol; // size 1 (scalar) or n (per-state); empty = 1e-8
        double initStep = 0.0;
        double maxStep = 0.0;
        double minStep = 0.0;
        int maxOrder = 0;
        long maxNumSteps = 0;
        int numRoots = 0;
        std::vector<int> rootDirection;
    };
    //=============================================================================
    enum class CvodesStatus
    {
        Success,
        RootFound,
        TooMuchWork,
        TolTooSmall,
        RhsFailure,
        LinearSolverFailure,
        Error
    };
    //=============================================================================
    struct CvodesStats
    {
        long steps = 0;
        long rhsEvals = 0;
        long rootEvals = 0;
        long errTestFails = 0;
        long nonlinConvFails = 0;
        long jacEvals = 0;
        double lastStep = 0.0;
    };
    //=============================================================================
    struct CvodesResult
    {
        CvodesStatus status = CvodesStatus::Success;
        std::vector<double> t;
        std::vector<double> y; // row-major, t.size() rows x n columns
        std::vector<int> rootsFound;
        CvodesStats stats;
        std::string message;
    };
    //=============================================================================
    NLSODE_SOLVERS_IMPEXP CvodesResult
    runCvodes(const RhsFn& rhs, const RootFn& roots, const std::vector<double>& y0,
        const std::vector<double>& tspan, const CvodesOptions& opts);
    //=============================================================================
    // Stateful CVODES session mapping one-to-one onto NFlow's ISolverBackend:
    // init / advanceTo (stops at roots) / reinit (cold restart) / stats.
    // SUNDIALS types stay hidden behind a pimpl.
    class NLSODE_SOLVERS_IMPEXP CvodesSession
    {
    public:
        CvodesSession();
        ~CvodesSession();
        CvodesSession(const CvodesSession&) = delete;
        CvodesSession&
        operator=(const CvodesSession&)
            = delete;

        bool
        init(RhsFn rhs, RootFn roots, const std::vector<double>& y0, double t0,
            const CvodesOptions& opts, std::string& err);

        struct AdvanceOut
        {
            CvodesStatus status = CvodesStatus::Success;
            double t = 0.0;
            std::vector<int> rootsFound;
            std::string message;
        };

        AdvanceOut
        advanceTo(double tStop, double* x);

        bool
        reinit(double t, const double* x, std::string& err);

        CvodesStats
        stats() const;

    private:
        struct Impl;
        Impl* impl_ { nullptr };
    };
    //=============================================================================
    // Options for a stateful IDAS (differential-algebraic) session. `id` marks
    // each state 1.0 = differential / 0.0 = algebraic (empty = all differential);
    // it drives IDASetId + IDACalcIC for consistent initial conditions.
    struct IdasOptions
    {
        double relTol = 1e-6;
        std::vector<double> absTol; // size 1 (scalar) or n (per-state); empty = 1e-8
        std::vector<double> id; // per-state 1.0 differential / 0.0 algebraic
        double initStep = 0.0;
        double maxStep = 0.0;
        long maxNumSteps = 0;
        int numRoots = 0;
        std::vector<int> rootDirection;
        bool calcIC = true; // IDACalcIC(IDA_YA_YDP_INIT) for consistent y_alg / y'_diff
    };
    //=============================================================================
    // Stateful IDAS session, the differential-algebraic analogue of
    // CvodesSession. init() takes the residual form F(t,y,y')=0 and computes
    // consistent initial conditions. SUNDIALS types stay hidden behind a pimpl.
    class NLSODE_SOLVERS_IMPEXP IdasSession
    {
    public:
        IdasSession();
        ~IdasSession();
        IdasSession(const IdasSession&) = delete;
        IdasSession&
        operator=(const IdasSession&)
            = delete;

        // y0 / yp0 are the initial state and its derivative (yp0 may be a guess;
        // IDACalcIC refines it when opts.calcIC and opts.id are set). tFirst is a
        // point in the integration direction used by IDACalcIC.
        bool
        init(ResidualFn residual, RootFn roots, const std::vector<double>& y0,
            const std::vector<double>& yp0, double t0, double tFirst, const IdasOptions& opts,
            std::string& err);

        struct AdvanceOut
        {
            CvodesStatus status = CvodesStatus::Success;
            double t = 0.0;
            std::vector<int> rootsFound;
            std::string message;
        };

        // Integrate to tStop (or the first root in [t, tStop]); writes the state
        // to y and its derivative to yp (yp may be null).
        AdvanceOut
        advanceTo(double tStop, double* y, double* yp);

        // Copy the current state into `y` and its derivative into `yp` (when
        // non-null). After init() this is the IDACalcIC-corrected consistent
        // initial condition, so the caller can record a correct t0 sample
        // instead of the raw guess. Returns false if the session is not
        // initialised.
        bool
        getState(double* y, double* yp) const;

        bool
        reinit(double t, const double* y, const double* yp, std::string& err);

        // Cap integration so the next advanceTo stops at `t` (IDASetStopTime) and
        // its multistep stencil never evaluates the residual past a scheduled
        // source discontinuity. Reset before every advance that needs it.
        void
        setStopTime(double t);

        CvodesStats
        stats() const;

    private:
        struct Impl;
        Impl* impl_ { nullptr };
    };
    //=============================================================================
} // namespace OdeCore
//=============================================================================
} // namespace Nelson
//=============================================================================
