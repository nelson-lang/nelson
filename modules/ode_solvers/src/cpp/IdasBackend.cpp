//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Differential-algebraic (IDAS) backend for the NFlow engine. Implements
// nflow_blocks' ISolverBackend over OdeCore::IdasSession, registered under the
// "dae" / "idas" solver names, so makeSolverBackend() returns a DAE backend
// without nflow_blocks depending on SUNDIALS. It drives OdeProblem::residual
// (F(t,x,x')=0) and OdeProblem::id (differential/algebraic mask) instead of
// the explicit rhs. See SundialsSolverBackend.hpp for how the NFlow side
// pulls it in.
//=============================================================================
#include "SolverBackend.hpp"
#include "OdeCore.hpp"
#ifdef NELSON_WITH_SUNDIALS
#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        constexpr const char* IDAS_SOLVER_NAMES[] = { "dae", "idas" };
        class IdasBackend : public ISolverBackend
        {
        public:
            explicit IdasBackend(std::string name) : name_(std::move(name)) { }

            const char*
            name() const override
            {
                return name_.c_str();
            }

            bool
            init(const OdeProblem& problem, const SolverOptions& opts, SimDiagnostic& diag) override
            {
                if (!problem.residual) {
                    diag.code = "solver.dae_no_residual";
                    diag.message = "the 'dae' solver requires a differential-algebraic residual "
                                   "F(t,x,x')=0; this model has none";
                    return false;
                }
                OdeCore::IdasOptions io;
                io.relTol = opts.relTol;
                io.absTol = opts.absTol;
                io.id = problem.id;
                io.initStep = opts.initStep;
                io.maxStep = opts.maxStep;
                io.numRoots = problem.ng;
                io.rootDirection = problem.rootDirection;
                io.calcIC = !problem.id.empty();

                xp_.assign(static_cast<size_t>(problem.nx > 0 ? problem.nx : 1), 0.0);
                OdeCore::ResidualFn residual = problem.residual;
                OdeCore::RootFn roots = problem.roots;
                std::string err;
                // tFirst only sets the IDACalcIC direction; a small forward step.
                if (!session_.init(residual, roots, problem.x0, problem.xp0, 0.0, 1.0, io, err)) {
                    diag.code = "solver.idas_init_failed";
                    diag.message = err;
                    return false;
                }
                return true;
            }

            AdvanceResult
            advanceTo(double tStop, double* x) override
            {
                AdvanceResult r;
                OdeCore::IdasSession::AdvanceOut out = session_.advanceTo(tStop, x, xp_.data());
                r.t = out.t;
                switch (out.status) {
                case OdeCore::CvodesStatus::Success:
                    r.status = AdvanceStatus::ReachedStop;
                    break;
                case OdeCore::CvodesStatus::RootFound:
                    r.status = AdvanceStatus::RootFound;
                    r.rootsFound = out.rootsFound;
                    break;
                default:
                    r.status = AdvanceStatus::Failed;
                    r.diagnostic.code = "solver.idas_step_failed";
                    r.diagnostic.message = out.message;
                    r.diagnostic.time = out.t;
                    break;
                }
                return r;
            }

            bool
            consistentInitialState(double* x) override
            {
                // res.y holds the IDACalcIC-corrected consistent IC after init;
                // xp_ receives the consistent derivative for later advanceTo.
                return session_.getState(x, xp_.data());
            }

            bool
            reinit(double t, const double* x, SimDiagnostic& diag) override
            {
                std::string err;
                if (!session_.reinit(t, x, xp_.data(), err)) {
                    diag.code = "solver.idas_reinit_failed";
                    diag.message = err;
                    return false;
                }
                return true;
            }

            void
            setStopTime(double t) override
            {
                session_.setStopTime(t);
            }

            SolverStats
            stats() const override
            {
                OdeCore::CvodesStats s = session_.stats();
                SolverStats out;
                out.steps = s.steps;
                out.rhsEvals = s.rhsEvals;
                out.rootEvals = s.rootEvals;
                out.errTestFails = s.errTestFails;
                out.convFails = s.nonlinConvFails;
                out.jacEvals = s.jacEvals;
                out.lastStepSize = s.lastStep;
                out.solverName = name_;
                return out;
            }

        private:
            OdeCore::IdasSession session_;
            std::vector<double> xp_; // internal derivative buffer (IDAS y')
            std::string name_;
        };
        //=============================================================================
    } // namespace
    //=============================================================================
    // Register the IDAS factory for the 'dae' / 'idas' solver names through the
    // supplied NFlow registrar. Called by registerSundialsSolverBackends().
    void
    registerIdasBackends(
        const std::function<void(const std::string&, SolverBackendFactory)>& registrar)
    {
        for (const char* solverName : IDAS_SOLVER_NAMES) {
            std::string name = solverName;
            registrar(name,
                [name](const SolverOptions&, SimDiagnostic&) -> std::unique_ptr<ISolverBackend> {
                    return std::unique_ptr<ISolverBackend>(new IdasBackend(name));
                });
        }
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
#endif
//=============================================================================
