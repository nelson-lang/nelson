//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Variable-step CVODES backend for the NFlow engine (ISolverBackend over
// OdeCore::CvodesSession). ode_solvers provides it and NFlow pulls it in through
// registerSundialsSolverBackends(), so ode_solvers never links nflow_blocks.
//=============================================================================
#include "SolverBackend.hpp"
#include "OdeCore.hpp"
#ifdef NELSON_WITH_SUNDIALS
#include <functional>
#include <memory>
#include <string>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        constexpr const char* CVODES_SOLVER_NAMES[]
            = { "variableStiff", "variableNonstiff", "cvodesBdf", "cvodesAdams" };
        class CvodesBackend : public ISolverBackend
        {
        public:
            explicit CvodesBackend(std::string name) : name_(std::move(name)) { }

            const char*
            name() const override
            {
                return name_.c_str();
            }

            bool
            init(const OdeProblem& problem, const SolverOptions& opts, SimDiagnostic& diag) override
            {
                OdeCore::CvodesOptions co;
                co.method
                    = (name_ == "variableNonstiff" || name_ == "cvodesAdams") ? "adams" : "bdf";
                co.relTol = opts.relTol;
                co.absTol = opts.absTol;
                co.initStep = opts.initStep;
                co.maxStep = opts.maxStep;
                co.maxOrder = opts.maxOrder;
                co.numRoots = problem.ng;
                co.rootDirection = problem.rootDirection;

                OdeCore::RhsFn rhs = problem.rhs;
                OdeCore::RootFn roots = problem.roots;
                std::string err;
                if (!session_.init(rhs, roots, problem.x0, 0.0, co, err)) {
                    diag.code = "solver.cvodes_init_failed";
                    diag.message = err;
                    return false;
                }
                return true;
            }

            AdvanceResult
            advanceTo(double tStop, double* x) override
            {
                AdvanceResult r;
                OdeCore::CvodesSession::AdvanceOut out = session_.advanceTo(tStop, x);
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
                    r.diagnostic.code = "solver.cvodes_step_failed";
                    r.diagnostic.message = out.message;
                    r.diagnostic.time = out.t;
                    break;
                }
                return r;
            }

            bool
            reinit(double t, const double* x, SimDiagnostic& diag) override
            {
                std::string err;
                if (!session_.reinit(t, x, err)) {
                    diag.code = "solver.cvodes_reinit_failed";
                    diag.message = err;
                    return false;
                }
                return true;
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
            OdeCore::CvodesSession session_;
            std::string name_;
        };
        //=============================================================================
    } // namespace
    //=============================================================================
    void
    registerCvodesBackends(
        const std::function<void(const std::string&, SolverBackendFactory)>& registrar)
    {
        for (const char* solverName : CVODES_SOLVER_NAMES) {
            std::string name = solverName;
            registrar(name,
                [name](const SolverOptions&, SimDiagnostic&) -> std::unique_ptr<ISolverBackend> {
                    return std::unique_ptr<ISolverBackend>(new CvodesBackend(name));
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
