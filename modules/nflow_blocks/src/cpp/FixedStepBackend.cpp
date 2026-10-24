//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SolverBackend.hpp"
#include <cmath>
#include <algorithm>
#include <mutex>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        // Native explicit fixed-step Runge-Kutta backend. Supports ode1 (forward
        // Euler) and ode4 (classical RK4) over the shared OdeProblem RHS, with
        // sign-change zero-crossing detection localised by bisection.
        class FixedStepBackend : public ISolverBackend
        {
        public:
            const char*
            name() const override
            {
                return opts_.name.c_str();
            }

            bool
            init(const OdeProblem& problem, const SolverOptions& opts, SimDiagnostic& diag) override
            {
                problem_ = problem;
                opts_ = opts;
                order_ = (opts.name == "ode4") ? 4 : 1;
                h_ = (opts.fixedStep > 0.0) ? opts.fixedStep : opts.maxStep;
                if (h_ <= 0.0) {
                    diag.code = "solver.no_step";
                    diag.message = "fixed-step solver requires a positive step size";
                    return false;
                }
                t_ = 0.0;
                x_ = problem.x0;
                x_.resize(problem_.nx, 0.0);
                stats_ = SolverStats {};
                stats_.solverName = opts.name;
                stats_.lastStepSize = h_;
                return true;
            }

            AdvanceResult
            advanceTo(double tStop, double* x) override
            {
                for (int i = 0; i < problem_.nx; ++i) {
                    x_[i] = x[i];
                }
                AdvanceResult res;

                std::vector<double> gPrev, gNew;
                const bool haveRoots = (problem_.ng > 0 && problem_.roots);
                if (haveRoots) {
                    gPrev.assign(problem_.ng, 0.0);
                    gNew.assign(problem_.ng, 0.0);
                    ++stats_.rootEvals;
                    problem_.roots(t_, x_.data(), gPrev.data());
                }

                const double eps = 1e-12;
                while (t_ < tStop - eps) {
                    const double h = std::min(h_, tStop - t_);
                    std::vector<double> xStart = x_;
                    if (!step(h)) {
                        res.status = AdvanceStatus::Failed;
                        res.t = t_;
                        res.diagnostic.code = "solver.rhs_failure";
                        res.diagnostic.message = "RHS reported an unrecoverable failure";
                        res.diagnostic.time = t_;
                        writeOut(x);
                        return res;
                    }
                    const double tEnd = t_ + h;
                    ++stats_.steps;

                    if (haveRoots) {
                        ++stats_.rootEvals;
                        problem_.roots(tEnd, x_.data(), gNew.data());
                        int crossed = firstCrossing(gPrev, gNew);
                        if (crossed >= 0) {
                            const double tc = localizeRoot(xStart, t_, tEnd, crossed);
                            res.status = AdvanceStatus::RootFound;
                            res.t = tc;
                            res.rootsFound.push_back(crossed);
                            t_ = tc;
                            stats_.tFinal = t_;
                            writeOut(x);
                            return res;
                        }
                        gPrev = gNew;
                    }
                    t_ = tEnd;
                }

                res.status = AdvanceStatus::ReachedStop;
                res.t = t_;
                stats_.tFinal = t_;
                writeOut(x);
                return res;
            }

            bool
            reinit(double t, const double* x, SimDiagnostic&) override
            {
                t_ = t;
                for (int i = 0; i < problem_.nx; ++i) {
                    x_[i] = x[i];
                }
                return true;
            }

            SolverStats
            stats() const override
            {
                return stats_;
            }

        private:
            void
            writeOut(double* x)
            {
                for (int i = 0; i < problem_.nx; ++i) {
                    x[i] = x_[i];
                }
            }

            // Advance x_ by one step of size h from t_ (does not modify t_).
            bool
            step(double h)
            {
                const int n = problem_.nx;
                std::vector<double> k1(n, 0.0);
                ++stats_.rhsEvals;
                if (!problem_.rhs(t_, x_.data(), k1.data())) {
                    return false;
                }
                if (order_ == 1) {
                    for (int i = 0; i < n; ++i) {
                        x_[i] += h * k1[i];
                    }
                    return true;
                }
                std::vector<double> tmp(n, 0.0), k2(n, 0.0), k3(n, 0.0), k4(n, 0.0);
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x_[i] + 0.5 * h * k1[i];
                }
                ++stats_.rhsEvals;
                if (!problem_.rhs(t_ + 0.5 * h, tmp.data(), k2.data())) {
                    return false;
                }
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x_[i] + 0.5 * h * k2[i];
                }
                ++stats_.rhsEvals;
                if (!problem_.rhs(t_ + 0.5 * h, tmp.data(), k3.data())) {
                    return false;
                }
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x_[i] + h * k3[i];
                }
                ++stats_.rhsEvals;
                if (!problem_.rhs(t_ + h, tmp.data(), k4.data())) {
                    return false;
                }
                for (int i = 0; i < n; ++i) {
                    x_[i] += (h / 6.0) * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]);
                }
                return true;
            }

            // Index of the first surface whose sign changes (honouring
            // rootDirection), or -1.
            int
            firstCrossing(const std::vector<double>& gPrev, const std::vector<double>& gNew) const
            {
                for (int i = 0; i < problem_.ng; ++i) {
                    if (gPrev[i] == 0.0 || gNew[i] == 0.0) {
                        // Treat an exact zero at the new point as a crossing.
                        if (gNew[i] == 0.0 && gPrev[i] != 0.0) {
                            return i;
                        }
                        continue;
                    }
                    if (gPrev[i] * gNew[i] < 0.0) {
                        const int dir = (gNew[i] > gPrev[i]) ? 1 : -1;
                        const int want = (i < (int)problem_.rootDirection.size())
                            ? problem_.rootDirection[i]
                            : 0;
                        if (want == 0 || want == dir) {
                            return i;
                        }
                    }
                }
                return -1;
            }

            // Bisection on the crossing surface within [tA, tB], integrating a
            // trial step from xStart (state at tA) to the mid time.
            double
            localizeRoot(const std::vector<double>& xStart, double tA, double tB, int surface)
            {
                std::vector<double> g(problem_.ng, 0.0);
                problem_.roots(tA, xStart.data(), g.data());
                const double gA = g[surface];
                double lo = tA, hi = tB;
                std::vector<double> xMid;
                for (int iter = 0; iter < 60 && (hi - lo) > 1e-12; ++iter) {
                    const double mid = 0.5 * (lo + hi);
                    xMid = xStart;
                    integrateFixedTo(xMid, tA, mid);
                    problem_.roots(mid, xMid.data(), g.data());
                    const double gm = g[surface];
                    if ((gA <= 0.0 && gm <= 0.0) || (gA > 0.0 && gm > 0.0)) {
                        lo = mid;
                    } else {
                        hi = mid;
                    }
                    // Leave x_ at the located crossing state.
                    x_ = xMid;
                }
                return 0.5 * (lo + hi);
            }

            // Integrate a copy of the state from tFrom to tTo using the base
            // order and one sub-step (used only inside root localisation).
            void
            integrateFixedTo(std::vector<double>& x, double tFrom, double tTo)
            {
                const int n = problem_.nx;
                const double h = tTo - tFrom;
                if (h <= 0.0) {
                    return;
                }
                std::vector<double> k1(n, 0.0);
                problem_.rhs(tFrom, x.data(), k1.data());
                if (order_ == 1) {
                    for (int i = 0; i < n; ++i) {
                        x[i] += h * k1[i];
                    }
                    return;
                }
                std::vector<double> tmp(n, 0.0), k2(n, 0.0), k3(n, 0.0), k4(n, 0.0);
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x[i] + 0.5 * h * k1[i];
                }
                problem_.rhs(tFrom + 0.5 * h, tmp.data(), k2.data());
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x[i] + 0.5 * h * k2[i];
                }
                problem_.rhs(tFrom + 0.5 * h, tmp.data(), k3.data());
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x[i] + h * k3[i];
                }
                problem_.rhs(tTo, tmp.data(), k4.data());
                for (int i = 0; i < n; ++i) {
                    x[i] += (h / 6.0) * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]);
                }
            }

            OdeProblem problem_;
            SolverOptions opts_;
            SolverStats stats_;
            std::vector<double> x_;
            double t_ = 0.0;
            double h_ = 0.0;
            int order_ = 1;
        };
        //=============================================================================
        // Native explicit adaptive backend: Dormand-Prince 5(4) over the shared
        // OdeProblem RHS. The embedded 4th-order error estimate drives step-size
        // control (no PI controller); each communication step lands exactly on
        // tStop. Zero-crossings are localised by bisection with a fixed-RK4 trial
        // integration, as in the fixed-step backend. Registered under "ode45".
        class DormandPrinceBackend : public ISolverBackend
        {
        public:
            const char*
            name() const override
            {
                return "ode45";
            }
            bool
            init(const OdeProblem& problem, const SolverOptions& opts, SimDiagnostic& diag) override
            {
                (void)diag;
                problem_ = problem;
                opts_ = opts;
                t_ = 0.0;
                x_ = problem.x0;
                x_.resize(problem_.nx, 0.0);
                rtol_ = (opts.relTol > 0.0) ? opts.relTol : 1e-6;
                atol_ = (!opts.absTol.empty() && opts.absTol[0] > 0.0) ? opts.absTol[0] : 1e-8;
                hMax_ = (opts.maxStep > 0.0) ? opts.maxStep : 0.0;
                // Initial step: the communication step if known, else a small
                // default; adapted after the first accepted step.
                h_ = (opts.fixedStep > 0.0) ? opts.fixedStep : (hMax_ > 0.0 ? hMax_ : 1e-2);
                stats_ = SolverStats {};
                stats_.solverName = "ode45";
                stats_.lastStepSize = h_;
                return true;
            }
            AdvanceResult
            advanceTo(double tStop, double* x) override
            {
                for (int i = 0; i < problem_.nx; ++i) {
                    x_[i] = x[i];
                }
                AdvanceResult res;
                const int n = problem_.nx;
                const bool haveRoots = (problem_.ng > 0 && problem_.roots);
                std::vector<double> gPrev, gNew;
                if (haveRoots) {
                    gPrev.assign(problem_.ng, 0.0);
                    gNew.assign(problem_.ng, 0.0);
                    ++stats_.rootEvals;
                    problem_.roots(t_, x_.data(), gPrev.data());
                }
                const double eps = 1e-12;
                std::vector<double> xNew(n, 0.0), err(n, 0.0);
                while (t_ < tStop - eps) {
                    double h = h_;
                    if (hMax_ > 0.0 && h > hMax_) {
                        h = hMax_;
                    }
                    if (h > tStop - t_) {
                        h = tStop - t_; // land exactly on the communication point
                    }
                    double errNorm = 0.0;
                    bool ok = false;
                    for (int attempt = 0; attempt < 50; ++attempt) {
                        if (!dp45Step(h, xNew, err)) {
                            res.status = AdvanceStatus::Failed;
                            res.t = t_;
                            res.diagnostic.code = "solver.rhs_failure";
                            res.diagnostic.message = "RHS reported an unrecoverable failure";
                            res.diagnostic.time = t_;
                            writeOut(x);
                            return res;
                        }
                        errNorm = errorNorm(xNew, err);
                        if (errNorm <= 1.0 || h <= 1e-13) {
                            ok = true;
                            break;
                        }
                        double fac = 0.9 * std::pow(errNorm, -0.2);
                        fac = std::max(0.2, std::min(1.0, fac));
                        h *= fac;
                        if (h < tStop - t_ && hMax_ > 0.0 && h > hMax_) {
                            h = hMax_;
                        }
                    }
                    (void)ok;
                    const double tEnd = t_ + h;
                    std::vector<double> xStart = x_;
                    x_ = xNew;
                    ++stats_.steps;
                    if (haveRoots) {
                        ++stats_.rootEvals;
                        problem_.roots(tEnd, x_.data(), gNew.data());
                        int crossed = firstCrossing(gPrev, gNew);
                        if (crossed >= 0) {
                            const double tRoot = localizeRoot(xStart, t_, tEnd, crossed);
                            res.status = AdvanceStatus::RootFound;
                            res.t = tRoot;
                            res.rootsFound.push_back(crossed);
                            t_ = tRoot;
                            stats_.tFinal = t_;
                            stats_.lastStepSize = h;
                            writeOut(x);
                            return res;
                        }
                        gPrev = gNew;
                    }
                    t_ = tEnd;
                    double grow = 0.9 * std::pow(std::max(errNorm, 1e-10), -0.2);
                    grow = std::max(0.2, std::min(5.0, grow));
                    h_ = h * grow;
                    stats_.lastStepSize = h;
                }
                writeOut(x);
                res.status = AdvanceStatus::ReachedStop;
                res.t = t_;
                stats_.tFinal = t_;
                return res;
            }
            bool
            reinit(double t, const double* x, SimDiagnostic&) override
            {
                t_ = t;
                for (int i = 0; i < problem_.nx; ++i) {
                    x_[i] = x[i];
                }
                return true;
            }
            SolverStats
            stats() const override
            {
                return stats_;
            }

        private:
            void
            writeOut(double* x)
            {
                for (int i = 0; i < problem_.nx; ++i) {
                    x[i] = x_[i];
                }
            }
            // One Dormand-Prince 5(4) step of size h from (t_, x_): fills xNew with
            // the 5th-order solution and err with the (5th-4th) error estimate.
            bool
            dp45Step(double h, std::vector<double>& xNew, std::vector<double>& err)
            {
                const int n = problem_.nx;
                std::vector<double> k1(n), k2(n), k3(n), k4(n), k5(n), k6(n), k7(n), tmp(n);
                auto rhs = [&](double tt, const std::vector<double>& xx,
                               std::vector<double>& kk) -> bool {
                    ++stats_.rhsEvals;
                    return problem_.rhs(tt, xx.data(), kk.data());
                };
                if (!rhs(t_, x_, k1)) {
                    return false;
                }
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x_[i] + h * (0.2 * k1[i]);
                }
                if (!rhs(t_ + 0.2 * h, tmp, k2)) {
                    return false;
                }
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x_[i] + h * (3.0 / 40 * k1[i] + 9.0 / 40 * k2[i]);
                }
                if (!rhs(t_ + 0.3 * h, tmp, k3)) {
                    return false;
                }
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x_[i] + h * (44.0 / 45 * k1[i] - 56.0 / 15 * k2[i] + 32.0 / 9 * k3[i]);
                }
                if (!rhs(t_ + 0.8 * h, tmp, k4)) {
                    return false;
                }
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x_[i]
                        + h
                            * (19372.0 / 6561 * k1[i] - 25360.0 / 2187 * k2[i]
                                + 64448.0 / 6561 * k3[i] - 212.0 / 729 * k4[i]);
                }
                if (!rhs(t_ + 8.0 / 9 * h, tmp, k5)) {
                    return false;
                }
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x_[i]
                        + h
                            * (9017.0 / 3168 * k1[i] - 355.0 / 33 * k2[i] + 46732.0 / 5247 * k3[i]
                                + 49.0 / 176 * k4[i] - 5103.0 / 18656 * k5[i]);
                }
                if (!rhs(t_ + h, tmp, k6)) {
                    return false;
                }
                // 5th-order solution (b5 = the k7 row, FSAL).
                for (int i = 0; i < n; ++i) {
                    xNew[i] = x_[i]
                        + h
                            * (35.0 / 384 * k1[i] + 500.0 / 1113 * k3[i] + 125.0 / 192 * k4[i]
                                - 2187.0 / 6784 * k5[i] + 11.0 / 84 * k6[i]);
                }
                if (!rhs(t_ + h, xNew, k7)) {
                    return false;
                }
                // Error = 5th-order - 4th-order solution (coefficient differences).
                for (int i = 0; i < n; ++i) {
                    err[i] = h
                        * ((35.0 / 384 - 5179.0 / 57600) * k1[i]
                            + (500.0 / 1113 - 7571.0 / 16695) * k3[i]
                            + (125.0 / 192 - 393.0 / 640) * k4[i]
                            + (-2187.0 / 6784 - (-92097.0 / 339200)) * k5[i]
                            + (11.0 / 84 - 187.0 / 2100) * k6[i] + (0.0 - 1.0 / 40) * k7[i]);
                }
                return true;
            }
            double
            errorNorm(const std::vector<double>& xNew, const std::vector<double>& err) const
            {
                const int n = problem_.nx;
                if (n == 0) {
                    return 0.0;
                }
                double acc = 0.0;
                for (int i = 0; i < n; ++i) {
                    const double sc = atol_ + rtol_ * std::max(std::abs(x_[i]), std::abs(xNew[i]));
                    const double r = (sc > 0.0) ? err[i] / sc : 0.0;
                    acc += r * r;
                }
                return std::sqrt(acc / n);
            }
            int
            firstCrossing(const std::vector<double>& gPrev, const std::vector<double>& gNew) const
            {
                for (int i = 0; i < problem_.ng; ++i) {
                    if (gPrev[i] == 0.0 || gNew[i] == 0.0) {
                        if (gNew[i] == 0.0 && gPrev[i] != 0.0) {
                            return i;
                        }
                        continue;
                    }
                    if (gPrev[i] * gNew[i] < 0.0) {
                        const int dir = (gNew[i] > gPrev[i]) ? 1 : -1;
                        const int want = (i < (int)problem_.rootDirection.size())
                            ? problem_.rootDirection[i]
                            : 0;
                        if (want == 0 || want == dir) {
                            return i;
                        }
                    }
                }
                return -1;
            }
            // Fixed-RK4 trial integration for root bisection (localisation does not
            // need adaptivity), mirroring the fixed-step backend.
            void
            integrateFixedTo(std::vector<double>& x, double tFrom, double tTo)
            {
                const int n = problem_.nx;
                const double h = tTo - tFrom;
                if (h <= 0.0) {
                    return;
                }
                std::vector<double> k1(n), k2(n), k3(n), k4(n), tmp(n);
                problem_.rhs(tFrom, x.data(), k1.data());
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x[i] + 0.5 * h * k1[i];
                }
                problem_.rhs(tFrom + 0.5 * h, tmp.data(), k2.data());
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x[i] + 0.5 * h * k2[i];
                }
                problem_.rhs(tFrom + 0.5 * h, tmp.data(), k3.data());
                for (int i = 0; i < n; ++i) {
                    tmp[i] = x[i] + h * k3[i];
                }
                problem_.rhs(tTo, tmp.data(), k4.data());
                for (int i = 0; i < n; ++i) {
                    x[i] += (h / 6.0) * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]);
                }
            }
            double
            localizeRoot(const std::vector<double>& xStart, double tA, double tB, int surface)
            {
                std::vector<double> g(problem_.ng, 0.0);
                problem_.roots(tA, xStart.data(), g.data());
                const double gA = g[surface];
                double lo = tA, hi = tB;
                std::vector<double> xMid;
                for (int iter = 0; iter < 60 && (hi - lo) > 1e-12; ++iter) {
                    const double mid = 0.5 * (lo + hi);
                    xMid = xStart;
                    integrateFixedTo(xMid, tA, mid);
                    problem_.roots(mid, xMid.data(), g.data());
                    const double gm = g[surface];
                    if ((gA <= 0.0 && gm <= 0.0) || (gA > 0.0 && gm > 0.0)) {
                        lo = mid;
                    } else {
                        hi = mid;
                    }
                    x_ = xMid;
                }
                return 0.5 * (lo + hi);
            }
            OdeProblem problem_;
            SolverOptions opts_;
            SolverStats stats_;
            std::vector<double> x_;
            double t_ = 0.0;
            double h_ = 0.0;
            double rtol_ = 1e-6;
            double atol_ = 1e-8;
            double hMax_ = 0.0;
        };
        //=============================================================================
    } // namespace
    //=============================================================================
    namespace {
        // Registry of externally-provided backend factories (name -> factory),
        // guarded for concurrent registration/lookup.
        std::mutex&
        registryMutex()
        {
            static std::mutex m;
            return m;
        }
        //=============================================================================
        std::unordered_map<std::string, SolverBackendFactory>&
        backendRegistry()
        {
            static std::unordered_map<std::string, SolverBackendFactory> registry;
            return registry;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    void
    registerSolverBackend(const std::string& name, SolverBackendFactory factory)
    {
        std::lock_guard<std::mutex> lock(registryMutex());
        backendRegistry()[name] = std::move(factory);
    }
    //=============================================================================
    void
    unregisterSolverBackend(const std::string& name)
    {
        std::lock_guard<std::mutex> lock(registryMutex());
        backendRegistry().erase(name);
    }
    //=============================================================================
    std::unique_ptr<ISolverBackend>
    makeSolverBackend(const SolverOptions& opts, SimDiagnostic& diag)
    {
        if (opts.name == "ode1" || opts.name == "ode4" || opts.name == "discrete") {
            return std::make_unique<FixedStepBackend>();
        }
        if (opts.name == "ode45") {
            return std::make_unique<DormandPrinceBackend>();
        }
        // Externally registered backends (e.g. CVODES from ode_solvers).
        SolverBackendFactory factory;
        {
            std::lock_guard<std::mutex> lock(registryMutex());
            auto it = backendRegistry().find(opts.name);
            if (it != backendRegistry().end()) {
                factory = it->second;
            }
        }
        if (factory) {
            return factory(opts, diag);
        }
        diag.code = "solver.backend_unavailable";
        diag.message = "solver '" + opts.name + "' is not available in this build";
        return nullptr;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
