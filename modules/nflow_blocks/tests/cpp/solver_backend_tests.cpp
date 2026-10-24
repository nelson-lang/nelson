//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "test_helpers.hpp"
#include "SolverBackend.hpp"
#include <cmath>
//=============================================================================
using Nelson::NFlow::AdvanceStatus;
using Nelson::NFlow::ISolverBackend;
using Nelson::NFlow::json;
using Nelson::NFlow::makeSolverBackend;
using Nelson::NFlow::OdeProblem;
using Nelson::NFlow::Phase;
using Nelson::NFlow::SimDiagnostic;
using Nelson::NFlow::SolverOptions;
TEST_CASE("ode4 integrates x' = -x within tolerance")
{
    SolverOptions opts;
    opts.name = "ode4";
    opts.fixedStep = 0.01;
    SimDiagnostic diag;
    auto solver = makeSolverBackend(opts, diag);
    REQUIRE(solver != nullptr);

    OdeProblem p;
    p.nx = 1;
    p.x0 = { 1.0 };
    p.rhs = [](double, const double* x, double* xdot) {
        xdot[0] = -x[0];
        return true;
    };
    REQUIRE(solver->init(p, opts, diag));

    std::vector<double> x = p.x0;
    auto res = solver->advanceTo(1.0, x.data());
    CHECK(res.status == AdvanceStatus::ReachedStop);
    // exact solution e^{-1}; RK4 at h=0.01 is accurate to well under 1e-6
    nflow_tests::checkClose(x[0], std::exp(-1.0), 1e-6);
}
//=============================================================================
TEST_CASE("ode4 integrates a harmonic oscillator")
{
    // x'' = -x  =>  x0' = x1, x1' = -x0 ; start (1, 0) => x0(t) = cos t
    SolverOptions opts;
    opts.name = "ode4";
    opts.fixedStep = 0.005;
    SimDiagnostic diag;
    auto solver = makeSolverBackend(opts, diag);
    REQUIRE(solver != nullptr);

    OdeProblem p;
    p.nx = 2;
    p.x0 = { 1.0, 0.0 };
    p.rhs = [](double, const double* x, double* xdot) {
        xdot[0] = x[1];
        xdot[1] = -x[0];
        return true;
    };
    REQUIRE(solver->init(p, opts, diag));

    std::vector<double> x = p.x0;
    auto res = solver->advanceTo(M_PI, x.data());
    CHECK(res.status == AdvanceStatus::ReachedStop);
    nflow_tests::checkClose(x[0], std::cos(M_PI), 1e-6); // = -1
    nflow_tests::checkClose(x[1], -std::sin(M_PI), 1e-6); // ~ 0
}
//=============================================================================
TEST_CASE("ode1 locates a zero-crossing")
{
    // x' = 1, start 0, surface g = x - 0.5 crosses at t = 0.5
    SolverOptions opts;
    opts.name = "ode1";
    opts.fixedStep = 0.1;
    SimDiagnostic diag;
    auto solver = makeSolverBackend(opts, diag);
    REQUIRE(solver != nullptr);

    OdeProblem p;
    p.nx = 1;
    p.ng = 1;
    p.x0 = { 0.0 };
    p.rhs = [](double, const double*, double* xdot) {
        xdot[0] = 1.0;
        return true;
    };
    p.roots = [](double, const double* x, double* g) {
        g[0] = x[0] - 0.5;
        return true;
    };
    REQUIRE(solver->init(p, opts, diag));

    std::vector<double> x = p.x0;
    auto res = solver->advanceTo(1.0, x.data());
    CHECK(res.status == AdvanceStatus::RootFound);
    REQUIRE(res.rootsFound.size() == 1);
    CHECK(res.rootsFound[0] == 0);
    nflow_tests::checkClose(res.t, 0.5, 1e-6);
    nflow_tests::checkClose(x[0], 0.5, 1e-6);
}
//=============================================================================
TEST_CASE("A2.2: a limited integrator stops the solver exactly at the saturation onset")
{
    // Drive the real integrator handler through a fixed-step backend, wiring its
    // DERIVATIVE and ZERO_CROSSING phases into the OdeProblem exactly the way the
    // engine does. Integrator bounded above at 1.0, input u = 1 => x grows at
    // rate 1 from 0 and must engage the limit at t = 1 with NO overshoot.
    nflow_tests::ensureHandlersRegistered();
    nflow_tests::Harness h;
    json p;
    p[nflow::kInitial] = 0.0;
    p[nflow::kMax] = 1.0; // lower limit left at -inf
    Nelson::NFlow::BlockId integ = h.addBlock("i", "integrator", 1, 1, p);
    h.connect(h.addInput(1.0), integ);
    h.run(integ, Phase::INIT);
    h.blockState[integ].xOffset = 0;
    h.blockState[integ].xLength = 1;
    h.blockState[integ].gOffset = 0;
    h.blockState[integ].gLength = 2; // sentinel (min=-inf) + live (max)

    OdeProblem prob;
    prob.nx = 1;
    prob.ng = 2;
    prob.x0 = { 0.0 };
    prob.rhs = [&](double t, const double* x, double* xdot) {
        h.t = t;
        h.x.assign(x, x + 1);
        h.xdot.assign(1, 0.0);
        h.run(integ, Phase::DERIVATIVE);
        xdot[0] = h.xdot[0];
        return true;
    };
    prob.roots = [&](double t, const double* x, double* g) {
        h.t = t;
        h.x.assign(x, x + 1);
        h.g.assign(2, 0.0);
        h.run(integ, Phase::ZERO_CROSSING);
        g[0] = h.g[0];
        g[1] = h.g[1];
        return true;
    };

    SolverOptions opts;
    opts.name = "ode1";
    opts.fixedStep = 0.1;
    SimDiagnostic diag;
    auto solver = makeSolverBackend(opts, diag);
    REQUIRE(solver != nullptr);
    REQUIRE(solver->init(prob, opts, diag));

    std::vector<double> x = prob.x0;
    auto res = solver->advanceTo(2.0, x.data());
    CHECK(res.status == AdvanceStatus::RootFound);
    REQUIRE(res.rootsFound.size() >= 1);
    // The live surface is index 1 (x - max); the min sentinel (index 0) never crosses.
    CHECK(res.rootsFound[0] == 1);
    nflow_tests::checkClose(res.t, 1.0, 1e-6);
    nflow_tests::checkClose(x[0], 1.0, 1e-6); // state stops on the limit, no overshoot
}
//=============================================================================
TEST_CASE("variable-step solver name is unavailable in this build")
{
    SolverOptions opts;
    opts.name = "variableStiff";
    SimDiagnostic diag;
    auto solver = makeSolverBackend(opts, diag);
    CHECK(solver == nullptr);
    CHECK(diag.code == "solver.backend_unavailable");
}
//=============================================================================
namespace {
struct DummyBackend : Nelson::NFlow::ISolverBackend
{
    const char*
    name() const override
    {
        return "dummy";
    }
    bool
    init(const Nelson::NFlow::OdeProblem&, const Nelson::NFlow::SolverOptions&,
        Nelson::NFlow::SimDiagnostic&) override
    {
        return true;
    }
    Nelson::NFlow::AdvanceResult
    advanceTo(double tStop, double*) override
    {
        Nelson::NFlow::AdvanceResult r;
        r.t = tStop;
        return r;
    }
    bool
    reinit(double, const double*, Nelson::NFlow::SimDiagnostic&) override
    {
        return true;
    }
    Nelson::NFlow::SolverStats
    stats() const override
    {
        return {};
    }
};
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("a backend factory can be registered and unregistered")
{
    using Nelson::NFlow::registerSolverBackend;
    using Nelson::NFlow::unregisterSolverBackend;
    SolverOptions opts;
    opts.name = "myCustomSolver";
    SimDiagnostic diag;

    CHECK(makeSolverBackend(opts, diag) == nullptr);

    registerSolverBackend("myCustomSolver",
        [](const SolverOptions&, SimDiagnostic&) -> std::unique_ptr<Nelson::NFlow::ISolverBackend> {
            return std::unique_ptr<Nelson::NFlow::ISolverBackend>(new DummyBackend());
        });

    auto backend = makeSolverBackend(opts, diag);
    CHECK(backend != nullptr);
    if (backend) {
        CHECK(std::string(backend->name()) == "dummy");
    }

    backend.reset();
    unregisterSolverBackend("myCustomSolver");
    CHECK(makeSolverBackend(opts, diag) == nullptr);
    CHECK(diag.code == "solver.backend_unavailable");
}
//=============================================================================
