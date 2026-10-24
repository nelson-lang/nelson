//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Regression guard for the ode_solvers <-> NFlow solver-backend layering.
//
// ode_solvers must provide the CVODES / IDAS ISolverBackend implementations
// without depending on nflow_blocks (which would pull Qt / webview into a
// headless math install). The NFlow side passes its registrar to
// registerSundialsSolverBackends(). These tests call it with a local registrar,
// so they fail if the backends stop being reachable or stop integrating
// correctly.
//=============================================================================
#include "doctest.h"
#include "SolverBackend.hpp"
#include "SundialsSolverBackend.hpp"
#include <cmath>
#include <map>
#include <memory>
#include <string>
//=============================================================================
using Nelson::NFlow::AdvanceStatus;
using Nelson::NFlow::ISolverBackend;
using Nelson::NFlow::OdeProblem;
using Nelson::NFlow::registerSundialsSolverBackends;
using Nelson::NFlow::SimDiagnostic;
using Nelson::NFlow::SolverBackendFactory;
using Nelson::NFlow::SolverOptions;
//=============================================================================
namespace {
// Collects what registerSundialsSolverBackends() registers, into a local map,
// without linking nflow_blocks.
std::map<std::string, SolverBackendFactory>
collectBackends()
{
    std::map<std::string, SolverBackendFactory> sink;
    registerSundialsSolverBackends([&sink](const std::string& name, SolverBackendFactory factory) {
        sink[name] = std::move(factory);
    });
    return sink;
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("ode_solvers registers the CVODES and IDAS NFlow backends")
{
    auto sink = collectBackends();
    CHECK(sink.count("variableStiff") == 1);
    CHECK(sink.count("variableNonstiff") == 1);
    CHECK(sink.count("cvodesBdf") == 1);
    CHECK(sink.count("cvodesAdams") == 1);
    CHECK(sink.count("dae") == 1);
    CHECK(sink.count("idas") == 1);
}
//=============================================================================
TEST_CASE("the registered CVODES backend integrates x' = -x to e^-1")
{
    auto sink = collectBackends();
    REQUIRE(sink.count("variableStiff") == 1);

    SolverOptions opts;
    opts.name = "variableStiff";
    opts.relTol = 1e-9;
    opts.absTol = { 1e-12 };
    SimDiagnostic diag;
    std::unique_ptr<ISolverBackend> backend = sink["variableStiff"](opts, diag);
    REQUIRE(backend != nullptr);
    CHECK(std::string(backend->name()) == "variableStiff");

    OdeProblem p;
    p.nx = 1;
    p.x0 = { 1.0 };
    p.rhs = [](double, const double* x, double* xdot) {
        xdot[0] = -x[0];
        return true;
    };
    REQUIRE(backend->init(p, opts, diag));

    std::vector<double> x = p.x0;
    auto res = backend->advanceTo(1.0, x.data());
    CHECK(res.status == AdvanceStatus::ReachedStop);
    CHECK(std::abs(x[0] - std::exp(-1.0)) < 1e-6);
}
//=============================================================================
TEST_CASE("the registered IDAS 'dae' backend requires a residual")
{
    auto sink = collectBackends();
    REQUIRE(sink.count("dae") == 1);

    SolverOptions opts;
    opts.name = "dae";
    SimDiagnostic diag;
    std::unique_ptr<ISolverBackend> backend = sink["dae"](opts, diag);
    REQUIRE(backend != nullptr);
    CHECK(std::string(backend->name()) == "dae");

    // A model with no F(t,x,x')=0 residual must be rejected at init, not crash.
    OdeProblem p;
    p.nx = 1;
    p.x0 = { 0.0 };
    SimDiagnostic initDiag;
    CHECK_FALSE(backend->init(p, opts, initDiag));
    CHECK(initDiag.code == "solver.dae_no_residual");
}
//=============================================================================
