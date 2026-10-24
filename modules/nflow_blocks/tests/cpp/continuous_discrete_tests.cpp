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
#include "NFlowCodegenHelpers.hpp"
#include <cmath>
#include <cstdlib>
#include <limits>
#include <string>
#include <vector>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::Phase;
static Harness
statefulWithInput(const std::string& type, json params, double input)
{
    Harness h;
    auto b = h.addBlock("b", type, 1, 1, std::move(params));
    h.connect(h.addInput(input), b);
    return h;
}
//=============================================================================
TEST_CASE("continuous handlers update state and outputs")
{
    {
        auto h = statefulWithInput("integrator",
            { { nflow::kInitial, 2.0 }, { nflow::kMin, -10.0 }, { nflow::kMax, 10.0 } }, 3.0);
        h.dt = 0.5;
        h.run(0, Phase::INIT);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 3.5);
    }
    {
        auto h = statefulWithInput("tf",
            { { nflow::kNum, json::array({ 2.0 }) }, { nflow::kDen, json::array({ 1.0 }) } }, 3.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 6.0);
    }
    {
        auto h = statefulWithInput("delay", { { nflow::kDelay, 0.1 } }, 5.0);
        h.dt = 0.1;
        h.run(0, Phase::INIT);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 5.0);
    }
    {
        auto h = statefulWithInput("stateSpace",
            { { nflow::kA, 0.0 }, { nflow::kB, 1.0 }, { nflow::kC, 1.0 }, { nflow::kD, 0.0 } },
            2.0);
        h.dt = 0.1;
        h.run(0, Phase::INIT);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 0.2);
    }
    {
        auto h = statefulWithInput("lpf", { { nflow::kCutoff, 1.0 } }, 1.0);
        h.dt = 0.1;
        h.run(0, Phase::INIT);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0 * M_PI * 0.1);
    }
    {
        auto h = statefulWithInput("hpf", { { nflow::kCutoff, 1.0 } }, 1.0);
        h.dt = 0.1;
        h.run(0, Phase::INIT);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 1.0 - 2.0 * M_PI * 0.1);
    }
    {
        // The first committed sample seeds u_prev with the input and reports 0;
        // the next ALGEBRAIC recomputes (u - u_prev) / dt from the current input.
        auto h = statefulWithInput("derivative", json::object(), 2.0);
        h.dt = 0.1;
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.0);
        h.run(0, Phase::UPDATE);
        h.setOutputValue(1, 4.0);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 20.0);
    }
    {
        auto h = statefulWithInput("pid",
            { { nflow::kKp, 2.0 }, { nflow::kKi, 1.0 }, { nflow::kKd, 0.0 }, { nflow::kMin, -10.0 },
                { nflow::kMax, 10.0 } },
            3.0);
        h.dt = 0.1;
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 6.3);
    }
}
//=============================================================================
TEST_CASE("discrete handlers update sampled state")
{
    {
        // A hold samples AT the hit instant, so the first ALGEBRAIC already
        // carries the input.
        auto h = statefulWithInput("zoh", { { nflow::kTs, 0.1 } }, 5.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 5.0);
    }
    {
        auto h = statefulWithInput("foh", { { nflow::kTs, 0.1 } }, 4.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 4.0);
    }
    {
        auto h = statefulWithInput("dtf",
            { { nflow::kNum, json::array({ 2.0 }) }, { nflow::kDen, json::array({ 1.0 }) },
                { nflow::kTs, 0.1 } },
            3.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 6.0);
    }
    {
        auto h = statefulWithInput("ddelay", { { nflow::kSteps, 2.0 }, { nflow::kTs, 0.1 } }, 7.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 0.0);
    }
    {
        auto h = statefulWithInput("dstateSpace",
            { { nflow::kA, 0.0 }, { nflow::kB, 1.0 }, { nflow::kC, 1.0 }, { nflow::kD, 0.0 },
                { nflow::kTs, 0.1 } },
            5.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 5.0);
    }
    {
        auto h = statefulWithInput("unitDelay", { { nflow::kInitial, 2.0 } }, 9.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 9.0);
    }
    {
        // difference is direct-feedthrough: its output (u - u_prev) is produced
        // in the topologically-ordered ALGEBRAIC phase, not OUTPUT. INIT seeds
        // u_prev = 1.0, so the first evaluation reports 4.0 - 1.0.
        auto h = statefulWithInput("difference", { { nflow::kICPrevInput, 1.0 } }, 4.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 3.0);
    }
    {
        // detect* family also feedthrough-in-ALGEBRAIC. detectChange fires 1.0
        // when the input differs from the previous sample; INIT seeds prev = 0,
        // input 4.0 differs -> 1.0. After UPDATE latches prev = 4.0, a second
        // ALGEBRAIC on the same input reports 0.0 (no change).
        auto h = statefulWithInput("detectChange", { { nflow::kInitial, 0.0 } }, 4.0);
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.0);
    }
}
//=============================================================================
// The derivative is zero at a limit only while the input pushes outward.
//=============================================================================
TEST_CASE("limited integrator holds its derivative at an engaged saturation limit")
{
    // xdot as a function of the scattered state x and the input u, for a
    // limited integrator with bounds [-1, 1].
    auto derivOf = [](double xval, double u) {
        auto h = statefulWithInput("integrator",
            { { nflow::kInitial, 0.0 }, { nflow::kMin, -1.0 }, { nflow::kMax, 1.0 } }, u);
        h.run(0, Phase::INIT);
        h.blockState[0].xOffset = 0;
        h.blockState[0].xLength = 1;
        h.x = { xval };
        h.xdot = { 0.0 };
        h.run(0, Phase::DERIVATIVE);
        return h.xdot[0];
    };
    checkClose(derivOf(0.5, 2.0), 2.0); // interior: free integration
    checkClose(derivOf(1.0, 2.0), 0.0); // at upper limit, pushed up: held
    checkClose(derivOf(1.0, -2.0), -2.0); // at upper limit, pulled down: released
    checkClose(derivOf(-1.0, -2.0), 0.0); // at lower limit, pushed down: held
    checkClose(derivOf(-1.0, 2.0), 2.0); // at lower limit, pushed up: released
}
//=============================================================================
// Limited integrator: zero-crossing surfaces are (x - min, x - max).
//=============================================================================
TEST_CASE("limited integrator exposes saturation limits as zero-crossing surfaces")
{
    {
        // Two finite limits -> two live surfaces.
        auto h = statefulWithInput("integrator",
            { { nflow::kInitial, 0.0 }, { nflow::kMin, -1.0 }, { nflow::kMax, 2.0 } }, 0.0);
        h.run(0, Phase::INIT);
        h.blockState[0].xOffset = 0;
        h.blockState[0].xLength = 1;
        h.blockState[0].gOffset = 0;
        h.blockState[0].gLength = 2;
        h.x = { 0.3 };
        h.g = { 0.0, 0.0 };
        h.run(0, Phase::ZERO_CROSSING);
        checkClose(h.g[0], 0.3 - (-1.0)); // x - min = 1.3
        checkClose(h.g[1], 0.3 - 2.0); // x - max = -1.7
    }
    {
        // One-sided limit (upper only): the lower surface is a non-crossing
        // sentinel (+1), the upper surface is live.
        auto h = statefulWithInput(
            "integrator", { { nflow::kInitial, 0.0 }, { nflow::kMax, 2.0 } }, 0.0);
        h.run(0, Phase::INIT);
        h.blockState[0].xOffset = 0;
        h.blockState[0].xLength = 1;
        h.blockState[0].gOffset = 0;
        h.blockState[0].gLength = 2;
        h.x = { 1.5 };
        h.g = { 0.0, 0.0 };
        h.run(0, Phase::ZERO_CROSSING);
        checkClose(h.g[0], 1.0); // min = -inf -> sentinel
        checkClose(h.g[1], 1.5 - 2.0); // x - max = -0.5
    }
}
//=============================================================================
// FMU-export / C codegen: an external input must drive a stateful block.
// Regression: the C integrator codegen defaulted its saturation limits to
// [0, 0] instead of unbounded, so the generated step clamped the state to zero
// and an input->integrator->output FMU gave y == 0.
// The test emits the C step for a default (no Min/Max) integrator, extracts the
// clamp bounds, and simulates the recurrence with u == 1: the output must track
// t == integral(1).
//=============================================================================
namespace {
static double
codegenLiteralToDouble(const std::string& raw)
{
    size_t a = raw.find_first_not_of(" \t");
    size_t b = raw.find_last_not_of(" \t");
    std::string s = (a == std::string::npos) ? std::string() : raw.substr(a, b - a + 1);
    if (s == "INFINITY") {
        return std::numeric_limits<double>::infinity();
    }
    if (s == "-INFINITY") {
        return -std::numeric_limits<double>::infinity();
    }
    return std::strtod(s.c_str(), nullptr);
}
//=============================================================================
static double
clampD(double v, double mn, double mx)
{
    return v < mn ? mn : (v > mx ? mx : v);
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("integrator C codegen: an external input drives the state")
{
    ensureHandlersRegistered();

    Nelson::NFlow::BlockCodegenTemplate tpl;
    REQUIRE(BlockRegistry::instance().getCodegen("integrator", "c", tpl));
    REQUIRE(tpl.emitStep);

    // Default integrator: no Min/Max params (the exported input->integrator case).
    json block = { { nflow::kId, "b" }, { nflow::kType, "integrator" },
        { nflow::kParams, json::object() } };
    std::unordered_map<std::string, double> vars;

    std::vector<std::string> stepLines;
    Nelson::NFlow::BlockCodegenArgs a;
    a.id = "b";
    a.in = { "u", "0.0", "0.0" };
    a.block = &block;
    a.params = &block[nflow::kParams];
    a.variables = &vars;
    a.dt = 0.1;
    a.fmt = [](double v) { return nflow::formatNumber(v); };
    a.wrap = [](const std::string& e) { return "(" + e + ")"; };
    a.sanitize = [](const std::string& s) { return s; };
    a.line = [&](const std::string& l) { stepLines.push_back(l); };
    tpl.emitStep(a);

    // Locate the state-update line: s->int_b = fmin(fmax(s->int_b + u * dt, MIN), MAX);
    std::string upd;
    for (const auto& l : stepLines) {
        if (l.find("s->int_b") != std::string::npos && l.find("fmax") != std::string::npos) {
            upd = l;
        }
    }
    REQUIRE(!upd.empty());
    CHECK(upd.find("u") != std::string::npos);

    size_t dtPos = upd.find("dt, ");
    REQUIRE(dtPos != std::string::npos);
    size_t mnStart = dtPos + 4;
    size_t mnEnd = upd.find("), ", mnStart);
    REQUIRE(mnEnd != std::string::npos);
    double mn = codegenLiteralToDouble(upd.substr(mnStart, mnEnd - mnStart));
    size_t mxStart = mnEnd + 3;
    size_t mxEnd = upd.find(");", mxStart);
    REQUIRE(mxEnd != std::string::npos);
    double mx = codegenLiteralToDouble(upd.substr(mxStart, mxEnd - mxStart));

    // A default integrator must be unbounded, not clamped to zero.
    CHECK(mn == -std::numeric_limits<double>::infinity());
    CHECK(mx == std::numeric_limits<double>::infinity());

    // Simulate the emitted recurrence (output-then-update) with u == 1: the
    // output must track t == integral(1). Under the [0, 0] bug it stays at 0.
    const double dt = 0.1;
    const double u = 1.0;
    double state = clampD(0.0, mn, mx); // nflow_init clamps the initial value
    for (int k = 0; k <= 50; ++k) {
        double out = state; // out_b = s->int_b (pre-update state)
        checkClose(out, k * dt);
        state = clampD(state + u * dt, mn, mx);
    }
}
//=============================================================================
// C codegen: the PID integral term must be unbounded by default (the same
// [0, 0] default that broke the integrator would silently kill the integral
// action of an exported PID).
//=============================================================================
TEST_CASE("pid C codegen: the integral term is unbounded by default")
{
    ensureHandlersRegistered();

    Nelson::NFlow::BlockCodegenTemplate tpl;
    REQUIRE(BlockRegistry::instance().getCodegen("pid", "c", tpl));
    REQUIRE(tpl.emitShared);

    json block = { { nflow::kId, "b" }, { nflow::kType, "pid" },
        { nflow::kParams, { { nflow::kKp, 1.0 }, { nflow::kKi, 1.0 } } } };
    std::unordered_map<std::string, double> vars;

    std::vector<std::string> constLines;
    Nelson::NFlow::BlockCodegenStateArgs sa;
    sa.id = "b";
    sa.block = &block;
    sa.params = &block[nflow::kParams];
    sa.variables = &vars;
    sa.dt = 0.1;
    sa.fmt = [](double v) { return nflow::formatNumber(v); };
    sa.addState = [](const std::string&, const std::string&, const std::string&) { };
    sa.declState = [](const std::string&) { };
    sa.addInit = [](const std::string&) { };
    sa.addConst = [&](const std::string& l) { constLines.push_back(l); };
    sa.addHelper = [](const std::string&) { };
    sa.once = [](const std::string&) { return true; };
    sa.allBlocks = nullptr;
    tpl.emitShared(sa);

    std::string paramsLine;
    for (const auto& l : constLines) {
        if (l.find("pid_params_b") != std::string::npos) {
            paramsLine = l;
        }
    }
    REQUIRE(!paramsLine.empty());
    // Unbounded integral limits: -INFINITY / INFINITY, not a [0, 0] clamp.
    CHECK(paramsLine.find("INFINITY") != std::string::npos);
}
//=============================================================================
