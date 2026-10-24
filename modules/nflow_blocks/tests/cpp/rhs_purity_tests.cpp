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
#include <cmath>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::BlockId;
using Nelson::NFlow::Phase;
// Runs the given phase twice at an identical (t, input) and asserts the output
// slot and the mutable BlockState scalars are unchanged the second time, i.e.
// the phase is a pure function of (t, x), safe to re-run at arbitrary solver
// stage times. This is the property a variable-step RHS relies on.
static void
checkPure(
    const std::string& type, json params, double input, int inputs = 1, Phase phase = Phase::OUTPUT)
{
    Harness h;
    BlockId b = h.addBlock("b", type, inputs, 1, std::move(params));
    if (inputs > 0) {
        h.connect(h.addInput(input), b);
    }
    h.run(b, Phase::INIT);
    h.run(b, phase);
    const double out1 = h.output(b);
    const double scalar1 = h.blockState[b].scalar;
    const double stout1 = h.blockState[b].output;

    h.run(b, phase);
    const double out2 = h.output(b);

    CHECK(out1 == out2);
    CHECK(h.blockState[b].scalar == scalar1);
    CHECK(h.blockState[b].output == stout1);
}
//=============================================================================
TEST_CASE("OUTPUT is a pure function of (t, x) for representative blocks")
{
    ensureHandlersRegistered();

    // Stateless math / logic.
    checkPure("gain", json { { "gain", 3.0 } }, 2.0);
    checkPure("constant", json { { nflow::kValue, 4.0 } }, 0.0, 0);
    checkPure("saturation", json { { nflow::kMin, -1.0 }, { nflow::kMax, 1.0 } }, 5.0);

    // Stateful continuous blocks: OUTPUT reads latched state only.
    checkPure("integrator", json { { nflow::kInitial, 2.0 } }, 3.0);
    checkPure("lpf", json { { nflow::kCutoff, 1.0 } }, 1.0);
    checkPure("hpf", json { { nflow::kCutoff, 1.0 } }, 1.0);

    // fileSource is stateless (upper_bound on ctx.t): no cursor advances in OUTPUT.
    {
        json p;
        p[nflow::kTimes] = json::array({ 0.0, 1.0, 2.0 });
        p[nflow::kValues] = json::array({ 5.0, 10.0, 15.0 });
        checkPure("fileSource", p, 0.0, 0);
    }
}
//=============================================================================
TEST_CASE("tf ALGEBRAIC feedthrough is idempotent")
{
    ensureHandlersRegistered();
    // Pure gain (empty state) exercises the ALGEBRAIC feedthrough path.
    json p;
    p[nflow::kNum] = json::array({ 2.0 });
    p[nflow::kDen] = json::array({ 1.0 });
    checkPure("tf", p, 3.0, 1, Phase::ALGEBRAIC);
}
//=============================================================================
TEST_CASE("noise OUTPUT is intentionally stochastic (documented purity exemption)")
{
    ensureHandlersRegistered();
    Harness h;
    BlockId b = h.addBlock("b", "noise", 0, 1, json { { nflow::kAmp, 1.0 } });
    h.run(b, Phase::INIT);
    h.run(b, Phase::OUTPUT);
    const double out1 = h.output(b);
    h.run(b, Phase::OUTPUT);
    const double out2 = h.output(b);
    // noise advances its RNG in OUTPUT, so it is NOT re-runnable at a fixed t.
    // It stays on the RHS-purity allow-list until it samples in UPDATE.
    CHECK(out1 != out2);
}
//=============================================================================
