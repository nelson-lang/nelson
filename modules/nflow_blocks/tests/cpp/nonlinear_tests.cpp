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
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::Phase;
static Harness
nonlinearWithInputs(const std::string& type, json params, const std::vector<double>& values)
{
    Harness h;
    auto b = h.addBlock("b", type, static_cast<int>(values.size()), 1, std::move(params));
    for (int i = 0; i < static_cast<int>(values.size()); ++i) {
        h.connect(h.addInput(values[static_cast<size_t>(i)]), b, i);
    }
    return h;
}
//=============================================================================
TEST_CASE("nonlinear algebraic handlers clamp and switch values")
{
    {
        auto h = nonlinearWithInputs(
            "saturation", { { nflow::kLowerLimit, -1.0 }, { nflow::kUpperLimit, 2.0 } }, { 5.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 2.0);
    }
    {
        auto h = nonlinearWithInputs(
            "deadZone", { { nflow::kLowerValue, -1.0 }, { nflow::kUpperValue, 1.0 } }, { 3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 2.0);
    }
    {
        auto h
            = nonlinearWithInputs("quantizer", { { nflow::kQuantizationInterval, 0.5 } }, { 1.24 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h = nonlinearWithInputs("switch",
            { { nflow::kCondition, nflow::kGt }, { nflow::kThreshold, 1.0 } },
            { 10.0, 2.0, -10.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 10.0);
    }
}
//=============================================================================
TEST_CASE("rate and backlash answer the current input, then commit")
{
    {
        auto h = nonlinearWithInputs(
            "rate", { { nflow::kRise, 2.0 }, { nflow::kFall, 2.0 } }, { 10.0 });
        h.dt = 0.5;
        // Direct feedthrough: the first ALGEBRAIC already slews towards the
        // input instead of replaying the state committed one step earlier.
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 2.0);
    }
    {
        auto h = nonlinearWithInputs("backlash", { { nflow::kBacklashWidth, 2.0 } }, { 5.0 });
        h.run(0, Phase::INIT);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 4.0);
        h.run(0, Phase::UPDATE);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 4.0);
    }
}
//=============================================================================
TEST_CASE("hysteresis relay latches with a dead band")
{
    Harness h;
    auto b = h.addBlock("b", "hysteresis", 1, 1,
        { { "uHigh", 1.0 }, { "uLow", -1.0 }, { "yHigh", 1.0 }, { "yLow", 0.0 } });
    auto in = h.addInput(0.5);
    h.connect(in, b);
    h.run(b, Phase::INIT);
    // Between thresholds: holds the initial low branch. ALGEBRAIC answers the
    // current input (direct feedthrough), UPDATE then commits the latch.
    h.run(b, Phase::ALGEBRAIC);
    checkClose(h.output(b), 0.0);
    h.run(b, Phase::UPDATE);
    // Rise past uHigh -> switches high in the SAME step, not the next one.
    h.setOutputValue(in, 1.5);
    h.run(b, Phase::ALGEBRAIC);
    checkClose(h.output(b), 1.0);
    h.run(b, Phase::UPDATE);
    // Back into the dead band -> HOLDS high (the hysteresis).
    h.setOutputValue(in, 0.5);
    h.run(b, Phase::ALGEBRAIC);
    checkClose(h.output(b), 1.0);
    h.run(b, Phase::UPDATE);
    h.setOutputValue(in, -1.5);
    h.run(b, Phase::ALGEBRAIC);
    checkClose(h.output(b), 0.0);
}
//=============================================================================
