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
using Nelson::NFlow::Phase;
TEST_CASE("source handlers compute scalar outputs")
{
    {
        auto h = oneBlock("constant", { { nflow::kValue, "2*pi" } });
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0 * M_PI);
    }
    {
        auto h = oneBlock("step", { { nflow::kStepTime, 1.5 } });
        h.t = 1.0;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 0.0);
        h.t = 1.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h = oneBlock("ramp", { { nflow::kSlope, 3.0 }, { nflow::kStart, 2.0 } });
        h.t = 2.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 1.5);
    }
    {
        auto h = oneBlock("impulse", { { nflow::kTime, 1.0 }, { nflow::kAmp, 2.0 } });
        h.dt = 0.1;
        h.t = 1.0;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 20.0);
    }
    {
        auto h = oneBlock(
            "sine", { { nflow::kAmp, 2.0 }, { nflow::kFreq, 1.0 }, { nflow::kPhase, 0.0 } });
        h.t = 0.25;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0);
    }
    {
        auto h = oneBlock("chirp",
            { { nflow::kAmp, 2.0 }, { nflow::kF0, 0.0 }, { nflow::kF1, 0.0 },
                { nflow::kT1, 1.0 } });
        h.t = 0.4;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 0.0);
    }
    {
        auto h = oneBlock("clock");
        h.t = 3.25;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 3.25);
    }
    {
        // pulse: amplitude 2, period 1, width 25% -> high on [0,0.25) of each period.
        auto h = oneBlock("pulse", { { nflow::kAmp, 2.0 }, { "Period", 1.0 }, { "Width", 25.0 } });
        h.t = 0.1;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0);
        h.t = 0.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 0.0);
        h.t = 1.1;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0); // next period, high again
    }
    {
        // sawTooth: amplitude 4, period 2 -> ramps 0..4 over each period.
        auto h = oneBlock("sawTooth", { { nflow::kAmp, 4.0 }, { "Period", 2.0 } });
        h.t = 0.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 1.0); // 4 * 0.5/2
        h.t = 2.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 1.0); // periodic: same as t=0.5
    }
    {
        // expSine: amp 2, freq 1, damping ln(2) -> at t=0.25, sin arg = pi/2 = 1,
        // envelope 2*exp(-ln(2)*0.25) = 2*2^{-0.25}.
        auto h = oneBlock("expSine",
            { { nflow::kAmp, 2.0 }, { nflow::kFreq, 1.0 }, { "Damping", std::log(2.0) },
                { nflow::kPhase, 0.0 } });
        h.t = 0.25;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0 * std::pow(2.0, -0.25));
    }
    {
        // trapezoid: amp 2, rising 1, width 1, falling 1, period 4.
        auto h = oneBlock("trapezoid",
            { { nflow::kAmp, 2.0 }, { "Rising", 1.0 }, { "Width", 1.0 }, { "Falling", 1.0 },
                { "Period", 4.0 } });
        h.t = 0.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 1.0); // rising: 2 * 0.5/1
        h.t = 1.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 2.0);
        h.t = 2.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 1.0); // falling: 2 * (1 - 0.5/1)
        h.t = 3.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 0.0);
    }
    {
        // exponentials: amp 1, riseTime 1, riseTau 1 -> at t=1 (end of rise) the
        // peak is 1*(1 - e^{-1}); falling with fallTau 1 keeps it there at t=1.
        auto h = oneBlock("exponentials",
            { { nflow::kAmp, 1.0 }, { "RiseTime", 1.0 }, { "RiseTau", 1.0 }, { "FallTau", 1.0 } });
        h.t = 1.0;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 1.0 - std::exp(-1.0)); // peak at end of rise
        h.t = 0.0;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 0.0);
    }
}
//=============================================================================
TEST_CASE("fileSource and labelSource read runtime data")
{
    {
        auto h = oneBlock("fileSource",
            { { nflow::kTimes, json::array({ 0.0, 1.0, 2.0 }) },
                { nflow::kValues, json::array({ 4.0, 5.0, 6.0 }) } });
        h.run(0, Phase::INIT);
        h.t = 1.5;
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 5.0);
    }
    {
        Harness h;
        auto source = h.addInput(7.0);
        auto sink = h.addBlock("sink", nflow::kLabelSink, 1, 1, { { nflow::kGotoTag, "x" } });
        auto labelSource
            = h.addBlock("src", nflow::kLabelSource, 0, 1, { { nflow::kGotoTag, "x" } });
        h.connect(source, sink);
        h.labelSinks["x"] = "sink";
        h.run(labelSource, Phase::OUTPUT);
        checkClose(h.output(labelSource), 7.0);
    }
}
//=============================================================================
TEST_CASE("noise and toggleSwitch stay within expected output contracts")
{
    {
        auto h = oneBlock("noise", { { nflow::kAmp, 3.0 } });
        h.run(0, Phase::OUTPUT);
        const double first = h.output(0);
        CHECK(first >= -3.0);
        CHECK(first <= 3.0);
        h.run(0, Phase::OUTPUT);
        CHECK(h.output(0) != first);

        auto h2 = oneBlock("noise", { { nflow::kAmp, 3.0 } });
        h2.run(0, Phase::OUTPUT);
        checkClose(h2.output(0), first);
    }
    {
        auto h = oneBlock("toggleSwitch",
            { { nflow::kState, 1.0 }, { nflow::kOnValue, 10.0 }, { nflow::kOffValue, -2.0 } });
        h.run(0, Phase::OUTPUT);
        checkClose(h.output(0), 10.0);
    }
}
//=============================================================================
