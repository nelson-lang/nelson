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
TEST_CASE("sink handlers collect samples and tolerate passive blocks")
{
    {
        Harness h;
        auto source = h.addInput(2.0);
        auto scope = h.addBlock("scope", "scope", 1, 1, json::object());
        h.connect(source, scope);
        h.run(scope, Phase::INIT);
        h.run(scope, Phase::AFTER_STEP);
        REQUIRE(h.blockState[scope].scopeSeries.size() == 1);
        REQUIRE(h.blockState[scope].scopeSeries[0].size() == 1);
        checkClose(h.blockState[scope].scopeSeries[0][0], 2.0);
    }
    {
        Harness h;
        auto x = h.addInput(1.0);
        auto y = h.addInput(3.0);
        auto xy = h.addBlock("xy", "xyScope", 2, 1, json::object());
        h.connect(x, xy, 0);
        h.connect(y, xy, 1);
        h.run(xy, Phase::INIT);
        h.run(xy, Phase::AFTER_STEP);
        checkClose(h.blockState[xy].xSeries[0], 1.0);
        checkClose(h.blockState[xy].ySeries[0], 3.0);
    }
    {
        Harness h;
        auto x = h.addInput(1.0);
        auto y = h.addInput(2.0);
        auto z = h.addInput(3.0);
        auto xyz = h.addBlock("xyz", "xyzScope", 3, 1, json::object());
        h.connect(x, xyz, 0);
        h.connect(y, xyz, 1);
        h.connect(z, xyz, 2);
        h.run(xyz, Phase::INIT);
        h.run(xyz, Phase::AFTER_STEP);
        checkClose(h.blockState[xyz].zSeries[0], 3.0);
    }
    {
        Harness h;
        auto source = h.addInput(9.0);
        auto display
            = h.addBlock("display", nflow::kDisplay, 1, 1, { { nflow::kDecimation, 1.0 } });
        h.connect(source, display);
        h.run(display, Phase::INIT);
        h.run(display, Phase::AFTER_STEP);
        checkClose(h.blockState[display].scalar, 9.0);
    }
    {
        auto labelSink = oneBlock(nflow::kLabelSink);
        CHECK_FALSE(labelSink.run(0, Phase::OUTPUT));
        auto terminator = oneBlock("terminator");
        CHECK_FALSE(terminator.run(0, Phase::OUTPUT));
    }
}
//=============================================================================
TEST_CASE("subsystem handler runs a minimal inner diagram")
{
    json spec = {
        { nflow::kBlocks,
            json::array({
                { { nflow::kId, "c" }, { nflow::kType, "constant" }, { nflow::kInputs, 0 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kValue, 3.0 } } } },
                { { nflow::kId, "out" }, { nflow::kType, nflow::kLabelSink }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kGotoTag, "out" } } } },
            }) },
        { nflow::kConnections,
            json::array({ { { nflow::kFrom, "c" }, { nflow::kTo, "out" }, { nflow::kFromIndex, 0 },
                { nflow::kToIndex, 0 } } }) },
        { nflow::kExternalOutputs,
            json::array({ { { nflow::kId, "out" }, { nflow::kName, "out" } } }) },
    };

    auto h = oneBlock("subsystem", { { nflow::kSubsystem, spec } }, 0, 1);
    h.run(0, Phase::INIT);
    h.run(0, Phase::ALGEBRAIC);
    checkClose(h.output(0), 3.0);
}
//=============================================================================
