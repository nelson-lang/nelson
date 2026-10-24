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
#include "StateLayout.hpp"
#include <cmath>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::BlockId;
using Nelson::NFlow::BlockRegistry;
using Nelson::NFlow::buildStateLayout;
using Nelson::NFlow::Phase;
using Nelson::NFlow::StateLayout;
TEST_CASE("buildStateLayout assigns contiguous continuous-state offsets")
{
    ensureHandlersRegistered();
    Harness h;
    BlockId integ = h.addBlock("i1", "integrator", 1, 1, json::object());
    json tfParams;
    tfParams[nflow::kNum] = json::array({ 1.0 });
    tfParams[nflow::kDen] = json::array({ 1.0, 2.0, 3.0 });
    BlockId tf = h.addBlock("t1", "tf", 1, 1, tfParams);
    BlockId gain = h.addBlock("g1", "gain", 1, 1, json { { "gain", 2.0 } });

    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);

    CHECK(layout.totalContinuous == 3);
    CHECK(h.blockState[integ].xOffset == 0);
    CHECK(h.blockState[integ].xLength == 1);
    CHECK(h.blockState[tf].xOffset == 1);
    CHECK(h.blockState[tf].xLength == 2);
    CHECK(h.blockState[gain].xOffset == -1);
    CHECK(h.blockState[gain].xLength == 0);
}
//=============================================================================
TEST_CASE("buildStateLayout: only a limited integrator allocates zero-crossings")
{
    ensureHandlersRegistered();
    {
        // Unbounded integrator (default params): no saturation surfaces, so the
        // global root layout of existing models is left unchanged.
        Harness h;
        BlockId integ = h.addBlock("i1", "integrator", 1, 1, json::object());
        StateLayout layout
            = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
        CHECK(layout.totalZeroCrossings == 0);
        CHECK(h.blockState[integ].gLength == 0);
    }
    {
        Harness h;
        json p;
        p[nflow::kMin] = -1.0;
        p[nflow::kMax] = 1.0;
        BlockId integ = h.addBlock("i1", "integrator", 1, 1, p);
        StateLayout layout
            = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
        CHECK(layout.totalZeroCrossings == 2);
        CHECK(h.blockState[integ].gOffset == 0);
        CHECK(h.blockState[integ].gLength == 2);
    }
}
//=============================================================================
TEST_CASE("A2.1: a subsystem's inner integrator is driven through the global solver")
{
    ensureHandlersRegistered();
    // Subsystem wrapping a single integrator: external input u -> integrator ->
    // labelSink (external output y). Under the global solver its inner state must
    // join the global vector and its DERIVATIVE must write xdot = u.
    json spec = {
        { nflow::kBlocks,
            json::array({
                // "u_in" is the external-input port (its output slot carries u);
                // it feeds the integrator, whose output goes to the labelSink.
                { { nflow::kId, "u_in" }, { nflow::kType, "constant" }, { nflow::kInputs, 0 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kValue, 0.0 } } } },
                { { nflow::kId, "int" }, { nflow::kType, "integrator" }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kInitial, 0.0 } } } },
                { { nflow::kId, "out" }, { nflow::kType, nflow::kLabelSink }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kGotoTag, "y" } } } },
            }) },
        { nflow::kConnections,
            json::array({
                { { nflow::kFrom, "u_in" }, { nflow::kTo, "int" }, { nflow::kFromIndex, 0 },
                    { nflow::kToIndex, 0 } },
                { { nflow::kFrom, "int" }, { nflow::kTo, "out" }, { nflow::kFromIndex, 0 },
                    { nflow::kToIndex, 0 } },
            }) },
        { nflow::kExternalInputs,
            json::array({ { { nflow::kId, "u_in" }, { nflow::kName, "u" } } }) },
        { nflow::kExternalOutputs,
            json::array({ { { nflow::kId, "out" }, { nflow::kName, "y" } } }) },
    };

    Harness h;
    BlockId sub = h.addBlock("sub", "subsystem", 1, 1, { { nflow::kSubsystem, spec } });
    h.connect(h.addInput(3.0), sub);
    h.run(sub, Phase::INIT);

    // StateLayout must recurse and allocate the inner integrator's continuous
    // state in the global vector (the subsystem block itself has no own state).
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    CHECK(layout.totalContinuous == 1);

    h.x = { 5.0 }; // inner integrator state (value irrelevant: xdot = u)
    h.xdot = { 0.0 };
    h.run(sub, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 3.0);
}
//=============================================================================
TEST_CASE("N0.1: fillDaeIds, per-state mask over whole-slice algebraicState")
{
    ensureHandlersRegistered();
    using Nelson::NFlow::BlockMetadata;
    using Nelson::NFlow::fillDaeIds;
    // Synthetic mixed-DAE type: 3 continuous states, the middle one algebraic
    // (the shape of a physical island: node potential between two energy-storage states).
    static bool mixedRegistered = false;
    if (!mixedRegistered) {
        BlockMetadata meta;
        meta.numContinuousStates = 3;
        meta.sampleTime = Nelson::NFlow::SampleTimeClass::Continuous;
        meta.algebraicStateMask = [](const Nelson::NFlow::Block&, const Nelson::NFlow::ValMap&,
                                      std::vector<double>& ids) { ids[1] = 0.0; };
        BlockRegistry::instance().registerHandler(
            "testMixedDae",
            [](Nelson::NFlow::SimCtx&, const Nelson::NFlow::Block&, Phase) { return true; },
            { Phase::DERIVATIVE }, "test.dae", meta);
        mixedRegistered = true;
    }

    Harness h;
    BlockId integ = h.addBlock("i1", "integrator", 1, 1, json::object());
    BlockId mixed = h.addBlock("m1", "testMixedDae", 0, 0, json::object());
    BlockId cons = h.addBlock("c1", "constraint", 1, 1, json::object());
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 5);
    CHECK(h.blockState[mixed].xOffset == 1);
    CHECK(h.blockState[mixed].xLength == 3);
    CHECK(h.blockState[cons].xOffset == 4);

    std::vector<double> id(5, 1.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    CHECK(id[0] == 1.0); // integrator: differential
    CHECK(id[1] == 1.0); // mixed[0]: differential
    CHECK(id[2] == 0.0); // mixed[1]: algebraic via the per-state mask
    CHECK(id[3] == 1.0); // mixed[2]: differential
    CHECK(id[4] == 0.0); // constraint: whole-slice algebraicState (unchanged)
    (void)integ;
}
//=============================================================================
TEST_CASE("N0.1: fillDaeIds recurses into a subsystem's inner constraint")
{
    ensureHandlersRegistered();
    using Nelson::NFlow::fillDaeIds;
    // Subsystem wrapping a constraint. The id pass must visit nested blocks,
    // so the constraint slice is marked algebraic.
    json spec = {
        { nflow::kBlocks,
            json::array({
                { { nflow::kId, "u_in" }, { nflow::kType, "constant" }, { nflow::kInputs, 0 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kValue, 0.0 } } } },
                { { nflow::kId, "cons" }, { nflow::kType, "constraint" }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, json::object() } },
                { { nflow::kId, "out" }, { nflow::kType, nflow::kLabelSink }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kGotoTag, "y" } } } },
            }) },
        { nflow::kConnections,
            json::array({
                { { nflow::kFrom, "u_in" }, { nflow::kTo, "cons" }, { nflow::kFromIndex, 0 },
                    { nflow::kToIndex, 0 } },
                { { nflow::kFrom, "cons" }, { nflow::kTo, "out" }, { nflow::kFromIndex, 0 },
                    { nflow::kToIndex, 0 } },
            }) },
        { nflow::kExternalInputs,
            json::array({ { { nflow::kId, "u_in" }, { nflow::kName, "u" } } }) },
        { nflow::kExternalOutputs,
            json::array({ { { nflow::kId, "out" }, { nflow::kName, "y" } } }) },
    };

    Harness h;
    BlockId integ = h.addBlock("i1", "integrator", 1, 1, json::object());
    BlockId sub = h.addBlock("sub", "subsystem", 1, 1, { { nflow::kSubsystem, spec } });
    h.connect(h.addInput(0.0), sub);
    h.run(sub, Phase::INIT);

    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 2); // top integrator + inner constraint

    std::vector<double> id(2, 1.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    CHECK(id[0] == 1.0);
    CHECK(id[1] == 0.0); // nested constraint is algebraic
    (void)integ;
    (void)sub;
}
//=============================================================================
TEST_CASE("integrator DERIVATIVE writes xdot = input")
{
    ensureHandlersRegistered();
    Harness h;
    BlockId integ = h.addBlock("b", "integrator", 1, 1, json::object());
    h.connect(h.addInput(3.0), integ);
    h.blockState[integ].xOffset = 0;
    h.blockState[integ].xLength = 1;
    h.x = { 5.0 }; // integrator derivative is independent of the state value
    h.xdot = { 0.0 };
    h.run(integ, Phase::INIT);
    h.run(integ, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 3.0);
}
//=============================================================================
TEST_CASE("lpf DERIVATIVE writes xdot = wc*(u - x)")
{
    ensureHandlersRegistered();
    Harness h;
    BlockId lpf = h.addBlock("b", "lpf", 1, 1, json { { nflow::kCutoff, 1.0 } });
    h.connect(h.addInput(1.0), lpf);
    h.blockState[lpf].xOffset = 0;
    h.blockState[lpf].xLength = 1;
    h.x = { 0.5 };
    h.xdot = { 0.0 };
    h.run(lpf, Phase::INIT);
    h.run(lpf, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 2.0 * M_PI * 1.0 * (1.0 - 0.5)); // = pi
}
//=============================================================================
TEST_CASE("stateSpace DERIVATIVE writes xdot = A*x + B*u")
{
    ensureHandlersRegistered();
    Harness h;
    BlockId ss = h.addBlock("b", "stateSpace", 1, 1,
        json { { nflow::kA, 2.0 }, { nflow::kB, 3.0 }, { nflow::kC, 1.0 }, { nflow::kD, 0.0 } });
    h.connect(h.addInput(4.0), ss);
    h.blockState[ss].xOffset = 0;
    h.blockState[ss].xLength = 1;
    h.x = { 1.0 };
    h.xdot = { 0.0 };
    h.run(ss, Phase::INIT);
    h.run(ss, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 2.0 * 1.0 + 3.0 * 4.0); // = 14
}
//=============================================================================
