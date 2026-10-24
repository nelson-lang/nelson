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
using Nelson::NFlow::buildStateLayout;
using Nelson::NFlow::fillDaeIds;
using Nelson::NFlow::StateLayout;
// mechanicalTranslationalIsland (acausal): 1-D translational mechanics with
// (position, velocity) states per mass node. These doctests pin the layout
// contract and the DERIVATIVE force balance; the mass-spring-damper transient
// is checked end-to-end against the analytic response in
// test_nflow_acausal_mechanical.m.
TEST_CASE("mechanicalIsland: mass gives two differential states")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "translational";
    params["nodes"] = 2;
    params["components"] = json::array({
        json { { "kind", "mass" }, { "node", 1 }, { "m", 1.0 }, { "s0", 1.0 }, { "v0", 0.0 } },
        json { { "kind", "spring" }, { "a", 1 }, { "b", 0 }, { "k", 4.0 } },
        json { { "kind", "damper" }, { "a", 1 }, { "b", 0 }, { "d", 1.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("m1", "mechanicalTranslationalIsland", 0, 0, params);
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 2); // s, v of node 1
    CHECK(h.blockState[isl].xLength == 2);

    // All states differential (id all 1) under the dae solver.
    std::vector<double> id(2, 1.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    CHECK(id[0] == 1.0);
    CHECK(id[1] == 1.0);
}
//=============================================================================
TEST_CASE("mechanicalIsland: DERIVATIVE writes s'=v, v'=F/m")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "translational";
    params["nodes"] = 2;
    params["components"] = json::array({
        json { { "kind", "mass" }, { "node", 1 }, { "m", 2.0 }, { "s0", 1.0 }, { "v0", 0.0 } },
        json { { "kind", "spring" }, { "a", 1 }, { "b", 0 }, { "k", 4.0 } },
        json { { "kind", "damper" }, { "a", 1 }, { "b", 0 }, { "d", 1.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("m1", "mechanicalTranslationalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // s=1, v=0.5: F_net = -k*s - d*v = -4*1 - 1*0.5 = -4.5; v' = F/m = -2.25.
    h.x = { 1.0, 0.5 };
    h.xdot = { 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 0.5); // s' = v
    checkClose(h.xdot[1], -2.25); // v' = (-4 - 0.5) / 2
    (void)isl;
}
//=============================================================================
TEST_CASE("mechanicalIsland: regularised Coulomb friction opposes motion")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "translational";
    params["nodes"] = 2;
    params["components"] = json::array({
        json { { "kind", "mass" }, { "node", 1 }, { "m", 2.0 }, { "s0", 0.0 }, { "v0", 0.0 } },
        json { { "kind", "friction" }, { "a", 1 }, { "Fc", 3.0 }, { "vEps", 1e-3 } },
    });
    Harness h;
    BlockId isl = h.addBlock("m1", "mechanicalTranslationalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // Sliding at v = 5 >> vEps: F = -Fc tanh(5/1e-3) ~ -Fc, so v' ~ -Fc/m.
    h.x = { 0.0, 5.0 };
    h.xdot = { 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 5.0);
    CHECK(std::abs(h.xdot[1] - (-3.0 / 2.0)) < 1e-6); // v' ~ -Fc/m = -1.5
    // Moving the other way flips the friction sign.
    h.x = { 0.0, -5.0 };
    h.run(isl, Phase::DERIVATIVE);
    CHECK(std::abs(h.xdot[1] - (3.0 / 2.0)) < 1e-6); // v' ~ +Fc/m
    (void)isl;
}
//=============================================================================
TEST_CASE("mechanicalIsland: clutch transmits a slip-reducing friction couple")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "translational";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "mass" }, { "node", 1 }, { "m", 1.0 }, { "v0", 10.0 } },
        json { { "kind", "mass" }, { "node", 2 }, { "m", 2.0 }, { "v0", 0.0 } },
        json { { "kind", "clutch" }, { "a", 1 }, { "b", 2 }, { "Fc", 5.0 }, { "vEps", 1e-3 } },
    });
    Harness h;
    BlockId isl = h.addBlock("m1", "mechanicalTranslationalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // slip = v1 - v2 = 10 >> vEps: T ~ Fc = 5. a decelerates, b accelerates.
    h.x = { 0.0, 10.0, 0.0, 0.0 };
    h.xdot.assign(4, 0.0);
    h.run(isl, Phase::DERIVATIVE);
    CHECK(std::abs(h.xdot[1] - (-5.0 / 1.0)) < 1e-6); // v1' ~ -T/m1
    CHECK(std::abs(h.xdot[3] - (5.0 / 2.0)) < 1e-6); // v2' ~ +T/m2
    (void)isl;
}
//=============================================================================
TEST_CASE("mechanicalIsland: relative position/speed sensors")
{
    ensureHandlersRegistered();
    // Two masses; relative sensors measure node1 - node2.
    json params;
    params["domain"] = "translational";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "mass" }, { "node", 1 }, { "m", 1.0 }, { "s0", 3.0 }, { "v0", 1.0 } },
        json { { "kind", "mass" }, { "node", 2 }, { "m", 1.0 }, { "s0", 1.0 }, { "v0", 0.25 } },
        json { { "kind", "relPositionSensor" }, { "a", 1 }, { "b", 2 }, { "outPort", 0 } },
        json { { "kind", "relSpeedSensor" }, { "a", 1 }, { "b", 2 }, { "outPort", 1 } },
    });
    Harness h;
    BlockId isl = h.addBlock("m1", "mechanicalTranslationalIsland", 0, 2, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // s1=3,v1=1 ; s2=1,v2=0.25 -> rel pos = 2, rel speed = 0.75.
    h.x = { 3.0, 1.0, 1.0, 0.25 };
    h.run(isl, Phase::OUTPUT);
    checkClose(h.output(isl, 0), 2.0);
    checkClose(h.output(isl, 1), 0.75);
    (void)isl;
}
//=============================================================================
TEST_CASE("mechanicalIsland: ideal gear folds inertia and states")
{
    ensureHandlersRegistered();
    // Two inertias J1 (node1) and J2 (node2) coupled by a gear ratio 2
    // (pos_1 = 2 pos_2), a torque on node1. Only ONE state pair (the group
    // root = node1); J_eff = J1*1 + J2*(1/2)^2.
    json params;
    params["domain"] = "rotational";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "mass" }, { "node", 1 }, { "m", 1.0 }, { "s0", 0.0 }, { "v0", 0.0 } },
        json { { "kind", "mass" }, { "node", 2 }, { "m", 1.0 }, { "s0", 0.0 }, { "v0", 0.0 } },
        json { { "kind", "gear" }, { "a", 1 }, { "b", 2 }, { "ratio", 2.0 } },
        json { { "kind", "force" }, { "node", 1 }, { "F", 1.25 } },
        json { { "kind", "speedSensor" }, { "node", 1 }, { "outPort", 0 } },
        json { { "kind", "speedSensor" }, { "node", 2 }, { "outPort", 1 } },
    });
    Harness h;
    BlockId isl = h.addBlock("g1", "mechanicalTranslationalIsland", 0, 2, params);
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 2); // one folded (s, v) pair, not two

    h.run(isl, Phase::INIT);
    // J_eff = 1 + 1*(1/2)^2 = 1.25; torque 1.25 on node1 -> v1' = 1.
    h.x = { 0.0, 0.0 };
    h.xdot = { 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 0.0);
    checkClose(h.xdot[1], 1.0); // v1' = 1.25 / 1.25
    // Speed sensor on node2 reads omega_2 = ratioToRoot[2] * v_root = 0.5 * v1.
    h.x = { 0.0, 4.0 }; // set v_root = 4
    h.run(isl, Phase::OUTPUT);
    checkClose(h.output(isl, 0), 4.0); // node1 speed = v_root
    checkClose(h.output(isl, 1), 2.0); // node2 speed = 0.5 * v_root
    (void)isl;
}
//=============================================================================
TEST_CASE("mechanicalIsland: fixed node offsets the spring rest force")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "translational";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "mass" }, { "node", 1 }, { "m", 1.0 }, { "s0", 0.0 }, { "v0", 0.0 } },
        json { { "kind", "spring" }, { "a", 1 }, { "b", 2 }, { "k", 3.0 } },
        json { { "kind", "fixed" }, { "node", 2 }, { "s0", 2.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("m1", "mechanicalTranslationalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // s1=0, spring to fixed node2 at s=2: k*(s1 - s2) = -6, so the force on
    // node1 is +6 and v' = 6/1.
    h.x = { 0.0, 0.0 };
    h.xdot = { 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 0.0);
    checkClose(h.xdot[1], 6.0); // pulled toward the fixed node at s=2
    (void)isl;
}
//=============================================================================
