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
// planarMechanicalIsland (acausal): 2-D planar multibody in absolute COM
// coordinates. These doctests pin the state layout (six differential states per
// body, two algebraic multipliers per Revolute), the gravity/torque force
// balance, and the sensor map; the free-body closed forms and the pendulum
// transient are checked end-to-end in test_nflow_acausal_planar.m.
TEST_CASE("planarIsland: a body gives six differential states")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.1 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 6);
    CHECK(h.blockState[isl].xLength == 6);

    std::vector<double> id(6, 0.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    for (int k = 0; k < 6; ++k) {
        CHECK(id[static_cast<size_t>(k)] == 1.0);
    }
}
//=============================================================================
TEST_CASE("planarIsland: revolute adds two algebraic multipliers")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.1 } },
        json { { "kind", "revolute" }, { "body", 0 }, { "ax", 1.0 }, { "ay", 0.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 8); // 6 body + 2 lambda

    std::vector<double> id(8, 0.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    for (int k = 0; k < 6; ++k) {
        CHECK(id[static_cast<size_t>(k)] == 1.0);
    }
    CHECK(id[6] == 0.0); // lambda_x algebraic
    CHECK(id[7] == 0.0); // lambda_y algebraic
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: prismatic adds two algebraic multipliers")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.1 } },
        json { { "kind", "prismatic" }, { "body", 0 }, { "dx", 1.0 }, { "dy", 0.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 8); // 6 body + 2 lambda

    std::vector<double> id(8, 1.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    CHECK(id[6] == 0.0); // lambda_1 algebraic
    CHECK(id[7] == 0.0); // lambda_2 algebraic
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: rolling wheel adds two algebraic multipliers")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.5 } },
        json { { "kind", "rollingWheel" }, { "body", 0 }, { "dx", 1.0 }, { "dy", 0.0 },
            { "radius", 1.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 8); // 6 body + 2 lambda
    std::vector<double> id(8, 1.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    CHECK(id[6] == 0.0); // normal reaction algebraic
    CHECK(id[7] == 0.0); // no-slip friction algebraic
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: distance joint adds one algebraic multiplier")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.1 } },
        json { { "kind", "distanceJoint" }, { "body", 0 }, { "ax", 0.0 }, { "ay", 0.0 },
            { "body2", -1 }, { "px", 0.0 }, { "py", 0.0 }, { "length", 1.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    REQUIRE(layout.totalContinuous == 7); // 6 body + 1 lambda

    std::vector<double> id(7, 1.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    for (int k = 0; k < 6; ++k) {
        CHECK(id[static_cast<size_t>(k)] == 1.0);
    }
    CHECK(id[6] == 0.0); // rod tension algebraic
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: free-body DERIVATIVE is gravity only")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 2.0 }, { "I", 0.5 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // State (xc,yc,phi,vx,vy,w) = (0,0,0, 3,4,5).
    h.x = { 0.0, 0.0, 0.0, 3.0, 4.0, 5.0 };
    h.xdot.assign(6, 0.0);
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 3.0); // xc' = vx
    checkClose(h.xdot[1], 4.0); // yc' = vy
    checkClose(h.xdot[2], 5.0); // phi' = w
    checkClose(h.xdot[3], 0.0); // vx' = 0
    checkClose(h.xdot[4], -9.81); // vy' = -g
    checkClose(h.xdot[5], 0.0); // w' = 0 (no torque)
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: force at an offset point produces a torque")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 0.0;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 2.0 }, { "I", 0.5 } },
        // Fy = 3 applied at body point (4, 0): torque about COM = rx*Fy = 4*3.
        json { { "kind", "forceY" }, { "body", 0 }, { "F", 3.0 }, { "ax", 4.0 }, { "ay", 0.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    h.x.assign(6, 0.0); // phi = 0 -> r = (4, 0)
    h.xdot.assign(6, 0.0);
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[4], 3.0 / 2.0); // vy' = Fy/m
    checkClose(h.xdot[5], 4.0 * 3.0 / 0.5); // w' = (rx*Fy)/I = 12/0.5 = 24
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: point mass freezes rotation and tolerates I = 0")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        // I = 0 is allowed for a point mass (kept invertible internally).
        json { { "kind", "body" }, { "body", 0 }, { "m", 2.0 }, { "I", 0.0 }, { "pointMass", 1 } },
        json { { "kind", "torque" }, { "body", 0 }, { "F", 5.0 } }, // ignored (frozen)
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    h.x = { 0.0, 0.0, 0.0, 1.0, 2.0, 0.0 };
    h.xdot.assign(6, 0.0);
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 1.0);
    checkClose(h.xdot[1], 2.0);
    checkClose(h.xdot[2], 0.0); // phi' frozen
    checkClose(h.xdot[4], -9.81); // ay = -g
    checkClose(h.xdot[5], 0.0); // w' frozen despite the applied torque
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: constant torque gives w' = tau / I")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 0.0;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.2 } },
        json { { "kind", "torque" }, { "body", 0 }, { "F", 1.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    h.x.assign(6, 0.0);
    h.xdot.assign(6, 0.0);
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[5], 5.0); // w' = tau/I = 1/0.2
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: frame / relative / distance sensors")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 0.0;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.1 } },
        // frame point at body offset (2, 0); with phi = pi/2 it rotates to (0, 2).
        json { { "kind", "framePositionX" }, { "body", 0 }, { "ax", 2.0 }, { "outPort", 0 } },
        json { { "kind", "framePositionY" }, { "body", 0 }, { "ax", 2.0 }, { "outPort", 1 } },
        // velocity of that point = v_COM + w x R a; with w = 1, R a = (0,2): (-2, 0)+v.
        json { { "kind", "frameVelocityX" }, { "body", 0 }, { "ax", 2.0 }, { "outPort", 2 } },
        json { { "kind", "frameVelocityY" }, { "body", 0 }, { "ax", 2.0 }, { "outPort", 3 } },
        // distance from the COM to the world origin.
        json { { "kind", "distanceSensor" }, { "body", 0 }, { "ax", 0.0 }, { "body2", -1 },
            { "px", 0.0 }, { "py", 0.0 }, { "outPort", 4 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 5, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // COM (3,4), phi=pi/2, v=(0.5,0), w=1. R(pi/2)(2,0) = (0,2).
    h.x = { 3.0, 4.0, std::acos(-1.0) / 2.0, 0.5, 0.0, 1.0 };
    h.run(isl, Phase::OUTPUT);
    checkClose(h.output(isl, 0), 3.0); // frame X = 3 + 0
    checkClose(h.output(isl, 1), 6.0); // frame Y = 4 + 2
    checkClose(h.output(isl, 2), 0.5 - 1.0 * 2.0); // vx - w*ry = 0.5 - 2 = -1.5
    checkClose(h.output(isl, 3), 0.0 + 1.0 * 0.0); // vy + w*rx = 0
    checkClose(h.output(isl, 4), 5.0); // |(3,4)| = 5
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: acceleration sensor reads the cached COM acceleration")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 2.0 }, { "I", 0.5 } },
        json { { "kind", "forceX" }, { "body", 0 }, { "F", 6.0 } },
        json { { "kind", "accelSensorX" }, { "body", 0 }, { "outPort", 0 } },
        json { { "kind", "accelSensorY" }, { "body", 0 }, { "outPort", 1 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 2, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    h.x.assign(6, 0.0);
    h.xdot.assign(6, 0.0);
    h.run(isl, Phase::DERIVATIVE); // caches (ax, ay, alpha)
    h.run(isl, Phase::OUTPUT);
    checkClose(h.output(isl, 0), 6.0 / 2.0); // ax = Fx/m = 3
    checkClose(h.output(isl, 1), -9.81); // ay = -g
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: relative torque drives two bodies oppositely")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 2;
    params["gravity"] = 0.0;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.5 } },
        json { { "kind", "body" }, { "body", 1 }, { "m", 1.0 }, { "I", 0.25 } },
        json { { "kind", "relativeTorque" }, { "body", 0 }, { "body2", 1 }, { "F", 2.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    h.x.assign(12, 0.0);
    h.xdot.assign(12, 0.0);
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[5], 2.0 / 0.5); // body 0: w' = +tau/I0 = 4
    checkClose(h.xdot[11], -2.0 / 0.25); // body 1: w' = -tau/I1 = -8
    (void)isl;
}
//=============================================================================
TEST_CASE("planarIsland: position/speed sensors read the pose")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "planar";
    params["bodies"] = 1;
    params["gravity"] = 9.81;
    params["components"] = json::array({
        json { { "kind", "body" }, { "body", 0 }, { "m", 1.0 }, { "I", 0.1 } },
        json { { "kind", "positionSensorX" }, { "body", 0 }, { "outPort", 0 } },
        json { { "kind", "positionSensorPhi" }, { "body", 0 }, { "outPort", 1 } },
        json { { "kind", "speedSensorY" }, { "body", 0 }, { "outPort", 2 } },
    });
    Harness h;
    BlockId isl = h.addBlock("p1", "planarMechanicalIsland", 0, 3, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    h.x = { 1.5, 2.5, 0.75, 3.0, 4.0, 5.0 };
    h.run(isl, Phase::OUTPUT);
    checkClose(h.output(isl, 0), 1.5); // xc
    checkClose(h.output(isl, 1), 0.75); // phi
    checkClose(h.output(isl, 2), 4.0); // vy
    (void)isl;
}
//=============================================================================
