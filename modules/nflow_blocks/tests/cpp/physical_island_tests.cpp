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
// The physicalIsland block (acausal) assembles an electrical netlist into a
// mixed differential/algebraic continuous-state slice. These doctests verify
// the layout contract the engine relies on (state count + per-state id mask)
// and the residual arithmetic in a single DERIVATIVE evaluation; the analytic
// transient behaviour is covered end-to-end by test_nflow_acausal_island.m.
TEST_CASE("physicalIsland: RC island state count and mixed id mask")
{
    ensureHandlersRegistered();
    // Nodes: 0 = ground, 1 = source top, 2 = R/C junction.
    // V(node1-0), R(node1-node2), C(node2-0).
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "resistor" }, { "a", 1 }, { "b", 2 }, { "R", 1.0 } },
        json { { "kind", "capacitor" }, { "a", 2 }, { "b", 0 }, { "C", 1.0 }, { "ic", 0.0 } },
    });

    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 0, params);

    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    // States: e1, e2 (algebraic), iV (algebraic), vC (diff), iC (algebraic) = 5.
    REQUIRE(layout.totalContinuous == 5);
    CHECK(h.blockState[isl].xOffset == 0);
    CHECK(h.blockState[isl].xLength == 5);

    std::vector<double> id(5, 1.0);
    fillDaeIds(h.blocks, h.blockState, BlockRegistry::instance(), h.variables, id);
    // Order: e1, e2, [vsource iV], [capacitor vC, iC].
    CHECK(id[0] == 0.0); // e1 algebraic
    CHECK(id[1] == 0.0); // e2 algebraic
    CHECK(id[2] == 0.0); // iV algebraic
    CHECK(id[3] == 1.0); // vC differential
    CHECK(id[4] == 0.0); // iC algebraic
}
//=============================================================================
TEST_CASE("physicalIsland: DERIVATIVE writes MNA residual pieces")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "resistor" }, { "a", 1 }, { "b", 2 }, { "R", 1.0 } },
        json { { "kind", "capacitor" }, { "a", 2 }, { "b", 0 }, { "C", 2.0 }, { "ic", 0.0 } },
    });

    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);

    // Pick a state: e1 = 1, e2 = 0.4, iV = 0.6, vC = 0.4, iC = 0.6.
    // Residuals expected:
    //   node1 KCL: iV (from vsource, leaves 1) + (e1-e2)/R (resistor leaves 1)
    //            = 0.6 + (1-0.4)/1 = 1.2
    //   node2 KCL: -(e1-e2)/R (resistor enters 2) + iC (cap leaves 2)
    //            = -0.6 + 0.6 = 0.0
    //   iV residual: (e1-e0) - V = 1 - 1 = 0
    //   vC deriv:    iC / C = 0.6 / 2 = 0.3
    //   iC residual: vC - (e2 - e0) = 0.4 - 0.4 = 0
    h.x = { 1.0, 0.4, 0.6, 0.4, 0.6 };
    h.xdot = { 0.0, 0.0, 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 1.2); // node1 KCL
    checkClose(h.xdot[1], 0.0); // node2 KCL
    checkClose(h.xdot[2], 0.0); // vsource residual
    checkClose(h.xdot[3], 0.3); // vC'
    checkClose(h.xdot[4], 0.0); // iC residual
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: VCVS residual and VCCS/conductor KCL")
{
    ensureHandlersRegistered();
    // node1 held at 1 V by a source; a VCVS (gain 3) drives node2 from node1;
    // a conductor G=2 from node2 to ground; a VCCS (gain 0.5) from node1 to
    // ground controlled by node2. Just check the DERIVATIVE residual pieces.
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json {
            { "kind", "vcvs" }, { "a", 2 }, { "b", 0 }, { "c", 1 }, { "d", 0 }, { "gain", 3.0 } },
        json { { "kind", "conductor" }, { "a", 2 }, { "b", 0 }, { "G", 2.0 } },
        json {
            { "kind", "vccs" }, { "a", 1 }, { "b", 0 }, { "c", 2 }, { "d", 0 }, { "gain", 0.5 } },
    });
    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // States: e1, e2, iV(vsource), iVcvs. Set e1=1, e2=3 (the solved point),
    // iV = -1.5 (chosen), iVcvs = 6.
    // node1 KCL: iV(leaves1) + iVccs(=0.5*(e2-e0)=1.5, leaves1) = iV + 1.5
    // node2 KCL: iVcvs(leaves2) + conductor 2*(e2-0)=6 (leaves2) = iVcvs + 6
    // vsource residual: e1 - 1
    // vcvs residual: (e2-0) - 3*(e1-0) = 3 - 3 = 0
    h.x = { 1.0, 3.0, -1.5, -6.0 };
    h.xdot = { 0.0, 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], -1.5 + 1.5); // node1 KCL = iV + iVccs = 0
    checkClose(h.xdot[1], -6.0 + 6.0); // node2 KCL = iVcvs + conductor = 0
    checkClose(h.xdot[2], 0.0); // vsource residual e1-1
    checkClose(h.xdot[3], 0.0); // vcvs residual e2 - 3*e1
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: short and idle residuals")
{
    ensureHandlersRegistered();
    // Short between nodes 1,2 -> residual e1 - e2; Idle between 2,0 -> residual i.
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "short" }, { "a", 1 }, { "b", 2 } },
        json { { "kind", "idle" }, { "a", 2 }, { "b", 0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // States: e1, e2, iV, iShort, iIdle. Set e1=1, e2=0.7, iIdle=0.4.
    h.x = { 1.0, 0.7, 0.0, 0.0, 0.4 };
    h.xdot = { 0.0, 0.0, 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[3], 1.0 - 0.7); // short residual e1 - e2
    checkClose(h.xdot[4], 0.4); // idle residual = i
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: ideal transformer couples primary and secondary")
{
    ensureHandlersRegistered();
    // Primary source at node1; transformer primary (1-0) secondary (2-0), n=2;
    // resistor load on secondary. Check the DERIVATIVE residual pieces.
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "transformer" }, { "a", 1 }, { "b", 0 }, { "c", 2 }, { "d", 0 },
            { "n", 2.0 } },
        json { { "kind", "resistor" }, { "a", 2 }, { "b", 0 }, { "R", 1.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // States: e1, e2, iV(vsource), i1(transformer). Set e1=1, e2=0.5, iV=0.25,
    // i1=0.25. Only the residual identities are asserted, not the node KCL
    // balance, which the solver drives to zero by choosing e2 and i1:
    //   transformer residual: (e1-0) - n(e2-0) = 1 - 2*0.5 = 0
    //   vsource residual: e1 - 1 = 0
    h.x = { 1.0, 0.5, 0.25, 0.25 };
    h.xdot = { 0.0, 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[3], 1.0 - 2.0 * 0.5); // transformer voltage residual = 0
    checkClose(h.xdot[2], 1.0 - 1.0); // vsource residual e1 - 1 = 0
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: ideal switch residual toggles with control")
{
    ensureHandlersRegistered();
    // vsource(1-0) - switch(1-2, control on port 0) - resistor(2-0).
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "switch" }, { "a", 1 }, { "b", 2 }, { "port", 0 } },
        json { { "kind", "resistor" }, { "a", 2 }, { "b", 0 }, { "R", 1.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 1, 0, params);
    BlockId ctl = h.addInput(1.0); // control = 1 (closed)
    h.connect(ctl, isl);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // States: e1, e2, iV, iSw. Closed: residual = e1 - e2.
    h.x = { 1.0, 0.4, 0.6, 0.6 };
    h.xdot = { 0.0, 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[3], 1.0 - 0.4); // closed: v = e1 - e2

    h.setOutputValue(ctl, 0.0); // control = 0 (open)
    h.xdot = { 0.0, 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[3], 0.6); // open: residual = i = 0.6
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: variable resistor reads R from a signal input")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "variableResistor" }, { "a", 1 }, { "b", 2 }, { "port", 0 } },
        json { { "kind", "resistor" }, { "a", 2 }, { "b", 0 }, { "R", 1.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 1, 0, params);
    BlockId rin = h.addInput(4.0); // R = 4
    h.connect(rin, isl);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // States: e1, e2, iV. e1=1, e2=0.5. Variable R=4: i = (1-0.5)/4 = 0.125.
    h.x = { 1.0, 0.5, 0.0 };
    h.xdot = { 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    // node1 KCL: iV(0) + i(0.125); node2: -i + e2/R(1) = -0.125 + 0.5.
    checkClose(h.xdot[0], 0.0 + 0.125);
    checkClose(h.xdot[1], -0.125 + 0.5);
    // Change R to 0.5 -> i = 0.5/0.5 = 1.0.
    h.setOutputValue(rin, 0.5);
    h.xdot = { 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 0.0 + 1.0);
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: gyrator KCL contributions")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "gyrator" }, { "a", 1 }, { "b", 0 }, { "c", 2 }, { "d", 0 }, { "G1", 1.0 },
            { "G2", 1.0 } },
        json { { "kind", "resistor" }, { "a", 2 }, { "b", 0 }, { "R", 2.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // States: e1, e2, iV. At the solved point e1=1, e2=2:
    //   gyrator i1 = G2 v2 = 2 (leaves node1), i2 = -G1 v1 = -1 (into node2)
    //   node2 KCL: i2(-1) + resistor e2/R=1 = 0.
    h.x = { 1.0, 2.0, 0.0 };
    h.xdot = { 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[1], -1.0 + 2.0 / 2.0); // node2 KCL = i2 + e2/R = 0
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: thermal radiation T^4 KCL contribution")
{
    ensureHandlersRegistered();
    // A body (node1) radiating to a fixed 2-unit temperature (node2 via vsource).
    json params;
    params["domain"] = "thermal";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "capacitor" }, { "a", 1 }, { "b", 0 }, { "C", 1.0 } },
        json { { "kind", "radiation" }, { "a", 1 }, { "b", 2 }, { "Gr", 0.5 } },
        json { { "kind", "vsource" }, { "a", 2 }, { "b", 0 }, { "V", 2.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // States: e1, e2, vC, iC, iV. T1=3, T2=2: q = 0.5 (3^4 - 2^4) = 0.5*(81-16)=32.5.
    // node1 KCL: iC(leaves via cap) + q(radiation leaves 1). Set e1=3,e2=2,iC=1.
    h.x = { 3.0, 2.0, 3.0, 1.0, 0.0 };
    h.xdot = { 0.0, 0.0, 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    checkClose(h.xdot[0], 1.0 + 32.5); // node1 KCL: iC + radiation
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: exponential diode KCL contribution")
{
    ensureHandlersRegistered();
    // vsource at node1; diode node1->node2; resistor node2->ground. Just check
    // the diode current appears in both node KCL residuals with opposite sign.
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "diode" }, { "a", 1 }, { "b", 2 }, { "Is", 1e-9 }, { "Vt", 0.025 } },
        json { { "kind", "resistor" }, { "a", 2 }, { "b", 0 }, { "R", 1000.0 } },
    });
    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 0, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    // States: e1, e2, iV(vsource). Set e1=1, e2=0.4, iV=0.
    // diode vd = e1-e2 = 0.6, i = 1e-9 (exp(0.6/0.025)-1) = 1e-9 (exp(24)-1).
    const double id = 1e-9 * (std::exp(0.6 / 0.025) - 1.0);
    h.x = { 1.0, 0.4, 0.0 };
    h.xdot = { 0.0, 0.0, 0.0 };
    h.run(isl, Phase::DERIVATIVE);
    // node1 KCL: iV(0) + i_diode(leaves 1); node2 KCL: -i_diode + e2/R (resistor).
    checkClose(h.xdot[0], 0.0 + id, 1e-6);
    checkClose(h.xdot[1], -id + 0.4 / 1000.0, 1e-6);
    (void)isl;
}
//=============================================================================
TEST_CASE("physicalIsland: sensors read node potentials in OUTPUT")
{
    ensureHandlersRegistered();
    json params;
    params["domain"] = "electrical";
    params["nodes"] = 3;
    params["components"] = json::array({
        json { { "kind", "vsource" }, { "a", 1 }, { "b", 0 }, { "V", 1.0 } },
        json { { "kind", "resistor" }, { "a", 1 }, { "b", 2 }, { "R", 1.0 } },
        json { { "kind", "capacitor" }, { "a", 2 }, { "b", 0 }, { "C", 1.0 } },
        json { { "kind", "voltageSensor" }, { "a", 2 }, { "b", 0 }, { "outPort", 0 } },
        json { { "kind", "potentialSensor" }, { "a", 1 }, { "outPort", 1 } },
    });

    Harness h;
    BlockId isl = h.addBlock("isl", "physicalIsland", 0, 2, params);
    buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);
    h.run(isl, Phase::INIT);
    h.x = { 1.0, 0.7, 0.3, 0.7, 0.3 }; // e1=1, e2=0.7, ...
    h.run(isl, Phase::OUTPUT);
    checkClose(h.output(isl, 0), 0.7); // voltageSensor node2 - ground
    checkClose(h.output(isl, 1), 1.0); // potentialSensor node1
    (void)isl;
}
//=============================================================================
