//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// A1.1: strongly-connected-component decomposition of the algebraic graph.
// The engine iterates only the blocks of a genuine loop, never the whole graph.
//=============================================================================
#include "test_helpers.hpp"
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::AlgebraicComponent;
using Nelson::NFlow::BlockId;
using Nelson::NFlow::computeAlgebraicSCCs;
static std::vector<AlgebraicComponent>
decompose(Harness& h, std::vector<int> algIndices)
{
    return computeAlgebraicSCCs(algIndices, h.blocks, h.inputIds, h.outputBase);
}
//=============================================================================
TEST_CASE("computeAlgebraicSCCs: an acyclic chain yields singleton components in order")
{
    Harness h;
    BlockId in = h.addInput(1.0);
    BlockId g1 = h.addBlock("g1", "gain", 1, 1, json { { "gain", 2.0 } });
    BlockId g2 = h.addBlock("g2", "gain", 1, 1, json { { "gain", 3.0 } });
    h.connect(in, g1);
    h.connect(g1, g2);

    auto comps = decompose(h, { (int)g1, (int)g2 });
    REQUIRE(comps.size() == 2);
    CHECK(comps[0].blocks.size() == 1);
    CHECK(comps[0].blocks[0] == (int)g1); // dependency evaluated first
    CHECK_FALSE(comps[0].cyclic);
    CHECK(comps[1].blocks[0] == (int)g2);
    CHECK_FALSE(comps[1].cyclic);
}
//=============================================================================
TEST_CASE("computeAlgebraicSCCs: a two-block feedback loop is one cyclic component")
{
    Harness h;
    BlockId in = h.addInput(1.0);
    BlockId s1 = h.addBlock("s1", "sum", 2, 1, json::object());
    BlockId g1 = h.addBlock("g1", "gain", 1, 1, json { { "gain", 0.5 } });
    h.connect(in, s1, 0); // s1.in0 = external
    h.connect(g1, s1, 1); // s1.in1 = g1  (feedback)
    h.connect(s1, g1, 0); // g1.in0 = s1

    auto comps = decompose(h, { (int)s1, (int)g1 });
    REQUIRE(comps.size() == 1);
    CHECK(comps[0].cyclic);
    CHECK(comps[0].blocks.size() == 2);
    bool hasS1 = false, hasG1 = false;
    for (int b : comps[0].blocks) {
        hasS1 = hasS1 || b == (int)s1;
        hasG1 = hasG1 || b == (int)g1;
    }
    CHECK(hasS1);
    CHECK(hasG1);
}
//=============================================================================
TEST_CASE("computeAlgebraicSCCs: an acyclic block feeding a loop comes before the loop")
{
    Harness h;
    BlockId in = h.addInput(1.0);
    BlockId pre = h.addBlock("pre", "gain", 1, 1, json { { "gain", 2.0 } }); // acyclic feeder
    BlockId s1 = h.addBlock("s1", "sum", 2, 1, json::object());
    BlockId g1 = h.addBlock("g1", "gain", 1, 1, json { { "gain", 0.5 } });
    h.connect(in, pre);
    h.connect(pre, s1, 0);
    h.connect(g1, s1, 1); // feedback
    h.connect(s1, g1, 0);

    auto comps = decompose(h, { (int)pre, (int)s1, (int)g1 });
    REQUIRE(comps.size() == 2);
    CHECK(comps[0].blocks.size() == 1);
    CHECK(comps[0].blocks[0] == (int)pre);
    CHECK_FALSE(comps[0].cyclic);
    CHECK(comps[1].cyclic);
    CHECK(comps[1].blocks.size() == 2);
}
//=============================================================================
TEST_CASE("computeAlgebraicSCCs: a single block wired to itself is a cyclic component")
{
    Harness h;
    BlockId g1 = h.addBlock("g1", "gain", 1, 1, json { { "gain", 0.5 } });
    h.connect(g1, g1);

    auto comps = decompose(h, { (int)g1 });
    REQUIRE(comps.size() == 1);
    CHECK(comps[0].blocks.size() == 1);
    CHECK(comps[0].blocks[0] == (int)g1);
    CHECK(comps[0].cyclic); // 1-block algebraic loop still needs iteration
}
//=============================================================================
TEST_CASE("computeAlgebraicSCCs: empty input yields no components")
{
    Harness h;
    auto comps = decompose(h, {});
    CHECK(comps.empty());
}
//=============================================================================
