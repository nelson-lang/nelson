//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Rip-up-and-reroute refinement invariants: quality never gets worse, a
// rolled-back refine is a no-op, and results are deterministic.
//=============================================================================
#include "doctest.h"
#include "RouterPathfinder.hpp"
#include "RouterTypes.hpp"
#include <algorithm>
#include <set>
#include <utility>
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
namespace {
//=============================================================================
CostOpts
refineOpts(int refineIterations)
{
    CostOpts opts;
    opts.deterministic = true;
    opts.incremental = false;
    opts.fullOptimize = true;
    opts.nearObstaclePenalty1 = 2.0;
    opts.nearObstaclePenalty2 = 0.0;
    opts.nearWirePenalty1 = 3.0;
    opts.nearWirePenalty2 = 1.0;
    opts.refineIterations = refineIterations;
    return opts;
}
//=============================================================================
// Crossing ladder: the hops are unavoidable, so refine must roll back to identical routes.
ComputeWireRoutesParams
ladderParams(int refineIterations)
{
    ComputeWireRoutesParams params;
    for (int i = 0; i < 4; ++i) {
        RouterNode l;
        l.id = "L" + std::to_string(i) + ":out:0";
        l.x = 0;
        l.y = i * 6;
        l.dir = "right";
        RouterNode r;
        r.id = "R" + std::to_string(i) + ":in:0";
        r.x = 30;
        r.y = i * 6;
        r.dir = "left";
        params.nodes.push_back(l);
        params.nodes.push_back(r);
    }
    for (int i = 0; i < 4; ++i) {
        RouterConnInput c;
        c.from = "L" + std::to_string(i) + ":out:0";
        c.to = "R" + std::to_string(3 - i) + ":in:0";
        c.key = "k" + std::to_string(i);
        params.connections.push_back(c);
    }
    params.opts = refineOpts(refineIterations);
    return params;
}
//=============================================================================
std::string
shapeOf(const RouteWiresResult& res)
{
    std::vector<std::string> keys;
    for (const auto& [key, _wr] : res.wires) {
        keys.push_back(key);
    }
    std::sort(keys.begin(), keys.end());
    std::string out;
    for (const auto& key : keys) {
        out += key + '=';
        for (const auto& p : res.wires.at(key).points) {
            out += std::to_string(p.x) + ',' + std::to_string(p.y) + ';';
        }
        out += '|';
    }
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
// Counts adjacent cell pairs between different nets.
static int
adjacencyOf(const RouteWiresResult& res)
{
    std::vector<const WireRouteResult*> wires;
    for (const auto& [key, wr] : res.wires) {
        (void)key;
        wires.push_back(&wr);
    }
    std::sort(wires.begin(), wires.end(),
        [](const WireRouteResult* a, const WireRouteResult* b) { return a->key < b->key; });
    int adj = 0;
    for (size_t i = 0; i < wires.size(); ++i) {
        for (size_t j = i + 1; j < wires.size(); ++j) {
            std::set<std::pair<int, int>> setJ;
            for (const auto& p : wires[j]->points) {
                setJ.insert({ p.x, p.y });
            }
            static const int dirs[4][2] = { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } };
            for (const auto& p : wires[i]->points) {
                for (const auto& d : dirs) {
                    if (setJ.count({ p.x + d[0], p.y + d[1] })) {
                        ++adj;
                    }
                }
            }
        }
    }
    return adj;
}
//=============================================================================
TEST_CASE("refine: a-posteriori scoring strictly reduces inter-wire adjacency")
{
    // The costs of early wires at routing time never see later wires; only the
    // re-scoring afterwards makes them candidates. Hops stay put.
    auto off = computeWireRoutes(ladderParams(0));
    auto on = computeWireRoutes(ladderParams(2));
    CHECK(adjacencyOf(on) < adjacencyOf(off));
    CHECK(on.costs.hops == off.costs.hops);
    auto more = computeWireRoutes(ladderParams(3));
    CHECK(adjacencyOf(more) == adjacencyOf(on));
}
//=============================================================================
TEST_CASE("refine: deterministic across repeated runs")
{
    auto first = shapeOf(computeWireRoutes(ladderParams(2)));
    for (int i = 0; i < 5; ++i) {
        CHECK(shapeOf(computeWireRoutes(ladderParams(2))) == first);
    }
}
//=============================================================================
TEST_CASE("refine: clean layouts are untouched (no badness, early exit)")
{
    ComputeWireRoutesParams base;
    RouterNode a { "a:out:0", 0, 0, "right" };
    RouterNode b { "b:in:0", 20, 0, "left" };
    RouterNode c { "c:out:0", 0, 8, "right" };
    RouterNode d { "d:in:0", 20, 8, "left" };
    base.nodes = { a, b, c, d };
    RouterConnInput c1;
    c1.from = "a:out:0";
    c1.to = "b:in:0";
    c1.key = "k1";
    RouterConnInput c2;
    c2.from = "c:out:0";
    c2.to = "d:in:0";
    c2.key = "k2";
    base.connections = { c1, c2 };

    auto p0 = base;
    p0.opts = refineOpts(0);
    auto off = computeWireRoutes(std::move(p0));
    auto p2 = base;
    p2.opts = refineOpts(2);
    auto on = computeWireRoutes(std::move(p2));
    CHECK(shapeOf(on) == shapeOf(off));
    CHECK(on.costs.hops == 0);
}
//=============================================================================
