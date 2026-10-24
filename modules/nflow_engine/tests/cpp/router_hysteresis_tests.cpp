//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Route hysteresis: reroutes keep the previous shape unless the fresh route is
// clearly better.
//=============================================================================
#include "doctest.h"
#include "RouterPathfinder.hpp"
#include "RouterTypes.hpp"
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
namespace {
//=============================================================================
CostOpts
hystOpts()
{
    CostOpts opts;
    opts.deterministic = true;
    opts.incremental = false;
    opts.fullOptimize = true;
    opts.nearObstaclePenalty1 = 2.0;
    opts.nearObstaclePenalty2 = 0.0;
    opts.nearWirePenalty1 = 3.0;
    opts.nearWirePenalty2 = 1.0;
    return opts;
}
//=============================================================================
// Diagonal pair: (0,0) right-exit → (10,10) left-entry; any vertical spine
// x∈[2..8] yields an equal-cost route.
ComputeWireRoutesParams
diagonalParams()
{
    ComputeWireRoutesParams params;
    RouterNode a;
    a.id = "a:out:0";
    a.x = 0;
    a.y = 0;
    a.dir = "right";
    RouterNode b;
    b.id = "b:in:0";
    b.x = 10;
    b.y = 10;
    b.dir = "left";
    params.nodes = { a, b };
    RouterConnInput conn;
    conn.from = "a:out:0";
    conn.to = "b:in:0";
    conn.key = "k1";
    params.connections = { conn };
    params.opts = hystOpts();
    return params;
}
//=============================================================================
std::vector<RPoint>
spinePath(int spineX)
{
    std::vector<RPoint> pts;
    for (int x = 0; x <= spineX; ++x) {
        pts.push_back({ x, 0 });
    }
    for (int y = 1; y <= 10; ++y) {
        pts.push_back({ spineX, y });
    }
    for (int x = spineX + 1; x <= 10; ++x) {
        pts.push_back({ x, 10 });
    }
    return pts;
}
//=============================================================================
std::string
sig(const std::vector<RPoint>& pts)
{
    std::string out;
    for (const auto& p : pts) {
        out += std::to_string(p.x) + ',' + std::to_string(p.y) + ';';
    }
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("hysteresis: keeps an equal-cost previous route instead of flip-flopping")
{
    auto fresh = computeWireRoutes(diagonalParams());
    REQUIRE(fresh.wires.count("k1") == 1);
    int freshSpine = -1;
    for (const auto& p : fresh.wires.at("k1").points) {
        if (p.y == 5) {
            freshSpine = p.x;
            break;
        }
    }
    int altSpine = (freshSpine == 5) ? 6 : 5;
    auto prev = spinePath(altSpine);

    auto params = diagonalParams();
    params.prevWires["k1"] = prev;
    auto rerouted = computeWireRoutes(std::move(params));
    REQUIRE(rerouted.wires.count("k1") == 1);
    CHECK(sig(rerouted.wires.at("k1").points) == sig(prev));
}
//=============================================================================
TEST_CASE("hysteresis: identical environment + own previous route is byte-stable")
{
    auto first = computeWireRoutes(diagonalParams());
    REQUIRE(first.wires.count("k1") == 1);
    auto shape = sig(first.wires.at("k1").points);

    auto current = first.wires.at("k1").points;
    for (int i = 0; i < 20; ++i) {
        auto params = diagonalParams();
        params.prevWires["k1"] = current;
        auto res = computeWireRoutes(std::move(params));
        REQUIRE(res.wires.count("k1") == 1);
        current = res.wires.at("k1").points;
        CHECK(sig(current) == shape);
    }
}
//=============================================================================
TEST_CASE("hysteresis: replaces a much worse previous route")
{
    std::vector<RPoint> detour;
    for (int x = 0; x <= 5; ++x) {
        detour.push_back({ x, 0 });
    }
    for (int y = 1; y <= 20; ++y) {
        detour.push_back({ 5, y });
    }
    for (int y = 19; y >= 10; --y) {
        detour.push_back({ 5, y });
    }
    for (int x = 6; x <= 10; ++x) {
        detour.push_back({ x, 10 });
    }

    auto params = diagonalParams();
    params.prevWires["k1"] = detour;
    auto res = computeWireRoutes(std::move(params));
    REQUIRE(res.wires.count("k1") == 1);
    CHECK(res.wires.at("k1").cost.length < 25);
}
//=============================================================================
TEST_CASE("hysteresis: infeasible previous route (new obstacle) is replaced")
{
    auto params = diagonalParams();
    RObstacle obs;
    obs.x0 = 4;
    obs.y0 = 4;
    obs.x1 = 6;
    obs.y1 = 6;
    obs.owner = "blocker";
    params.obstacles = { obs };
    params.prevWires["k1"] = spinePath(5);
    auto res = computeWireRoutes(std::move(params));
    REQUIRE(res.wires.count("k1") == 1);
    const auto& pts = res.wires.at("k1").points;
    for (size_t i = 1; i + 1 < pts.size(); ++i) {
        bool inside = pts[i].x >= 4 && pts[i].x <= 6 && pts[i].y >= 4 && pts[i].y <= 6;
        CHECK_FALSE(inside);
    }
}
//=============================================================================
TEST_CASE("hysteresis: disabled → router re-derives its own preferred shape")
{
    auto optsOff = hystOpts();
    optsOff.reuseBonus = 0.0;
    optsOff.acceptThreshold = 0.0;

    auto base = diagonalParams();
    base.opts = optsOff;
    auto fresh = computeWireRoutes(std::move(base));
    REQUIRE(fresh.wires.count("k1") == 1);
    int freshSpine = -1;
    for (const auto& p : fresh.wires.at("k1").points) {
        if (p.y == 5) {
            freshSpine = p.x;
            break;
        }
    }
    int altSpine = (freshSpine == 5) ? 6 : 5;

    auto params = diagonalParams();
    params.opts = optsOff;
    params.prevWires["k1"] = spinePath(altSpine);
    auto res = computeWireRoutes(std::move(params));
    REQUIRE(res.wires.count("k1") == 1);
    CHECK(sig(res.wires.at("k1").points) == sig(fresh.wires.at("k1").points));
}
//=============================================================================
