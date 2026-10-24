//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "doctest.h"
#include "RouterEngine.hpp"
#include "RouterPathfinder.hpp"
#include "RouterTypes.hpp"
#include <algorithm>
#include <nlohmann/json.hpp>
#include <random>
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
namespace {
//=============================================================================
CostOpts
deterministicOpts()
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
RouterNode
makeNode(const std::string& id, int x, int y, const std::string& dir)
{
    RouterNode n;
    n.id = id;
    n.x = x;
    n.y = y;
    n.dir = dir;
    return n;
}
//=============================================================================
RouterConnInput
makeConn(const std::string& from, const std::string& to, const std::string& key)
{
    RouterConnInput c;
    c.from = from;
    c.to = to;
    c.key = key;
    return c;
}
//=============================================================================
bool
pathIsOrthogonal(const std::vector<RPoint>& pts)
{
    for (size_t i = 1; i < pts.size(); ++i) {
        int dx = pts[i].x - pts[i - 1].x;
        int dy = pts[i].y - pts[i - 1].y;
        if (dx != 0 && dy != 0) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
pathAvoids(const std::vector<RPoint>& pts, const RObstacle& obs)
{
    for (size_t i = 1; i + 1 < pts.size(); ++i) {
        const auto& p = pts[i];
        if (p.x >= obs.x0 && p.x <= obs.x1 && p.y >= obs.y0 && p.y <= obs.y1) {
            return false;
        }
    }
    return true;
}
//=============================================================================
std::string
pathSignature(const std::vector<RPoint>& pts)
{
    std::string sig;
    for (const auto& p : pts) {
        sig += std::to_string(p.x) + ',' + std::to_string(p.y) + ';';
    }
    return sig;
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("router unit: straight connection routes as a single segment")
{
    ComputeWireRoutesParams params;
    params.nodes = { makeNode("a:out:0", 0, 0, "right"), makeNode("b:in:0", 10, 0, "left") };
    params.connections = { makeConn("a:out:0", "b:in:0", "k1") };
    params.opts = deterministicOpts();

    auto result = computeWireRoutes(std::move(params));
    REQUIRE(result.wires.count("k1") == 1);
    const auto& wr = result.wires.at("k1");
    REQUIRE(wr.points.size() >= 2);
    CHECK(wr.points.front().x == 0);
    CHECK(wr.points.front().y == 0);
    CHECK(wr.points.back().x == 10);
    CHECK(wr.points.back().y == 0);
    CHECK(wr.cost.turns == 0);
    CHECK(pathIsOrthogonal(wr.points));
}
//=============================================================================
TEST_CASE("router unit: obstacle forces a detour around the block")
{
    ComputeWireRoutesParams params;
    params.nodes = { makeNode("a:out:0", 0, 0, "right"), makeNode("b:in:0", 20, 0, "left") };
    params.connections = { makeConn("a:out:0", "b:in:0", "k1") };
    RObstacle obs;
    obs.x0 = 8;
    obs.y0 = -3;
    obs.x1 = 12;
    obs.y1 = 3;
    obs.owner = "blocker";
    params.obstacles = { obs };
    params.opts = deterministicOpts();

    auto result = computeWireRoutes(std::move(params));
    REQUIRE(result.wires.count("k1") == 1);
    const auto& wr = result.wires.at("k1");
    REQUIRE(wr.points.size() >= 2);
    CHECK(pathIsOrthogonal(wr.points));
    CHECK(pathAvoids(wr.points, obs));
    CHECK(wr.cost.turns >= 2);
}
//=============================================================================
TEST_CASE("router unit: results are byte-stable across repeated runs")
{
    auto runOnce = []() {
        ComputeWireRoutesParams params;
        params.nodes = {
            makeNode("a:out:0", 0, 0, "right"),
            makeNode("b:in:0", 20, 0, "left"),
            makeNode("c:out:0", 0, 6, "right"),
            makeNode("d:in:0", 20, 6, "left"),
            makeNode("e:out:0", 0, 12, "right"),
            makeNode("f:in:0", 20, 12, "left"),
        };
        params.connections = {
            makeConn("a:out:0", "d:in:0", "k1"),
            makeConn("c:out:0", "f:in:0", "k2"),
            makeConn("e:out:0", "b:in:0", "k3"),
        };
        params.opts = deterministicOpts();
        auto result = computeWireRoutes(std::move(params));
        std::string sig;
        for (const auto& key : { "k1", "k2", "k3" }) {
            auto it = result.wires.find(key);
            sig += std::string(key) + '=';
            sig += (it != result.wires.end()) ? pathSignature(it->second.points) : "none";
            sig += '|';
        }
        return sig;
    };

    auto first = runOnce();
    for (int i = 0; i < 9; ++i) {
        CHECK(runOnce() == first);
    }
}
//=============================================================================
TEST_CASE("router unit: connection array order does not change results")
{
    auto runWithOrder = [](std::vector<int> orderIdx) {
        std::vector<RouterConnInput> base = {
            makeConn("a:out:0", "d:in:0", "k1"),
            makeConn("c:out:0", "f:in:0", "k2"),
            makeConn("e:out:0", "b:in:0", "k3"),
        };
        ComputeWireRoutesParams params;
        params.nodes = {
            makeNode("a:out:0", 0, 0, "right"),
            makeNode("b:in:0", 20, 0, "left"),
            makeNode("c:out:0", 0, 6, "right"),
            makeNode("d:in:0", 20, 6, "left"),
            makeNode("e:out:0", 0, 12, "right"),
            makeNode("f:in:0", 20, 12, "left"),
        };
        for (int i : orderIdx) {
            params.connections.push_back(base[i]);
        }
        params.opts = deterministicOpts();
        auto result = computeWireRoutes(std::move(params));
        std::string sig;
        for (const auto& key : { "k1", "k2", "k3" }) {
            auto it = result.wires.find(key);
            sig += std::string(key) + '=';
            sig += (it != result.wires.end()) ? pathSignature(it->second.points) : "none";
            sig += '|';
        }
        return sig;
    };

    // All six permutations of the input must give identical routes: the routing
    // order (score desc, connection key asc) is content-based.
    auto reference = runWithOrder({ 0, 1, 2 });
    std::vector<int> perm = { 0, 1, 2 };
    while (std::next_permutation(perm.begin(), perm.end())) {
        CHECK(runWithOrder(perm) == reference);
    }
}
//=============================================================================
TEST_CASE("router unit: feedback connection (from right of to) still routes")
{
    ComputeWireRoutesParams params;
    params.nodes = { makeNode("a:out:0", 20, 0, "right"), makeNode("b:in:0", 0, 0, "left") };
    params.connections = { makeConn("a:out:0", "b:in:0", "k1") };
    params.opts = deterministicOpts();

    auto result = computeWireRoutes(std::move(params));
    REQUIRE(result.wires.count("k1") == 1);
    const auto& wr = result.wires.at("k1");
    REQUIRE(wr.points.size() >= 4);
    CHECK(pathIsOrthogonal(wr.points));
    CHECK(wr.points.front().x == 20);
    CHECK(wr.points.back().x == 0);
}
//=============================================================================
TEST_CASE("router unit: pinned wires echo their shape verbatim through routeWires")
{
    using json = nlohmann::json;
    json input;
    input["deterministic"] = true;
    input["width"] = 800;
    input["height"] = 600;
    input["blocks"] = json::array();
    auto makeBlock = [](const char* id, double x, double y, bool out) {
        json b;
        b["id"] = id;
        b["x"] = x;
        b["y"] = y;
        b["width"] = 80;
        b["height"] = 40;
        b["rotation"] = 0;
        json port;
        port["type"] = out ? "out" : "in";
        port["index"] = 0;
        port["x"] = out ? 80 : 0;
        port["y"] = 20;
        b["ports"] = json::array({ port });
        return b;
    };
    input["blocks"].push_back(makeBlock("a", 100, 100, true)); // out port (180,120)
    input["blocks"].push_back(makeBlock("b", 500, 100, false)); // in port (500,120)

    json connJ;
    connJ["from"] = "a";
    connJ["to"] = "b";
    connJ["fromIndex"] = 0;
    connJ["toIndex"] = 0;
    connJ["points"] = json::array({ json::array({ 180, 120 }), json::array({ 300, 120, 1 }),
        json::array({ 300, 220, 1 }), json::array({ 420, 220, 1 }), json::array({ 420, 120, 1 }),
        json::array({ 500, 120 }) });
    input["connections"] = json::array({ connJ });

    auto result = json::parse(Nelson::NFlow::routeWires(input.dump()));
    REQUIRE(result.value("ok", false));
    const auto& route = result["routes"][0];
    REQUIRE(route.is_array());
    std::vector<std::pair<int, int>> got;
    for (const auto& p : route) {
        got.push_back({ p["x"].get<int>(), p["y"].get<int>() });
    }
    std::vector<std::pair<int, int>> expected
        = { { 180, 120 }, { 300, 120 }, { 300, 220 }, { 420, 220 }, { 420, 120 }, { 500, 120 } };
    CHECK(got == expected);
}
//=============================================================================
TEST_CASE("router unit: deterministic mode ignores wall-clock limits")
{
    ComputeWireRoutesParams params;
    params.nodes = { makeNode("a:out:0", 0, 0, "right"), makeNode("b:in:0", 40, 0, "left") };
    params.connections = { makeConn("a:out:0", "b:in:0", "k1") };
    params.opts = deterministicOpts();
    params.opts.maxTimeMs = 0; // would fail instantly without deterministic mode

    auto result = computeWireRoutes(std::move(params));
    CHECK(result.wires.count("k1") == 1);
    CHECK(result.failedKeys.empty());
}
//=============================================================================
