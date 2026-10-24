//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Native nudge pass, behavioral mirror of router.nudge.spec.ts: identical
// inputs must produce identical outputs in both implementations (the TS
// spec asserts the same geometries).
//=============================================================================
#include "doctest.h"
#include "RouterEngine.hpp"
#include "RouterNudge.hpp"
#include <nlohmann/json.hpp>
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
namespace {
//=============================================================================
struct OwnedConn
{
    std::vector<RPoint> points;
    std::string from;
    std::string to;
    bool pinned = false;
};
//=============================================================================
std::vector<NudgeConn>
bind(std::vector<OwnedConn>& owned)
{
    std::vector<NudgeConn> out;
    for (auto& c : owned) {
        NudgeConn nc;
        nc.points = &c.points;
        nc.from = c.from;
        nc.to = c.to;
        nc.pinned = c.pinned;
        out.push_back(nc);
    }
    return out;
}
//=============================================================================
std::string
sig(const std::vector<OwnedConn>& conns)
{
    std::string out;
    for (const auto& c : conns) {
        for (const auto& p : c.points) {
            out += std::to_string(p.x) + ',' + std::to_string(p.y) + ';';
        }
        out += '|';
    }
    return out;
}
//=============================================================================
// Two nets forced through the same horizontal channel at y=200.
std::vector<OwnedConn>
overlappingPair()
{
    std::vector<OwnedConn> conns(2);
    conns[0].from = "p";
    conns[0].to = "u";
    conns[0].points
        = { { 100, 100 }, { 110, 100 }, { 110, 200 }, { 400, 200 }, { 400, 120 }, { 410, 120 } };
    conns[1].from = "q";
    conns[1].to = "v";
    conns[1].points
        = { { 100, 300 }, { 110, 300 }, { 110, 200 }, { 400, 200 }, { 400, 320 }, { 410, 320 } };
    return conns;
}
//=============================================================================
int
overlapCount(const std::vector<OwnedConn>& conns)
{
    int count = 0;
    for (size_t i = 0; i < conns.size(); ++i) {
        for (size_t j = i + 1; j < conns.size(); ++j) {
            const auto& A = conns[i].points;
            const auto& B = conns[j].points;
            for (size_t a = 1; a < A.size(); ++a) {
                for (size_t b = 1; b < B.size(); ++b) {
                    bool ah = A[a - 1].y == A[a].y && A[a - 1].x != A[a].x;
                    bool bh = B[b - 1].y == B[b].y && B[b - 1].x != B[b].x;
                    bool av = A[a - 1].x == A[a].x && A[a - 1].y != A[a].y;
                    bool bv = B[b - 1].x == B[b].x && B[b - 1].y != B[b].y;
                    if (ah && bh && A[a].y == B[b].y) {
                        int lo = (std::max)((std::min)(A[a - 1].x, A[a].x),
                            (std::min)(B[b - 1].x, B[b].x));
                        int hi = (std::min)((std::max)(A[a - 1].x, A[a].x),
                            (std::max)(B[b - 1].x, B[b].x));
                        if (hi - lo > 0) {
                            ++count;
                        }
                    } else if (av && bv && A[a].x == B[b].x) {
                        int lo = (std::max)((std::min)(A[a - 1].y, A[a].y),
                            (std::min)(B[b - 1].y, B[b].y));
                        int hi = (std::min)((std::max)(A[a - 1].y, A[a].y),
                            (std::max)(B[b - 1].y, B[b].y));
                        if (hi - lo > 0) {
                            ++count;
                        }
                    }
                }
            }
        }
    }
    return count;
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("nudge: separates two nets sharing an exact channel")
{
    auto conns = overlappingPair();
    CHECK(overlapCount(conns) > 0);
    auto bound = bind(conns);
    int moved = nudgeConnections(bound, {});
    CHECK(moved > 0);
    CHECK(overlapCount(conns) == 0);
    // Offsets are whole grid cells; endpoints untouched.
    for (const auto& c : conns) {
        for (const auto& p : c.points) {
            CHECK(p.x % 10 == 0);
            CHECK(p.y % 10 == 0);
        }
    }
    CHECK(conns[0].points.front().x == 100);
    CHECK(conns[0].points.front().y == 100);
    CHECK(conns[1].points.back().x == 410);
    CHECK(conns[1].points.back().y == 320);
}
//=============================================================================
TEST_CASE("nudge: idempotent, a second pass changes nothing")
{
    auto conns = overlappingPair();
    auto bound = bind(conns);
    nudgeConnections(bound, {});
    auto after = sig(conns);
    auto bound2 = bind(conns);
    int movedAgain = nudgeConnections(bound2, {});
    CHECK(movedAgain == 0);
    CHECK(sig(conns) == after);
}
//=============================================================================
TEST_CASE("nudge: shared-source siblings are not separated")
{
    std::vector<OwnedConn> shared(2);
    shared[0].from = "s";
    shared[0].to = "a";
    shared[0].points = { { 100, 200 }, { 110, 200 }, { 300, 200 }, { 300, 100 }, { 400, 100 } };
    shared[1].from = "s";
    shared[1].to = "b";
    shared[1].points = { { 100, 200 }, { 110, 200 }, { 300, 200 }, { 300, 300 }, { 400, 300 } };
    auto before = sig(shared);
    auto bound = bind(shared);
    int moved = nudgeConnections(bound, {});
    CHECK(moved == 0);
    CHECK(sig(shared) == before);
}
//=============================================================================
TEST_CASE("nudge: reverts a move that would cross a block")
{
    auto conns = overlappingPair();
    std::vector<NudgeRect> rects = {
        { 120.0, 380.0, 150.0, 194.0 }, // wallUp
        { 120.0, 380.0, 206.0, 250.0 }, // wallDown
    };
    auto before = sig(conns);
    auto bound = bind(conns);
    nudgeConnections(bound, rects);
    // No valid separation exists; nothing got worse.
    CHECK(sig(conns) == before);
}
//=============================================================================
TEST_CASE("nudge: pinned wires are immovable")
{
    auto conns = overlappingPair();
    conns[0].pinned = true;
    conns[1].pinned = true;
    auto before = sig(conns);
    auto bound = bind(conns);
    int moved = nudgeConnections(bound, {});
    CHECK(moved == 0);
    CHECK(sig(conns) == before);
}
//=============================================================================
TEST_CASE("nudge: lone-segment centering matches the TS behavior")
{
    // H run at y=200 hugging a block whose bottom edge is at 194 (6 px
    // clearance), open space below.
    std::vector<OwnedConn> conns(2);
    conns[0].from = "a";
    conns[0].to = "b";
    conns[0].points = { { 100, 200 }, { 110, 200 }, { 400, 200 }, { 400, 260 }, { 410, 260 } };
    conns[1].from = "c";
    conns[1].to = "d";
    conns[1].points = { { 100, 400 }, { 110, 400 }, { 400, 400 }, { 410, 400 } };
    std::vector<NudgeRect> rects = { { 150.0, 350.0, 146.0, 194.0 } };
    auto bound = bind(conns);
    int moved = nudgeConnections(bound, rects);
    CHECK(moved > 0);
    // The long run moved to y=210 (guard bend at the stub).
    bool has210 = false;
    for (const auto& p : conns[0].points) {
        if (p.y == 210) {
            has210 = true;
        }
        CHECK(p.y % 10 == 0);
    }
    CHECK(has210);
    CHECK(conns[0].points.front().x == 100);
    CHECK(conns[0].points.front().y == 200);
    CHECK(conns[0].points.back().x == 410);
    CHECK(conns[0].points.back().y == 260);
}
//=============================================================================
TEST_CASE("nudge: routeWires accepts the opt-in flag and keeps routes valid")
{
    using json = nlohmann::json;
    json input;
    input["deterministic"] = true;
    input["nudge"] = true;
    input["width"] = 1200;
    input["height"] = 800;
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
    input["blocks"] = json::array({ makeBlock("a", 100, 100, true), makeBlock("b", 500, 100, false),
        makeBlock("c", 100, 300, true), makeBlock("d", 500, 300, false) });
    auto conn = [](const char* f, const char* t) {
        json c;
        c["from"] = f;
        c["to"] = t;
        c["fromIndex"] = 0;
        c["toIndex"] = 0;
        return c;
    };
    input["connections"] = json::array({ conn("a", "b"), conn("c", "d") });

    auto result = json::parse(Nelson::NFlow::routeWires(input.dump()));
    REQUIRE(result.value("ok", false));
    REQUIRE(result["routes"].is_array());
    REQUIRE(result["routes"].size() == 2);
    for (const auto& route : result["routes"]) {
        REQUIRE(route.is_array());
        REQUIRE(route.size() >= 2);
        // Orthogonality preserved through the nudge pass.
        for (size_t i = 1; i < route.size(); ++i) {
            int dx = route[i]["x"].get<int>() - route[i - 1]["x"].get<int>();
            int dy = route[i]["y"].get<int>() - route[i - 1]["y"].get<int>();
            CHECK((dx == 0 || dy == 0));
        }
    }
}
//=============================================================================
