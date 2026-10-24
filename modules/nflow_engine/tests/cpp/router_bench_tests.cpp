//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Scalability benchmark for the native router: synthetic diagrams of 100/200/400
// blocks, with the full route wall time printed. It gates the optional
// visibility-graph router (R4b), which is only worth building if p95 full-route
// time exceeds ~1.5 s around 150 blocks.
// The assertions are loose so the bench does not flake on slow CI machines.
//=============================================================================
#include "doctest.h"
#include "RouterEngine.hpp"
#include <chrono>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
//=============================================================================
using json = nlohmann::json;
//=============================================================================
namespace {
//=============================================================================
json
makeBenchSnapshot(int cols, int rows)
{
    json input;
    input["timeLimitMs"] = 60000;
    input["width"] = cols * 250 + 200;
    input["height"] = rows * 150 + 200;
    input["blocks"] = json::array();
    input["connections"] = json::array();

    auto blockId = [](int r, int c) { return "g" + std::to_string(r) + "_" + std::to_string(c); };

    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            json b;
            b["id"] = blockId(r, c);
            b["x"] = 100 + c * 250;
            b["y"] = 100 + r * 150;
            b["width"] = 80;
            b["height"] = 40;
            b["rotation"] = 0;
            json pin;
            pin["type"] = "in";
            pin["index"] = 0;
            pin["x"] = 0;
            pin["y"] = 20;
            json pout;
            pout["type"] = "out";
            pout["index"] = 0;
            pout["x"] = 80;
            pout["y"] = 20;
            b["ports"] = json::array({ pin, pout });
            input["blocks"].push_back(b);
        }
    }
    std::string prev;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            std::string id = blockId(r, c);
            if (!prev.empty()) {
                json conn;
                conn["from"] = prev;
                conn["to"] = id;
                conn["fromIndex"] = 0;
                conn["toIndex"] = 0;
                input["connections"].push_back(conn);
            }
            prev = id;
        }
    }
    for (int r = 1; r < rows; r += 3) {
        json fb;
        fb["from"] = blockId(r, cols - 1);
        fb["to"] = blockId(r - 1, 0);
        fb["fromIndex"] = 0;
        fb["toIndex"] = 0;
        input["connections"].push_back(fb);
    }
    return input;
}
//=============================================================================
double
benchOnce(int cols, int rows)
{
    auto input = makeBenchSnapshot(cols, rows);
    auto start = std::chrono::steady_clock::now();
    auto result = json::parse(Nelson::NFlow::routeWires(input.dump()));
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::steady_clock::now() - start)
                  .count();
    REQUIRE(result.value("ok", false));
    int blocks = cols * rows;
    int conns = static_cast<int>(input["connections"].size());
    std::cout << "[router bench] " << blocks << " blocks, " << conns << " wires: " << ms << " ms\n";
    return static_cast<double>(ms);
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("router bench: full route wall time at 100/200/400 blocks")
{
    double t100 = benchOnce(10, 10);
    double t200 = benchOnce(20, 10);
    double t400 = benchOnce(20, 20);
    // The R4b decision uses the printed numbers, not these ceilings.
    CHECK(t100 < 30000.0);
    CHECK(t200 < 60000.0);
    CHECK(t400 < 120000.0);
}
//=============================================================================
