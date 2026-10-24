//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Parity/golden tests for the native router.
//
// Each fixtures/*.json file is a routing snapshot ({blocks, connections,
// width, height}). The test routes it in deterministic mode and compares the
// raw grid routes (pre-stub) and pixel routes (post-stub) against the
// committed golden in fixtures/golden/<name>.routes.json.
//
// The same fixtures are consumed by the TS router parity spec
// (modules/nflow_gui/src/ts/router/router.parity.spec.ts), which asserts the
// TS router matches the same goldens, proving TS == C++ by transitivity.
//
// To regenerate goldens after an intentional routing change:
//   set NFLOW_ROUTER_REGEN=1 and run this binary once.
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "doctest.h"
#include "RouterEngine.hpp"
#include <algorithm>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>
//=============================================================================
using json = nlohmann::json;
namespace fs = std::filesystem;
//=============================================================================
namespace {
//=============================================================================
std::string
fixturesDir()
{
    return NFLOW_ROUTER_FIXTURES_DIR;
}
//=============================================================================
bool
regenRequested()
{
    const char* env = std::getenv("NFLOW_ROUTER_REGEN");
    return env != nullptr && std::string(env) == "1";
}
//=============================================================================
json
readJsonFile(const fs::path& path)
{
    std::ifstream in(path);
    if (!in.good()) {
        std::cerr << "cannot open " << path.string() << "\n";
    }
    REQUIRE(in.good());
    json j;
    in >> j;
    return j;
}
//=============================================================================
void
writeJsonFile(const fs::path& path, const json& j)
{
    fs::create_directories(path.parent_path());
    std::ofstream out(path);
    if (!out.good()) {
        std::cerr << "cannot write " << path.string() << "\n";
    }
    REQUIRE(out.good());
    out << j.dump(2) << '\n';
}
//=============================================================================
std::vector<fs::path>
listFixtures()
{
    std::vector<fs::path> fixtures;
    fs::path dir(fixturesDir());
    if (!fs::exists(dir)) {
        return fixtures;
    }
    for (const auto& entry : fs::directory_iterator(dir)) {
        if (entry.is_regular_file() && entry.path().extension() == ".json") {
            fixtures.push_back(entry.path());
        }
    }
    std::sort(fixtures.begin(), fixtures.end());
    return fixtures;
}
//=============================================================================
json
routeFixture(const json& fixture)
{
    json input = fixture;
    input["deterministic"] = true;
    input["emitGrid"] = true;
    if (!input.contains("timeLimitMs")) {
        input["timeLimitMs"] = 4000;
    }
    auto resultStr = Nelson::NFlow::routeWires(input.dump());
    auto result = json::parse(resultStr);
    if (!result.value("ok", false)) {
        std::cerr << "routeWires failed: " << result.value("error", std::string()) << "\n";
    }
    REQUIRE(result.value("ok", false));
    return result;
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("router parity: fixtures corpus matches committed goldens")
{
    auto fixtures = listFixtures();
    if (fixtures.empty()) {
        std::cerr << "no fixtures found in " << fixturesDir() << "\n";
    }
    REQUIRE(!fixtures.empty());

    for (const auto& fixturePath : fixtures) {
        auto fixture = readJsonFile(fixturePath);
        auto result = routeFixture(fixture);

        json golden;
        golden["gridRoutes"] = result["gridRoutes"];
        golden["routes"] = result["routes"];

        fs::path goldenPath
            = fs::path(fixturesDir()) / "golden" / (fixturePath.stem().string() + ".routes.json");

        if (regenRequested()) {
            writeJsonFile(goldenPath, golden);
            std::cout << "regenerated " << goldenPath.string() << "\n";
            continue;
        }

        if (!fs::exists(goldenPath)) {
            std::cerr << "missing golden " << goldenPath.string()
                      << " (run with NFLOW_ROUTER_REGEN=1 to create)\n";
        }
        REQUIRE(fs::exists(goldenPath));
        auto expected = readJsonFile(goldenPath);
        if (golden != expected) {
            std::cerr << "route mismatch for " << fixturePath.stem().string() << "\n";
        }
        CHECK(golden == expected);
    }
}
//=============================================================================
TEST_CASE("router parity: routing is byte-stable across repeated runs")
{
    auto fixtures = listFixtures();
    REQUIRE(!fixtures.empty());

    for (const auto& fixturePath : fixtures) {
        auto fixture = readJsonFile(fixturePath);
        auto first = routeFixture(fixture).dump();
        for (int i = 0; i < 4; ++i) {
            bool same = routeFixture(fixture).dump() == first;
            if (!same) {
                std::cerr << "unstable routing for " << fixturePath.stem().string() << "\n";
            }
            CHECK(same);
        }
    }
}
//=============================================================================
TEST_CASE("router parity: connection order does not change routes")
{
    auto fixtures = listFixtures();
    REQUIRE(!fixtures.empty());

    for (const auto& fixturePath : fixtures) {
        auto fixture = readJsonFile(fixturePath);
        auto conns = fixture["connections"];
        if (!conns.is_array() || conns.size() < 2) {
            continue;
        }
        auto reference = routeFixture(fixture);

        // Reverse the connections array; per-connection routes must not move.
        json reversedFixture = fixture;
        json reversedConns = json::array();
        for (auto it = conns.rbegin(); it != conns.rend(); ++it) {
            reversedConns.push_back(*it);
        }
        reversedFixture["connections"] = reversedConns;
        auto reversed = routeFixture(reversedFixture);

        // routes arrays align with the (differently ordered) connections
        // arrays; compare per original index.
        const auto n = conns.size();
        for (size_t i = 0; i < n; ++i) {
            size_t j = n - 1 - i;
            bool sameRoutes = reference["routes"][i] == reversed["routes"][j];
            bool sameGrid = reference["gridRoutes"][i] == reversed["gridRoutes"][j];
            if (!sameRoutes || !sameGrid) {
                std::cerr << "order-dependent route in " << fixturePath.stem().string()
                          << " at index " << i << "\n";
            }
            CHECK(sameRoutes);
            CHECK(sameGrid);
        }
    }
}
//=============================================================================
