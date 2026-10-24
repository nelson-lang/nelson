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
#include <algorithm>
#include <chrono>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <nlohmann/json.hpp>
#include <set>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::Phase;
TEST_CASE("BlockRegistry registers every builtin runtime handler")
{
    ensureHandlersRegistered();
    auto& reg = BlockRegistry::instance();

    const std::vector<std::string> expected = { "constant", "step", "ramp", "impulse", "sine",
        "chirp", "noise", "clock", "fileSource", nflow::kLabelSource, "toggleSwitch", "gain", "sum",
        "mult", "abs", "divide", "negate", "bias", "min", "max", "mathFunction", "roundingFunction",
        "sign", "sqrt", "trigFunction", "sumElements", "productOfElements", "dotProduct",
        "crossProduct", "lookup1D", "lookup2D", "lookupND", "directLookup", "and", "or", "xor",
        "not", "compareToConstant", "compareToZero", "relationalOperator", "multiportSwitch",
        "saturation", "deadZone", "quantizer", "switch", "rate", "backlash", "integrator", "tf",
        "delay", "stateSpace", "lpf", "hpf", "derivative", "pid", "zoh", "foh", "dtf", "ddelay",
        "dstateSpace", "unitDelay", "difference", "scope", "xyScope", "xyzScope", nflow::kLabelSink,
        "terminator", nflow::kDisplay, "subsystem" };

    for (const auto& type : expected) {
        CHECK(static_cast<bool>(reg.getHandler(type)));
    }
}
//=============================================================================
TEST_CASE("BlockRegistry exposes expected handler phases")
{
    ensureHandlersRegistered();
    auto& reg = BlockRegistry::instance();

    const std::vector<std::string> outputOnly = { "constant", "step", "ramp", "impulse", "sine",
        "chirp", "noise", "clock", nflow::kLabelSource, "toggleSwitch" };
    for (const auto& type : outputOnly) {
        CHECK(reg.hasPhase(type, Phase::OUTPUT));
        CHECK_FALSE(reg.hasPhase(type, Phase::ALGEBRAIC));
    }

    const std::vector<std::string> algebraic = { "gain", "sum", "mult", "abs", "divide", "negate",
        "bias", "min", "max", "mathFunction", "roundingFunction", "sign", "sqrt", "trigFunction",
        "lookup1D", "lookup2D", "lookupND", "directLookup", "and", "or", "xor", "not",
        "compareToConstant", "compareToZero", "relationalOperator", "multiportSwitch", "saturation",
        "deadZone", "quantizer", "switch" };
    for (const auto& type : algebraic) {
        CHECK(reg.hasPhase(type, Phase::ALGEBRAIC));
    }

    const std::vector<std::string> stateful = { "fileSource", "integrator", "tf", "delay",
        "stateSpace", "lpf", "hpf", "ddelay", "unitDelay", "subsystem" };
    for (const auto& type : stateful) {
        CHECK(reg.hasPhase(type, Phase::INIT));
        CHECK(reg.hasPhase(type, Phase::OUTPUT));
    }

    // derivative and the discrete difference / detect* family are stateful
    // DIRECT-FEEDTHROUGH blocks: their output reads the current input, so it is
    // produced in the topologically ordered ALGEBRAIC phase (UPDATE advances the
    // stored previous input), NOT the declaration-ordered OUTPUT phase.
    // rate / backlash / hysteresis answer the current input and only carry the
    // committed value as memory, and a zoh takes its sample AT the hit instant;
    // reading their committed state back as the output would delay each of them
    // by a step.
    // pid, dtf, foh and dstateSpace answer the current input the same way.
    const std::vector<std::string> feedthroughInAlgebraic = { "derivative", "difference",
        "detectChange", "risingEdge", "fallingEdge", "detectIncrease", "detectDecrease", "rate",
        "backlash", "hysteresis", "zoh", "pid", "dtf", "foh", "dstateSpace" };
    for (const auto& type : feedthroughInAlgebraic) {
        CHECK(reg.hasPhase(type, Phase::INIT));
        CHECK(reg.hasPhase(type, Phase::ALGEBRAIC));
        CHECK(reg.hasPhase(type, Phase::UPDATE));
        CHECK_FALSE(reg.hasPhase(type, Phase::OUTPUT));
    }

    CHECK(reg.hasPhase("scope", Phase::AFTER_STEP));
    CHECK(reg.hasPhase("xyScope", Phase::AFTER_STEP));
    CHECK(reg.hasPhase("xyzScope", Phase::AFTER_STEP));
    CHECK(reg.hasPhase(nflow::kDisplay, Phase::AFTER_STEP));
}
//=============================================================================
TEST_CASE("BlockRegistry search paths are ordered and idempotent")
{
    namespace fs = std::filesystem;
    auto& reg = BlockRegistry::instance();
    const auto suffix = std::chrono::steady_clock::now().time_since_epoch().count();
    const fs::path tempRoot
        = fs::current_path() / "temp" / ("nflow_registry_" + std::to_string(suffix));
    const fs::path firstRoot = tempRoot / "first";
    const fs::path secondRoot = tempRoot / "second";
    fs::create_directories(firstRoot / "alpha");
    fs::create_directories(secondRoot / "duplicate");
    fs::create_directories(secondRoot / "reserved");
    {
        std::ofstream(firstRoot / "alpha" / "library.json")
            << R"({"id":"custom.alpha","title":"First","blocks":[]})";
        std::ofstream(secondRoot / "duplicate" / "library.json")
            << R"({"id":"custom.alpha","title":"Second","blocks":[]})";
        std::ofstream(secondRoot / "reserved" / "library.json")
            << R"({"id":"builtin.invalid","title":"Reserved","blocks":[]})";
    }

    std::string duplicateFirstRoot = firstRoot.string();
#ifdef _WIN32
    std::transform(duplicateFirstRoot.begin(), duplicateFirstRoot.end(), duplicateFirstRoot.begin(),
        [](unsigned char value) { return static_cast<char>(std::toupper(value)); });
#endif

    CHECK(reg.setSearchPaths({ firstRoot.string(), secondRoot.string(), duplicateFirstRoot,
              (tempRoot / "unavailable").string() })
        == 1);
    CHECK(reg.searchPaths().size() == 3);
    REQUIRE(reg.discoveredManifests().size() == 1);
    auto manifest = nlohmann::json::parse(reg.discoveredManifests().front());
    CHECK(manifest.value("title", "") == "First");
    CHECK(fs::path(manifest.value("_dir", "")).lexically_normal()
        == (firstRoot / "alpha").lexically_normal());
    CHECK(reg.discoveryMessages().size() == 3);

    std::ofstream(firstRoot / "alpha" / "library.json")
        << R"({"id":"custom.alpha","title":"Updated","blocks":[]})";
    reg.scanDirectory(firstRoot.string());
    CHECK(reg.discoveredManifests().size() == 1);
    manifest = nlohmann::json::parse(reg.discoveredManifests().front());
    CHECK(manifest.value("title", "") == "Updated");

    reg.setSearchPaths({ (firstRoot / "alpha").string() });
    REQUIRE(reg.searchPaths().size() == 1);
    CHECK(fs::path(reg.searchPaths().front()).lexically_normal() == firstRoot.lexically_normal());
    CHECK(reg.discoveredManifests().size() == 1);

    fs::create_directories(tempRoot / "invalid" / "broken");
    std::ofstream(tempRoot / "invalid" / "broken" / "library.json") << "not-json";
    reg.setSearchPaths({ (tempRoot / "invalid").string() });
    CHECK(reg.discoveredManifests().empty());
    CHECK(reg.discoveryMessages().size() == 1);

    reg.setSearchPaths({});
    CHECK(reg.searchPaths().empty());
    CHECK(reg.discoveredManifests().empty());
    fs::remove_all(tempRoot);
}
//=============================================================================
