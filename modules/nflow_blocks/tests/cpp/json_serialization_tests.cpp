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
#include "NFlowCodegenHelpers.hpp"
#include <clocale>
#include <string>
#include <utility>
#include <vector>
//=============================================================================
using Nelson::NFlow::Block;
using Nelson::NFlow::BlockState;
using Nelson::NFlow::buildOutputJson;
using Nelson::NFlow::json;
//=============================================================================
namespace {
//=============================================================================
struct NumericLocaleGuard
{
    std::string previous;

    NumericLocaleGuard()
    {
        const char* current = std::setlocale(LC_NUMERIC, nullptr);
        if (current) {
            previous = current;
        }
    }

    ~NumericLocaleGuard()
    {
        if (!previous.empty()) {
            std::setlocale(LC_NUMERIC, previous.c_str());
        }
    }
};
//=============================================================================
bool
selectCommaDecimalLocale()
{
    const char* candidates[] = { "fr_FR.UTF-8", "fr_FR.utf8", "French_France.1252" };
    for (const char* candidate : candidates) {
        if (std::setlocale(LC_NUMERIC, candidate)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("simulation JSON numbers ignore the process numeric locale")
{
    NumericLocaleGuard localeGuard;
    selectCommaDecimalLocale();

    CHECK(nflow::doubleToStr(0.05).find(',') == std::string::npos);

    Block scope;
    scope.id = "scope";
    scope.type = "scope";
    scope.nid = 0;
    scope.inputs = 1;

    BlockState state;
    state.scopeSeries = { { 0.25 } };
    const std::string extra = "\"progress\":" + nflow::doubleToStr(0.005)
        + ",\"currentTime\":" + nflow::doubleToStr(0.05);
    std::vector<Block> blocks;
    blocks.push_back(std::move(scope));
    std::vector<BlockState> states;
    states.push_back(std::move(state));
    const std::string serialized
        = buildOutputJson("running", "", { 0.05 }, blocks, states, { { 0 } }, extra);

    json parsed;
    bool parsedOk = true;
    try {
        parsed = json::parse(serialized);
    } catch (...) {
        parsedOk = false;
    }
    CHECK(parsedOk);
    CHECK(parsed["progress"] == doctest::Approx(0.005));
    CHECK(parsed["currentTime"] == doctest::Approx(0.05));
    CHECK(parsed["scopes"][0]["times"][0] == doctest::Approx(0.05));
    CHECK(parsed["scopes"][0]["series"][0][0] == doctest::Approx(0.25));
}
//=============================================================================
