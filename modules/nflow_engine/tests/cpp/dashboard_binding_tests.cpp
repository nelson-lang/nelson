//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "DashboardBindingManager.hpp"
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
static Block
makeBlock(const std::string& id, const std::string& type, int nid, json params = json::object())
{
    Block block;
    block.id = id;
    block.type = type;
    block.nid = nid;
    block.params = std::move(params);
    return block;
}
//=============================================================================
TEST_CASE("dashboard variables preserve scalar composite elements")
{
    ValMap variables;
    flattenDashboardVariable("matrix", json::array({ { 1.0, 2.0 }, { 3.0, 4.0 } }), variables);
    CHECK(variables.at("matrix(1,1)") == 1.0);
    CHECK(variables.at("matrix(2,1)") == 3.0);
    CHECK(variables.at("matrix(2,2)") == 4.0);
}
//=============================================================================
TEST_CASE("dashboard binding validation resolves nested paths")
{
    std::vector<Block> blocks;
    blocks.push_back(makeBlock("sub", "subsystem", 0));
    blocks.push_back(makeBlock("view", "dashboardDisplay", 1,
        { { "Binding",
            { { "kind", "signal" }, { "blockPath", { "sub", "gain" } },
                { "outputPortIndex", 1 } } } }));
    blocks[0].outputs = 1;
    std::vector<BlockState> states(2);
    states[0].subsystem = std::make_unique<BlockState::SubsystemState>();
    Block inner = makeBlock("gain", "gain", 0, { { "Gain", 2.0 } });
    inner.outputs = 1;
    inner.outWidths = { 1 };
    states[0].subsystem->blocks.push_back(std::move(inner));
    states[0].subsystem->blockStateVec.resize(1);

    CHECK(validateDashboardBindings(blocks, states, {}).empty());
    blocks[1].params["Binding"]["outputPortIndex"] = 2;
    const auto errors = validateDashboardBindings(blocks, states, {});
    REQUIRE(errors.size() == 1);
    CHECK(errors[0].blockId == "view");
    CHECK(errors[0].message.find("output port") != std::string::npos);
}
//=============================================================================
TEST_CASE("dashboard controls validate and update composite targets")
{
    std::vector<Block> blocks;
    blocks.push_back(makeBlock("gain", "gain", 0, { { "Vector", { 1.0, 2.0 } } }));
    blocks.push_back(makeBlock("control", "dashboardSlider", 1,
        { { "Binding",
            { { "kind", "parameter" }, { "blockPath", { "gain" } }, { "paramName", "Vector" },
                { "element", "(2)" } } } }));
    std::vector<BlockState> states(2);
    CHECK(validateDashboardBindings(blocks, states, {}).empty());
    CHECK(states[1].scalar == 2.0);

    DashboardBindingDiagnostic error;
    json event = { { "id", "control" }, { "type", "setDashboardValue" }, { "value", 7.0 } };
    ValMap variables;
    CHECK(applyDashboardBindingValue(blocks, states, variables, event, error));
    CHECK(blocks[0].params["Vector"][1].get<double>() == 7.0);
    CHECK(states[1].scalar == 7.0);
}
//=============================================================================
TEST_CASE("dashboard variable events update the flattened runtime value")
{
    std::vector<Block> blocks;
    blocks.push_back(makeBlock("control", "dashboardEdit", 0,
        { { "Binding",
            { { "kind", "variable" }, { "varName", "matrix" }, { "workspace", "diagram" },
                { "element", "(2,1)" } } } }));
    std::vector<BlockState> states(1);
    ValMap variables { { "matrix(2,1)", 3.0 } };
    CHECK(validateDashboardBindings(blocks, states, variables).empty());
    DashboardBindingDiagnostic error;
    const json event = { { "id", "control" }, { "type", "setDashboardValue" }, { "value", 9.0 } };
    CHECK(applyDashboardBindingValue(blocks, states, variables, event, error));
    CHECK(variables.at("matrix(2,1)") == 9.0);
}
//=============================================================================
TEST_CASE("dashboard controls inside subsystems update their own runtime state")
{
    std::vector<Block> blocks;
    blocks.push_back(makeBlock("gain", "gain", 0, { { "Gain", 2.0 } }));
    blocks.push_back(makeBlock("sub", "subsystem", 1));
    std::vector<BlockState> states(2);
    states[1].subsystem = std::make_unique<BlockState::SubsystemState>();
    auto& subsystem = *states[1].subsystem;
    subsystem.blocks.push_back(makeBlock("nested-control", "dashboardSlider", 0,
        { { "Binding",
            { { "kind", "parameter" }, { "blockPath", { "gain" } }, { "paramName", "Gain" } } } }));
    subsystem.blockStateVec.resize(1);

    CHECK(validateDashboardBindings(blocks, states, {}).empty());
    CHECK(subsystem.blockStateVec[0].scalar == 2.0);
    DashboardBindingDiagnostic error;
    ValMap variables;
    const json event
        = { { "id", "nested-control" }, { "type", "setDashboardValue" }, { "value", 8.0 } };
    CHECK(applyDashboardBindingValue(blocks, states, variables, event, error));
    CHECK(blocks[0].params["Gain"].get<double>() == 8.0);
    CHECK(subsystem.blockStateVec[0].scalar == 8.0);
    CHECK(states[0].scalar == 0.0);
}
//=============================================================================
