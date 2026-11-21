//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "NFlowBusSignalNames.hpp"
using Nelson::NFlow::inferBusSignalNames;
using nlohmann::json;
//=============================================================================
TEST_CASE("bus creator inherits signal names across branches")
{
    auto d = json::parse(
        R"({"blocks":[{"id":"bus","type":"busCreator","inputs":2,"params":{}}],"connections":[{"from":"a","to":"scope","label":"altitude"},{"from":"a","to":"bus","toIndex":0},{"from":"b","to":"bus","toIndex":1,"label":"speed"}]})");
    inferBusSignalNames(d);
    CHECK(d["blocks"][0]["params"]["MemberNames"] == json::array({ "altitude", "speed" }));
}
//=============================================================================
TEST_CASE("duplicate names are disambiguated and explicit members are retained")
{
    auto d = json::parse(
        R"({"blocks":[{"id":"bus","type":"busCreator","inputs":2,"params":{}},{"id":"other","type":"busCreator","inputs":1,"params":{"MemberNames":["explicit"]}}],"connections":[{"from":"a","to":"bus","toIndex":0,"label":"x"},{"from":"b","to":"bus","toIndex":1,"label":"x"},{"from":"a","to":"other","label":"ignored"}]})");
    inferBusSignalNames(d);
    CHECK(d["blocks"][0]["params"]["MemberNames"] == json::array({ "x1", "x2" }));
    CHECK(d["blocks"][1]["params"]["MemberNames"] == json::array({ "explicit" }));
}
//=============================================================================
TEST_CASE("nested diagrams and unnamed signals retain deterministic names")
{
    auto d = json::parse(
        R"({"blocks":[{"id":"sub","type":"subsystem","params":{"subsystem":{"blocks":[{"id":"inner","type":"busCreator","inputs":2,"params":{}}],"connections":[{"from":"a","to":"inner","toIndex":0,"label":"speed"},{"from":"b","to":"inner","toIndex":1}]}}}],"connections":[]})");
    inferBusSignalNames(d);
    CHECK(d["blocks"][0]["params"]["subsystem"]["blocks"][0]["params"]["MemberNames"]
        == json::array({ "speed", "signal2" }));
}
//=============================================================================
