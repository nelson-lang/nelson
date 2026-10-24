//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Conditional-lowering artifacts (condGate / condHold / condLatch) and the
// zero-crossing seam metadata (crossingCount): native coverage of the seams
// the .m tests only exercise end-to-end.
//=============================================================================
#include "test_helpers.hpp"
#include <nlohmann/json.hpp>
#include <string>
#include <vector>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::BlockCodegenArgs;
using Nelson::NFlow::BlockCodegenTemplate;
//=============================================================================
namespace {
// Minimal emission context: collects a.line output for one block.
struct EmitProbe
{
    nlohmann::json block;
    nlohmann::json params;
    std::unordered_map<std::string, double> variables;
    std::vector<std::string> out;
    std::vector<std::string> inExprs { "in0_expr", "in1_expr", "in2_expr" };

    BlockCodegenArgs
    args(const std::string& id)
    {
        // Emitters read parameters through BlockDescriptor(*a.block), i.e.
        // from the block's nested "params" object.
        block["params"] = params;
        BlockCodegenArgs a;
        a.id = id;
        a.block = &block;
        a.params = &params;
        a.variables = &variables;
        a.in = inExprs;
        a.dt = 0.1;
        a.fmt = [](double v) { return std::to_string(v); };
        a.sanitize = [](const std::string& s) { return s; };
        a.line = [this](const std::string& l) { out.push_back(l); };
        return a;
    }

    bool
    contains(const std::string& needle) const
    {
        for (const auto& l : out) {
            if (l.find(needle) != std::string::npos) {
                return true;
            }
        }
        return false;
    }
};
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("cond lowering artifacts register phaseless handlers and codegen pairs")
{
    ensureHandlersRegistered();
    auto& reg = BlockRegistry::instance();
    for (const char* type : { "condGate", "condHold", "condLatch" }) {
        CHECK(reg.getHandler(type) != nullptr);
        for (const char* lang : { "c", "rust" }) {
            BlockCodegenTemplate t;
            CHECK(reg.getCodegen(type, lang, t));
            CHECK_FALSE(t.notImplemented);
        }
    }
}
//=============================================================================
TEST_CASE("condGate emits the level guard and the trigger edge memory")
{
    ensureHandlersRegistered();
    auto& reg = BlockRegistry::instance();
    BlockCodegenTemplate tc;
    REQUIRE(reg.getCodegen("condGate", "c", tc));

    // Level mode (enable / action): out = control > 0.
    EmitProbe level;
    level.block = { { "id", "g1" }, { "type", "condGate" } };
    level.params = nlohmann::json::object();
    auto la = level.args("g1");
    tc.emitStep(la);
    CHECK(level.contains("out_g1 = (in0_expr > 0.0) ? (1.0) : (0.0);"));

    // Trigger mode: edge vs the latched previous value, gated by the
    // first-latch flag (no fire before the first latch).
    EmitProbe trig;
    trig.block = { { "id", "g2" }, { "type", "condGate" } };
    trig.params = { { "mode", "trigger" }, { "tt", 0 } };
    auto ta = trig.args("g2");
    tc.emitStep(ta);
    CHECK(trig.contains("nfgh_g2"));
    CHECK(trig.contains("nfgp_g2 <= 0.0 && in0_expr > 0.0"));
}
//=============================================================================
TEST_CASE("condHold latches while its gate is open and replays the latch")
{
    ensureHandlersRegistered();
    auto& reg = BlockRegistry::instance();
    BlockCodegenTemplate tc;
    REQUIRE(reg.getCodegen("condHold", "c", tc));

    EmitProbe hold;
    hold.block = { { "id", "h1" }, { "type", "condHold" } };
    hold.params = { { "gate", "sub__nfg" } };
    auto ha = hold.args("h1");
    tc.emitStep(ha);
    CHECK(hold.contains("if (out_sub__nfg != 0.0) { s->ch_h1 = in0_expr; }"));
    CHECK(hold.contains("out_h1 = s->ch_h1;"));
}
//=============================================================================
TEST_CASE("condLatch rewrites its gate's edge memory")
{
    ensureHandlersRegistered();
    auto& reg = BlockRegistry::instance();
    BlockCodegenTemplate tc;
    REQUIRE(reg.getCodegen("condLatch", "c", tc));

    EmitProbe latch;
    latch.block = { { "id", "l1" }, { "type", "condLatch" } };
    latch.params = { { "gate", "sub__nfg" } };
    auto la = latch.args("l1");
    tc.emitStep(la);
    CHECK(latch.contains("s->nfgp_sub__nfg = in0_expr;"));
    CHECK(latch.contains("s->nfgh_sub__nfg = 1.0;"));
}
//=============================================================================
TEST_CASE("crossingCount mirrors the runtime's surface rules")
{
    ensureHandlersRegistered();
    auto& reg = BlockRegistry::instance();
    auto countOf = [&](const char* type, nlohmann::json params) -> int {
        BlockCodegenTemplate t;
        REQUIRE(reg.getCodegen(type, "c", t));
        if (!t.crossingCount) {
            return 0;
        }
        EmitProbe p;
        p.block = { { "id", "x" }, { "type", type } };
        p.params = std::move(params);
        auto a = p.args("x");
        return t.crossingCount(a);
    };
    // switch: always one surface (control - threshold).
    CHECK(countOf("switch", nlohmann::json::object()) == 1);
    // deadZone: the two zone edges.
    CHECK(countOf("deadZone", { { "Start", -1.0 }, { "End", 1.0 } }) == 2);
    // saturation: 2 when any bound is finite, none when both are infinite.
    CHECK(countOf("saturation", { { "LowerLimit", -1.0 }, { "UpperLimit", 1.0 } }) == 2);
    CHECK(countOf("saturation", nlohmann::json::object()) == 0);
    // integrator: bounded -> 2, unbounded -> 0 (the runtime's rule).
    CHECK(
        countOf("integrator", { { "LowerSaturationLimit", 0.0 }, { "UpperSaturationLimit", 1.35 } })
        == 2);
    CHECK(countOf("integrator", nlohmann::json::object()) == 0);
    // step: the t - Time discontinuity surface.
    CHECK(countOf("step", { { "Time", 0.37 } }) == 1);
}
//=============================================================================
TEST_CASE("codegen registrations are language-symmetric")
{
    ensureHandlersRegistered();
    const std::vector<std::string> bad = BlockRegistry::instance().codegenLanguageAsymmetries();
    CHECK(bad.empty());
}
//=============================================================================
