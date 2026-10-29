//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests the C-ABI programmatic codegen bridge: a plugin's nfEmitBlockCodegen
// reaches the generators through an NfCodegenContext whose callbacks mirror the
// internal BlockCodegenArgs. A trivial "cumprod" emitter must reproduce exactly
// what its declarative JSON template would expand to.
//=============================================================================
#include "test_helpers.hpp"
#include "BlockPlugin.h"
#include "NFlowCodegenHelpers.hpp"
#include <string>
#include <vector>
//=============================================================================
using namespace nflow_tests;
using Nelson::NFlow::BlockCodegenArgs;
using Nelson::NFlow::BlockCodegenStateArgs;
using Nelson::NFlow::BlockCodegenTemplate;
//=============================================================================
namespace {
// A stand-in plugin emitter, byte-for-byte the body of cumprod.cpp's
// nfEmitBlockCodegen. Declares one accumulator and emits the running product.
int
demoCumprodEmit(const char* type, const char* lang, NfCodegenContext* ctx)
{
    if (!type || !lang || !ctx) {
        return 0;
    }
    if (std::string(type) != "demoCumprod") {
        return 0;
    }
    ctx->addState(ctx->self, "cumprod", ctx->fmt(ctx->self, 1.0), nullptr);
    const std::string out = ctx->outputVar(ctx->self);
    const std::string acc = ctx->stateRef(ctx->self, "cumprod");
    const std::string in0 = ctx->inputExpr(ctx->self, 0);
    ctx->line(ctx->self, (out + " = " + acc + " * " + in0 + ";").c_str());
    ctx->line(ctx->self, (acc + " = " + out + ";").c_str());
    return 1;
}
// An emitter that echoes typed parameter reads, to exercise the param bridge.
int
demoParamEmit(const char* type, const char* lang, NfCodegenContext* ctx)
{
    (void)lang;
    if (!type || std::string(type) != "demoParams" || !ctx) {
        return 0;
    }
    const double gain = ctx->paramNumber(ctx->self, "gain", -1.0);
    const double missing = ctx->paramNumber(ctx->self, "nope", 42.0);
    const int hasGain = ctx->hasParam(ctx->self, "gain");
    const int hasNope = ctx->hasParam(ctx->self, "nope");
    const int flag = ctx->paramBool(ctx->self, "flag", 0);
    const char* label = ctx->paramString(ctx->self, "label", "none");
    double coeffs[8];
    const int n = ctx->paramList(ctx->self, "coeffs", coeffs, 8);
    ctx->line(ctx->self, (std::string("gain=") + ctx->fmt(ctx->self, gain)).c_str());
    ctx->line(ctx->self, (std::string("missing=") + ctx->fmt(ctx->self, missing)).c_str());
    ctx->line(ctx->self, (std::string("hasGain=") + std::to_string(hasGain)).c_str());
    ctx->line(ctx->self, (std::string("hasNope=") + std::to_string(hasNope)).c_str());
    ctx->line(ctx->self, (std::string("flag=") + std::to_string(flag)).c_str());
    ctx->line(ctx->self, (std::string("label=") + label).c_str());
    ctx->line(ctx->self, (std::string("ncoeffs=") + std::to_string(n)).c_str());
    if (n >= 2) {
        ctx->line(ctx->self, (std::string("c1=") + ctx->fmt(ctx->self, coeffs[1])).c_str());
    }
    return 1;
}
} // namespace
//=============================================================================
TEST_CASE("plugin programmatic codegen: cumprod C emitter matches its JSON template")
{
    // Register both languages so the registry stays language-symmetric.
    BlockRegistry::instance().registerCodegenEmitter("demoCumprod", "c", &demoCumprodEmit);
    BlockRegistry::instance().registerCodegenEmitter("demoCumprod", "rust", &demoCumprodEmit);

    json block = { { nflow::kId, "b" }, { nflow::kType, "demoCumprod" },
        { nflow::kParams, json::object() } };
    std::unordered_map<std::string, double> vars;

    BlockCodegenTemplate tpl;
    REQUIRE(BlockRegistry::instance().getCodegen("demoCumprod", "c", tpl));
    REQUIRE(tpl.emitStep);
    REQUIRE(tpl.emitState);

    // State pass: addState("cumprod", ...) -> field cumprod_b, init from fmt.
    std::vector<std::string> fields;
    std::vector<std::string> inits;
    BlockCodegenStateArgs sa;
    sa.id = "b";
    sa.block = &block;
    sa.params = &block[nflow::kParams];
    sa.variables = &vars;
    sa.fmt = [](double v) { return nflow::formatNumber(v); };
    sa.addState = [&](const std::string& n, const std::string& init, const std::string&) {
        fields.push_back(n);
        inits.push_back(init);
    };
    tpl.emitState(sa);
    REQUIRE(fields.size() == 1);
    CHECK(fields[0] == "cumprod_b");
    CHECK(inits[0] == nflow::formatNumber(1.0));

    // Step pass: out_b = s->cumprod_b * (u);  then latch.
    std::vector<std::string> lines;
    BlockCodegenArgs a;
    a.id = "b";
    a.in = { "(u)", "0.0", "0.0" };
    a.block = &block;
    a.params = &block[nflow::kParams];
    a.variables = &vars;
    a.fmt = [](double v) { return nflow::formatNumber(v); };
    a.line = [&](const std::string& l) { lines.push_back(l); };
    tpl.emitStep(a);
    REQUIRE(lines.size() == 2);
    CHECK(lines[0] == "out_b = s->cumprod_b * (u);");
    CHECK(lines[1] == "s->cumprod_b = out_b;");
}
//=============================================================================
TEST_CASE("plugin programmatic codegen: cumprod Rust emitter uses dotted state and f64")
{
    BlockRegistry::instance().registerCodegenEmitter("demoCumprod", "c", &demoCumprodEmit);
    BlockRegistry::instance().registerCodegenEmitter("demoCumprod", "rust", &demoCumprodEmit);

    json block = { { nflow::kId, "b" }, { nflow::kType, "demoCumprod" },
        { nflow::kParams, json::object() } };
    std::unordered_map<std::string, double> vars;

    BlockCodegenTemplate tpl;
    REQUIRE(BlockRegistry::instance().getCodegen("demoCumprod", "rust", tpl));

    std::vector<std::string> inits;
    BlockCodegenStateArgs sa;
    sa.id = "b";
    sa.block = &block;
    sa.params = &block[nflow::kParams];
    sa.variables = &vars;
    sa.fmt = [](double v) { return nflow::formatNumber(v) + "_f64"; };
    sa.addState = [&](const std::string&, const std::string& init, const std::string&) {
        inits.push_back(init);
    };
    tpl.emitState(sa);
    REQUIRE(inits.size() == 1);
    CHECK(inits[0] == nflow::formatNumber(1.0) + "_f64");

    std::vector<std::string> lines;
    BlockCodegenArgs a;
    a.id = "b";
    a.in = { "(u)", "0.0", "0.0" };
    a.block = &block;
    a.params = &block[nflow::kParams];
    a.variables = &vars;
    a.fmt = [](double v) { return nflow::formatNumber(v) + "_f64"; };
    a.line = [&](const std::string& l) { lines.push_back(l); };
    tpl.emitStep(a);
    REQUIRE(lines.size() == 2);
    CHECK(lines[0] == "out_b = s.cumprod_b * (u);");
    CHECK(lines[1] == "s.cumprod_b = out_b;");
}
//=============================================================================
TEST_CASE("plugin programmatic codegen: typed parameter callbacks")
{
    BlockRegistry::instance().registerCodegenEmitter("demoParams", "c", &demoParamEmit);
    BlockRegistry::instance().registerCodegenEmitter("demoParams", "rust", &demoParamEmit);

    json block = { { nflow::kId, "p" }, { nflow::kType, "demoParams" },
        { nflow::kParams,
            { { "gain", 2.5 }, { "flag", true }, { "label", "hello" },
                { "coeffs", json::array({ 1.0, 2.0, 3.0 }) } } } };
    std::unordered_map<std::string, double> vars;

    BlockCodegenTemplate tpl;
    REQUIRE(BlockRegistry::instance().getCodegen("demoParams", "c", tpl));

    std::vector<std::string> lines;
    BlockCodegenArgs a;
    a.id = "p";
    a.in = { "0.0", "0.0", "0.0" };
    a.block = &block;
    a.params = &block[nflow::kParams];
    a.variables = &vars;
    a.fmt = [](double v) { return nflow::formatNumber(v); };
    a.line = [&](const std::string& l) { lines.push_back(l); };
    tpl.emitStep(a);

    REQUIRE(lines.size() == 8);
    CHECK(lines[0] == "gain=" + nflow::formatNumber(2.5));
    CHECK(lines[1] == "missing=" + nflow::formatNumber(42.0)); // absent -> fallback
    CHECK(lines[2] == "hasGain=1");
    CHECK(lines[3] == "hasNope=0");
    CHECK(lines[4] == "flag=1");
    CHECK(lines[5] == "label=hello");
    CHECK(lines[6] == "ncoeffs=3");
    CHECK(lines[7] == "c1=" + nflow::formatNumber(2.0));
}
//=============================================================================
