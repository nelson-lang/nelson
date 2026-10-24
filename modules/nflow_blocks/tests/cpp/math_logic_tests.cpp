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
#include <cmath>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::Phase;
static Harness
blockWithInputs(const std::string& type, json params, const std::vector<double>& values)
{
    Harness h;
    auto b = h.addBlock("b", type, static_cast<int>(values.size()), 1, std::move(params));
    for (int i = 0; i < static_cast<int>(values.size()); ++i) {
        h.connect(h.addInput(values[static_cast<size_t>(i)]), b, i);
    }
    return h;
}
//=============================================================================
TEST_CASE("math handlers compute direct algebraic outputs")
{
    {
        auto h = blockWithInputs("gain", { { nflow::kGain, 4.0 } }, { 2.5 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 10.0);
    }
    {
        auto h = blockWithInputs("sum", { { nflow::kSumInputs, "+-+" } }, { 10.0, 3.0, 2.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 9.0);
    }
    {
        auto h = blockWithInputs("mult", json::object(), { 2.0, 3.0, 4.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 24.0);
    }
    {
        auto h = blockWithInputs("abs", json::object(), { -4.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 4.0);
    }
    {
        auto h = blockWithInputs("divide", json::object(), { 9.0, 3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 3.0);
    }
    {
        auto h = blockWithInputs("negate", json::object(), { 5.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -5.0);
    }
    {
        auto h = blockWithInputs("bias", { { nflow::kBias, 1.25 } }, { 2.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 3.25);
    }
    {
        auto h = blockWithInputs("min", json::object(), { 2.0, -3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -3.0);
    }
    {
        auto h = blockWithInputs("max", json::object(), { 2.0, -3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 2.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "sqrt" } }, { 9.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 3.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "exp" } }, { 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "sign" } }, { -7.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -1.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "sin" } }, { 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.0);
    }
    {
        // 10^u at u=2 -> 100
        auto h = blockWithInputs("mathFunction", { { "Function", "10^u" } }, { 2.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 100.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "square" } }, { -3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 9.0);
    }
    {
        // 'magnitude^2' is another tool's name for the same thing on a real signal.
        // It must not fall through to the identity.
        auto h = blockWithInputs("mathFunction", { { "Function", "magnitude^2" } }, { -3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 9.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "reciprocal" } }, { 4.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.25);
    }
    {
        auto h = blockWithInputs("roundingFunction", { { "Operator", "floor" } }, { 2.7 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 2.0);
    }
    {
        auto h = blockWithInputs("roundingFunction", { { "Operator", "ceil" } }, { 2.1 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 3.0);
    }
    {
        // ties away from zero: -2.5 -> -3
        auto h = blockWithInputs("roundingFunction", { { "Operator", "round" } }, { -2.5 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -3.0);
    }
    {
        // truncate toward zero: -2.7 -> -2
        auto h = blockWithInputs("roundingFunction", { { "Operator", "fix" } }, { -2.7 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -2.0);
    }
    {
        auto h = blockWithInputs("sign", json::object(), { -7.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -1.0);
    }
    {
        auto h = blockWithInputs("sign", json::object(), { 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.0);
    }
    {
        auto h = blockWithInputs("sqrt", { { "Function", "sqrt" } }, { 9.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 3.0);
    }
    {
        auto h = blockWithInputs("sqrt", { { "Function", "signedSqrt" } }, { -16.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -4.0);
    }
    {
        auto h = blockWithInputs("sqrt", { { "Function", "rSqrt" } }, { 4.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.5);
    }
    {
        auto h = blockWithInputs("trigFunction", { { "Function", "sin" } }, { 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.0);
    }
    {
        auto h = blockWithInputs("trigFunction", { { "Function", "cos" } }, { 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h = blockWithInputs("trigFunction", { { "Function", "tanh" } }, { 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.0);
    }
    {
        auto h = blockWithInputs("trigFunction", { { "Function", "acosh" } }, { 1.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.0);
    }
    {
        // trigFunction atan2(1, 1) = pi/4 (u1 = y on port 0, u2 = x on port 1)
        auto h = blockWithInputs("trigFunction", { { "Function", "atan2" } }, { 1.0, 1.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), std::atan(1.0));
    }
    {
        // trigFunction sincos: sin on port 0, cos on port 1
        Harness h;
        auto b = h.addBlock("b", "trigFunction", 1, 2, { { "Function", "sincos" } });
        h.connect(h.addInput(0.0), b);
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(b, 0), 0.0);
        checkClose(h.output(b, 1), 1.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "pow" } }, { 2.0, 3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 8.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "hypot" } }, { 3.0, 4.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 5.0);
    }
    {
        // rem keeps the sign of the dividend: rem(-7, 3) = -1
        auto h = blockWithInputs("mathFunction", { { "Function", "rem" } }, { -7.0, 3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -1.0);
    }
    {
        // mod keeps the sign of the divisor: mod(-7, 3) = 2
        auto h = blockWithInputs("mathFunction", { { "Function", "mod" } }, { -7.0, 3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 2.0);
    }
    {
        auto h = blockWithInputs("mathFunction", { { "Function", "mod" } }, { 7.0, -3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), -2.0);
    }
}
//=============================================================================
TEST_CASE("multiportSwitch routes the control-selected data input")
{
    // Port 0 = control (one-based), ports 1..3 = data.
    auto run = [](double ctrl) {
        auto h = blockWithInputs("multiportSwitch", json::object(), { ctrl, 10.0, 20.0, 30.0 });
        h.run(0, Phase::ALGEBRAIC);
        return h.output(0);
    };
    checkClose(run(1.0), 10.0);
    checkClose(run(2.0), 20.0);
    checkClose(run(3.0), 30.0);
    // Truncates to the integer part, clamps out-of-range to the first / last data port.
    checkClose(run(1.6), 10.0);
    checkClose(run(0.0), 10.0);
    checkClose(run(9.0), 30.0);
}
//=============================================================================
TEST_CASE("logic handlers compute boolean outputs")
{
    {
        auto h = blockWithInputs("and", json::object(), { 1.0, 1.0, 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 0.0);
    }
    {
        auto h = blockWithInputs("or", json::object(), { 0.0, 1.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h = blockWithInputs("xor", json::object(), { 1.0, 1.0, 1.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h = blockWithInputs("not", json::object(), { 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h = blockWithInputs(
            "compareToConstant", { { nflow::kConst, 3.0 }, { nflow::kRelop, "gt" } }, { 4.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h = blockWithInputs("compareToZero", { { nflow::kRelop, "eq" } }, { 0.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
    {
        auto h
            = blockWithInputs("relationalOperator", { { nflow::kOperator, "le" } }, { 2.0, 3.0 });
        h.run(0, Phase::ALGEBRAIC);
        checkClose(h.output(0), 1.0);
    }
}
//=============================================================================
// L3: switchCase: integer control routes to the matching case's output port.
//=============================================================================
TEST_CASE("switchCase routes control to the matching case output")
{
    ensureHandlersRegistered();
    json p;
    p["CaseConditions"] = "{1, [7 9 4]}";
    p["ShowDefaultCase"] = "on"; // 3 outputs: case0, case1, default

    auto check = [&](double u, double e0, double e1, double e2) {
        Harness h;
        BlockId sw = h.addBlock("sw", "switchCase", 1, 3, p);
        h.connect(h.addInput(u), sw);
        h.run(sw, Phase::ALGEBRAIC);
        checkClose(h.output(sw, 0), e0);
        checkClose(h.output(sw, 1), e1);
        checkClose(h.output(sw, 2), e2);
    };
    check(1.0, 1.0, 0.0, 0.0); // case 0
    check(9.0, 0.0, 1.0, 0.0); // case 1 (in the list)
    check(4.0, 0.0, 1.0, 0.0); // case 1 (another list value)
    check(5.0, 0.0, 0.0, 1.0); // no match -> default
    check(1.9, 1.0, 0.0, 0.0); // truncated toward zero: 1.9 -> 1 -> case 0
}
//=============================================================================
TEST_CASE("switchCase without a default leaves all outputs 0 on no match")
{
    ensureHandlersRegistered();
    json p;
    p["CaseConditions"] = "{1, 2}";
    p["ShowDefaultCase"] = "off";
    Harness h;
    BlockId sw = h.addBlock("sw", "switchCase", 1, 2, p);
    h.connect(h.addInput(5.0), sw); // no match, no default
    h.run(sw, Phase::ALGEBRAIC);
    checkClose(h.output(sw, 0), 0.0);
    checkClose(h.output(sw, 1), 0.0);
}
//=============================================================================
// L3: if block: a restricted boolean expression selects the first true clause.
//=============================================================================
TEST_CASE("if block selects the first true clause")
{
    ensureHandlersRegistered();
    json p;
    p["IfExpression"] = "u1 > 0.5";
    p["ElseIfExpressions"] = "u2 > 0.5";
    p["ShowElse"] = "on"; // 3 outputs: if, elseif, else

    auto sel = [&](double u1, double u2) {
        Harness h;
        BlockId iff = h.addBlock("if", "if", 2, 3, p);
        h.connect(h.addInput(u1), iff, 0);
        h.connect(h.addInput(u2), iff, 1);
        h.run(iff, Phase::ALGEBRAIC);
        for (int k = 0; k < 3; ++k) {
            if (h.output(iff, k) > 0.5) {
                return k;
            }
        }
        return -1;
    };
    CHECK(sel(1.0, 0.0) == 0);
    CHECK(sel(0.0, 1.0) == 1);
    CHECK(sel(0.0, 0.0) == 2);
    CHECK(sel(1.0, 1.0) == 0); // if wins (first true clause)
}
//=============================================================================
TEST_CASE("if condition parser handles comparisons, logic and unary minus")
{
    ensureHandlersRegistered();
    auto truth = [&](const std::string& expr, double u1, double u2) {
        json p;
        p["IfExpression"] = expr;
        p["ShowElse"] = "on"; // 2 outputs: if, else
        Harness h;
        BlockId iff = h.addBlock("if", "if", 2, 2, p);
        h.connect(h.addInput(u1), iff, 0);
        h.connect(h.addInput(u2), iff, 1);
        h.run(iff, Phase::ALGEBRAIC);
        return h.output(iff, 0) > 0.5;
    };
    CHECK(truth("u1 > 0.5 & u2 < 0.5", 1.0, 0.0));
    CHECK_FALSE(truth("u1 > 0.5 & u2 < 0.5", 1.0, 1.0));
    CHECK(truth("~(u1 > 0.5)", 0.0, 0.0));
    CHECK(truth("u1 == 2", 2.0, 0.0));
    CHECK(truth("u1 ~= 0", 3.0, 0.0));
    CHECK(truth("u1 != 0", 3.0, 0.0));
    CHECK(truth("(u1 > 0) | (u2 > 0)", 0.0, 1.0));
    CHECK_FALSE(truth("(u1 > 0) | (u2 > 0)", 0.0, 0.0));
    CHECK(truth("-u1 < 0", 2.0, 0.0)); // unary minus: -2 < 0
    CHECK_FALSE(truth("bogus $$", 1.0, 0.0)); // malformed -> false
}
//=============================================================================
