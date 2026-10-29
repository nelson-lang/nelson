//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include <cmath>
#include <cstdlib>
#include <functional>
#include <string>
#include <vector>
#include "NFlowCodegenLang.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "logic_blocks.hpp"
//=============================================================================
// if: an action-selector driven by a restricted boolean expression over its
// inputs u1..un. The if clause and each elseif clause are evaluated in order;
// the first true clause drives its output to 1.0 and every other output to 0.0.
// With showElse, an all-false result drives the last (else) output.
//
// Grammar (deliberately restricted; NOT the general expression engine):
//   or   := and ('|' and)*
//   and  := not ('&' not)*
//   not  := '~' not | rel
//   rel  := unary ( ('<'|'<='|'>'|'>='|'=='|'~='|'!=') unary )?
//   unary:= '-' unary | primary
//   prim := number | 'u' <int> | '(' or ')'
// A bare value is true when non-zero; comparisons/logic yield 1.0 / 0.0.
//=============================================================================
namespace {
struct IfConditionParser
{
    const char* p;
    const std::vector<double>& u;
    bool ok = true;

    explicit IfConditionParser(const char* s, const std::vector<double>& in) : p(s), u(in) { }

    void
    skipws()
    {
        while (*p == ' ' || *p == '\t') {
            ++p;
        }
    }

    double
    parsePrimary()
    {
        skipws();
        if (*p == '(') {
            ++p;
            double v = parseOr();
            skipws();
            if (*p == ')') {
                ++p;
            } else {
                ok = false;
            }
            return v;
        }
        if (*p == 'u' || *p == 'U') {
            ++p;
            if (*p < '0' || *p > '9') {
                ok = false;
                return 0.0;
            }
            long k = std::strtol(p, const_cast<char**>(&p), 10); // 1-based input index
            if (k >= 1 && k <= static_cast<long>(u.size())) {
                return u[static_cast<size_t>(k - 1)];
            }
            ok = false;
            return 0.0;
        }
        if ((*p >= '0' && *p <= '9') || *p == '.') {
            char* end = nullptr;
            double v = std::strtod(p, &end);
            if (end == p) {
                ok = false;
                return 0.0;
            }
            p = end;
            return v;
        }
        ok = false;
        return 0.0;
    }

    double
    parseUnary()
    {
        skipws();
        if (*p == '-') {
            ++p;
            return -parseUnary();
        }
        if (*p == '+') {
            ++p;
            return parseUnary();
        }
        return parsePrimary();
    }

    double
    parseRel()
    {
        double a = parseUnary();
        skipws();
        int op = 0; // 1:< 2:<= 3:> 4:>= 5:== 6:!=
        if (*p == '<') {
            op = (p[1] == '=') ? 2 : 1;
            p += (op == 2) ? 2 : 1;
        } else if (*p == '>') {
            op = (p[1] == '=') ? 4 : 3;
            p += (op == 4) ? 2 : 1;
        } else if (*p == '=' && p[1] == '=') {
            op = 5;
            p += 2;
        } else if ((*p == '~' || *p == '!') && p[1] == '=') {
            op = 6;
            p += 2;
        }
        if (op == 0) {
            return a;
        }
        double b = parseUnary();
        switch (op) {
        case 1:
            return (a < b) ? 1.0 : 0.0;
        case 2:
            return (a <= b) ? 1.0 : 0.0;
        case 3:
            return (a > b) ? 1.0 : 0.0;
        case 4:
            return (a >= b) ? 1.0 : 0.0;
        case 5:
            return (a == b) ? 1.0 : 0.0;
        default:
            return (a != b) ? 1.0 : 0.0;
        }
    }

    double
    parseNot()
    {
        skipws();
        if (*p == '~' && p[1] != '=') {
            ++p;
            double r = parseNot();
            return (r == 0.0) ? 1.0 : 0.0;
        }
        return parseRel();
    }

    double
    parseAnd()
    {
        double v = parseNot();
        skipws();
        while (*p == '&') {
            ++p;
            double r = parseNot();
            v = ((v != 0.0) && (r != 0.0)) ? 1.0 : 0.0;
            skipws();
        }
        return v;
    }

    double
    parseOr()
    {
        double v = parseAnd();
        skipws();
        while (*p == '|') {
            ++p;
            double r = parseAnd();
            v = ((v != 0.0) || (r != 0.0)) ? 1.0 : 0.0;
            skipws();
        }
        return v;
    }
};
//=============================================================================
// Parse + evaluate a restricted if-condition. An empty expression and any parse
// error evaluate to false; the clause is not selected.
bool
evalIfCondition(const std::string& expr, const std::vector<double>& u)
{
    if (expr.empty()) {
        return false;
    }
    IfConditionParser parser(expr.c_str(), u);
    double v = parser.parseOr();
    parser.skipws();
    if (!parser.ok || *parser.p != '\0') {
        return false;
    }
    return v != 0.0;
}
//=============================================================================
// The elseif clauses are stored as a single comma-separated string (e.g.
// "u2 > 0, u3 < 0"); split on top-level commas (not inside parentheses).
std::vector<std::string>
splitElseIfExpressions(const std::string& s)
{
    std::vector<std::string> out;
    int depth = 0;
    size_t start = 0;
    for (size_t i = 0; i < s.size(); ++i) {
        const char c = s[i];
        if (c == '(') {
            ++depth;
        } else if (c == ')') {
            if (depth > 0) {
                --depth;
            }
        } else if (c == ',' && depth == 0) {
            out.push_back(s.substr(start, i - start));
            start = i + 1;
        }
    }
    if (start < s.size()) {
        out.push_back(s.substr(start));
    } else if (start == s.size() && !s.empty() && s.back() == ',') {
        out.emplace_back();
    }
    std::vector<std::string> cleaned;
    for (const std::string& e : out) {
        size_t a = e.find_first_not_of(" \t");
        if (a != std::string::npos) {
            cleaned.push_back(e);
        }
    }
    return cleaned;
}
//=============================================================================
bool
paramOnOff(const Nelson::NFlow::json& params, const char* key, bool defaultValue)
{
    if (!params.contains(key)) {
        return defaultValue;
    }
    const auto& v = params[key];
    if (v.is_boolean()) {
        return v.get<bool>();
    }
    if (v.is_string()) {
        const std::string s = v.get<std::string>();
        return s == "on" || s == "true" || s == "1";
    }
    if (v.is_number()) {
        return v.get<double>() != 0.0;
    }
    return defaultValue;
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleIf(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    const int nOut = std::max(1, b.outputs);
    const int nIn = std::max(0, b.inputs);
    std::vector<double> u(static_cast<size_t>(nIn), 0.0);
    for (int i = 0; i < nIn; ++i) {
        u[static_cast<size_t>(i)] = getInput(ctx, b.nid, i, 0.0);
    }

    int selected = -1;
    if (evalIfCondition(asJson(b.params).value(std::string("IfExpression"), std::string()), u)) {
        selected = 0;
    }
    if (selected < 0 && asJson(b.params).contains("ElseIfExpressions")
        && asJson(b.params)["ElseIfExpressions"].is_string()) {
        const std::vector<std::string> elifs
            = splitElseIfExpressions(asJson(b.params)["ElseIfExpressions"].get<std::string>());
        for (int i = 0; i < static_cast<int>(elifs.size()); ++i) {
            if (evalIfCondition(elifs[i], u)) {
                selected = 1 + i;
                break;
            }
        }
    }
    if (selected < 0 && paramOnOff(asJson(b.params), "ShowElse", false)) {
        selected = nOut - 1; // the else port is last
    }
    for (int p = 0; p < nOut; ++p) {
        setOutput(ctx, b.nid, (p == selected) ? 1.0 : 0.0, p);
    }
    return false;
}
//=============================================================================
// Code generation. The clause grammar above is evaluated at simulation time;
// here the same grammar is translated into a target expression worth 1.0 or
// 0.0, exactly as the evaluator would answer. Both walk the same productions in
// the same order, so a clause cannot parse differently in the two. A clause the
// parser refuses becomes 0.0, as in evalIfCondition.
//=============================================================================
namespace {
struct IfConditionTranslator
{
    const char* p;
    const std::vector<std::string>& u; // input expressions, 1-based in the text
    Nelson::NFlow::CodegenLang L;
    // Language-aware literal formatter: a bare "0" is an integer in Rust and
    // will not compare against an f64.
    const std::function<std::string(double)>& fmt;
    bool ok = true;

    IfConditionTranslator(const char* s, const std::vector<std::string>& in,
        Nelson::NFlow::CodegenLang lang, const std::function<std::string(double)>& f)
        : p(s), u(in), L(lang), fmt(f)
    {
    }

    void
    skipws()
    {
        while (*p == ' ' || *p == '\t') {
            ++p;
        }
    }
    // "1.0 when non-zero, else 0.0", in the target language.
    std::string
    truth(const std::string& e) const
    {
        return "(" + L.ternary("(" + e + ") != 0.0", "1.0", "0.0") + ")";
    }
    std::string
    parsePrimary()
    {
        skipws();
        if (*p == '(') {
            ++p;
            std::string v = parseOr();
            skipws();
            if (*p == ')') {
                ++p;
            } else {
                ok = false;
            }
            return "(" + v + ")";
        }
        if (*p == 'u' || *p == 'U') {
            ++p;
            if (*p < '0' || *p > '9') {
                ok = false;
                return "0.0";
            }
            long k = std::strtol(p, const_cast<char**>(&p), 10); // 1-based input index
            if (k >= 1 && k <= static_cast<long>(u.size())) {
                return "(" + u[static_cast<size_t>(k - 1)] + ")";
            }
            ok = false;
            return "0.0";
        }
        if ((*p >= '0' && *p <= '9') || *p == '.') {
            char* end = nullptr;
            const double d = std::strtod(p, &end);
            if (end == p) {
                ok = false;
                return "0.0";
            }
            p = end;
            return fmt(d);
        }
        ok = false;
        return "0.0";
    }
    std::string
    parseUnary()
    {
        skipws();
        if (*p == '-') {
            ++p;
            return "(-(" + parseUnary() + "))";
        }
        if (*p == '+') {
            ++p;
            return parseUnary();
        }
        return parsePrimary();
    }
    std::string
    parseRel()
    {
        std::string a = parseUnary();
        skipws();
        std::string op;
        if (*p == '<') {
            op = (p[1] == '=') ? "<=" : "<";
        } else if (*p == '>') {
            op = (p[1] == '=') ? ">=" : ">";
        } else if (*p == '=' && p[1] == '=') {
            op = "==";
        } else if ((*p == '~' || *p == '!') && p[1] == '=') {
            op = "!=";
        }
        if (op.empty()) {
            return a;
        }
        p += op.size();
        std::string b = parseUnary();
        return "(" + L.ternary("(" + a + ") " + op + " (" + b + ")", "1.0", "0.0") + ")";
    }
    std::string
    parseNot()
    {
        skipws();
        if (*p == '~' && p[1] != '=') {
            ++p;
            const std::string r = parseNot();
            return "(" + L.ternary("(" + r + ") == 0.0", "1.0", "0.0") + ")";
        }
        return parseRel();
    }
    std::string
    parseAnd()
    {
        std::string v = parseNot();
        skipws();
        while (*p == '&') {
            ++p;
            const std::string r = parseNot();
            v = "(" + L.ternary("(" + v + ") != 0.0 && (" + r + ") != 0.0", "1.0", "0.0") + ")";
            skipws();
        }
        return v;
    }
    std::string
    parseOr()
    {
        std::string v = parseAnd();
        skipws();
        while (*p == '|') {
            ++p;
            const std::string r = parseAnd();
            v = "(" + L.ternary("(" + v + ") != 0.0 || (" + r + ") != 0.0", "1.0", "0.0") + ")";
            skipws();
        }
        return v;
    }
};
//=============================================================================
std::string
translateIfCondition(const std::string& expr, const std::vector<std::string>& u,
    Nelson::NFlow::CodegenLang L, const std::function<std::string(double)>& fmt)
{
    if (expr.empty()) {
        return "0.0";
    }
    IfConditionTranslator tr(expr.c_str(), u, L, fmt);
    const std::string v = tr.parseOr();
    tr.skipws();
    if (!tr.ok || *tr.p != '\0') {
        return "0.0";
    }
    return tr.truth(v);
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
makeIfCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    t.emitStep = [L](const BlockCodegenArgs& a) {
        const json& p = *asJson(a.params);
        const int nOut = std::max(1, a.block->value(nflow::kOutputs, 1));
        std::vector<std::string> u(a.in.begin(), a.in.end());
        std::vector<std::string> clauses;
        clauses.push_back(p.contains("IfExpression") && p["IfExpression"].is_string()
                ? p["IfExpression"].get<std::string>()
                : std::string());
        if (p.contains("ElseIfExpressions") && p["ElseIfExpressions"].is_string()) {
            for (const auto& e :
                splitElseIfExpressions(p["ElseIfExpressions"].get<std::string>())) {
                clauses.push_back(e);
            }
        }
        const bool showElse = paramOnOff(p, "ShowElse", false);
        // A clause drives its port only when it holds and no earlier one did (the
        // first true clause wins, as in the evaluator). `taken` accumulates
        // "some earlier clause held".
        std::string taken;
        for (int k = 0; k < nOut; ++k) {
            const bool isElse = showElse && (k == nOut - 1);
            std::string cond;
            if (isElse) {
                cond = taken.empty()
                    ? std::string("1.0")
                    : ("(" + L.ternary("(" + taken + ") != 0.0", "0.0", "1.0") + ")");
            } else if (k < static_cast<int>(clauses.size())) {
                const std::string c
                    = translateIfCondition(clauses[static_cast<size_t>(k)], u, L, a.fmt);
                cond = taken.empty()
                    ? c
                    : ("(" + L.ternary("(" + taken + ") == 0.0 && (" + c + ") != 0.0", "1.0", "0.0")
                          + ")");
            } else {
                cond = "0.0"; // more ports than clauses: never selected
            }
            a.line(nflow::codegenOutVar(a.id, k) + " = " + cond + ";");
            if (!isElse) {
                taken = taken.empty()
                    ? cond
                    : ("("
                          + L.ternary(
                              "(" + taken + ") != 0.0 || (" + cond + ") != 0.0", "1.0", "0.0")
                          + ")");
            }
        }
    };
    return t;
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCIf()
{
    return makeIfCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustIf()
{
    return makeIfCodegen({ true });
}
//=============================================================================
