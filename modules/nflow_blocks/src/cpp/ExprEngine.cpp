//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#define _USE_MATH_DEFINES
#include "ExprEngine.hpp"
#include <cctype>
#include <cmath>
#include <limits>
#include <set>
#include <nlohmann/json.hpp>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>
//=============================================================================
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_E
#define M_E 2.71828182845904523536
#endif
//=============================================================================
using json = nlohmann::json;
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    enum class TokType
    {
        Number,
        Ident,
        Op,
        LParen,
        RParen,
        Comma
    };
    //=============================================================================
    struct Token
    {
        TokType type;
        std::string text;
        double num = 0.0;
    };
    //=============================================================================
    enum class RpnType
    {
        Number,
        Ident,
        Op,
        Func
    };
    //=============================================================================
    struct RpnToken
    {
        RpnType type;
        std::string text;
        double num = 0.0;
        int argc = 0;
    };
    //=============================================================================
    static std::vector<Token>
    lexExpr(const std::string& expr)
    {
        std::vector<Token> tokens;
        int i = 0;
        int n = static_cast<int>(expr.size());

        while (i < n) {
            char c = expr[i];
            if (std::isspace(static_cast<unsigned char>(c))) {
                ++i;
                continue;
            }
            if (std::isdigit(static_cast<unsigned char>(c)) || c == '.') {
                int start = i;
                while (i < n
                    && (std::isdigit(static_cast<unsigned char>(expr[i])) || expr[i] == '.')) {
                    ++i;
                }
                if (i < n && (expr[i] == 'e' || expr[i] == 'E')) {
                    ++i;
                    if (i < n && (expr[i] == '+' || expr[i] == '-')) {
                        ++i;
                    }
                    while (i < n && std::isdigit(static_cast<unsigned char>(expr[i]))) {
                        ++i;
                    }
                }
                Token tok;
                tok.type = TokType::Number;
                tok.text = expr.substr(start, i - start);
                tok.num = std::stod(tok.text);
                tokens.push_back(tok);
                continue;
            }
            if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
                int start = i;
                while (i < n
                    && (std::isalnum(static_cast<unsigned char>(expr[i])) || expr[i] == '_')) {
                    ++i;
                }
                Token tok;
                tok.type = TokType::Ident;
                tok.text = expr.substr(start, i - start);
                tokens.push_back(tok);
                continue;
            }
            if (c == '+' || c == '-' || c == '*' || c == '/' || c == '^' || c == '%') {
                Token tok;
                tok.type = TokType::Op;
                tok.text = std::string(1, c);
                tokens.push_back(tok);
                ++i;
                continue;
            }
            if (c == '(') {
                tokens.push_back({ TokType::LParen, "(", 0 });
                ++i;
                continue;
            }
            if (c == ')') {
                tokens.push_back({ TokType::RParen, ")", 0 });
                ++i;
                continue;
            }
            if (c == ',') {
                tokens.push_back({ TokType::Comma, ",", 0 });
                ++i;
                continue;
            }
            // Unknown character: skip
            ++i;
        }
        return tokens;
    }
    //=============================================================================
    static int
    opPrecedence(const std::string& op)
    {
        if (op == "+" || op == "-") {
            return 1;
        }
        if (op == "*" || op == "/" || op == "%") {
            return 2;
        }
        if (op == "^") {
            return 3;
        }
        return 0;
    }
    //=============================================================================
    static bool
    isRightAssoc(const std::string& op)
    {
        return op == "^";
    }
    //=============================================================================
    static std::vector<RpnToken>
    shuntingYard(const std::vector<Token>& tokens)
    {
        std::vector<RpnToken> output;
        struct StackItem
        {
            std::string text;
            bool isFunc;
            int argc;
        };
        std::stack<StackItem> opStack;
        std::stack<int> argCounts;

        for (int i = 0; i < static_cast<int>(tokens.size()); ++i) {
            const auto& tok = tokens[i];

            if (tok.type == TokType::Number) {
                output.push_back({ RpnType::Number, tok.text, tok.num, 0 });
                continue;
            }
            if (tok.type == TokType::Ident) {
                bool isFunc = (i + 1 < static_cast<int>(tokens.size()))
                    && tokens[i + 1].type == TokType::LParen;
                if (isFunc) {
                    opStack.push({ tok.text, true, 0 });
                    argCounts.push(0);
                } else {
                    output.push_back({ RpnType::Ident, tok.text, 0, 0 });
                }
                continue;
            }
            if (tok.type == TokType::Op) {
                // Unary minus: the previous token is absent, an operator or LParen
                bool unary = false;
                if (tok.text == "-") {
                    if (i == 0 || tokens[i - 1].type == TokType::Op
                        || tokens[i - 1].type == TokType::LParen
                        || tokens[i - 1].type == TokType::Comma) {
                        unary = true;
                    }
                }
                if (unary) {
                    // Emit a synthetic 0, then the operator will become 0 - x
                    output.push_back({ RpnType::Number, "0", 0.0, 0 });
                }
                while (!opStack.empty() && !opStack.top().isFunc) {
                    const auto& top = opStack.top();
                    if (top.text == "(") {
                        break;
                    }
                    int tp = opPrecedence(top.text);
                    int cp = opPrecedence(tok.text);
                    if (tp > cp || (tp == cp && !isRightAssoc(tok.text))) {
                        output.push_back({ RpnType::Op, top.text, 0, 2 });
                        opStack.pop();
                    } else {
                        break;
                    }
                }
                opStack.push({ tok.text, false, 2 });
                continue;
            }
            if (tok.type == TokType::LParen) {
                opStack.push({ "(", false, 0 });
                continue;
            }
            if (tok.type == TokType::Comma) {
                while (!opStack.empty() && opStack.top().text != "(") {
                    const auto& top = opStack.top();
                    if (top.isFunc) {
                        output.push_back({ RpnType::Func, top.text, 0, top.argc });
                    } else {
                        output.push_back({ RpnType::Op, top.text, 0, 2 });
                    }
                    opStack.pop();
                }
                if (!argCounts.empty()) {
                    ++argCounts.top();
                }
                continue;
            }
            if (tok.type == TokType::RParen) {
                while (!opStack.empty() && opStack.top().text != "(") {
                    const auto& top = opStack.top();
                    if (top.isFunc) {
                        output.push_back({ RpnType::Func, top.text, 0, top.argc });
                    } else {
                        output.push_back({ RpnType::Op, top.text, 0, 2 });
                    }
                    opStack.pop();
                }
                if (!opStack.empty() && opStack.top().text == "(") {
                    opStack.pop();
                }
                if (!opStack.empty() && opStack.top().isFunc) {
                    int argc = 0;
                    if (!argCounts.empty()) {
                        argc = argCounts.top() + 1; // +1 for the last argument
                        argCounts.pop();
                    }
                    auto& top = opStack.top();
                    output.push_back({ RpnType::Func, top.text, 0, argc });
                    opStack.pop();
                }
                continue;
            }
        }

        while (!opStack.empty()) {
            const auto& top = opStack.top();
            if (top.isFunc) {
                output.push_back({ RpnType::Func, top.text, 0, top.argc });
            } else {
                output.push_back({ RpnType::Op, top.text, 0, 2 });
            }
            opStack.pop();
        }

        return output;
    }
    //=============================================================================
    static double
    clamp(double x, double lo, double hi)
    {
        return std::max(lo, std::min(hi, x));
    }
    //=============================================================================
    static double
    sinc(double x)
    {
        if (x == 0.0) {
            return 1.0;
        }
        double px = M_PI * x;
        return std::sin(px) / px;
    }
    //=============================================================================
    static bool
    getBuiltinConst(const std::string& name, double& out)
    {
        if (name == "pi" || name == "PI") {
            out = M_PI;
            return true;
        }
        if (name == "e" || name == "E") {
            out = M_E;
            return true;
        }
        if (name == "inf" || name == "Inf") {
            out = std::numeric_limits<double>::infinity();
            return true;
        }
        return false;
    }
    //=============================================================================
    static bool
    callBuiltinFunc(const std::string& name, std::stack<double>& stack, int argc, std::string& err)
    {
        auto pop = [&]() -> double {
            if (stack.empty()) {
                throw std::runtime_error("stack underflow");
            }
            double v = stack.top();
            stack.pop();
            return v;
        };

        auto
            requires
        = [&](int n) {
              if (argc < n) {
                  throw std::runtime_error("wrong number of arguments for " + name);
              }
          };

        if (name == "abs" || name == "fabs") {
            requires(1);
            stack.push(std::abs(pop()));
            return true;
        }
        if (name == "ceil") {
            requires(1);
            stack.push(std::ceil(pop()));
            return true;
        }
        if (name == "floor") {
            requires(1);
            stack.push(std::floor(pop()));
            return true;
        }
        if (name == "round") {
            requires(1);
            double v = pop();
            stack.push(std::round(v));
            return true;
        }
        if (name == "sign") {
            requires(1);
            double v = pop();
            stack.push(v > 0 ? 1.0 : v < 0 ? -1.0 : 0.0);
            return true;
        }
        if (name == "sqrt") {
            requires(1);
            stack.push(std::sqrt(pop()));
            return true;
        }
        if (name == "exp") {
            requires(1);
            stack.push(std::exp(pop()));
            return true;
        }
        if (name == "log") {
            requires(1);
            stack.push(std::log(pop()));
            return true;
        }
        if (name == "log10") {
            requires(1);
            stack.push(std::log10(pop()));
            return true;
        }
        if (name == "log2") {
            requires(1);
            stack.push(std::log2(pop()));
            return true;
        }
        if (name == "acos") {
            requires(1);
            stack.push(std::acos(pop()));
            return true;
        }
        if (name == "asin") {
            requires(1);
            stack.push(std::asin(pop()));
            return true;
        }
        if (name == "atan") {
            requires(1);
            stack.push(std::atan(pop()));
            return true;
        }
        if (name == "cos") {
            requires(1);
            stack.push(std::cos(pop()));
            return true;
        }
        if (name == "cosh") {
            requires(1);
            stack.push(std::cosh(pop()));
            return true;
        }
        if (name == "sin") {
            requires(1);
            stack.push(std::sin(pop()));
            return true;
        }
        if (name == "sinh") {
            requires(1);
            stack.push(std::sinh(pop()));
            return true;
        }
        if (name == "tan") {
            requires(1);
            stack.push(std::tan(pop()));
            return true;
        }
        if (name == "tanh") {
            requires(1);
            stack.push(std::tanh(pop()));
            return true;
        }
        if (name == "sinc") {
            requires(1);
            stack.push(sinc(pop()));
            return true;
        }
        if (name == "pow") {
            requires(2);
            double b2 = pop();
            double b1 = pop();
            stack.push(std::pow(b1, b2));
            return true;
        }
        if (name == "atan2") {
            requires(2);
            double x = pop();
            double y = pop();
            stack.push(std::atan2(y, x));
            return true;
        }
        if (name == "min" || name == "fmin") {
            requires(2);
            double b = pop();
            double a = pop();
            stack.push(std::min(a, b));
            return true;
        }
        if (name == "max" || name == "fmax") {
            requires(2);
            double b = pop();
            double a = pop();
            stack.push(std::max(a, b));
            return true;
        }
        if (name == "clamp") {
            requires(3);
            double hi = pop();
            double lo = pop();
            double v = pop();
            stack.push(clamp(v, lo, hi));
            return true;
        }
        (void)err;
        return false;
    }
    //=============================================================================
    static double
    runRpn(const std::vector<RpnToken>& rpn, const std::unordered_map<std::string, double>& scope)
    {
        std::stack<double> stack;

        for (const auto& tok : rpn) {
            if (tok.type == RpnType::Number) {
                stack.push(tok.num);
                continue;
            }
            if (tok.type == RpnType::Ident) {
                double constVal;
                if (getBuiltinConst(tok.text, constVal)) {
                    stack.push(constVal);
                    continue;
                }
                auto it = scope.find(tok.text);
                if (it == scope.end()) {
                    throw std::runtime_error("undefined variable: " + tok.text);
                }
                stack.push(it->second);
                continue;
            }
            if (tok.type == RpnType::Op) {
                if (stack.size() < 2) {
                    throw std::runtime_error("stack underflow in operator");
                }
                double b = stack.top();
                stack.pop();
                double a = stack.top();
                stack.pop();
                if (tok.text == "+") {
                    stack.push(a + b);
                } else if (tok.text == "-") {
                    stack.push(a - b);
                } else if (tok.text == "*") {
                    stack.push(a * b);
                } else if (tok.text == "/") {
                    stack.push(a / b);
                } else if (tok.text == "%") {
                    stack.push(std::fmod(a, b));
                } else if (tok.text == "^") {
                    stack.push(std::pow(a, b));
                } else {
                    throw std::runtime_error("unknown operator: " + tok.text);
                }
                continue;
            }
            if (tok.type == RpnType::Func) {
                std::string err;
                if (!callBuiltinFunc(tok.text, stack, tok.argc, err)) {
                    throw std::runtime_error("unknown function: " + tok.text);
                }
                continue;
            }
        }

        if (stack.empty()) {
            throw std::runtime_error("empty expression");
        }
        return stack.top();
    }
    //=============================================================================
    // Code-generation emitter (RPN -> C / Rust scalar expression).
    // Reconstructs an infix expression from the parsed RPN so the emitted code
    // matches the interpreter's semantics: notably "^" becomes pow() (in C/Rust
    // "^" is XOR), and every function/constant is mapped to its C or Rust form.
    // Returns "" when the expression uses something that cannot be emitted
    // (unknown function, wrong arity) so callers reject it.
    //=============================================================================
    static std::string
    emitNumberLit(double v, const std::string& text, bool rust)
    {
        if (!rust) {
            return text;
        }
        std::string s = text;
        if (s.find('.') == std::string::npos && s.find('e') == std::string::npos
            && s.find('E') == std::string::npos) {
            s += ".0";
        }
        (void)v;
        return s + "_f64";
    }
    //=============================================================================
    static std::string
    emitCall(const std::string& fn, const std::vector<std::string>& a, bool rust)
    {
        std::string p = rust ? "libm::" : "";
        auto one = [&](const std::string& name) { return p + name + "(" + a[0] + ")"; };
        auto two
            = [&](const std::string& name) { return p + name + "(" + a[0] + ", " + a[1] + ")"; };
        static const std::set<std::string> unary { "sin", "cos", "tan", "asin", "acos", "atan",
            "sinh", "cosh", "tanh", "sqrt", "exp", "log", "log10", "log2", "floor", "ceil", "round",
            "fabs" };
        if (a.size() == 1 && unary.count(fn)) {
            return one(fn);
        }
        if (a.size() == 2 && (fn == "atan2" || fn == "pow")) {
            return two(fn);
        }
        if (a.size() == 2 && (fn == "fmin" || fn == "min")) {
            return two("fmin");
        }
        if (a.size() == 2 && (fn == "fmax" || fn == "max")) {
            return two("fmax");
        }
        if (a.size() == 3 && fn == "clamp") {
            return p + "fmin(" + p + "fmax(" + a[0] + ", " + a[1] + "), " + a[2] + ")";
        }
        if (a.size() == 1 && fn == "sign") {
            if (rust) {
                return "(if " + a[0] + " > 0.0_f64 { 1.0_f64 } else if " + a[0]
                    + " < 0.0_f64 { -1.0_f64 } else { 0.0_f64 })";
            }
            return "((" + a[0] + ") > 0.0 ? 1.0 : ((" + a[0] + ") < 0.0 ? -1.0 : 0.0))";
        }
        if (a.size() == 1 && fn == "sinc") {
            // sinc(x) = x==0 ? 1 : sin(pi*x)/(pi*x)
            if (rust) {
                return "(if " + a[0] + " == 0.0_f64 { 1.0_f64 } else { libm::sin("
                    + "core::f64::consts::PI * " + a[0] + ") / (core::f64::consts::PI * " + a[0]
                    + ") })";
            }
            return "((" + a[0] + ") == 0.0 ? 1.0 : sin(M_PI * (" + a[0] + ")) / (M_PI * (" + a[0]
                + ")))";
        }
        return ""; // unknown / unsupported function -> reject
    }
    //=============================================================================
    std::string
    emitMathExpression(const std::string& expr, bool rust, const std::string& uReplacement)
    {
        std::vector<Token> tokens = lexExpr(expr);
        std::vector<RpnToken> rpn = shuntingYard(tokens);
        std::vector<std::string> st;
        for (const auto& tk : rpn) {
            if (tk.type == RpnType::Number) {
                st.push_back(emitNumberLit(tk.num, tk.text, rust));
            } else if (tk.type == RpnType::Ident) {
                if (tk.text == "u") {
                    st.push_back(uReplacement);
                } else if (tk.text == "pi" || tk.text == "PI") {
                    st.push_back(rust ? "core::f64::consts::PI" : "M_PI");
                } else if (tk.text == "e" || tk.text == "E") {
                    st.push_back(rust ? "core::f64::consts::E" : "M_E");
                } else if (tk.text == "inf" || tk.text == "Inf") {
                    st.push_back(rust ? "f64::INFINITY" : "INFINITY");
                } else {
                    st.push_back(tk.text); // t, dt, diagram variables
                }
            } else if (tk.type == RpnType::Op) {
                if (st.size() < 2) {
                    return "";
                }
                std::string b = st.back();
                st.pop_back();
                std::string a = st.back();
                st.pop_back();
                if (tk.text == "^") {
                    st.push_back(std::string(rust ? "libm::pow(" : "pow(") + a + ", " + b + ")");
                } else {
                    st.push_back("(" + a + " " + tk.text + " " + b + ")");
                }
            } else if (tk.type == RpnType::Func) {
                if (static_cast<int>(st.size()) < tk.argc || tk.argc < 0) {
                    return "";
                }
                std::vector<std::string> args(tk.argc);
                for (int k = tk.argc - 1; k >= 0; --k) {
                    args[k] = st.back();
                    st.pop_back();
                }
                std::string call = emitCall(tk.text, args, rust);
                if (call.empty()) {
                    return "";
                }
                st.push_back(call);
            }
        }
        if (st.size() != 1) {
            return "";
        }
        return st.front();
    }
    //=============================================================================
    double
    evalExpressionValue(
        const std::string& expr, const std::unordered_map<std::string, double>& variables)
    {
        auto tokens = lexExpr(expr);
        auto rpn = shuntingYard(tokens);
        return runRpn(rpn, variables);
    }
    //=============================================================================
    std::string
    emitMathExpressionCodegen(
        const std::string& expr, const std::string& lang, const std::string& uReplacement)
    {
        return emitMathExpression(expr, lang == "rust", uReplacement);
    }
    //=============================================================================
    std::string
    evalExpressionJson(const std::string& jsonStr)
    {
        json input;
        try {
            input = json::parse(jsonStr);
        } catch (...) {
            return R"({"ok":false,"error":"JSON parse error"})";
        }

        std::string expr = input.value("expr", std::string());
        if (expr.empty()) {
            return R"({"ok":false,"error":"missing expr"})";
        }

        std::unordered_map<std::string, double> variables;
        if (input.contains("variables") && input["variables"].is_object()) {
            for (auto& [k, v] : input["variables"].items()) {
                if (v.is_number()) {
                    variables[k] = v.get<double>();
                }
            }
        }

        try {
            double val = evalExpressionValue(expr, variables);
            json out;
            out["ok"] = true;
            out["value"] = val;
            return out.dump();
        } catch (const std::exception& ex) {
            json out;
            out["ok"] = false;
            out["error"] = std::string(ex.what());
            return out.dump();
        }
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
