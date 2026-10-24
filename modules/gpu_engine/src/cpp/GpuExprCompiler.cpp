//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <cstdio>
#include <unordered_map>
#include <vector>
#include "GpuExprCompiler.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "AbstractSyntaxTree.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // Elementwise arithmetic ops -> "(L op R)". In an anonymous body every element
    // is a scalar, so `*` and `.*` are equivalent (both emit "*").
    const std::unordered_map<int, const char*> arithmeticBinary = {
        { OP_PLUS, "+" }, { OP_SUBTRACT, "-" }, { OP_TIMES, "*" }, { OP_DOT_TIMES, "*" },
        { OP_RDIV, "/" }, { OP_DOT_RDIV, "/" }
        // OP_LDIV / OP_DOT_LDIV and power are handled specially below.
    };
    //=============================================================================
    // Relational ops -> "f32((L) op (R))".
    const std::unordered_map<int, const char*> relationalBinary = { { OP_LT, "<" },
        { OP_LEQ, "<=" }, { OP_GT, ">" }, { OP_GEQ, ">=" }, { OP_EQ, "==" }, { OP_NEQ, "!=" } };
    //=============================================================================
    // Unary scalar math functions with a direct WGSL builtin.
    const std::unordered_map<std::string, const char*> unaryFunctions
        = { { "sin", "sin" }, { "cos", "cos" }, { "tan", "tan" }, { "asin", "asin" },
              { "acos", "acos" }, { "atan", "atan" }, { "sinh", "sinh" }, { "cosh", "cosh" },
              { "tanh", "tanh" }, { "exp", "exp" }, { "log", "log" }, { "log2", "log2" },
              { "sqrt", "sqrt" }, { "abs", "abs" }, { "floor", "floor" }, { "ceil", "ceil" },
              { "round", "round" }, { "sign", "sign" }, { "trunc", "trunc" }, { "fix", "trunc" } };
    //=============================================================================
    // Binary scalar math functions with a direct WGSL builtin.
    const std::unordered_map<std::string, const char*> binaryFunctions
        = { { "min", "min" }, { "max", "max" }, { "atan2", "atan2" }, { "pow", "pow" } };
    //=============================================================================
    class ExprCompiler
    {
    public:
        ExprCompiler(const stringVector& parameters,
            const std::unordered_map<std::string, double>& capturedScalars)
            : m_capturedScalars(capturedScalars)
        {
            for (size_t k = 0; k < parameters.size(); ++k) {
                m_parameterIndex[parameters[k]] = k;
            }
        }

        // Switch to the saturating-integer emission mode for a small integer class.
        void
        setIntegerMode(long lo, long hi)
        {
            m_integerMode = true;
            m_lo = std::to_string(lo);
            m_hi = std::to_string(hi);
        }

        bool
        emit(AbstractSyntaxTreePtr node, std::string& out)
        {
            if (node == nullptr) {
                m_reason = "empty node";
                return false;
            }
            if (isNumericConstant(node->type)) {
                if (m_integerMode) {
                    return emitIntegerLiteral(textToDouble(node->text), out);
                }
                out += formatLiteral(textToDouble(node->text));
                return true;
            }
            if (node->type == id_node) {
                return emitIdentifier(node, out);
            }
            if (node->type == non_terminal) {
                // OP_RHS wraps a variable / call reference in an expression; unwrap it
                if (node->opNum == OP_RHS) {
                    if (node->down == nullptr) {
                        m_reason = "empty rhs reference";
                        return false;
                    }
                    return emit(node->down, out);
                }
                return emitOperator(node, out);
            }
            m_reason = "unsupported node type";
            return false;
        }

        const std::string&
        reason() const
        {
            return m_reason;
        }

    private:
        static bool
        isNumericConstant(NODE_TYPE t)
        {
            return t == const_double_node || t == const_float_node || t == const_int_node
                || t == const_int8_node || t == const_uint8_node || t == const_int16_node
                || t == const_uint16_node || t == const_int32_node || t == const_uint32_node
                || t == const_int64_node || t == const_uint64_node;
        }

        static double
        textToDouble(const std::string& text)
        {
            try {
                return std::stod(text);
            } catch (...) {
                return 0.0;
            }
        }

        static std::string
        formatLiteral(double value)
        {
            char buffer[64];
            std::snprintf(buffer, sizeof(buffer), "%.9g", value);
            std::string s(buffer);
            // WGSL needs a float literal: ensure a decimal point or exponent.
            if (s.find('.') == std::string::npos && s.find('e') == std::string::npos
                && s.find('E') == std::string::npos && s.find("inf") == std::string::npos
                && s.find("nan") == std::string::npos) {
                s += ".0";
            }
            return "f32(" + s + ")";
        }

        // integer-mode literal: only integer values are accepted (a fractional
        // constant would need the host's rounding-on-conversion semantics)
        bool
        emitIntegerLiteral(double value, std::string& out)
        {
            if (value != std::floor(value) || value > 2147483647.0 || value < -2147483648.0) {
                m_reason = "non-integer constant in an integer expression";
                return false;
            }
            out += "i32(" + std::to_string((long)value) + ")";
            return true;
        }

        bool
        emitIdentifier(AbstractSyntaxTreePtr node, std::string& out)
        {
            if (node->down == nullptr) {
                auto pit = m_parameterIndex.find(node->text);
                if (pit != m_parameterIndex.end()) {
                    out += "p" + std::to_string(pit->second);
                    return true;
                }
                auto cit = m_capturedScalars.find(node->text);
                if (cit != m_capturedScalars.end()) {
                    if (m_integerMode) {
                        return emitIntegerLiteral(cit->second, out);
                    }
                    out += formatLiteral(cit->second);
                    return true;
                }
                m_reason = "unknown identifier '" + node->text + "'";
                return false;
            }
            if (node->down->type == non_terminal && node->down->opNum == OP_PARENS) {
                return emitFunctionCall(node, out);
            }
            m_reason = "unsupported postfix on '" + node->text + "'";
            return false;
        }

        bool
        emitFunctionCall(AbstractSyntaxTreePtr node, std::string& out)
        {
            const std::string& name = node->text;
            // a parameter used as f(x) means indexing p(x): unsupported
            if (m_parameterIndex.count(name) != 0) {
                m_reason = "indexing a parameter ('" + name + "(...)') is not supported";
                return false;
            }
            std::vector<AbstractSyntaxTreePtr> args;
            for (AbstractSyntaxTreePtr a = node->down->down; a != nullptr; a = a->right) {
                args.push_back(a);
            }
            if (m_integerMode) {
                // only min / max / abs stay integer-exact; everything else falls back
                if ((name == "min" || name == "max") && args.size() == 2) {
                    std::string l;
                    std::string r;
                    if (!emit(args[0], l) || !emit(args[1], r)) {
                        return false;
                    }
                    out += name + "((" + l + "), (" + r + "))";
                    return true;
                }
                if (name == "abs" && args.size() == 1) {
                    std::string a;
                    if (!emit(args[0], a)) {
                        return false;
                    }
                    out += clampExpr("abs(" + a + ")");
                    return true;
                }
                m_reason = "function '" + name + "' not supported for integer arrayfun";
                return false;
            }
            auto u = unaryFunctions.find(name);
            if (u != unaryFunctions.end()) {
                if (args.size() != 1) {
                    m_reason = name + " expects 1 argument";
                    return false;
                }
                out += u->second;
                out += "(";
                if (!emit(args[0], out)) {
                    return false;
                }
                out += ")";
                return true;
            }
            auto b = binaryFunctions.find(name);
            if (b != binaryFunctions.end()) {
                if (args.size() != 2) {
                    m_reason = name + " expects 2 arguments";
                    return false;
                }
                out += b->second;
                out += "(";
                if (!emit(args[0], out)) {
                    return false;
                }
                out += ", ";
                if (!emit(args[1], out)) {
                    return false;
                }
                out += ")";
                return true;
            }
            // special-cased functions without a direct 1:1 WGSL builtin
            if (name == "hypot" && args.size() == 2) {
                return emitCall2(args, out, "sqrt((", ")*(", ")+(", ")*(", "))");
            }
            if ((name == "mod" || name == "rem") && args.size() == 2) {
                // mod uses floor, rem uses trunc; the two differ on sign
                const char* rounder = (name == "mod") ? "floor" : "trunc";
                out += "((";
                if (!emit(args[0], out)) {
                    return false;
                }
                out += ") - (";
                if (!emit(args[1], out)) {
                    return false;
                }
                out += ") * ";
                out += rounder;
                out += "((";
                if (!emit(args[0], out)) {
                    return false;
                }
                out += ") / (";
                if (!emit(args[1], out)) {
                    return false;
                }
                out += ")))";
                return true;
            }
            m_reason = "unsupported function '" + name + "'";
            return false;
        }

        // emit "prefix arg0 mid1 arg0 mid2 arg1 mid3 arg1 suffix" (used for hypot)
        bool
        emitCall2(const std::vector<AbstractSyntaxTreePtr>& args, std::string& out,
            const char* prefix, const char* mid1, const char* mid2, const char* mid3,
            const char* suffix)
        {
            out += prefix;
            if (!emit(args[0], out)) {
                return false;
            }
            out += mid1;
            if (!emit(args[0], out)) {
                return false;
            }
            out += mid2;
            if (!emit(args[1], out)) {
                return false;
            }
            out += mid3;
            if (!emit(args[1], out)) {
                return false;
            }
            out += suffix;
            return true;
        }

        // wrap an i32 expression so it saturates to the class range
        std::string
        clampExpr(const std::string& inner) const
        {
            return "clamp(" + inner + ", " + m_lo + ", " + m_hi + ")";
        }

        // integer-mode operator emission (saturating i32). Returns false for any op
        // outside the safe subset so the caller can fall back to the host.
        bool
        emitIntegerOperator(
            int op, AbstractSyntaxTreePtr left, AbstractSyntaxTreePtr right, std::string& out)
        {
            if (op == OP_UMINUS || op == OP_UPLUS || op == OP_NOT) {
                std::string inner;
                if (!emit(left, inner)) {
                    return false;
                }
                if (op == OP_NOT) {
                    out += "i32((" + inner + ") == 0)";
                } else if (op == OP_UMINUS) {
                    out += clampExpr("-(" + inner + ")");
                } else {
                    out += "(" + inner + ")";
                }
                return true;
            }
            if (right == nullptr) {
                m_reason = "malformed binary operator";
                return false;
            }
            std::string l;
            std::string r;
            if (!emit(left, l) || !emit(right, r)) {
                return false;
            }
            const char* arith = nullptr;
            if (op == OP_PLUS) {
                arith = "+";
            } else if (op == OP_SUBTRACT) {
                arith = "-";
            } else if (op == OP_TIMES || op == OP_DOT_TIMES) {
                arith = "*";
            }
            if (arith != nullptr) {
                out += clampExpr("(" + l + ") " + arith + " (" + r + ")");
                return true;
            }
            const std::unordered_map<int, const char*> rel = { { OP_LT, "<" }, { OP_LEQ, "<=" },
                { OP_GT, ">" }, { OP_GEQ, ">=" }, { OP_EQ, "==" }, { OP_NEQ, "!=" } };
            auto rit = rel.find(op);
            if (rit != rel.end()) {
                out += "i32((" + l + ") " + rit->second + " (" + r + "))";
                return true;
            }
            if (op == OP_AND || op == OP_SAND || op == OP_OR || op == OP_SOR) {
                const char* logic = (op == OP_AND || op == OP_SAND) ? " && " : " || ";
                out += "i32(((" + l + ") != 0)" + logic + "((" + r + ") != 0))";
                return true;
            }
            m_reason = "operator not supported for integer arrayfun (only +, -, .*, "
                       "comparisons, logical, min, max, abs)";
            return false;
        }

        bool
        emitOperator(AbstractSyntaxTreePtr node, std::string& out)
        {
            int op = node->opNum;
            AbstractSyntaxTreePtr left = node->down;
            AbstractSyntaxTreePtr right = (left != nullptr) ? left->right : nullptr;

            if (m_integerMode) {
                if (left == nullptr) {
                    m_reason = "malformed operator";
                    return false;
                }
                return emitIntegerOperator(op, left, right, out);
            }

            // unary
            if (op == OP_UMINUS || op == OP_UPLUS || op == OP_NOT) {
                if (left == nullptr) {
                    m_reason = "malformed unary operator";
                    return false;
                }
                if (op == OP_NOT) {
                    out += "f32((";
                    if (!emit(left, out)) {
                        return false;
                    }
                    out += ") == 0.0)";
                    return true;
                }
                out += (op == OP_UMINUS) ? "(-(" : "((";
                if (!emit(left, out)) {
                    return false;
                }
                out += "))";
                return true;
            }
            if (left == nullptr || right == nullptr) {
                m_reason = "malformed binary operator";
                return false;
            }
            auto ar = arithmeticBinary.find(op);
            if (ar != arithmeticBinary.end()) {
                out += "((";
                if (!emit(left, out)) {
                    return false;
                }
                out += ") ";
                out += ar->second;
                out += " (";
                if (!emit(right, out)) {
                    return false;
                }
                out += "))";
                return true;
            }
            if (op == OP_LDIV || op == OP_DOT_LDIV) {
                // a \ b == b / a
                out += "((";
                if (!emit(right, out)) {
                    return false;
                }
                out += ") / (";
                if (!emit(left, out)) {
                    return false;
                }
                out += "))";
                return true;
            }
            if (op == OP_POWER || op == OP_MPOWER) {
                out += "pow((";
                if (!emit(left, out)) {
                    return false;
                }
                out += "), (";
                if (!emit(right, out)) {
                    return false;
                }
                out += "))";
                return true;
            }
            auto rel = relationalBinary.find(op);
            if (rel != relationalBinary.end()) {
                out += "f32((";
                if (!emit(left, out)) {
                    return false;
                }
                out += ") ";
                out += rel->second;
                out += " (";
                if (!emit(right, out)) {
                    return false;
                }
                out += "))";
                return true;
            }
            if (op == OP_AND || op == OP_SAND || op == OP_OR || op == OP_SOR) {
                const char* logic = (op == OP_AND || op == OP_SAND) ? " && " : " || ";
                out += "f32(((";
                if (!emit(left, out)) {
                    return false;
                }
                out += ") != 0.0)";
                out += logic;
                out += "((";
                if (!emit(right, out)) {
                    return false;
                }
                out += ") != 0.0))";
                return true;
            }
            m_reason = "unsupported operator";
            return false;
        }

        std::unordered_map<std::string, size_t> m_parameterIndex;
        std::unordered_map<std::string, double> m_capturedScalars;
        std::string m_reason;
        bool m_integerMode = false;
        std::string m_lo;
        std::string m_hi;
    };
    //=============================================================================
    // Descend the parsed body (OP_BLOCK -> statement -> OP_ASSIGN) to the RHS.
    static AbstractSyntaxTreePtr
    findAssignmentRhs(AbstractSyntaxTreePtr node)
    {
        if (node == nullptr) {
            return nullptr;
        }
        if (node->type == non_terminal && node->opNum == OP_ASSIGN) {
            return (node->down != nullptr) ? node->down->right : nullptr;
        }
        AbstractSyntaxTreePtr fromDown = findAssignmentRhs(node->down);
        if (fromDown != nullptr) {
            return fromDown;
        }
        return findAssignmentRhs(node->right);
    }
    //=============================================================================
    static bool
    isBooleanRootOp(AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != non_terminal) {
            return false;
        }
        switch (node->opNum) {
        case OP_LT:
        case OP_LEQ:
        case OP_GT:
        case OP_GEQ:
        case OP_EQ:
        case OP_NEQ:
        case OP_AND:
        case OP_SAND:
        case OP_OR:
        case OP_SOR:
        case OP_NOT:
            return true;
        default:
            return false;
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
// Shared body: locate the RHS expression, collect scalar captures, then emit
// through `compiler` (already set to the float or integer mode by the caller).
static GpuExprCompileResult
compileWith(AnonymousMacroFunctionDef* def, bool integerMode, long lo, long hi)
{
    GpuExprCompileResult result;
    if (def == nullptr || def->isFunctionHandle()) {
        result.failureReason = "named function handle (no translatable body)";
        return result;
    }
    def->updateCode();
    if (def->code == nullptr) {
        result.failureReason = "body not available as AST";
        return result;
    }
    AbstractSyntaxTreePtr rhs = findAssignmentRhs(def->code);
    if (rhs == nullptr) {
        result.failureReason = "could not locate body expression";
        return result;
    }
    // eligible captured variables: real numeric scalars, inlined as constants
    std::unordered_map<std::string, double> capturedScalars;
    stringVector names = def->getVariableNames();
    std::vector<ArrayOf> values = def->getVariables();
    for (size_t k = 0; k < names.size() && k < values.size(); ++k) {
        const ArrayOf& v = values[k];
        if (v.isScalar() && !v.isComplex() && !v.isSparse()
            && (v.isNumeric() || v.dataClass() == NLS_LOGICAL)) {
            capturedScalars[names[k]] = v.asDoubleScalar();
        }
    }

    ExprCompiler compiler(def->getArguments(), capturedScalars);
    if (integerMode) {
        compiler.setIntegerMode(lo, hi);
    }
    std::string wgsl;
    if (!compiler.emit(rhs, wgsl)) {
        result.failureReason = compiler.reason();
        return result;
    }
    result.ok = true;
    result.wgslExpr = wgsl;
    result.parameterCount = def->getArguments().size();
    result.logicalResult = isBooleanRootOp(rhs);
    return result;
}
//=============================================================================
GpuExprCompileResult
GpuCompileAnonymousBody(AnonymousMacroFunctionDef* def)
{
    return compileWith(def, false, 0, 0);
}
//=============================================================================
GpuExprCompileResult
GpuCompileAnonymousBodyInteger(AnonymousMacroFunctionDef* def, long lo, long hi)
{
    return compileWith(def, true, lo, hi);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
