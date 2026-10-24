//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include <unordered_set>
#include <vector>
#include "AnonymousElementwiseAnalyzer.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "AbstractSyntaxTree.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // Free identifiers that always resolve to a numeric scalar constant, hence are
    // identical whether evaluated on scalars or full arrays.
    const std::unordered_set<std::string>&
    nullaryScalarConstants()
    {
        static const std::unordered_set<std::string> s = { "pi", "e", "eps", "Inf", "inf", "NaN",
            "nan", "i", "j", "true", "false", "realmax", "realmin" };
        return s;
    }
    //=============================================================================
    // Strictly element-wise scalar->scalar functions (1 argument).
    const std::unordered_set<std::string>&
    elementwiseUnaryFunctions()
    {
        static const std::unordered_set<std::string> s
            = { "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "exp", "log",
                  "log2", "log10", "sqrt", "abs", "sign", "floor", "ceil", "round", "fix" };
        return s;
    }
    //=============================================================================
    // Element-wise functions in their strictly 2-argument (both element-wise) form
    // that ALSO broadcast singleton dimensions (safe when operands differ in size).
    // min/max of a SINGLE array argument is a reduction and is excluded (only the
    // 2-argument element-wise form is accepted).
    const std::unordered_set<std::string>&
    broadcastingBinaryFunctions()
    {
        static const std::unordered_set<std::string> s = { "power", "hypot", "atan2" };
        return s;
    }
    //=============================================================================
    // 2-argument element-wise functions that require operands of the SAME size
    // (they do NOT broadcast singleton dimensions in Nelson). Safe only when every
    // operand already has the same shape (e.g. inside arrayfun), not when the caller
    // relies on broadcasting (e.g. bsxfun).
    const std::unordered_set<std::string>&
    sameSizeBinaryFunctions()
    {
        static const std::unordered_set<std::string> s = { "mod", "rem", "min", "max" };
        return s;
    }
    //=============================================================================
    // Named function handles (e.g. @plus) that are standard binary element-wise
    // builtins broadcasting singleton dimensions: for these `fun(A, B)` is exactly
    // the corresponding operator with implicit broadcasting. Excludes min/max/mod/
    // rem (do not broadcast) and any non element-wise or unary builtin.
    const std::unordered_set<std::string>&
    broadcastingNamedHandles()
    {
        static const std::unordered_set<std::string> s = { "plus", "minus", "times", "rdivide",
            "ldivide", "power", "hypot", "atan2", "lt", "le", "gt", "ge", "eq", "ne", "and", "or" };
        return s;
    }
    //=============================================================================
    // Named binary function handles that are element-wise when their two operands
    // have the SAME size (e.g. inside arrayfun, where every input array is the same
    // size). Broadcasting is not required here, so the same-size-only builtins
    // (max/min/mod/rem) and xor are also included.
    const std::unordered_set<std::string>&
    sameSizeBinaryNamedHandles()
    {
        static const std::unordered_set<std::string> s
            = { "plus", "minus", "times", "rdivide", "ldivide", "power", "hypot", "atan2", "max",
                  "min", "mod", "rem", "lt", "le", "gt", "ge", "eq", "ne", "and", "or", "xor" };
        return s;
    }
    //=============================================================================
    static bool
    isNumericConstantNode(NODE_TYPE t)
    {
        return t == const_double_node || t == const_float_node || t == const_int_node
            || t == const_int8_node || t == const_uint8_node || t == const_int16_node
            || t == const_uint16_node || t == const_int32_node || t == const_uint32_node
            || t == const_int64_node || t == const_uint64_node || t == const_dcomplex_node
            || t == const_complex_node;
    }
    //=============================================================================
    // Descend the parsed body (OP_BLOCK -> statement -> OP_ASSIGN) to the RHS
    // expression, mirroring BytecodeCompiler::compileAssign.
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
    class ElementwiseVectorizabilityChecker
    {
    public:
        ElementwiseVectorizabilityChecker(const std::unordered_set<std::string>& parameters,
            const std::unordered_set<std::string>& scalarCaptures,
            const std::unordered_set<std::string>& nonScalarCaptures, bool requireBroadcastSafe)
            : m_parameters(parameters)
            , m_scalarCaptures(scalarCaptures)
            , m_nonScalarCaptures(nonScalarCaptures)
            , m_requireBroadcastSafe(requireBroadcastSafe)
        {
        }

        bool
        check(AbstractSyntaxTreePtr node) const
        {
            if (node == nullptr) {
                return false;
            }
            if (isNumericConstantNode(node->type)) {
                return true;
            }
            if (node->type == id_node) {
                return checkIdentifier(node);
            }
            if (node->type == non_terminal) {
                if (node->opNum == OP_RHS) {
                    return node->down != nullptr && check(node->down);
                }
                return checkOperator(node);
            }
            return false;
        }

    private:
        bool
        checkIdentifier(AbstractSyntaxTreePtr node) const
        {
            if (node->down == nullptr) {
                const std::string& name = node->text;
                if (m_parameters.count(name) != 0) {
                    return true;
                }
                if (m_scalarCaptures.count(name) != 0) {
                    return true;
                }
                // A captured non-scalar array used element-wise could broadcast to a
                // different size than the per-element path (which errors on a
                // non-scalar in uniform output): reject to preserve behavior.
                if (m_nonScalarCaptures.count(name) != 0) {
                    return false;
                }
                return nullaryScalarConstants().count(name) != 0;
            }
            if (node->down->type == non_terminal && node->down->opNum == OP_PARENS) {
                return checkFunctionCall(node);
            }
            return false;
        }

        bool
        checkFunctionCall(AbstractSyntaxTreePtr node) const
        {
            const std::string& name = node->text;
            // f(x) where f is a parameter or capture means indexing, not a call.
            if (m_parameters.count(name) != 0 || m_scalarCaptures.count(name) != 0
                || m_nonScalarCaptures.count(name) != 0) {
                return false;
            }
            std::vector<AbstractSyntaxTreePtr> args;
            for (AbstractSyntaxTreePtr a = node->down->down; a != nullptr; a = a->right) {
                args.push_back(a);
            }
            if (elementwiseUnaryFunctions().count(name) != 0) {
                return args.size() == 1 && check(args[0]);
            }
            if (broadcastingBinaryFunctions().count(name) != 0) {
                return args.size() == 2 && check(args[0]) && check(args[1]);
            }
            // Same-size-only functions are safe unless the caller broadcasts.
            if (!m_requireBroadcastSafe && sameSizeBinaryFunctions().count(name) != 0) {
                return args.size() == 2 && check(args[0]) && check(args[1]);
            }
            return false;
        }

        bool
        checkOperator(AbstractSyntaxTreePtr node) const
        {
            AbstractSyntaxTreePtr left = node->down;
            AbstractSyntaxTreePtr right = (left != nullptr) ? left->right : nullptr;
            switch (node->opNum) {
            case OP_UMINUS:
            case OP_UPLUS:
            case OP_NOT:
                return left != nullptr && check(left);
            // Binary element-wise arithmetic (dotted forms only), relational, and
            // NON short-circuit logical operators. All identical scalar-vs-array.
            case OP_PLUS:
            case OP_SUBTRACT:
            case OP_DOT_TIMES:
            case OP_DOT_RDIV:
            case OP_DOT_LDIV:
            case OP_POWER: // .^
            case OP_LT:
            case OP_LEQ:
            case OP_GT:
            case OP_GEQ:
            case OP_EQ:
            case OP_NEQ:
            case OP_AND: // &
            case OP_OR: // |
                return left != nullptr && right != nullptr && check(left) && check(right);
            // Everything else differs on arrays vs scalars and is rejected:
            //   OP_TIMES(*) OP_RDIV(/) OP_LDIV(\) OP_MPOWER(^) OP_COLON(:)
            //   OP_TRANSPOSE(') OP_DOT_TRANSPOSE(.') OP_SAND(&&) OP_SOR(||)
            //   OP_BRACKETS([ ]) OP_BRACES OP_DOT ...
            default:
                return false;
            }
        }

        const std::unordered_set<std::string>& m_parameters;
        const std::unordered_set<std::string>& m_scalarCaptures;
        const std::unordered_set<std::string>& m_nonScalarCaptures;
        bool m_requireBroadcastSafe;
    };
    //=============================================================================
    static bool
    analyzeAnonymousBody(AnonymousMacroFunctionDef* def, bool requireBroadcastSafe)
    {
        if (def == nullptr || def->isFunctionHandle()) {
            return false;
        }
        def->updateCode();
        if (def->code == nullptr) {
            return false;
        }
        AbstractSyntaxTreePtr rhs = findAssignmentRhs(def->code);
        if (rhs == nullptr) {
            return false;
        }
        stringVector parameters = def->getArguments();
        std::unordered_set<std::string> parameterSet(parameters.begin(), parameters.end());
        std::unordered_set<std::string> scalarCaptures;
        std::unordered_set<std::string> nonScalarCaptures;
        stringVector capturedNames = def->getVariableNames();
        std::vector<ArrayOf> capturedValues = def->getVariables();
        for (size_t k = 0; k < capturedNames.size() && k < capturedValues.size(); ++k) {
            const ArrayOf& v = capturedValues[k];
            if (v.isScalar() && !v.isSparse() && (v.isNumeric() || v.isLogical())) {
                scalarCaptures.insert(capturedNames[k]);
            } else {
                nonScalarCaptures.insert(capturedNames[k]);
            }
        }
        ElementwiseVectorizabilityChecker checker(
            parameterSet, scalarCaptures, nonScalarCaptures, requireBroadcastSafe);
        return checker.check(rhs);
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
isAnonymousBodyElementwiseVectorizable(AnonymousMacroFunctionDef* def)
{
    return analyzeAnonymousBody(def, false);
}
//=============================================================================
bool
isNamedHandleElementwiseSameSize(AnonymousMacroFunctionDef* def, size_t numArgs)
{
    if (def == nullptr || !def->isFunctionHandle()) {
        return false;
    }
    // getContent() returns the bare builtin name (leading '@' stripped).
    const std::string name = def->getContent();
    if (numArgs == 1) {
        return elementwiseUnaryFunctions().count(name) != 0;
    }
    if (numArgs == 2) {
        return sameSizeBinaryNamedHandles().count(name) != 0;
    }
    return false;
}
//=============================================================================
bool
isAnonymousBodyElementwiseBroadcastSafe(AnonymousMacroFunctionDef* def)
{
    if (def == nullptr) {
        return false;
    }
    if (def->isFunctionHandle()) {
        // Named handle (@plus, @times, ...): no inspectable body. Accept only the
        // binary element-wise builtins that broadcast singleton dimensions.
        return broadcastingNamedHandles().count(def->getContent()) != 0;
    }
    // Operands may differ in singleton dimensions (e.g. bsxfun): every accepted
    // operator/function must broadcast, so the same-size-only functions
    // (min/max/mod/rem) are additionally rejected.
    return analyzeAnonymousBody(def, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
