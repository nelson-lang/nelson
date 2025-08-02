//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_map>
#include "AbstractSyntaxTree.hpp"
#include "StringHelpers.hpp"
#include "Keywords.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Map of keyword tokens to their string representations
static const std::unordered_map<int, std::string> keywords = { { NLS_KEYWORD_BREAK, "break" },
    { NLS_KEYWORD_CASE, "case" }, { NLS_KEYWORD_CATCH, "catch" },
    { NLS_KEYWORD_CONTINUE, "continue" }, { NLS_KEYWORD_ELSE, "else" },
    { NLS_KEYWORD_ELSEIF, "elseif" }, { NLS_KEYWORD_END, "end" }, { NLS_KEYWORD_FOR, "for" },
    { NLS_KEYWORD_FUNCTION, "function" }, { NLS_KEYWORD_IF, "if" },
    { NLS_KEYWORD_KEYBOARD, "keyboard" }, { NLS_KEYWORD_OTHERWISE, "otherwise" },
    { NLS_KEYWORD_ABORT, "abort" }, { NLS_KEYWORD_RETURN, "return" },
    { NLS_KEYWORD_SWITCH, "switch" }, { NLS_KEYWORD_TRY, "try" }, { NLS_KEYWORD_WHILE, "while" },
    { NLS_KEYWORD_ENDFUNCTION, "endfunction" } };
//=============================================================================
// Map of binary operators to their string representations
static const std::unordered_map<int, std::string> binaryOperators
    = { { OP_ASSIGN, "=" }, { OP_PLUS, "+" }, { OP_SUBTRACT, "-" }, { OP_TIMES, "*" },
          { OP_RDIV, "/" }, { OP_LDIV, "\\" }, { OP_POWER, ".^" }, { OP_MPOWER, "^" },
          { OP_DOT_TIMES, ".*" }, { OP_DOT_RDIV, "./" }, { OP_DOT_LDIV, ".\\" }, { OP_AND, "&" },
          { OP_OR, "|" }, { OP_SAND, "&&" }, { OP_SOR, "||" }, { OP_LT, "<" }, { OP_LEQ, "<=" },
          { OP_GT, ">" }, { OP_GEQ, ">=" }, { OP_EQ, "==" }, { OP_NEQ, "~=" } };
//=============================================================================
// Map of unary operators to their string representations and position (left or right)
struct UnaryOpInfo
{
    std::string op;
    bool isRight; // true for right operators like ', false for left operators like ~
};
//=============================================================================
static const std::unordered_map<int, UnaryOpInfo> unaryOperators
    = { { OP_DOT_TRANSPOSE, { ".'", true } }, { OP_TRANSPOSE, { "'", true } },
          { OP_NOT, { "~", false } }, { OP_UMINUS, { "-", false } }, { OP_UPLUS, { "+", false } } };

//=============================================================================
// Forward declarations for helper methods
//=============================================================================
static std::string
formatUnaryOperator(AbstractSyntaxTreePtr node, const std::string& op, bool isRight);
static std::string
formatBinaryOperator(AbstractSyntaxTreePtr node, const std::string& op, bool firstLevel);
static std::string
formatConstantNode(const AbstractSyntaxTree& node);
static std::string
formatBracesOrBrackets(AbstractSyntaxTreePtr node, bool isBrackets);
static std::string
formatFunctionCall(AbstractSyntaxTreePtr node, bool firstLevel);
static std::string
formatRange(AbstractSyntaxTreePtr node, bool firstLevel);
//=============================================================================
std::string
AbstractSyntaxTree::toString(bool firstLevel)
{
    // Check if this is a binary operator that can be handled by the map
    auto binaryOpIt = binaryOperators.find(opNum);
    if (binaryOpIt != binaryOperators.end()) {
        return formatBinaryOperator(down, binaryOpIt->second, firstLevel);
    }

    // Check if this is a unary operator that can be handled by the map
    auto unaryOpIt = unaryOperators.find(opNum);
    if (unaryOpIt != unaryOperators.end()) {
        return formatUnaryOperator(down, unaryOpIt->second.op, unaryOpIt->second.isRight);
    }

    // Handle constant nodes
    if (type == const_character_array_node || type == const_string_node || type == const_int_node
        || type == const_uint64_node || type == const_uint32_node || type == const_uint16_node
        || type == const_uint8_node || type == const_int64_node || type == const_int32_node
        || type == const_int16_node || type == const_int8_node || type == const_float_node
        || type == const_double_node || type == const_complex_node || type == const_dcomplex_node) {
        return formatConstantNode(*this);
    }

    // Handle other node types and operators
    std::string result;

    switch (type) {
    case reserved_node: {
        auto it = keywords.find(tokenNumber);
        if (it != keywords.end()) {
            result = it->second;
        }
    } break;

    case id_node: {
        result = text;
        if (down) {
            result += down->toString(false);
        } else if (right) {
            result += right->toString(false);
        }
        return result;
    }

    case non_terminal:
        // Handle non-terminal nodes based on operator type
        switch (opNum) {
        case OP_BLOCK: {
            // Block of statements separated by newlines
            AbstractSyntaxTreePtr current = down;
            while (current != nullptr) {
                result += current->toString(false);
                if (current->right != nullptr) {
                    result += "\n";
                }
                current = current->right;
            }
        } break;

        case OP_COLON: {
            result = formatRange(down, false);
        } break;

        case OP_PARENS: {
            result = "(";
            if (down) {
                result += down->toString(false);
                AbstractSyntaxTreePtr c = down->right;
                while (c) {
                    result += "," + c->toString(false);
                    c = c->right;
                }
            }
            result += ")";
            if (right) {
                result += right->toString(false);
            }
        } break;

        case OP_EMPTY: {
            result = "[]";
        } break;

        case OP_EMPTY_CELL: {
            result = "{}";
        } break;

        case OP_BRACES: {
            result = formatBracesOrBrackets(down, false);
        } break;

        case OP_BRACKETS: {
            result = formatBracesOrBrackets(down, true);
        } break;

        case OP_DOT: {
            // Structure field access: expr.field
            result = "." + (down ? down->toString(false) : "");
            if (right) {
                result += right->toString(false);
            }
        } break;

        case OP_DOTDYN: {
            // Dynamic field access: expr.(expr)
            result = ".(" + (down ? down->toString(false) : "");
            if (right) {
                result += right->toString(false);
            }
            result += ")";
        } break;

        case OP_FUNCTION_HANDLE_ANONYMOUS: {
            // Anonymous function: @(args) expr
            if (down) {
                stringVector arguments = down->toStringList();
                result = "@(";
                for (size_t k = 0; k < arguments.size(); ++k) {
                    result += arguments[k];
                    if (k < arguments.size() - 1) {
                        result += ",";
                    }
                }
                result += ") ";
                if (down->right) {
                    result += down->right->toString(true);
                }
            }
        } break;

        case OP_FUNCTION_HANDLE_NAMED: {
            // Named function handle: @function_name
            if (down) {
                result = "@" + down->toString(false);
            }
        } break;

        case OP_SCALL: {
            result = formatFunctionCall(down, false);
        } break;

        case OP_MULTICALL: {
            // Multiple outputs: [out1, out2] = func(args)
            if (down && down->right) {
                result = "[";
                AbstractSyntaxTreePtr outputs = down;
                bool first = true;
                while (outputs != nullptr && outputs->opNum != OP_ASSIGN) {
                    if (!first) {
                        result += ", ";
                    }
                    result += outputs->toString(false);
                    outputs = outputs->right;
                    first = false;
                }
                result += "] = " + down->right->toString(false);
            }
        } break;

        default:
            // If we don't know how to handle this operator, try to output text or child nodes
            if (!text.empty()) {
                result = text;
            } else if (down) {
                AbstractSyntaxTreePtr current = down;
                while (current != nullptr) {
                    result += current->toString(false);
                    current = current->right;
                }
            } else {
                result = "!!!ERROR!!!";
            }
        }
        break;
    }
    return result;
}
//=============================================================================
// Helper methods implementation
//=============================================================================
// Format constant nodes based on their type
static std::string
formatConstantNode(const AbstractSyntaxTree& node)
{
    switch (node.type) {
    case const_character_array_node:
        return "'" + node.text + "'";
    case const_string_node:
        return "\"" + node.text + "\"";
    case const_complex_node:
    case const_dcomplex_node:
        return node.text + "i";
    default:
        // Other numeric types
        return node.text;
    }
}
//=============================================================================
// Format binary operators with proper parentheses based on precedence
static std::string
formatBinaryOperator(AbstractSyntaxTreePtr node, const std::string& op, bool firstLevel)
{
    if (node && node->right) {
        // For second-level operations, add parentheses around child expressions
        std::string leftStr = node->toString(false);
        std::string rightStr = node->right->toString(false);

        std::string result = leftStr + op + rightStr;
        return firstLevel ? result : "(" + result + ")";
    }
    return "";
}
//=============================================================================
// Format unary operators with proper placement (prefix or postfix)
static std::string
formatUnaryOperator(AbstractSyntaxTreePtr node, const std::string& op, bool isRight)
{
    if (!node) {
        return "";
    }
    return isRight ? node->toString(true) + op : op + node->toString(true);
}
//=============================================================================
// Format arrays with brackets or braces
static std::string
formatBracesOrBrackets(AbstractSyntaxTreePtr node, bool isBrackets)
{
    std::string result = isBrackets ? "[" : "{";

    AbstractSyntaxTreePtr rowNode = node;
    bool firstRow = true;

    while (rowNode) {
        if (!firstRow) {
            result += "; ";
        }

        // Handle row content
        if (rowNode->opNum == OP_SEMICOLON) {
            AbstractSyntaxTreePtr col = rowNode->down;
            bool firstCol = true;
            while (col) {
                if (!firstCol) {
                    result += ", ";
                }
                result += col->toString(true);
                col = col->right;
                firstCol = false;
            }
        } else {
            // Single row case
            AbstractSyntaxTreePtr col = rowNode;
            bool firstCol = true;
            while (col) {
                if (!firstCol) {
                    result += ", ";
                }
                result += col->toString(true);
                col = col->right;
                firstCol = false;
            }
            break; // Done with single-row matrix
        }

        rowNode = rowNode->right;
        firstRow = false;
    }

    result += isBrackets ? "]" : "}";
    return result;
}
//=============================================================================
// Format function calls with arguments
static std::string
formatFunctionCall(AbstractSyntaxTreePtr node, bool firstLevel)
{
    if (!node) {
        return "";
    }

    std::string result = node->toString(firstLevel) + "(";
    AbstractSyntaxTreePtr args = node->right;
    bool first = true;

    while (args != nullptr) {
        if (!first) {
            result += ", ";
        }
        result += args->toString(firstLevel);
        args = args->right;
        first = false;
    }

    result += ")";
    return result;
}
//=============================================================================
// Format range expressions (start:step:end or start:end)
static std::string
formatRange(AbstractSyntaxTreePtr node, bool firstLevel)
{
    if (!node) {
        return "";
    }

    std::string result = node->toString(firstLevel);
    AbstractSyntaxTreePtr current = node->right;

    while (current != nullptr) {
        result += ":" + current->toString(firstLevel);
        current = current->right;
    }

    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
