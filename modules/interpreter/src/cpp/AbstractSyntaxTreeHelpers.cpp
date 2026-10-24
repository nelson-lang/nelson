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
static const std::unordered_map<int, std::string> keywords = { { NLS_KEYWORD_BREAK, "break" },
    { NLS_KEYWORD_CASE, "case" }, { NLS_KEYWORD_CATCH, "catch" },
    { NLS_KEYWORD_CONTINUE, "continue" }, { NLS_KEYWORD_ELSE, "else" },
    { NLS_KEYWORD_ELSEIF, "elseif" }, { NLS_KEYWORD_END, "end" }, { NLS_KEYWORD_FOR, "for" },
    { NLS_KEYWORD_FUNCTION, "function" }, { NLS_KEYWORD_IF, "if" },
    { NLS_KEYWORD_KEYBOARD, "keyboard" }, { NLS_KEYWORD_OTHERWISE, "otherwise" },
    { NLS_KEYWORD_ABORT, "abort" }, { NLS_KEYWORD_RETURN, "return" },
    { NLS_KEYWORD_SWITCH, "switch" }, { NLS_KEYWORD_TRY, "try" }, { NLS_KEYWORD_WHILE, "while" } };
//=============================================================================
static const std::unordered_map<int, std::string> binaryOperators
    = { { OP_ASSIGN, "=" }, { OP_PLUS, "+" }, { OP_SUBTRACT, "-" }, { OP_TIMES, "*" },
          { OP_RDIV, "/" }, { OP_LDIV, "\\" }, { OP_POWER, ".^" }, { OP_MPOWER, "^" },
          { OP_DOT_TIMES, ".*" }, { OP_DOT_RDIV, "./" }, { OP_DOT_LDIV, ".\\" }, { OP_AND, "&" },
          { OP_OR, "|" }, { OP_SAND, "&&" }, { OP_SOR, "||" }, { OP_LT, "<" }, { OP_LEQ, "<=" },
          { OP_GT, ">" }, { OP_GEQ, ">=" }, { OP_EQ, "==" }, { OP_NEQ, "~=" } };
//=============================================================================
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
static std::string
formatUnaryOperator(AbstractSyntaxTreePtr node, const std::string& op, bool isRight);
static std::string
formatBinaryOperator(
    AbstractSyntaxTreePtr node, const std::string& op, bool firstLevel, bool isPower);
static std::string
formatConstantNode(const AbstractSyntaxTree& node);
static std::string
formatBracesOrBrackets(AbstractSyntaxTreePtr node, bool isBrackets);
static std::string
formatFunctionCall(AbstractSyntaxTreePtr node, bool firstLevel);
static std::string
formatRange(AbstractSyntaxTreePtr node, bool firstLevel);
static std::string
formatPostfix(AbstractSyntaxTreePtr node, bool firstLevel);
static bool
isRangeNode(AbstractSyntaxTreePtr node);
static std::string
formatArgument(AbstractSyntaxTreePtr node);
static std::string
formatTightOperand(AbstractSyntaxTreePtr node);
static std::string
formatIndexSuffixes(AbstractSyntaxTreePtr suffix);
//=============================================================================
std::string
AbstractSyntaxTree::toString(bool firstLevel)
{
    auto binaryOpIt = binaryOperators.find(opNum);
    if (binaryOpIt != binaryOperators.end()) {
        bool isPower = (opNum == OP_POWER || opNum == OP_MPOWER);
        return formatBinaryOperator(down, binaryOpIt->second, firstLevel, isPower);
    }

    auto unaryOpIt = unaryOperators.find(opNum);
    if (unaryOpIt != unaryOperators.end()) {
        return formatUnaryOperator(down, unaryOpIt->second.op, unaryOpIt->second.isRight);
    }

    if (type == const_character_array_node || type == const_string_node || type == const_int_node
        || type == const_uint64_node || type == const_uint32_node || type == const_uint16_node
        || type == const_uint8_node || type == const_int64_node || type == const_int32_node
        || type == const_int16_node || type == const_int8_node || type == const_float_node
        || type == const_double_node || type == const_complex_node || type == const_dcomplex_node) {
        return formatConstantNode(*this);
    }

    std::string result;

    switch (type) {
    case reserved_node: {
        auto it = keywords.find(tokenNumber);
        if (it != keywords.end()) {
            result = it->second;
        }
    } break;

    case id_node: {
        result = text + formatIndexSuffixes(down ? down : right);
        return result;
    }

    case non_terminal:
        switch (opNum) {
        case OP_BLOCK: {
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
            // ':' binds looser than every arithmetic operator: as an operand
            // it must stay parenthesized, or "(1:5)*x" re-parses as "1:(5*x)".
            result = formatRange(down, false);
            if (!firstLevel) {
                result = "(" + result + ")";
            }
        } break;

        case OP_ALL: {
            result = ":";
        } break;

        case OP_PARENS: {
            result = "(";
            if (down) {
                result += formatArgument(down);
                AbstractSyntaxTreePtr c = down->right;
                while (c) {
                    result += "," + formatArgument(c);
                    c = c->right;
                }
            }
            result += ")" + formatIndexSuffixes(right);
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
            result = "." + (down ? down->toString(false) : "") + formatIndexSuffixes(right);
        } break;

        case OP_DOTDYN: {
            result = ".(" + (down ? down->toString(false) : "") + ")" + formatIndexSuffixes(right);
        } break;

        case OP_FUNCTION_HANDLE_ANONYMOUS: {
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
            if (down) {
                result = "@" + down->toString(false);
            }
        } break;

        case OP_POSTFIX: {
            result = formatPostfix(down, firstLevel);
        } break;

        case OP_SUPERCLASS_CALL: {
            // obj@Super(args): left operand, then the super reference
            if (down != nullptr && down->right != nullptr) {
                result = down->toString(false) + "@" + down->right->toString(false);
            }
        } break;

        case OP_ARGUMENTS: {
            // Opaque arguments validation block: render the captured source.
            if (down != nullptr) {
                result = down->text;
            }
        } break;

        case OP_SCALL: {
            result = formatFunctionCall(down, false);
        } break;

        case OP_MULTICALL: {
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
    default:
        break;
    }
    return result;
}
//=============================================================================
static std::string
formatConstantNode(const AbstractSyntaxTree& node)
{
    // the text of a literal is unescaped: a quote is doubled again
    std::string text = node.text;
    switch (node.type) {
    case const_character_array_node:
        StringHelpers::replace_all(text, "'", "''");
        return "'" + text + "'";
    case const_string_node:
        StringHelpers::replace_all(text, "\"", "\"\"");
        return "\"" + text + "\"";
    case const_complex_node:
    case const_dcomplex_node:
        return node.text + "i";
    default:
        return node.text;
    }
}
//=============================================================================
static std::string
formatBinaryOperator(
    AbstractSyntaxTreePtr node, const std::string& op, bool firstLevel, bool isPower)
{
    if (node && node->right) {
        std::string leftStr = isPower ? formatTightOperand(node) : node->toString(false);
        std::string rightStr = node->right->toString(false);

        std::string result = leftStr + op + rightStr;
        return firstLevel ? result : "(" + result + ")";
    }
    return "";
}
//=============================================================================
static std::string
formatUnaryOperator(AbstractSyntaxTreePtr node, const std::string& op, bool isRight)
{
    if (!node) {
        return "";
    }
    // The operand is serialized with firstLevel=false so a lower-precedence
    // binary operand stays parenthesized: "-(a+b)" must not become "-a+b".
    return isRight ? formatTightOperand(node) + op : op + node->toString(false);
}
//=============================================================================
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
            break;
        }

        rowNode = rowNode->right;
        firstRow = false;
    }

    result += isBrackets ? "]" : "}";
    return result;
}
//=============================================================================
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
static std::string
formatRange(AbstractSyntaxTreePtr node, bool firstLevel)
{
    if (!node) {
        return "";
    }

    // "a:b:c" is parsed as the range "(a:b)" followed by ":c": the inner range
    // is written bare, otherwise "(a:b):c" would no longer be a stepped range.
    std::string result = node->toString(firstLevel || isRangeNode(node));
    AbstractSyntaxTreePtr current = node->right;

    while (current != nullptr) {
        result += ":" + current->toString(firstLevel);
        current = current->right;
    }

    return result;
}
//=============================================================================
static std::string
formatPostfix(AbstractSyntaxTreePtr node, bool firstLevel)
{
    if (node == nullptr) {
        return "";
    }
    std::string result = "(" + node->toString(firstLevel) + ")";
    AbstractSyntaxTreePtr suffix = node->right;
    while (suffix != nullptr) {
        result += suffix->toString(false);
        suffix = suffix->right;
    }
    return result;
}
//=============================================================================
static bool
isRangeNode(AbstractSyntaxTreePtr node)
{
    return node != nullptr && node->type == non_terminal && node->opNum == OP_COLON;
}
//=============================================================================
// An index argument is delimited by ',' and ')': a range needs no parentheses.
static std::string
formatArgument(AbstractSyntaxTreePtr node)
{
    return node->toString(isRangeNode(node));
}
//=============================================================================
// Power and postfix transpose bind tighter than a prefix '-', '+' or '~':
// "(-x)^2" written bare would re-parse as "-(x^2)".
static std::string
formatTightOperand(AbstractSyntaxTreePtr node)
{
    std::string result = node->toString(false);
    if (node->type == non_terminal
        && (node->opNum == OP_UMINUS || node->opNum == OP_UPLUS || node->opNum == OP_NOT)) {
        return "(" + result + ")";
    }
    return result;
}
//=============================================================================
// Index suffixes a(1).b{2}(3) are chained through 'right'. A brace index shares
// OP_BRACES with a cell literal, whose 'right' is its next peer: it is written
// here with the suffixes that follow it.
static std::string
formatIndexSuffixes(AbstractSyntaxTreePtr suffix)
{
    if (suffix == nullptr) {
        return "";
    }
    if (suffix->type == non_terminal && suffix->opNum == OP_BRACES) {
        return formatBracesOrBrackets(suffix->down, false) + formatIndexSuffixes(suffix->right);
    }
    return suffix->toString(false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
