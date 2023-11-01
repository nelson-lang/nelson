//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
static std::unordered_map<int, std::string> keywords = { { NLS_KEYWORD_BREAK, "break" },
    { NLS_KEYWORD_CASE, "case" }, { NLS_KEYWORD_CATCH, "catch" },
    { NLS_KEYWORD_CONTINUE, "continue" }, { NLS_KEYWORD_ELSE, "else" },
    { NLS_KEYWORD_ELSEIF, "elseif" }, { NLS_KEYWORD_END, "end" }, { NLS_KEYWORD_FOR, "for" },
    { NLS_KEYWORD_FUNCTION, "function" }, { NLS_KEYWORD_IF, "if" },
    { NLS_KEYWORD_KEYBOARD, "keyboard" }, { NLS_KEYWORD_OTHERWISE, "otherwise" },
    { NLS_KEYWORD_QUIT, "quit" }, { NLS_KEYWORD_ABORT, "abort" }, { NLS_KEYWORD_RETURN, "return" },
    { NLS_KEYWORD_SWITCH, "switch" }, { NLS_KEYWORD_TRY, "try" }, { NLS_KEYWORD_WHILE, "while" },
    { NLS_KEYWORD_ENDFUNCTION, "endfunction" } };
//=============================================================================
static std::string
expression(AbstractSyntaxTreePtr expr, bool firstLevel)
{
    if (expr == nullptr) {
        return "";
    }
    if (expr->isEmpty()) {
        return "";
    }
    std::string res;
    switch (expr->type) {
    case id_node: {
        if (expr->down) {
            res = expr->text + expression(expr->down, false);
        } else {
            res = expr->text + expression(expr->right, false);
        }
    } break;
    case const_character_array_node: {
        res = "'" + expr->text + "'";
    } break;
    case const_string_node: {
        res = "\"" + expr->text + "\"";
    } break;
    case const_int_node:
    case const_uint64_node:
    case const_uint32_node:
    case const_uint16_node:
    case const_uint8_node:
    case const_int64_node:
    case const_int32_node:
    case const_int16_node:
    case const_int8_node:
    case const_float_node:
    case const_double_node: {
        res = expr->text;
    } break;
    case const_complex_node:
    case const_dcomplex_node: {
        res = expr->text + "i";
    } break;
    case reserved_node: {
        auto it = keywords.find(expr->tokenNumber);
        if (it != keywords.end()) {
            res = it->second;
        }
    } break;
    default: {
        switch (expr->opNum) {
        case OP_COLON: {
            if ((expr->down != nullptr) && (expr->down->opNum == (OP_COLON))) {
                res = expression(expr->down->down, false) + ":"
                    + expression(expr->down->down->right, false) + ":"
                    + expression(expr->down->right, false);
            } else {
                res = expression(expr->down, false) + ":" + expression(expr->down->right, false);
            }
        } break;
        case OP_SEMICOLON: {
            if (expr->right != nullptr) {
                std::string left = expression(expr->down, false);
                std::string right = expression(expr->right, false);
                if (right.empty()) {
                    res = left;
                } else {
                    res = left + ";" + right;
                }
            } else {
                std::string left = expression(expr->down, false);
                std::string right = expression(expr->down->right, false);
                if (right.empty()) {
                    res = left;
                } else {
                    res = left + "," + right;
                }
            }
        } break;
        case OP_EMPTY: {
            res = "[]";
        } break;
        case OP_EMPTY_CELL: {
            res = "{}";
        } break;
        case OP_BRACKETS: {
            res = "[" + expression(expr->down, false) + "]";
        } break;
        case OP_BRACES: {
            res = "{" + expression(expr->down, false) + "}";
        } break;
        case OP_PARENS: {
            res = "(";
            expr = expr->down;
            while (expr != nullptr) {
                res = res + expression(expr, false) + ",";
                expr = expr->right;
            }
            if (StringHelpers::ends_with(res, ",")) {
                res.pop_back();
            }
            res = res + ")";
        } break;
        case OP_PLUS: {
            res = expression(expr->down, false) + "+" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_SUBTRACT: {
            res = expression(expr->down, false) + "-" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_TIMES: {
            res = expression(expr->down, false) + "*" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_SOR: {
            res = expression(expr->down, false) + "||" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_OR: {
            res = expression(expr->down, false) + "|" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_SAND: {
            res = expression(expr->down, false) + "&&" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_AND: {
            res = expression(expr->down, false) + "&" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_LT: {
            res = expression(expr->down, false) + "<" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_LEQ: {
            res = expression(expr->down, false) + "<=" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_GT: {
            res = expression(expr->down, false) + ">" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_GEQ: {
            res = expression(expr->down, false) + ">=" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_EQ: {
            res = expression(expr->down, false) + "==" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_NEQ: {
            res = expression(expr->down, false) + "~=" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_DOT_TIMES: {
            res = expression(expr->down, false) + ".*" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_UPLUS: {
            res = "+" + expression(expr->down, false);
        } break;
        case OP_UMINUS: {
            res = "-" + expression(expr->down, false);
        } break;
        case OP_NOT: {
            res = "~" + expression(expr->down, false);
        } break;
        case OP_TRANSPOSE: {
            res = expression(expr->down, false) + "'";
        } break;
        case OP_DOT_TRANSPOSE: {
            res = expression(expr->down, false) + ".'";
        } break;
        case OP_RHS: {
            res = expression(expr->down, false);
        } break;
        case OP_RDIV: {
            res = expression(expr->down, false) + "/" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_LDIV: {
            res = expression(expr->down, false) + "\\" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_DOT_RDIV: {
            res = expression(expr->down, false) + "./" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_DOT_LDIV: {
            res = expression(expr->down, false) + ".\\" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_POWER: {
            res = expression(expr->down, false) + ".^" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_MPOWER: {
            res = expression(expr->down, false) + "^" + expression(expr->down->right, false);
            if (!firstLevel) {
                res = "(" + res + ")";
            }
        } break;
        case OP_FUNCTION_HANDLE_ANONYMOUS: {
            res = "@(";
            if (expr->down->right == nullptr) {
                expr = expr->down;
                res = res + ")";
            } else {
                stringVector arguments = expr->down->toStringList();
                expr = expr->down->right;
                size_t nbArguments = arguments.size();
                for (size_t k = 0; k < arguments.size(); ++k) {
                    res = res + arguments[k];
                    if (k < nbArguments - 1) {
                        res = res + ",";
                    }
                }
                res = res + ")";
            }
            res = res + expression(expr, false);
        } break;
        case OP_FUNCTION_HANDLE_NAMED: {
            res = res + "@";
        } break;
        default: {
        } break;
        }
    } break;
    }
    return res;
}
//=============================================================================
std::string
AbstractSyntaxTree::toString()
{
    return expression(this, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
