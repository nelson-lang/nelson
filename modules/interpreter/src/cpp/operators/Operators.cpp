//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <unordered_map>
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::unordered_map<OperatorType, std::string> symbolOperators = {
    { PLUS, "+" },
    { MINUS, "-" },
    { UMINUS, "-" },
    { UPLUS, "+" },
    { TIMES, "*" },
    { MTIMES, ".*" },
    { RDIVIDE, "./" },
    { LDIVIDE, ".\\" },
    { MRDIVIDE, "/" },
    { MLDIVIDE, "\\" },
    { POWER, ".^" },
    { MPOWER, "^" },
    { LT, "<" },
    { GT, ">" },
    { LE, "<=" },
    { GE, ">=" },
    { NE, "~=" },
    { EQ, "==" },
    { AND, "&" },
    { OR, "|" },
    { SHORTCUTAND, "&&" },
    { SHORTCUTOR, "||" },
    { NOT, "~" },
    { COLON, ":" },
    { CTRANSPOSE, "" },
    { TRANSPOSE, "" },
    { HORZCAT, "" },
    { VERTCAT, "" },
    { SUBSREF, "" },
    { SUBSASGN, "" },
    { SUBSINDEX, "" },
};
//=============================================================================
static std::unordered_map<OperatorType, std::string> nameOperators = {
    { PLUS, PLUS_OPERATOR_STR },
    { MINUS, MINUS_OPERATOR_STR },
    { UMINUS, UMINUS_OPERATOR_STR },
    { UPLUS, UPLUS_OPERATOR_STR },
    { TIMES, TIMES_OPERATOR_STR },
    { MTIMES, MTIMES_OPERATOR_STR },
    { RDIVIDE, RDIVIDE_OPERATOR_STR },
    { LDIVIDE, LDIVIDE_OPERATOR_STR },
    { MRDIVIDE, MRDIVIDE_OPERATOR_STR },
    { MLDIVIDE, MLDIVIDE_OPERATOR_STR },
    { POWER, POWER_OPERATOR_STR },
    { MPOWER, MPOWER_OPERATOR_STR },
    { LT, LT_OPERATOR_STR },
    { GT, GT_OPERATOR_STR },
    { LE, LE_OPERATOR_STR },
    { GE, GE_OPERATOR_STR },
    { NE, NE_OPERATOR_STR },
    { EQ, EQ_OPERATOR_STR },
    { AND, AND_OPERATOR_STR },
    { OR, OR_OPERATOR_STR },
    { SHORTCUTAND, SHORTCUTAND_OPERATOR_STR },
    { SHORTCUTOR, SHORTCUTOR_OPERATOR_STR },
    { NOT, NOT_OPERATOR_STR },
    { COLON, COLON_OPERATOR_STR },
    { CTRANSPOSE, CTRANSPOSE_OPERATOR_STR },
    { TRANSPOSE, TRANSPOSE_OPERATOR_STR },
    { HORZCAT, HORZCAT_OPERATOR_STR },
    { VERTCAT, VERTCAT_OPERATOR_STR },
    { SUBSREF, SUBSREF_OPERATOR_STR },
    { SUBSASGN, SUBSASGN_OPERATOR_STR },
    { SUBSINDEX, SUBSINDEX_OPERATOR_STR },
};
//=============================================================================
std::string
getOperatorName(OperatorType operatorType)
{
    return nameOperators[operatorType];
}
//=============================================================================
std::string
getOperatorSymbol(OperatorType operatorType)
{
    return symbolOperators[operatorType];
}
//=============================================================================
} // namespace Nelson
//=============================================================================
