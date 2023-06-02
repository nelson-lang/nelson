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
    { PLUS_OP, "+" },
    { MINUS_OP, "-" },
    { UMINUS_OP, "-" },
    { UPLUS_OP, "+" },
    { TIMES_OP, "*" },
    { MTIMES_OP, ".*" },
    { RDIVIDE_OP, "./" },
    { LDIVIDE_OP, ".\\" },
    { MRDIVIDE_OP, "/" },
    { MLDIVIDE_OP, "\\" },
    { POWER_OP, ".^" },
    { MPOWER_OP, "^" },
    { LT_OP, "<" },
    { GT_OP, ">" },
    { LE_OP, "<=" },
    { GE_OP, ">=" },
    { NE_OP, "~=" },
    { EQ_OP, "=" },
    { AND_OP, "&" },
    { OR_OP, "|" },
    { SHORTCUTAND_OP, "&&" },
    { SHORTCUTOR_OP, "||" },
    { NOT_OP, "~" },
    { COLON_OP, ":" },
    { CTRANSPOSE_OP, "" },
    { TRANSPOSE_OP, "" },
    { HORZCAT_OP, "" },
    { VERTCAT_OP, "" },
    { SUBSREF_OP, "" },
    { SUBSASGN_OP, "" },
    { SUBSINDEX_OP, "" },
};
//=============================================================================
static std::unordered_map<OperatorType, std::string> nameOperators = {
    { PLUS_OP, "plus" },
    { MINUS_OP, "minus" },
    { UMINUS_OP, "uminus" },
    { UPLUS_OP, "uplus" },
    { TIMES_OP, "times" },
    { MTIMES_OP, "mtimes" },
    { RDIVIDE_OP, "rdivide" },
    { LDIVIDE_OP, "ldivide" },
    { MRDIVIDE_OP, "mrdivide" },
    { MLDIVIDE_OP, "mldivide" },
    { POWER_OP, "power" },
    { MPOWER_OP, "mpower" },
    { LT_OP, "lt" },
    { GT_OP, ">" },
    { LE_OP, "le" },
    { GE_OP, "ge" },
    { NE_OP, "ne" },
    { EQ_OP, "eq" },
    { AND_OP, "and" },
    { OR_OP, "or" },
    { SHORTCUTAND_OP, "shortcutand" },
    { SHORTCUTOR_OP, "shortcutor" },
    { NOT_OP, "not" },
    { COLON_OP, "colon" },
    { CTRANSPOSE_OP, "ctranspose" },
    { TRANSPOSE_OP, "transpose" },
    { HORZCAT_OP, "horzcat" },
    { VERTCAT_OP, "vertcat" },
    { SUBSREF_OP, "subsref" },
    { SUBSASGN_OP, "subsasgn" },
    { SUBSINDEX_OP, "subsinded" },
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
