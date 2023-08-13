//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// UNARY
#define UMINUS_OPERATOR_STR "uminus"
#define UPLUS_OPERATOR_STR "uplus"
#define NOT_OPERATOR_STR "not"
#define SUBSINDEX_OPERATOR_STR "subsindex"
#define SUBSREF_OPERATOR_STR "subsref" // not implemented
#define SUBSASGN_OPERATOR_STR "subsasgn" // not implemented
// BINARY
#define PLUS_OPERATOR_STR "plus"
#define MINUS_OPERATOR_STR "minus"
#define TIMES_OPERATOR_STR "times"
#define MTIMES_OPERATOR_STR "mtimes"
#define RDIVIDE_OPERATOR_STR "rdivide"
#define LDIVIDE_OPERATOR_STR "ldivide"
#define MRDIVIDE_OPERATOR_STR "mrdivide"
#define MLDIVIDE_OPERATOR_STR "mldivide"
#define POWER_OPERATOR_STR "power"
#define MPOWER_OPERATOR_STR "mpower"
#define LT_OPERATOR_STR "lt"
#define GT_OPERATOR_STR "gt"
#define LE_OPERATOR_STR "le"
#define GE_OPERATOR_STR "ge"
#define NE_OPERATOR_STR "ne"
#define EQ_OPERATOR_STR "eq"
#define AND_OPERATOR_STR "and"
#define OR_OPERATOR_STR "or"
#define SHORTCUTAND_OPERATOR_STR "shortcutand"
#define SHORTCUTOR_OPERATOR_STR "shortcutor"
#define CTRANSPOSE_OPERATOR_STR "ctranspose"
#define TRANSPOSE_OPERATOR_STR "transpose"
// MULTIPLE
#define COLON_OPERATOR_STR "colon"
#define HORZCAT_OPERATOR_STR "horzcat"
#define VERTCAT_OPERATOR_STR "vertcat"
//=============================================================================
enum OperatorType
{
    PLUS = 0,
    MINUS,
    UMINUS,
    UPLUS,
    TIMES,
    MTIMES,
    RDIVIDE,
    LDIVIDE,
    MRDIVIDE,
    MLDIVIDE,
    POWER,
    MPOWER,
    LT,
    GT,
    LE,
    GE,
    NE,
    EQ,
    AND,
    OR,
    SHORTCUTAND,
    SHORTCUTOR,
    NOT,
    COLON,
    CTRANSPOSE,
    TRANSPOSE,
    HORZCAT,
    VERTCAT,
    SUBSREF,
    SUBSASGN,
    SUBSINDEX
};
//=============================================================================
NLSINTERPRETER_IMPEXP std::string
getOperatorName(OperatorType operatorType);
//=============================================================================
NLSINTERPRETER_IMPEXP std::string
getOperatorSymbol(OperatorType operatorType);
//=============================================================================
} // namespace Nelson
//=============================================================================
