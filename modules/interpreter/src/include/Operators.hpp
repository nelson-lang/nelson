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
enum OperatorType
{
    PLUS_OP = 0,
    MINUS_OP,
    UMINUS_OP,
    UPLUS_OP,
    TIMES_OP,
    MTIMES_OP,
    RDIVIDE_OP,
    LDIVIDE_OP,
    MRDIVIDE_OP,
    MLDIVIDE_OP,
    POWER_OP,
    MPOWER_OP,
    LT_OP,
    GT_OP,
    LE_OP,
    GE_OP,
    NE_OP,
    EQ_OP,
    AND_OP,
    OR_OP,
    SHORTCUTAND_OP,
    SHORTCUTOR_OP,
    NOT_OP,
    COLON_OP,
    CTRANSPOSE_OP,
    TRANSPOSE_OP,
    HORZCAT_OP,
    VERTCAT_OP,
    SUBSREF_OP,
    SUBSASGN_OP,
    SUBSINDEX_OP
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
