//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "nlsOperators_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum BITWISE_OPERATOR
{
    BIT_AND,
    BIT_OR,
    BIT_XOR
};
//=============================================================================
NLSOPERATORS_IMPEXP ArrayOf
BitwiseOperator(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B,
    const std::wstring& assumedType, bool withAssumedType);
//=============================================================================
} // namespace Nelson
//=============================================================================
