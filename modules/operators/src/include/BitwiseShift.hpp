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
#include "ArrayOf.hpp"
#include "nlsOperators_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// bitshift(A, N, assumedType): shift A left by N (N > 0) or right by -N (N < 0).
// A must be an integer array (or a non-negative integer-valued double); N is a
// signed integer scalar or an array matching A. The result keeps A's class and,
// on a left shift, truncates to the class width (matching the host integer
// types). When A is a double and assumedType is given, the operation is bounded
// by that integer type's width while the result stays double.
NLSOPERATORS_IMPEXP ArrayOf
Bitshift(const ArrayOf& A, const ArrayOf& N, const std::wstring& assumedType = L"",
    bool withAssumedType = false);
//=============================================================================
} // namespace Nelson
//=============================================================================
