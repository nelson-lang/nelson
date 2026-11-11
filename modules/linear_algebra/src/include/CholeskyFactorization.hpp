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
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
NLSLINEAR_ALGEBRA_IMPEXP ArrayOf
CholeskyFactorization(
    const ArrayOf& A, bool lowerTriangle, bool& needToOverload, int& info, bool& nonRealDiagonal);
//=============================================================================
// 1-based index of the first diagonal entry with a nonzero imaginary part, 0 if none
NLSLINEAR_ALGEBRA_IMPEXP int
firstNonRealDiagonal(const ArrayOf& A);
//=============================================================================
} // namespace Nelson
//=============================================================================
