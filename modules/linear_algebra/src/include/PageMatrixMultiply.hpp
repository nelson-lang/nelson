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
//=============================================================================
// Page-wise matrix multiplication (pagemtimes).
// transpA / transpB: 0 = none, 1 = transpose, 2 = ctranspose (conjugate).
NLSLINEAR_ALGEBRA_IMPEXP ArrayOf
PageMatrixMultiply(
    const ArrayOf& A, int transpA, const ArrayOf& B, int transpB, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
