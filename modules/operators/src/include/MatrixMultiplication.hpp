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
NLSOPERATORS_IMPEXP ArrayOf
matrixMultiplication(const ArrayOf& A, const ArrayOf& B, bool& needToOverload);
// C = A * B of real double matrices into a buffer of rowsA * columnsB values, the result A * B
// gives; false (C untouched) for sizes computed by BLAS
NLSOPERATORS_IMPEXP bool
realDoubleMatrixProductInto(const double* A, indexType rowsA, indexType columnsA, const double* B,
    indexType columnsB, double* C);
//=============================================================================
} // namespace Nelson
//=============================================================================
