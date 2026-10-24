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
enum class QRMode
{
    Full,
    Economy
};
//=============================================================================
enum class QRPermutationOutput
{
    Matrix,
    Vector
};
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP ArrayOfVector
QRDecomposition(const ArrayOf& A, int nLhs, QRMode mode, QRPermutationOutput permutationOutput);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP ArrayOfVector
QRSparseLeastSquares(const ArrayOf& S, const ArrayOf& B, int nLhs, QRMode mode,
    QRPermutationOutput permutationOutput);
//=============================================================================
} // namespace Nelson
//=============================================================================
