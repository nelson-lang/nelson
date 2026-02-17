//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "VertCatSparseDouble.hpp"
#include "CtransposeSparseDouble.hpp"
#include "HorzCatSparseDouble.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
VertCatSparseDouble(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!A.isSparseDoubleType()) {
        raiseError2(L"nelson:validators:mustBeType", 1, NLS_SPARSE_DOUBLE_STR);
    }
    if (!B.isSparseDoubleType()) {
        raiseError2(L"nelson:validators:mustBeType", 2, NLS_SPARSE_DOUBLE_STR);
    }
    if (A.isEmpty(false)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        ArrayOf C(A);
        return C;
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getColumns() != dimsB.getColumns()) {
        raiseError2(L"nelson:runtime:dimensionsNotConsistent");
    }
    A = CtransposeSparseDouble(A);
    B = CtransposeSparseDouble(B);
    C = HorzCatSparseDouble(A, B);
    C = CtransposeSparseDouble(C);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
