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
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_DOUBLE_EXPECTED);
    }
    if (!B.isSparseDoubleType()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_SPARSE_DOUBLE_EXPECTED);
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
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
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
