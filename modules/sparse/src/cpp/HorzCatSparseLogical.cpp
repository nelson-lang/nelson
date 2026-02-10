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
#include "HorzCatSparseLogical.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
HorzCatSparseLogical(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!A.isSparseLogicalType()) {
        raiseError(L"Nelson:sparse:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
            ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 1, NLS_SPARSE_LOGICAL_STR);
    }
    if (!B.isSparseLogicalType()) {
        raiseError(L"Nelson:sparse:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
            ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 2, NLS_SPARSE_LOGICAL_STR);
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
    if (dimsA.getRows() != dimsB.getRows()) {
        raiseError(
            L"Nelson:sparse:ERROR_DIMENSIONS_NOT_CONSISTENT", ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.getSparseDataPointer();
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)B.getSparseDataPointer();
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatC;
    indexType newColumnsSize = dimsA.getColumns() + dimsB.getColumns();
    indexType newRowsSize = dimsA.getRows();
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    try {
        spMatC = new Eigen::SparseMatrix<logical, 0, signedIndexType>(newRowsSize, newColumnsSize);
    } catch (const std::bad_alloc&) {
        spMatC = nullptr;
        raiseError(L"Nelson:sparse:ERROR_MEMORY_ALLOCATION", ERROR_MEMORY_ALLOCATION);
    }
    spMatC->middleCols(0, spMatA->cols()) = *spMatA;
    spMatC->middleCols(spMatA->cols(), spMatB->cols()) = *spMatB;
    C = ArrayOf(NLS_LOGICAL, dimsC, (void*)spMatC, true);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
