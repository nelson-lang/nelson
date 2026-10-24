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
#include "ErrorID.hpp"
#include "ConcatenateDimensions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
HorzCatSparseLogical(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!A.isSparseLogicalType()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse logical");
    }
    if (!B.isSparseLogicalType()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
            L"sparse logical");
    }
    if (A.isEmpty(false)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        ArrayOf C(A);
        return C;
    }
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (dimsA.rows() != dimsB.rows()) {
        raiseConcatenationDimensionMismatch();
    }
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.sparseDataPointer();
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)B.sparseDataPointer();
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatC;
    indexType newColumnsSize = dimsA.columns() + dimsB.columns();
    indexType newRowsSize = dimsA.rows();
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    try {
        spMatC = new Eigen::SparseMatrix<logical, 0, signedIndexType>(newRowsSize, newColumnsSize);
    } catch (const std::bad_alloc&) {
        spMatC = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    spMatC->middleCols(0, spMatA->cols()) = *spMatA;
    spMatC->middleCols(spMatA->cols(), spMatB->cols()) = *spMatB;
    C = ArrayOf(NLS_LOGICAL, dimsC, (void*)spMatC, true);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
