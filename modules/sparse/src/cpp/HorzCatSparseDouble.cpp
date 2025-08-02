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
#include "HorzCatSparseDouble.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
HorzCatSparseDouble(ArrayOf A, ArrayOf B)
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
    if (dimsA.getRows() != dimsB.getRows()) {
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    if (A.isComplex() || B.isComplex()) {
        A.promoteType(NLS_DCOMPLEX);
        B.promoteType(NLS_DCOMPLEX);
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatA
            = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)A.getSparseDataPointer();
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatB
            = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)B.getSparseDataPointer();
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatC;
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        indexType newRowsSize = dimsA.getRows();
        indexType newColumnsSize = dimsA.getColumns() + dimsB.getColumns();
        Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
        try {
            spMatC = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(
                newRowsSize, newColumnsSize);
        } catch (const std::bad_alloc&) {
            spMatC = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        spMatC->middleCols(0, spMatA->cols()) = *spMatA;
        spMatC->middleCols(spMatA->cols(), spMatB->cols()) = *spMatB;
        C = ArrayOf(NLS_DCOMPLEX, dimsC, (void*)spMatC, true);
    } else {
        Eigen::SparseMatrix<double, 0, signedIndexType>* spMatA
            = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
        Eigen::SparseMatrix<double, 0, signedIndexType>* spMatB
            = (Eigen::SparseMatrix<double, 0, signedIndexType>*)B.getSparseDataPointer();
        Eigen::SparseMatrix<double, 0, signedIndexType>* spMatC;
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        indexType newRowsSize = dimsA.getRows();
        indexType newColumnsSize = dimsA.getColumns() + dimsB.getColumns();
        Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
        try {
            spMatC
                = new Eigen::SparseMatrix<double, 0, signedIndexType>(newRowsSize, newColumnsSize);
        } catch (const std::bad_alloc&) {
            spMatC = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        spMatC->middleCols(0, spMatA->cols()) = *spMatA;
        spMatC->middleCols(spMatA->cols(), spMatB->cols()) = *spMatB;
        C = ArrayOf(NLS_DOUBLE, dimsC, (void*)spMatC, true);
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
