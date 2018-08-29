//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "HorzCatSparseLogical.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
HorzCatSparseLogical(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!A.isSparseLogicalType()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_LOGICAL_EXPECTED);
    }
    if (!B.isSparseLogicalType()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_SPARSE_LOGICAL_EXPECTED);
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
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatA
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.getSparseDataPointer();
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatB
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)B.getSparseDataPointer();
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatC;
    indexType newColumnsSize = dimsA.getColumns() + dimsB.getColumns();
    indexType newRowsSize = dimsA.getRows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    try {
        spMatC = new Eigen::SparseMatrix<logical, 0, signedIndexType>(newRowsSize, newColumnsSize);
    } catch (const std::bad_alloc& e) {
        e.what();
        spMatC = nullptr;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    spMatC->middleCols(0, spMatA->cols()) = *spMatA;
    spMatC->middleCols(spMatA->cols(), spMatB->cols()) = *spMatB;
    C = ArrayOf(NLS_LOGICAL, dimsC, (void*)spMatC, true);
    return C;
}
//=============================================================================
}
//=============================================================================
