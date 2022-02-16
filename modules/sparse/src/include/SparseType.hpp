//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "nlsSparse_exports.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4290)
#endif
//=============================================================================
extern "C"
{
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_EyeSparseMatrixConstructor(NelsonType dclass, indexType rows, indexType cols);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_LogicalSparseMatrixConstructor(indexType rows, indexType cols, bool bMotif);
    //=============================================================================
    NLSSPARSE_IMPEXP void
    Eigen_DeleteSparseMatrix(NelsonType dclass, indexType rows, indexType cols, void** cp);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_MakeDenseArrayOf(NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_MakeSparseArrayOf(NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_TypeConvertSparse(
        NelsonType dclass, indexType rows, indexType cols, const void* cp, NelsonType oclass);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_CopySparseMatrix(NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSSPARSE_IMPEXP indexType
    Eigen_CountNonzerosMax(NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSSPARSE_IMPEXP indexType
    Eigen_CountNonzeros(NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_SparseMatrixConstructor(NelsonType dclass, indexType rows, indexType cols, ArrayOfMatrix m);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_GetSparseVectorSubsets(NelsonType dclass, indexType rows, indexType cols, const void* src,
        const indexType* indx, indexType irows, indexType icols);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_GetSparseNDimSubsets(NelsonType dclass, indexType rows, indexType cols, const void* src,
        const indexType* rindx, indexType irows, const indexType* cindx, indexType icols);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_SetSparseVectorSubsets(NelsonType dclass, indexType& rows, indexType& cols, const void* src,
        const indexType* indx, indexType irows, indexType icols, const void* data, int advance);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_SetSparseNDimSubsets(NelsonType dclass, indexType& rows, indexType& cols, const void* src,
        const indexType* rindx, indexType irows, const indexType* cindx, indexType icols,
        const void* data, int advance);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_GetSparseScalarElement(NelsonType dclass, indexType rows, indexType cols, const void* src,
        indexType rindx, indexType cindx);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_SparseToIJV(NelsonType dclass, indexType rows, indexType cols, const void* cp, indexType*& I,
        indexType*& J, int& nnz);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_makeSparseFromIJV(NelsonType dclass, indexType rows, indexType cols, indexType nnz,
        indexType* I, int istride, indexType* J, int jstride, const void* cp, int cpstride,
        bool bScalarV);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_DeleteSparseMatrixCols(
        NelsonType dclass, indexType rows, indexType cols, const void* cp, bool* dmap);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_DeleteSparseMatrixRows(
        NelsonType dclass, indexType rows, indexType cols, const void* cp, bool* dmap);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_DeleteSparseMatrixVectorSubset(NelsonType dclass, indexType& rows, indexType& cols,
        const void* cp, const indexType* todel, indexType delete_len);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_ReshapeSparseMatrix(NelsonType dclass, indexType rows, indexType cols, indexType newrows,
        indexType newcols, const void* cp);
    //=============================================================================
    NLSSPARSE_IMPEXP void*
    Eigen_CopyResizeSparseMatrix(NelsonType dclass, const void* src, indexType rows, indexType cols,
        indexType maxrow, indexType maxcol);
    //=============================================================================
};
//=============================================================================
