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
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "SparseConstructors.hpp"
#include "CheckIJV.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
SparseConstructor(indexType m, indexType n)
{
    ArrayOf res;
    try {
        Dimensions dims(m, n);
        Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
            = new Eigen::SparseMatrix<double, 0, signedIndexType>(m, n);
        res = ArrayOf(NLS_DOUBLE, dims, (void*)spmat, true);
    } catch (const std::bad_alloc& e) {
        e.what();
        Error(ERROR_MEMORY_ALLOCATION);
    }
    return res;
}
//=============================================================================
ArrayOf
SparseConstructor(ArrayOf a)
{
    ArrayOf res;
    if (a.isSparse()) {
        res = a;
        res.ensureSingleOwner();
    } else {
        res = a;
        res.ensureSingleOwner();
        res.makeSparse();
    }
    return res;
}
//=============================================================================
ArrayOf
SparseConstructor(ArrayOf I, ArrayOf J, ArrayOf V)
{
    ArrayOf res;
#if (defined(_LP64) || defined(_WIN64))
    I.promoteType(NLS_UINT64);
    J.promoteType(NLS_UINT64);
#else
    I.promoteType(NLS_UINT32);
    J.promoteType(NLS_UINT32);
#endif
    int istride = 0, jstride = 0, vstride = 0;
    size_t olen = 0;
    CheckIJV(I.getLength(), J.getLength(), V.getLength(), istride, jstride, vstride, olen);
    // Calculate the number of rows in the matrix
    indexType* ip = (indexType*)I.getDataPointer();
    indexType rows = 0;
    for (indexType i = 0; i < I.getLength(); i++) {
        rows = ((double)ip[i] > (double)rows) ? ip[i] : rows;
    }
    indexType* jp = (indexType*)J.getDataPointer();
    indexType cols = 0;
    for (indexType j = 0; j < J.getLength(); j++) {
        cols = ((double)jp[j] > (double)cols) ? jp[j] : cols;
    }
    Dimensions dim(rows, cols);
    bool bScalarV = false;
    if (V.isScalar()) {
        bScalarV = true;
    }
    void* spmat = Eigen_makeSparseFromIJV(V.getDataClass(), rows, cols, olen, ip, istride, jp,
        jstride, V.getDataPointer(), vstride, bScalarV);
    res = ArrayOf(V.getDataClass(), dim, spmat, true);
    return res;
}
//=============================================================================
ArrayOf
SparseConstructor(ArrayOf I, ArrayOf J, ArrayOf V, indexType m, indexType n)
{
    ArrayOf res;
    res = SparseConstructor(I, J, V);
    Dimensions dims = res.getDimensions();
    Dimensions newdims(m, n);
    if (dims.getElementCount() > newdims.getElementCount()) {
        Error(_W("Index exceeds matrix dimensions."));
    }
    void* spmat = Eigen_CopyResizeSparseMatrix(
        res.getDataClass(), res.getSparseDataPointer(), dims.getRows(), dims.getColumns(), m, n);
    return ArrayOf(V.getDataClass(), newdims, spmat, true);
}
//=============================================================================
ArrayOf
SparseConstructor(ArrayOf I, ArrayOf J, ArrayOf V, indexType m, indexType n, indexType nnz)
{
    ArrayOf res = SparseConstructor(I, J, V, m, n);
    switch (res.getDataClass()) {
    case NLS_LOGICAL: {
        Eigen::SparseMatrix<logical, 0, signedIndexType>* spmat
            = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)res.getSparseDataPointer();
        if (nnz >= (indexType)spmat->nonZeros()) {
            spmat->reserve(nnz);
            spmat->finalize();
            spmat->makeCompressed();
        } else {
            Error(_W("Index exceeds matrix dimensions."));
        }
    } break;
    case NLS_DOUBLE: {
        Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
            = (Eigen::SparseMatrix<double, 0, signedIndexType>*)res.getSparseDataPointer();
        indexType nonZeros = (indexType)spmat->nonZeros();
        if (nnz >= nonZeros) {
            spmat->reserve(nnz - nonZeros);
            spmat->finalize();
            spmat->makeCompressed();
        } else {
            Error(_W("Index exceeds matrix dimensions."));
        }
    } break;
    case NLS_DCOMPLEX: {
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spmat
            = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)res.getSparseDataPointer();
        if (nnz >= (indexType)spmat->nonZeros()) {
            spmat->reserve(nnz);
            spmat->finalize();
            spmat->makeCompressed();
        } else {
            Error(_W("Index exceeds matrix dimensions."));
        }
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
