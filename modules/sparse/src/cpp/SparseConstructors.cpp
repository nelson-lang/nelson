//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "SparseConstructors.hpp"
#include "CheckIJV.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
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
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    return res;
}
//=============================================================================
ArrayOf
SparseConstructor(const ArrayOf& a)
{
    ArrayOf res;
    if (a.isSparse()) {
        res = a;
        res.ensureSingleOwner();
    } else {
        res = a;
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
    CheckIJV(I.getElementCount(), J.getElementCount(), V.getElementCount(), istride, jstride,
        vstride, olen);
    if (I.isEmpty() || J.isEmpty() || V.isEmpty()) {
        Dimensions dim(0, 0);
        Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
            = new Eigen::SparseMatrix<double, 0, signedIndexType>(dim.getRows(), dim.getColumns());
        return ArrayOf(NLS_DOUBLE, dim, spmat, true);
    }
    // Calculate the number of rows in the matrix
    auto* ip = (indexType*)I.getDataPointer();
    indexType rows = 0;
    for (indexType i = 0; i < I.getElementCount(); i++) {
        rows = (static_cast<double>(ip[i]) > static_cast<double>(rows)) ? ip[i] : rows;
        if (ip[i] < 1) {
            Error(_W("Index into matrix must be positive."));
        }
    }
    auto* jp = (indexType*)J.getDataPointer();
    indexType cols = 0;
    for (indexType j = 0; j < J.getElementCount(); j++) {
        cols = (static_cast<double>(jp[j]) > static_cast<double>(cols)) ? jp[j] : cols;
        if (jp[j] < 1) {
            Error(_W("Index into matrix must be positive."));
        }
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
SparseConstructor(const ArrayOf& I, const ArrayOf& J, const ArrayOf& V, indexType m, indexType n)
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
SparseConstructor(
    const ArrayOf& I, const ArrayOf& J, const ArrayOf& V, indexType m, indexType n, indexType nnz)
{
    ArrayOf res = SparseConstructor(I, J, V, m, n);
    switch (res.getDataClass()) {
    case NLS_LOGICAL: {
        Eigen::SparseMatrix<logical, 0, signedIndexType>* spmat
            = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)res.getSparseDataPointer();
        indexType nonZeros = (indexType)spmat->nonZeros();
        if (nnz >= nonZeros) {
            spmat->reserve(nnz - nonZeros);
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
        indexType nonZeros = (indexType)spmat->nonZeros();
        if (nnz >= nonZeros) {
            spmat->reserve(nnz - nonZeros);
            spmat->finalize();
            spmat->makeCompressed();
        } else {
            Error(_W("Index exceeds matrix dimensions."));
        }
    } break;
    default: {
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
