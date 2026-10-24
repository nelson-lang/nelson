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
#include "Decomplexify.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
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
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
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
    if (V.isSparse()) {
        V.makeDense();
    }
#if (defined(_LP64) || defined(_WIN64))
    I.promoteType(NLS_UINT64);
    J.promoteType(NLS_UINT64);
#else
    I.promoteType(NLS_UINT32);
    J.promoteType(NLS_UINT32);
#endif
    int istride = 0, jstride = 0, vstride = 0;
    size_t olen = 0;
    CheckIJV(I.elementCount(), J.elementCount(), V.elementCount(), istride, jstride, vstride, olen);
    if (I.isEmpty() || J.isEmpty() || V.isEmpty()) {
        Dimensions dim(0, 0);
        Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
            = new Eigen::SparseMatrix<double, 0, signedIndexType>(dim.rows(), dim.columns());
        return ArrayOf(NLS_DOUBLE, dim, spmat, true);
    }
    auto* ip = (indexType*)I.dataPointer();
    indexType rows = 0;
    for (indexType i = 0; i < I.elementCount(); i++) {
        rows = (static_cast<double>(ip[i]) > static_cast<double>(rows)) ? ip[i] : rows;
        if (ip[i] < 1) {
            Error(L"Nelson:sparse:indexMustBePositive", L"Index into matrix must be positive.");
        }
    }
    auto* jp = (indexType*)J.dataPointer();
    indexType cols = 0;
    for (indexType j = 0; j < J.elementCount(); j++) {
        cols = (static_cast<double>(jp[j]) > static_cast<double>(cols)) ? jp[j] : cols;
        if (jp[j] < 1) {
            Error(L"Nelson:sparse:indexMustBePositive", L"Index into matrix must be positive.");
        }
    }
    Dimensions dim(rows, cols);
    bool bScalarV = false;
    if (V.isScalar()) {
        bScalarV = true;
    }
    void* spmat = Eigen_makeSparseFromIJV(V.dataClass(), rows, cols, olen, ip, istride, jp, jstride,
        V.dataPointer(), vstride, bScalarV);
    res = ArrayOf(V.dataClass(), dim, spmat, true);
    // The (i, j, v) triplet form drops an all-zero imaginary part: a complex
    // value array whose imaginary part is entirely zero yields a real matrix.
    return decomplexify(res);
}
//=============================================================================
ArrayOf
SparseConstructor(const ArrayOf& I, const ArrayOf& J, const ArrayOf& V, indexType m, indexType n)
{
    ArrayOf res;
    res = SparseConstructor(I, J, V);
    Dimensions dims = res.dimensions();
    Dimensions newdims(m, n);
    // SparseConstructor(I, J, V) sizes the result as [max(I), max(J)]: any
    // index outside [m, n] must error instead of being silently truncated.
    if (dims.rows() > newdims.rows() || dims.columns() > newdims.columns()) {
        Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
    }
    void* spmat = Eigen_CopyResizeSparseMatrix(
        res.dataClass(), res.sparseDataPointer(), dims.rows(), dims.columns(), m, n);
    // An empty (I, J, V) triplet yields a real 0x0 result whatever V's class is,
    // so the intended class comes from V; a non-empty triplet may have
    // canonicalized a zero-imaginary complex value to real, so its class comes
    // from the constructed result.
    bool emptyTriplet = I.isEmpty() || J.isEmpty() || V.isEmpty();
    NelsonType resultClass = emptyTriplet ? V.dataClass() : res.dataClass();
    return ArrayOf(resultClass, newdims, spmat, true);
}
//=============================================================================
ArrayOf
SparseConstructor(
    const ArrayOf& I, const ArrayOf& J, const ArrayOf& V, indexType m, indexType n, indexType nnz)
{
    ArrayOf res = SparseConstructor(I, J, V, m, n);
    if (nnz < res.nnz()) {
        Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
    }
    res.setSparseNzmax(nnz);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
