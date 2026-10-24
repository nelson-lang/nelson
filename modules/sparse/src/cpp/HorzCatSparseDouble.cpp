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
#include "ErrorID.hpp"
#include "ConcatenateDimensions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSparseFloatingType(const ArrayOf& value)
{
    if (!value.isSparse()) {
        return false;
    }
    NelsonType dataClass = value.dataClass();
    return dataClass == NLS_DOUBLE || dataClass == NLS_DCOMPLEX || dataClass == NLS_SINGLE
        || dataClass == NLS_SCOMPLEX;
}
//=============================================================================
static NelsonType
getSparseConcatenateDataClass(const ArrayOf& A, const ArrayOf& B)
{
    NelsonType aClass = A.dataClass();
    NelsonType bClass = B.dataClass();
    bool hasDouble = aClass == NLS_DOUBLE || bClass == NLS_DOUBLE;
    bool hasDoubleComplex = aClass == NLS_DCOMPLEX || bClass == NLS_DCOMPLEX;
    bool hasSingleComplex = aClass == NLS_SCOMPLEX || bClass == NLS_SCOMPLEX;
    if (hasDoubleComplex || (hasDouble && hasSingleComplex)) {
        return NLS_DCOMPLEX;
    }
    if (hasDouble) {
        return NLS_DOUBLE;
    }
    if (hasSingleComplex) {
        return NLS_SCOMPLEX;
    }
    return NLS_SINGLE;
}
//=============================================================================
template <class T>
static ArrayOf
horzCatSparseTyped(ArrayOf A, ArrayOf B, NelsonType dataClass)
{
    A.promoteType(dataClass);
    B.promoteType(dataClass);
    const auto* spMatA
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    const auto* spMatB
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(B.sparseDataPointer());
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMatC;
    indexType newRowsSize = A.rows();
    indexType newColumnsSize = A.columns() + B.columns();
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    try {
        spMatC = new Eigen::SparseMatrix<T, 0, signedIndexType>(newRowsSize, newColumnsSize);
    } catch (const std::bad_alloc&) {
        spMatC = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    spMatC->middleCols(0, spMatA->cols()) = *spMatA;
    spMatC->middleCols(spMatA->cols(), spMatB->cols()) = *spMatB;
    return ArrayOf(dataClass, dimsC, (void*)spMatC, true);
}
//=============================================================================
ArrayOf
HorzCatSparseDouble(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!isSparseFloatingType(A)) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse double");
    }
    if (!isSparseFloatingType(B)) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
            L"sparse double");
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
    NelsonType dataClass = getSparseConcatenateDataClass(A, B);
    switch (dataClass) {
    case NLS_DOUBLE:
        C = horzCatSparseTyped<double>(A, B, NLS_DOUBLE);
        break;
    case NLS_DCOMPLEX:
        C = horzCatSparseTyped<doublecomplex>(A, B, NLS_DCOMPLEX);
        break;
    case NLS_SINGLE:
        C = horzCatSparseTyped<single>(A, B, NLS_SINGLE);
        break;
    case NLS_SCOMPLEX:
        C = horzCatSparseTyped<singlecomplex>(A, B, NLS_SCOMPLEX);
        break;
    default:
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse double");
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
