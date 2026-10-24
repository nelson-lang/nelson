//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static T
hypothenusRealNelson(T a, T b)
{
    // NaN with an infinite operand returns NaN, not Inf.
    if (std::isnan(a) || std::isnan(b)) {
        return (T)std::nan("");
    }
    return (T)std::hypot(a, b);
}
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = hypothenusRealNelson<T>(ptrA[0], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
scalar_scalar_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    ptrC[0] = hypothenusRealNelson<T>(ptrA[0], ptrB[0]);
    return ArrayOf(classDestination, dimsC, ptrC, false);
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = hypothenusRealNelson<T>(ptrA[k], ptrB[0]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = hypothenusRealNelson<T>(ptrA[k], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    indexType q = 0;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsA.rows();
            ptrC[m] = hypothenusRealNelson<T>(ptrA[q], ptrB[m]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsB = B.dimensions();
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsB.rows();
            ptrC[m] = hypothenusRealNelson<T>(ptrA[j], ptrB[m]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    Dimensions dimsB = B.dimensions();
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;

    indexType q = 0;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsB.rows();
            ptrC[m] = hypothenusRealNelson<T>(ptrA[m], ptrB[q]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsA = A.dimensions();
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsA.rows();
            ptrC[m] = hypothenusRealNelson<T>(ptrA[m], ptrB[j]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    indexType rows = std::max(dimsA.rows(), dimsB.rows());
    indexType columns = std::max(dimsA.columns(), dimsB.columns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;

    indexType m = 0;
    for (indexType i = 0; i < dimsA.columns(); i++) {
        for (indexType j = 0; j < dimsB.rows(); j++) {
            ptrC[m] = hypothenusRealNelson<T>(ptrA[i], ptrB[j]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    indexType rows = std::max(dimsA.rows(), dimsB.rows());
    indexType columns = std::max(dimsA.columns(), dimsB.columns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;

    indexType m = 0;
    indexType elementCountA = dimsA.elementCount();
    indexType elementCountB = dimsB.elementCount();
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            ptrC[m] = hypothenusRealNelson<T>(ptrA[j], ptrB[i]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
real_hypotenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    return binaryOperator2DDispatch(classDestination, A, B, scalar_scalar_real_hypotenuse<T>,
        scalar_matrix_real_hypotenuse<T>, matrix_scalar_real_hypotenuse<T>,
        matrix_matrix_real_hypotenuse<T>, row_column_real_hypotenuse<T>,
        column_row_real_hypotenuse<T>, row_matrix_real_hypotenuse<T>, matrix_row_real_hypotenuse<T>,
        column_matrix_real_hypotenuse<T>, matrix_column_real_hypotenuse<T>,
        [](const ArrayOf&, const ArrayOf&, NelsonType) -> ArrayOf {
            Error(L"Nelson:sizeDimensionsMustMatch",
                L"Arrays have incompatible sizes for this operation.");
            return {};
        });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
