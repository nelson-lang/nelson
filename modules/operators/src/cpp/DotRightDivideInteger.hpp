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
#include "ArrayOf.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "IntegerOperations.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_integer_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = scalar_scalar_integer_divide(ptrA[0], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_integer_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = scalar_scalar_integer_divide(ptrA[k], ptrB[0]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_integer_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)Cp;
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = scalar_scalar_integer_divide<T>(ptrA[k], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_integer_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
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
            ptrC[m] = scalar_scalar_integer_divide(ptrA[q], ptrB[m]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_integer_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)res.dataPointer();
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsB.rows();
            ptrC[m] = scalar_scalar_integer_divide(ptrA[j], ptrB[m]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_integer_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)res.dataPointer();

    indexType q = 0;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsB.rows();
            ptrC[m] = scalar_scalar_integer_divide<T>(ptrA[m], ptrB[q]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_integer_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)res.dataPointer();

    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsA.rows();
            ptrC[m] = scalar_scalar_integer_divide(ptrA[m], ptrB[j]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_integer_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrC = (T*)res.dataPointer();
    indexType m = 0;
    for (indexType i = 0; i < dimsA.columns(); i++) {
        for (indexType j = 0; j < dimsB.rows(); j++) {
            ptrC[m] = scalar_scalar_integer_divide(ptrA[i], ptrB[j]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_integer_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrC = (T*)res.dataPointer();

    indexType m = 0;
    indexType elementCountA = dimsA.elementCount();
    indexType elementCountB = dimsB.elementCount();
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            ptrC[m] = scalar_scalar_integer_divide(ptrA[j], ptrB[i]);
            m++;
        }
    }

    return res;
}
//=============================================================================
template <class T>
ArrayOf
integer_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    return binaryOperator2DDispatch(classDestination, A, B, matrix_matrix_integer_dotRightDivide<T>,
        scalar_matrix_integer_dotRightDivide<T>, matrix_scalar_integer_dotRightDivide<T>,
        matrix_matrix_integer_dotRightDivide<T>, row_column_integer_dotRightDivide<T>,
        column_row_integer_dotRightDivide<T>, row_matrix_integer_dotRightDivide<T>,
        matrix_row_integer_dotRightDivide<T>, column_matrix_integer_dotRightDivide<T>,
        matrix_column_integer_dotRightDivide<T>,
        [](const ArrayOf& lhs, const ArrayOf& rhs, NelsonType destinationClass) {
            return binaryOperatorBroadcastNumeric<T>(lhs, rhs, destinationClass, "./",
                [](T a, T b) { return scalar_scalar_integer_divide<T>(a, b); });
        });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
