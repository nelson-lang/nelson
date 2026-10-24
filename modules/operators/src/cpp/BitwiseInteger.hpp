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
#include "IntegerOperations.hpp"
#include "BitwiseOperators.hpp"
#include "MatrixCheck.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T> using BitwiseOperation = T (*)(T, T);
//=============================================================================
template <class T>
BitwiseOperation<T>
getBitwiseOperation(BITWISE_OPERATOR bitwiseOperator)
{
    BitwiseOperation<T> op;
    switch (bitwiseOperator) {
    default:
    case BITWISE_OPERATOR::BIT_AND:
        op = scalar_scalar_integer_bitand<T>;
        break;
    case BITWISE_OPERATOR::BIT_OR:
        op = scalar_scalar_integer_bitor<T>;
        break;
    case BITWISE_OPERATOR::BIT_XOR:
        op = scalar_scalar_integer_bitxor<T>;
        break;
    }
    return op;
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, T a, T b)
{
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    return op(a, b);
}
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = op(ptrA[0], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = op(ptrA[k], ptrB[0]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (long long k = 0; k < (long long)Clen; ++k) {
        ptrC[k] = op(ptrA[k], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);
    indexType q = 0;
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsA.rows();
            ptrC[m] = op(ptrA[q], ptrB[m]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsB = B.dimensions();
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsB.rows();
            ptrC[m] = op(ptrA[j], ptrB[m]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);

    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    indexType q = 0;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsB.rows();
            ptrC[m] = op(ptrA[m], ptrB[q]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsA.rows();
            ptrC[m] = op(ptrA[m], ptrB[j]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
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
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    indexType m = 0;
    for (indexType i = 0; i < dimsA.columns(); i++) {
        for (indexType j = 0; j < dimsB.rows(); j++) {
            ptrC[m] = op(ptrA[i], ptrB[j]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination,
    const ArrayOf& A, const ArrayOf& B)
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
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    T* ptrC = static_cast<T*>(Cp);
    indexType m = 0;
    indexType elementCountA = dimsA.elementCount();
    indexType elementCountB = dimsB.elementCount();

    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            ptrC[m] = op(ptrA[j], ptrB[i]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
integer_bitwise(BITWISE_OPERATOR bitwiseOperator, NelsonType classDestination, const ArrayOf& A,
    const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        if (A.isScalar()) {
            T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(classDestination, 1));
            ptrC[0] = scalar_scalar_integer_bitwise<T>(bitwiseOperator,
                static_cast<const T*>(A.dataPointer())[0],
                static_cast<const T*>(B.dataPointer())[0]);
            return ArrayOf(classDestination, Dimensions(1, 1), ptrC, false);
        }
        return matrix_matrix_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
    }

    if (A.isScalar() || B.isScalar()) {
        if (A.isScalar()) {
            return scalar_matrix_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
        }
        return matrix_scalar_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
    }

    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    return binaryOperatorBroadcastNumeric<T>(
        A, B, classDestination, "bitwise", [op](T a, T b) { return op(a, b); });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
