//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
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
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
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
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
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
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
    T* ptrC = static_cast<T*>(Cp);
    indexType q = 0;
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsA.getRows();
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
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsB = B.getDimensions();
    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsB.getRows();
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
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
    T* ptrC = static_cast<T*>(Cp);

    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    indexType q = 0;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsB.getRows();
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
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsA.getRows();
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
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    indexType rows = std::max(dimsA.getRows(), dimsB.getRows());
    indexType columns = std::max(dimsA.getColumns(), dimsB.getColumns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
    T* ptrC = static_cast<T*>(Cp);
    BitwiseOperation<T> op = getBitwiseOperation<T>(bitwiseOperator);
    indexType m = 0;
    for (indexType i = 0; i < dimsA.getColumns(); i++) {
        for (indexType j = 0; j < dimsB.getRows(); j++) {
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
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    indexType rows = std::max(dimsA.getRows(), dimsB.getRows());
    indexType columns = std::max(dimsA.getColumns(), dimsB.getColumns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    const T* ptrA = static_cast<const T*>(A.getDataPointer());
    const T* ptrB = static_cast<const T*>(B.getDataPointer());
    T* ptrC = static_cast<T*>(Cp);
    indexType m = 0;
    indexType elementCountA = dimsA.getElementCount();
    indexType elementCountB = dimsB.getElementCount();

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
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        // A.isRowVector() && B.isRowVector() with same size
        // A.isColumnVector() && B.isColumnVector() with same size
        // A.isScalar() && B.isScalar() with same size
        // A.isMatrix() && B.isMatrix() with same size
        if (A.isScalar()) {
            T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(classDestination, 1));
            ptrC[0] = scalar_scalar_integer_bitwise<T>(bitwiseOperator,
                static_cast<const T*>(A.getDataPointer())[0],
                static_cast<const T*>(B.getDataPointer())[0]);
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

    if (A.isVector() || B.isVector()) {
        if ((A.isRowVector() && B.isRowVector()) || (A.isColumnVector() && B.isColumnVector())) {
            Error(_("Size mismatch on arguments."));
        }
        if (A.isRowVector() && B.isColumnVector()) {
            return row_column_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
        }
        if (A.isColumnVector() && B.isRowVector()) {
            return column_row_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
        }
        if (A.getRows() == B.getRows()) {
            if (A.isVector()) {
                if (!B.is2D()) {
                    Error(_("Size mismatch on arguments."));
                }
                return row_matrix_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
            }
            if (!A.is2D()) {
                Error(_("Size mismatch on arguments."));
            }
            return matrix_row_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
        }
        if (A.getColumns() == B.getColumns()) {
            if (A.isVector()) {
                if (!B.is2D()) {
                    Error(_("Size mismatch on arguments."));
                }
                return column_matrix_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
            }
            if (!A.is2D()) {
                Error(_("Size mismatch on arguments."));
            }
            return matrix_column_integer_bitwise<T>(bitwiseOperator, classDestination, A, B);
        }
        Error(_("Size mismatch on arguments."));
    }
    return res;
}
//=============================================================================
}
//=============================================================================
