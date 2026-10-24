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
#include "lapack_eigen_config.hpp"
#include "omp_for_loop.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    T* ptrA = (T*)A.dataPointer();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB((T*)B.dataPointer(), 1, Clen);
    matC = ptrA[0] - matB.array();
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA((T*)A.dataPointer(), 1, Clen);
    T* ptrB = (T*)B.dataPointer();
    matC = matA.array() - ptrB[0];
    return res;
}
//=============================================================================
template <class T>
ArrayOf
scalar_scalar_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(classDestination, 1));
    ptrC[0] = ((T*)A.dataPointer())[0] - ((T*)B.dataPointer())[0];
    return ArrayOf(classDestination, Dimensions(1, 1), ptrC, false);
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (long long k = 0; k < (long long)Clen; ++k) {
        ptrC[k] = ptrA[k] - ptrB[k];
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrC = (T*)res.dataPointer();
    indexType q = 0;
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * dimsA.rows();
            ptrC[m] = ptrA[q] - ptrB[m];
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = ptrA[j] - ptrB[m];
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = ptrA[m] - ptrB[q];
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = ptrA[m] - ptrB[j];
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = ptrA[i] - ptrB[j];
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = ptrA[j] - ptrB[i];
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
real_subtraction(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    return binaryOperator2DDispatch(classDestination, A, B, scalar_scalar_real_subtraction<T>,
        scalar_matrix_real_subtraction<T>, matrix_scalar_real_subtraction<T>,
        matrix_matrix_real_subtraction<T>, row_column_real_subtraction<T>,
        column_row_real_subtraction<T>, row_matrix_real_subtraction<T>,
        matrix_row_real_subtraction<T>, column_matrix_real_subtraction<T>,
        matrix_column_real_subtraction<T>,
        [](const ArrayOf& lhs, const ArrayOf& rhs, NelsonType destinationClass) {
            return binaryOperatorBroadcastNumeric<T>(
                lhs, rhs, destinationClass, "-", [](T a, T b) { return a - b; });
        });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
