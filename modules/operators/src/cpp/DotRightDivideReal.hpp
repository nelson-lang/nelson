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
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    T* ptrA = (T*)A.dataPointer();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB((T*)B.dataPointer(), 1, Clen);
    matC = ptrA[0] / matB.array();
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA((T*)A.dataPointer(), 1, Clen);
    T* ptrB = (T*)B.dataPointer();
    matC = matA.array() / ptrB[0];
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    T* Cp = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA((T*)A.dataPointer(), 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB((T*)B.dataPointer(), 1, Clen);
    matC = matA.array() / matB.array();
#else
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
        Cp[k] = ptrA[k] / ptrB[k];
    }
#endif
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    // Column-outer order keeps the inner write contiguous so it vectorizes.
    for (indexType j = 0; j < dimsC.columns(); j++) {
        for (indexType i = 0; i < dimsC.rows(); i++) {
            indexType m = i + j * dimsA.rows();
            ptrC[m] = ptrA[i] / ptrB[m];
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)res.dataPointer();
    for (indexType j = 0; j < res.columns(); j++) {
        T aj = ptrA[j];
        for (indexType i = 0; i < res.rows(); i++) {
            indexType m = i + j * B.rows();
            ptrC[m] = aj / ptrB[m];
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    for (indexType j = 0; j < res.columns(); j++) {
        for (indexType i = 0; i < res.rows(); i++) {
            indexType m = i + j * B.rows();
            ptrC[m] = ptrA[m] / ptrB[i];
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    for (indexType j = 0; j < res.columns(); j++) {
        T bj = ptrB[j];
        for (indexType i = 0; i < res.rows(); i++) {
            indexType m = i + j * A.rows();
            ptrC[m] = ptrA[m] / bj;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    indexType rows = std::max(A.rows(), B.rows());
    indexType columns = std::max(A.columns(), B.columns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)res.dataPointer();

    indexType m = 0;
    for (indexType i = 0; i < A.columns(); i++) {
        for (indexType j = 0; j < B.rows(); j++) {
            ptrC[m] = ptrA[i] / ptrB[j];
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    indexType rows = std::max(A.rows(), B.rows());
    indexType columns = std::max(A.columns(), B.columns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.dataPointer();
    T* ptrB = (T*)B.dataPointer();
    T* ptrC = (T*)res.dataPointer();

    indexType m = 0;
    indexType elementCountA = A.elementCount();
    indexType elementCountB = B.elementCount();
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            ptrC[m] = ptrA[j] / ptrB[i];
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    return binaryOperator2DDispatch(classDestination, A, B, matrix_matrix_real_dotRightDivide<T>,
        scalar_matrix_real_dotRightDivide<T>, matrix_scalar_real_dotRightDivide<T>,
        matrix_matrix_real_dotRightDivide<T>, row_column_real_dotRightDivide<T>,
        column_row_real_dotRightDivide<T>, row_matrix_real_dotRightDivide<T>,
        matrix_row_real_dotRightDivide<T>, column_matrix_real_dotRightDivide<T>,
        matrix_column_real_dotRightDivide<T>,
        [](const ArrayOf& lhs, const ArrayOf& rhs, NelsonType destinationClass) {
            return binaryOperatorBroadcastNumeric<T>(
                lhs, rhs, destinationClass, "./", [](T a, T b) { return a / b; });
        });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
