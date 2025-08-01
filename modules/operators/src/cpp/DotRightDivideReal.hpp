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
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    T* ptrA = (T*)A.getDataPointer();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (T*)B.getDataPointer(), 1, Clen);
    matC = ptrA[0] / matB.array();
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (T*)A.getDataPointer(), 1, Clen);
    T* ptrB = (T*)B.getDataPointer();
    matC = matA.array() / ptrB[0];
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    T* Cp = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (T*)A.getDataPointer(), 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (T*)B.getDataPointer(), 1, Clen);
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
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)res.getDataPointer();
    indexType q = 0;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsA.getRows();
            ptrC[m] = ptrA[q] / ptrB[m];
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)res.getDataPointer();
    for (indexType i = 0; i < res.getRows(); i++) {
        for (indexType j = 0; j < res.getColumns(); j++) {
            indexType m = i + j * B.getRows();
            ptrC[m] = ptrA[j] / ptrB[m];
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
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    indexType q = 0;
    for (indexType i = 0; i < res.getRows(); i++) {
        for (indexType j = 0; j < res.getColumns(); j++) {
            indexType m = i + j * B.getRows();
            ptrC[m] = ptrA[m] / ptrB[q];
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_real_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    for (indexType i = 0; i < res.getRows(); i++) {
        for (indexType j = 0; j < res.getColumns(); j++) {
            indexType m = i + j * A.getRows();
            ptrC[m] = ptrA[m] / ptrB[j];
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
    indexType rows = std::max(A.getRows(), B.getRows());
    indexType columns = std::max(A.getColumns(), B.getColumns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)res.getDataPointer();

    indexType m = 0;
    for (indexType i = 0; i < A.getColumns(); i++) {
        for (indexType j = 0; j < B.getRows(); j++) {
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
    indexType rows = std::max(A.getRows(), B.getRows());
    indexType columns = std::max(A.getColumns(), B.getColumns());
    Dimensions dimsC(rows, columns);
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)res.getDataPointer();

    indexType m = 0;
    indexType elementCountA = A.getElementCount();
    indexType elementCountB = B.getElementCount();
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
            ptrC[0] = ((T*)A.getDataPointer())[0] / ((T*)B.getDataPointer())[0];
            res = ArrayOf(classDestination, Dimensions(1, 1), ptrC, false);
        } else {
            res = matrix_matrix_real_dotRightDivide<T>(classDestination, A, B);
        }
    } else {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                res = scalar_matrix_real_dotRightDivide<T>(classDestination, A, B);
            } else {
                res = matrix_scalar_real_dotRightDivide<T>(classDestination, A, B);
            }
        } else {
            if (A.isVector() || B.isVector()) {
                if ((A.isRowVector() && B.isRowVector())
                    || (A.isColumnVector() && B.isColumnVector())) {
                    Error(_("Size mismatch on arguments to arithmetic operator") + " " + "./");
                } else if (A.isRowVector() && B.isColumnVector()) {
                    res = row_column_real_dotRightDivide<T>(classDestination, A, B);
                } else if (A.isColumnVector() && B.isRowVector()) {
                    res = column_row_real_dotRightDivide<T>(classDestination, A, B);
                } else if (A.getRows() == B.getRows()) {
                    if (A.isVector()) {
                        if (!B.is2D()) {
                            Error(_("Size mismatch on arguments to arithmetic operator") + " "
                                + "./");
                        }
                        res = row_matrix_real_dotRightDivide<T>(classDestination, A, B);
                    } else {
                        if (!A.is2D()) {
                            Error(_("Size mismatch on arguments to arithmetic operator") + " "
                                + "./");
                        }
                        res = matrix_row_real_dotRightDivide<T>(classDestination, A, B);
                    }
                } else if (A.getColumns() == B.getColumns()) {
                    if (A.isVector()) {
                        if (!B.is2D()) {
                            Error(_("Size mismatch on arguments to arithmetic operator") + " "
                                + "./");
                        }
                        res = column_matrix_real_dotRightDivide<T>(classDestination, A, B);
                    } else {
                        if (!A.is2D()) {
                            Error(_("Size mismatch on arguments to arithmetic operator") + " "
                                + "./");
                        }
                        res = matrix_column_real_dotRightDivide<T>(classDestination, A, B);
                    }
                } else {
                    Error(_("Size mismatch on arguments to arithmetic operator") + " " + "./");
                }
            } else {
                Error(_("Size mismatch on arguments to arithmetic operator") + " " + "./");
            }
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
