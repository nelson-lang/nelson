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
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
        (std::complex<T>*)Cz, 1, Clen);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (std::complex<T>*)Bz, 1, Clen);
    matC = Az[0] / matB.array();
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
        (std::complex<T>*)Cz, 1, Clen);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (std::complex<T>*)Az, 1, Clen);
    matC = matA.array() / Bz[0];
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(
        (std::complex<T>*)Cz, 1, Clen);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (std::complex<T>*)Az, 1, Clen);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (std::complex<T>*)Bz, 1, Clen);
    matC = matA.cwiseQuotient(matB);
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    indexType m = 0;
    for (indexType i = 0; i < dimsA.getColumns(); i++) {
        for (indexType j = 0; j < dimsB.getRows(); j++) {
            Cz[m] = Az[i] / Bz[j];
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    indexType m = 0;
    indexType elementCountA = dimsA.getElementCount();
    indexType elementCountB = dimsB.getElementCount();
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            Cz[m] = Az[j] / Bz[i];
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    Dimensions dimsA = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    indexType q = 0;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsA.getRows();
            Cz[m] = Az[q] / Bz[m];
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    Dimensions dimsB = B.getDimensions();

    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    indexType q = 0;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsB.getRows();
            Cz[m] = Az[m] / Bz[q];
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    Dimensions dimsB = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsB.getRows();
            Cz[m] = Az[j] / Bz[m];
        }
    }

    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_complex_dotRightDivide(
    NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    Dimensions dimsA = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrC);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsA.getRows();
            Cz[m] = Az[m] / Bz[j];
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
complex_dotRightDivide(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();

    if (SameSizeCheck(dimsA, dimsB)) {
        // A.isRowVector() && B.isRowVector() with same size
        // A.isColumnVector() && B.isColumnVector() with same size
        // A.isScalar() && B.isScalar() with same size
        // A.isMatrix() && B.isMatrix() with same size
        res = matrix_matrix_complex_dotRightDivide<T>(classDestination, A, B);
    } else {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                res = scalar_matrix_complex_dotRightDivide<T>(classDestination, A, B);
            } else {
                res = matrix_scalar_complex_dotRightDivide<T>(classDestination, A, B);
            }
        } else {
            if (A.isVector() || B.isVector()) {
                if ((A.isRowVector() && B.isRowVector())
                    || (A.isColumnVector() && B.isColumnVector())) {
                    Error(_("Size mismatch on arguments to arithmetic operator") + " " + "./");
                } else if (A.isRowVector() && B.isColumnVector()) {
                    res = row_column_complex_dotRightDivide<T>(classDestination, A, B);
                } else if (A.isColumnVector() && B.isRowVector()) {
                    res = column_row_complex_dotRightDivide<T>(classDestination, A, B);
                } else if (dimsA.getRows() == dimsB.getRows()) {
                    if (A.isVector()) {
                        if (!B.is2D()) {
                            Error(_("Size mismatch on arguments to arithmetic operator") + " "
                                + "./");
                        }
                        res = row_matrix_complex_dotRightDivide<T>(classDestination, A, B);
                    } else {
                        if (!A.is2D()) {
                            Error(_("Size mismatch on arguments to arithmetic operator") + " "
                                + "./");
                        }
                        res = matrix_row_complex_dotRightDivide<T>(classDestination, A, B);
                    }
                } else if (dimsA.getColumns() == dimsB.getColumns()) {
                    if (A.isVector()) {
                        if (!B.is2D()) {
                            Error(_("Size mismatch on arguments to arithmetic operator") + " "
                                + "./");
                        }
                        res = column_matrix_complex_dotRightDivide<T>(classDestination, A, B);
                    } else {
                        if (!A.is2D()) {
                            Error(_("Size mismatch on arguments to arithmetic operator") + " "
                                + "./");
                        }
                        res = matrix_column_complex_dotRightDivide<T>(classDestination, A, B);
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
