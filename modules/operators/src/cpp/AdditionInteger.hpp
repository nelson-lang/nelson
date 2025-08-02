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
#include "ArrayOf.hpp"
#include "IntegerOperations.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = scalar_scalar_integer_addition(ptrA[0], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = scalar_scalar_integer_addition(ptrA[k], ptrB[0]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (long long k = 0; k < (long long)Clen; ++k) {
        ptrC[k] = scalar_scalar_integer_addition(ptrA[k], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    indexType q = 0;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsA.getRows();
            ptrC[m] = scalar_scalar_integer_addition(ptrA[q], ptrB[m]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsB = B.getDimensions();
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsB.getRows();
            ptrC[m] = scalar_scalar_integer_addition(ptrA[j], ptrB[m]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;

    indexType q = 0;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsB.getRows();
            ptrC[m] = scalar_scalar_integer_addition(ptrA[m], ptrB[q]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsA.getRows();
            ptrC[m] = scalar_scalar_integer_addition(ptrA[m], ptrB[j]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    indexType m = 0;
    for (indexType i = 0; i < dimsA.getColumns(); i++) {
        for (indexType j = 0; j < dimsB.getRows(); j++) {
            ptrC[m] = scalar_scalar_integer_addition(ptrA[i], ptrB[j]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    indexType m = 0;
    indexType elementCountA = dimsA.getElementCount();
    indexType elementCountB = dimsB.getElementCount();
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            ptrC[m] = scalar_scalar_integer_addition(ptrA[j], ptrB[i]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
integer_addition(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[0] = scalar_scalar_integer_addition<T>(
                ((T*)A.getDataPointer())[0], ((T*)B.getDataPointer())[0]);
            res = ArrayOf(classDestination, Dimensions(1, 1), ptrC, false);
        } else {
            res = matrix_matrix_integer_addition<T>(classDestination, A, B);
        }
    } else {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                res = scalar_matrix_integer_addition<T>(classDestination, A, B);
            } else {
                res = matrix_scalar_integer_addition<T>(classDestination, A, B);
            }
        } else {
            if (A.isVector() || B.isVector()) {
                if ((A.isRowVector() && B.isRowVector())
                    || (A.isColumnVector() && B.isColumnVector())) {
                    Error(_("Size mismatch on arguments to arithmetic operator") + " " + "+");
                } else if (A.isRowVector() && B.isColumnVector()) {
                    res = row_column_integer_addition<T>(classDestination, A, B);
                } else if (A.isColumnVector() && B.isRowVector()) {
                    res = column_row_integer_addition<T>(classDestination, A, B);
                } else if (A.getRows() == B.getRows()) {
                    if (A.isVector()) {
                        if (!B.is2D()) {
                            Error(
                                _("Size mismatch on arguments to arithmetic operator") + " " + "+");
                        }
                        res = row_matrix_integer_addition<T>(classDestination, A, B);
                    } else {
                        if (!A.is2D()) {
                            Error(
                                _("Size mismatch on arguments to arithmetic operator") + " " + "+");
                        }
                        res = matrix_row_integer_addition<T>(classDestination, A, B);
                    }
                } else if (A.getColumns() == B.getColumns()) {
                    if (A.isVector()) {
                        if (!B.is2D()) {
                            Error(
                                _("Size mismatch on arguments to arithmetic operator") + " " + "+");
                        }
                        res = column_matrix_integer_addition<T>(classDestination, A, B);
                    } else {
                        if (!A.is2D()) {
                            Error(
                                _("Size mismatch on arguments to arithmetic operator") + " " + "+");
                        }
                        res = matrix_column_integer_addition<T>(classDestination, A, B);
                    }
                } else {
                    Error(_("Size mismatch on arguments to arithmetic operator") + " " + "+");
                }
            } else {
                Error(_("Size mismatch on arguments to arithmetic operator") + " " + "+");
            }
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
