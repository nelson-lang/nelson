//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
#include "IntegerOperations.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
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
    if (mustCastIntegerAsLongDouble(classDestination)) {
        matC = (matB.template cast<long double>().array() + (long double)ptrA[0])
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<long double, T>));
    } else if (mustCastIntegerAsDouble(classDestination)) {
        matC = (matB.template cast<double>().array() + (double)ptrA[0])
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<double, T>));
    } else {
        matC = (matB.template cast<single>().array() + (single)ptrA[0])
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<single, T>));
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
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
    if (mustCastIntegerAsLongDouble(classDestination)) {
        matC = (matA.template cast<long double>().array() + (long double)ptrB[0])
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<long double, T>));
    } else if (mustCastIntegerAsDouble(classDestination)) {
        matC = (matA.template cast<double>().array() + (double)ptrB[0])
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<double, T>));
    } else {
        matC = (matA.template cast<single>().array() + (single)ptrB[0])
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<single, T>));
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (T*)A.getDataPointer(), 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (T*)B.getDataPointer(), 1, Clen);
    if (mustCastIntegerAsLongDouble(classDestination)) {
        matC = (matA.template cast<long double>() + matB.template cast<long double>())
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<long double, T>));
    } else if (mustCastIntegerAsDouble(classDestination)) {
        matC = (matA.template cast<double>() + matB.template cast<double>())
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<double, T>));
    } else {
        matC = (matA.template cast<single>() + matB.template cast<single>())
                   .array()
                   .round()
                   .unaryExpr(std::ref(numeric_cast<single, T>));
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
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
    if (mustCastIntegerAsLongDouble(classDestination)) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsA.getRows();
                ptrC[m] = numeric_cast<long double, T>(
                    roundl((long double)ptrA[q] + (long double)ptrB[m]));
            }
            q++;
        }
    } else if (mustCastIntegerAsDouble(classDestination)) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsA.getRows();
                ptrC[m] = numeric_cast<double, T>(round((double)ptrA[q] + (double)ptrB[m]));
            }
            q++;
        }
    } else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsA.getRows();
                ptrC[m] = numeric_cast<single, T>(roundf((single)ptrA[q] + (single)ptrB[m]));
            }
            q++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)res.getDataPointer();

    if (mustCastIntegerAsLongDouble(classDestination)) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsB.getRows();
                ptrC[m] = numeric_cast<long double, T>(
                    roundl((long double)ptrA[j] + (long double)ptrB[m]));
            }
        }
    } else if (mustCastIntegerAsDouble(classDestination)) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsB.getRows();
                ptrC[m] = numeric_cast<double, T>(round((double)ptrA[j] + (double)ptrB[m]));
            }
        }
    } else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsB.getRows();
                ptrC[m] = numeric_cast<single, T>(roundf((single)ptrA[j] + (single)ptrB[m]));
            }
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrC = (T*)res.getDataPointer();

    indexType q = 0;

    if (mustCastIntegerAsLongDouble(classDestination)) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsB.getRows();
                ptrC[m] = numeric_cast<long double, T>(
                    roundl((long double)ptrA[m] + (long double)ptrB[q]));
            }
            q++;
        }
    } else if (mustCastIntegerAsDouble(classDestination)) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsB.getRows();
                ptrC[m] = numeric_cast<double, T>(round((double)ptrA[m] + (double)ptrB[q]));
            }
            q++;
        }
    } else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsB.getRows();
                ptrC[m] = numeric_cast<single, T>(roundf((single)ptrA[m] + (single)ptrB[q]));
            }
            q++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrC = (T*)res.getDataPointer();

    if (mustCastIntegerAsLongDouble(classDestination)) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsA.getRows();
                ptrC[m] = numeric_cast<long double, T>(
                    roundl((long double)ptrA[m] + (long double)ptrB[j]));
            }
        }
    } else if (mustCastIntegerAsDouble(classDestination)) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsA.getRows();
                ptrC[m] = numeric_cast<double, T>(round((double)ptrA[m] + (double)ptrB[j]));
            }
        }
    } else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * dimsA.getRows();
                ptrC[m] = numeric_cast<single, T>(roundf((single)ptrA[m] + (single)ptrB[j]));
            }
        }
    }

    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrC = (T*)res.getDataPointer();

    indexType m = 0;

    if (mustCastIntegerAsLongDouble(classDestination)) {
        for (indexType i = 0; i < dimsA.getColumns(); i++) {
            for (indexType j = 0; j < dimsB.getRows(); j++) {
                ptrC[m] = numeric_cast<long double, T>(
                    roundl((long double)ptrA[i] + (long double)ptrB[j]));
                m++;
            }
        }
    } else if (mustCastIntegerAsDouble(classDestination)) {
        for (indexType i = 0; i < dimsA.getColumns(); i++) {
            for (indexType j = 0; j < dimsB.getRows(); j++) {
                ptrC[m] = numeric_cast<long double, T>(round((double)ptrA[i] + (double)ptrB[j]));
                m++;
            }
        }
    } else {
        for (indexType i = 0; i < dimsA.getColumns(); i++) {
            for (indexType j = 0; j < dimsB.getRows(); j++) {
                ptrC[m] = numeric_cast<single, T>(roundf((single)ptrA[i] + (single)ptrB[j]));
                m++;
            }
        }
    }

    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
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
    T* ptrC = (T*)res.getDataPointer();

    indexType m = 0;
    if (mustCastIntegerAsLongDouble(classDestination)) {
        for (indexType i = 0; i < dimsB.getElementCount(); i++) {
            for (indexType j = 0; j < dimsA.getElementCount(); j++) {
                ptrC[m] = numeric_cast<long double, T>(
                    roundl((long double)ptrA[j] + (long double)ptrB[i]));
                m++;
            }
        }
    } else if (mustCastIntegerAsDouble(classDestination)) {
        for (indexType i = 0; i < dimsB.getElementCount(); i++) {
            for (indexType j = 0; j < dimsA.getElementCount(); j++) {
                ptrC[m] = numeric_cast<double, T>(round((double)ptrA[j] + (double)ptrB[i]));
                m++;
            }
        }
    } else {
        for (indexType i = 0; i < dimsB.getElementCount(); i++) {
            for (indexType j = 0; j < dimsA.getElementCount(); j++) {
                ptrC[m] = numeric_cast<single, T>(roundf((single)ptrA[j] + (single)ptrB[i]));
                m++;
            }
        }
    }

    return res;
}
//=============================================================================
template <class T>
ArrayOf
integer_addition(Class classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();

    if (SameSizeCheck(dimsA, dimsB)) {
        // A.isRowVector() && B.isRowVector() with same size
        // A.isColumnVector() && B.isColumnVector() with same size
        // A.isScalar() && B.isScalar() with same size
        // A.isMatrix() && B.isMatrix() with same size
        res = matrix_matrix_integer_addition<T>(classDestination, A, B);
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
                    Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
                } else if (A.isRowVector() && B.isColumnVector()) {
                    res = row_column_integer_addition<T>(classDestination, A, B);
                } else if (A.isColumnVector() && B.isRowVector()) {
                    res = column_row_integer_addition<T>(classDestination, A, B);
                } else if (dimsA.getRows() == dimsB.getRows()) {
                    if (A.isVector()) {
                        if (!B.is2D()) {
                            Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
                        }
                        res = row_matrix_integer_addition<T>(classDestination, A, B);
                    } else {
                        if (!A.is2D()) {
                            Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
                        }
                        res = matrix_row_integer_addition<T>(classDestination, A, B);
                    }
                } else if (dimsA.getColumns() == dimsB.getColumns()) {
                    if (A.isVector()) {
                        if (!B.is2D()) {
                            Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
                        }
                        res = column_matrix_integer_addition<T>(classDestination, A, B);
                    } else {
                        if (!A.is2D()) {
                            Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
                        }
                        res = matrix_column_integer_addition<T>(classDestination, A, B);
                    }
                } else {
                    Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
                }
            } else {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
            }
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
