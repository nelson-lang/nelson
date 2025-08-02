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
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static T
hypothenusRealNelson(T a, T b)
{
    // special cases:
    // hypot(NaN, Inf)
    // hypot(NaN, -Inf)
    // hypot(Inf, NaN)
    // hypot(-Inf, NaN)
    if (std::isnan(a) || std::isnan(b)) {
        return (T)std::nan("NaN");
    }
    return (T)std::hypot(a, b);
}
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = hypothenusRealNelson<T>(ptrA[0], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
scalar_scalar_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    ptrC[0] = hypothenusRealNelson<T>(ptrA[0], ptrB[0]);
    return ArrayOf(classDestination, dimsC, ptrC, false);
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = hypothenusRealNelson<T>(ptrA[k], ptrB[0]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, ptrC, false);
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = hypothenusRealNelson<T>(ptrA[k], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = hypothenusRealNelson<T>(ptrA[q], ptrB[m]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = hypothenusRealNelson<T>(ptrA[j], ptrB[m]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);

    Dimensions dimsB = B.getDimensions();
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;

    indexType q = 0;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsB.getRows();
            ptrC[m] = hypothenusRealNelson<T>(ptrA[m], ptrB[q]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    res = ArrayOf(classDestination, dimsC, Cp, false);
    Dimensions dimsA = A.getDimensions();
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * dimsA.getRows();
            ptrC[m] = hypothenusRealNelson<T>(ptrA[m], ptrB[j]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = hypothenusRealNelson<T>(ptrA[i], ptrB[j]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            ptrC[m] = hypothenusRealNelson<T>(ptrA[j], ptrB[i]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
real_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    if (A.isScalar() || B.isScalar()) {
        if (A.isScalar()) {
            res = scalar_matrix_real_hypothenuse<T>(classDestination, A, B);
        } else {
            res = matrix_scalar_real_hypothenuse<T>(classDestination, A, B);
        }
    } else {
        if (A.isVector() || B.isVector()) {
            if ((A.isRowVector() && B.isRowVector())
                || (A.isColumnVector() && B.isColumnVector())) {
                Error(_W("Arrays have incompatible sizes for this operation."),
                    L"Nelson:sizeDimensionsMustMatch");
            } else if (A.isRowVector() && B.isColumnVector()) {
                res = row_column_real_hypothenuse<T>(classDestination, A, B);
            } else if (A.isColumnVector() && B.isRowVector()) {
                res = column_row_real_hypothenuse<T>(classDestination, A, B);
            } else if (A.getRows() == B.getRows()) {
                if (A.isVector()) {
                    if (!B.is2D()) {
                        Error(_W("Arrays have incompatible sizes for this operation."),
                            L"Nelson:sizeDimensionsMustMatch");
                    }
                    res = row_matrix_real_hypothenuse<T>(classDestination, A, B);
                } else {
                    if (!A.is2D()) {
                        Error(_W("Arrays have incompatible sizes for this operation."),
                            L"Nelson:sizeDimensionsMustMatch");
                    }
                    res = matrix_row_real_hypothenuse<T>(classDestination, A, B);
                }
            } else if (A.getColumns() == B.getColumns()) {
                if (A.isVector()) {
                    if (!B.is2D()) {
                        Error(_W("Arrays have incompatible sizes for this operation."),
                            L"Nelson:sizeDimensionsMustMatch");
                    }
                    res = column_matrix_real_hypothenuse<T>(classDestination, A, B);
                } else {
                    if (!A.is2D()) {
                        Error(_W("Arrays have incompatible sizes for this operation."),
                            L"Nelson:sizeDimensionsMustMatch");
                    }
                    res = matrix_column_real_hypothenuse<T>(classDestination, A, B);
                }
            } else {
                Error(_W("Arrays have incompatible sizes for this operation."),
                    L"Nelson:sizeDimensionsMustMatch");
            }
        } else {
            Error(_W("Arrays have incompatible sizes for this operation."),
                L"Nelson:sizeDimensionsMustMatch");
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
