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
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
hypothenusComplexNelson(std::complex<T> a, std::complex<T> b)
{
    T _a = std::abs(a);
    T _b = std::abs(b);
    if (std::isnan(_a) || std::isnan(_b)) {
        return (T)std::nan("NaN");
    }
    return (T)std::hypot(_a, _b);
}
//=============================================================================
template <class T>
ArrayOf
scalar_matrix_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        Cz[k] = hypothenusComplexNelson<T>(Az[0], Bz[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_scalar_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        Cz[k] = hypothenusComplexNelson<T>(Az[k], Bz[0]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_matrix_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        Cz[k] = hypothenusComplexNelson<T>(Az[k], Bz[k]);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_column_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            Cz[m] = hypothenusComplexNelson<T>(Az[i], Bz[j]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_row_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    indexType elementCountB = dimsB.getElementCount();
    indexType elementCountA = dimsA.getElementCount();
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            Cz[m] = hypothenusComplexNelson<T>(Az[j], Bz[i]);
            m++;
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
row_matrix_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            Cz[m] = hypothenusComplexNelson<T>(Az[q], Bz[m]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_row_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            Cz[m] = hypothenusComplexNelson<T>(Az[m], Bz[q]);
        }
        q++;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
column_matrix_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            Cz[m] = hypothenusComplexNelson<T>(Az[j], Bz[m]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
matrix_column_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
            Cz[m] = hypothenusComplexNelson<T>(Az[m], Bz[j]);
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
scalar_scalar_complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
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
    Cz[0] = hypothenusComplexNelson<T>(Az[0], Bz[0]);
    return res;
}
//=============================================================================
template <class T>
ArrayOf
complex_hypothenuse(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    if (A.isScalar() || B.isScalar()) {
        if (A.isScalar()) {
            res = scalar_matrix_complex_hypothenuse<T>(classDestination, A, B);
        } else {
            res = matrix_scalar_complex_hypothenuse<T>(classDestination, A, B);
        }
    } else {
        if (A.isVector() || B.isVector()) {
            if ((A.isRowVector() && B.isRowVector())
                || (A.isColumnVector() && B.isColumnVector())) {
                Error(_W("Arrays have incompatible sizes for this operation."),
                    L"Nelson:sizeDimensionsMustMatch");
            } else if (A.isRowVector() && B.isColumnVector()) {
                res = row_column_complex_hypothenuse<T>(classDestination, A, B);
            } else if (A.isColumnVector() && B.isRowVector()) {
                res = column_row_complex_hypothenuse<T>(classDestination, A, B);
            } else if (A.getRows() == B.getRows()) {
                if (A.isVector()) {
                    if (!B.is2D()) {
                        Error(_W("Arrays have incompatible sizes for this operation."),
                            L"Nelson:sizeDimensionsMustMatch");
                    }
                    res = row_matrix_complex_hypothenuse<T>(classDestination, A, B);
                } else {
                    if (!A.is2D()) {
                        Error(_W("Arrays have incompatible sizes for this operation."),
                            L"Nelson:sizeDimensionsMustMatch");
                    }
                    res = matrix_row_complex_hypothenuse<T>(classDestination, A, B);
                }
            } else if (A.getColumns() == B.getColumns()) {
                if (A.isVector()) {
                    if (!B.is2D()) {
                        Error(_W("Arrays have incompatible sizes for this operation."),
                            L"Nelson:sizeDimensionsMustMatch");
                    }
                    res = column_matrix_complex_hypothenuse<T>(classDestination, A, B);
                } else {
                    if (!A.is2D()) {
                        Error(_W("Arrays have incompatible sizes for this operation."),
                            L"Nelson:sizeDimensionsMustMatch");
                    }
                    res = matrix_column_complex_hypothenuse<T>(classDestination, A, B);
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
