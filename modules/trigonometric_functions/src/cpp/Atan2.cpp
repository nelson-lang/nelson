//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl.h>
#endif
#include <algorithm>
#include "Atan2.hpp"
#include "ArrayOf.hpp"
#include "MatrixCheck.hpp"
#include "NewWithException.hpp"
#include "nlsTrigonometric_functions_exports.h"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
matrix_matrix_atan2_double(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    double* ptrA = (double*)a.getDataPointer();
    double* ptrB = (double*)b.getDataPointer();
    double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, Clen);
#if defined(_NLS_WITH_VML)
    vdAtan2((MKL_INT)Clen, ptrA, ptrB, ptrC);
#else
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
        ptrC[k] = atan2(ptrA[k], ptrB[k]);
    }
#endif
    return ArrayOf(NLS_DOUBLE, dimsC, ptrC, false);
}
//=============================================================================
static ArrayOf
matrix_matrix_atan2_single(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    single* ptrA = (single*)a.getDataPointer();
    single* ptrB = (single*)b.getDataPointer();
    single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, Clen);
#if defined(_NLS_WITH_VML)
    vsAtan2((MKL_INT)Clen, ptrA, ptrB, ptrC);
#else
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
        ptrC[k] = atan2(ptrA[k], ptrB[k]);
    }
#endif
    return ArrayOf(NLS_SINGLE, dimsC, ptrC, false);
}
//=============================================================================
template <class T>
static ArrayOf
scalar_matrix_atan2(NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* ptrA = (T*)a.getDataPointer();
    T* ptrB = (T*)b.getDataPointer();
    T* ptrC = (T*)Cp;
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
        ptrC[k] = atan2(ptrA[0], ptrB[k]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
matrix_scalar_atan2(NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* ptrA = (T*)a.getDataPointer();
    T* ptrB = (T*)b.getDataPointer();
    T* ptrC = (T*)Cp;
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
        ptrC[k] = atan2(ptrA[k], ptrB[0]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static void
vector_column_row_atan2(T* C, const T* A, indexType NA, const T* B, indexType NB)
{
    for (indexType i = 0; i < NA; i++) {
        for (indexType j = 0; j < NB; j++) {
            indexType m = i + j * NA;
            C[m] = atan2(A[i], B[j]);
        }
    }
}
//=============================================================================
template <class T>
static void
vector_row_column_atan2(T* C, const T* A, indexType NA, const T* B, indexType NB)
{
    indexType m = 0;
    for (indexType i = 0; i < NA; i++) {
        for (indexType j = 0; j < NB; j++) {
            C[m] = atan2(A[i], B[j]);
            m++;
        }
    }
}
//=============================================================================
template <class T>
static ArrayOf
vector_matrix_atan2(NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    const T* ptrA = (const T*)a.getDataPointer();
    const T* ptrB = (const T*)b.getDataPointer();
    indexType q = 0;
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getRows();
            C[m] = atan2(ptrB[m], ptrA[q]);
        }
        q++;
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
vector_column_atan2(NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    const T* ptrA = (const T*)a.getDataPointer();
    const T* ptrB = (const T*)b.getDataPointer();
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * b.getRows();
            C[m] = atan2(ptrB[m], ptrA[j]);
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
Atan2(NelsonType classDestination, const ArrayOf& A, const ArrayOf& B)
{
    void* Cp = nullptr;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    Dimensions dimsC;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                return ArrayOf(B);
            }
            return ArrayOf(A);
        }
        if (!(SameSizeCheck(dimsA, dimsB))) {
            Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + L"atan2");
        }
        return ArrayOf(B);
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        if (classDestination == NLS_DOUBLE) {
            return matrix_matrix_atan2_double(A, B);
        }
        return matrix_matrix_atan2_single(A, B);
    }
    if (A.isScalar() || B.isScalar()) {
        if (A.isScalar()) {
            if (classDestination == NLS_DOUBLE) {
                return scalar_matrix_atan2<double>(classDestination, A, B);
            }
            return scalar_matrix_atan2<single>(classDestination, A, B);
        }
        // b.isScalar()
        if (classDestination == NLS_DOUBLE) {
            return matrix_scalar_atan2<double>(classDestination, A, B);
        }
        return matrix_scalar_atan2<single>(classDestination, A, B);
    }
    if (A.isVector() || B.isVector()) {
        if (A.isRowVector() && B.isColumnVector()) {
            dimsC = Dimensions(
                std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
            indexType Clen = dimsC.getElementCount();
            Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
            if (classDestination == NLS_DOUBLE) {
                vector_row_column_atan2<double>((double*)Cp, (const double*)A.getDataPointer(),
                    dimsA.getElementCount(), (const double*)B.getDataPointer(),
                    dimsB.getElementCount());
            } else {
                vector_row_column_atan2<single>((single*)Cp, (const single*)A.getDataPointer(),
                    dimsA.getElementCount(), (const single*)B.getDataPointer(),
                    dimsB.getElementCount());
            }
        } else if (A.isColumnVector() && B.isRowVector()) {
            dimsC = Dimensions(
                std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
            indexType Clen = dimsC.getElementCount();
            Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
            if (classDestination == NLS_DOUBLE) {
                vector_column_row_atan2<double>((double*)Cp, (const double*)A.getDataPointer(),
                    dimsA.getElementCount(), (const double*)B.getDataPointer(),
                    dimsB.getElementCount());
            } else {
                vector_column_row_atan2<single>((single*)Cp, (const single*)A.getDataPointer(),
                    dimsA.getElementCount(), (const single*)B.getDataPointer(),
                    dimsB.getElementCount());
            }
        } else if ((A.isRowVector() && B.isRowVector())
            || (A.isColumnVector() && B.isColumnVector())) {
            Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + L"atan2");
        } else {
            if (dimsA[0] == dimsB[0]) {
                if (A.isVector()) {
                    if (classDestination == NLS_DOUBLE) {
                        return vector_matrix_atan2<double>(classDestination, A, B);
                    }
                    return vector_matrix_atan2<single>(classDestination, A, B);
                }
                if (classDestination == NLS_DOUBLE) {
                    return vector_matrix_atan2<double>(classDestination, B, A);
                }
                return vector_matrix_atan2<single>(classDestination, B, A);
            }
            if (dimsA[1] == dimsB[1]) {
                if (A.isVector()) {
                    if (classDestination == NLS_DOUBLE) {
                        return vector_column_atan2<double>(classDestination, A, B);
                    }
                    return vector_column_atan2<single>(classDestination, A, B);
                }
                if (classDestination == NLS_DOUBLE) {
                    return vector_column_atan2<double>(classDestination, B, A);
                }
                return vector_column_atan2<single>(classDestination, B, A);
            }
            Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + L"atan2");
        }
    } else {
        Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + L"atan2");
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
Atan2(ArrayOf A, ArrayOf B, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    if ((A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE)
        && (B.getDataClass() == NLS_DOUBLE || B.getDataClass() == NLS_SINGLE)) {
        NelsonType destinationClass;
        if (A.getDataClass() == B.getDataClass()) {
            destinationClass = A.getDataClass();
        } else {
            destinationClass = NLS_SINGLE;
        }
        if (destinationClass == NLS_SINGLE) {
            A.promoteType(NLS_SINGLE);
            B.promoteType(NLS_SINGLE);
            res = Atan2(destinationClass, A, B);
        } else {
            A.promoteType(NLS_DOUBLE);
            B.promoteType(NLS_DOUBLE);
            res = Atan2(destinationClass, A, B);
        }
    } else {
        needToOverload = true;
    }

    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
