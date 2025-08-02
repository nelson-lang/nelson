//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Equals.hpp"
#include "MatrixCheck.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
matrix_matrix_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    ArrayOf res;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);

    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();

    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }

    if (relationOperator) {
        if (Clen == 1) {
            Cp[0] = relationOperator(classA, ptrA, ptrB, 0, 0);
        } else {
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
                Cp[i] = relationOperator(classA, ptrA, ptrB, i, i);
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
scalar_matrix_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();

    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
            Cp[i] = relationOperator(classA, ptrA, ptrB, 0, i);
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_scalar_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();
    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
            Cp[i] = relationOperator(classA, ptrA, ptrB, i, 0);
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
vector_row_column_operator(const Dimensions& outputDimensions, const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    ArrayOf res;
    indexType Clen = outputDimensions.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    indexType nbElementsA = A.getElementCount();
    indexType nbElementsB = B.getElementCount();
    NelsonType classA = A.getDataClass();
    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        indexType m = 0;
        for (indexType i = 0; i < nbElementsA; i++) {
            for (indexType j = 0; j < nbElementsB; j++) {
                Cp[m] = relationOperator(classA, ptrA, ptrB, i, j);
                m++;
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, outputDimensions, Cp, false);
}
//=============================================================================
static ArrayOf
vector_column_row_operator(const Dimensions& outputDimensions, const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    ArrayOf res;
    indexType Clen = outputDimensions.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();
    indexType elementCountA = A.getElementCount();
    indexType elementCountB = B.getElementCount();
    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        indexType m = 0;
        for (indexType i = 0; i < elementCountB; i++) {
            for (indexType j = 0; j < elementCountA; j++) {
                Cp[m] = relationOperator(classA, ptrA, ptrB, j, i);
                m++;
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, outputDimensions, Cp, false);
}
//=============================================================================
static ArrayOf
vector_matrix_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();
    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        indexType q = 0;
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * A.getRows();
                Cp[m] = relationOperator(classA, ptrA, ptrB, q, m);
            }
            q++;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_vector_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();
    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        indexType q = 0;
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * A.getRows();
                Cp[m] = relationOperator(classA, ptrA, ptrB, m, q);
            }
            q++;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
vector_column_matrix_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{

    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();

    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;

    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * A.getColumns();
                Cp[m] = relationOperator(classA, ptrA, ptrB, j, m);
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_vector_column_operator(const ArrayOf& A, const ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();

    logical (*relationOperator)(NelsonType, void*, void*, indexType, indexType) = nullptr;
    switch (classA) {
    case NLS_STRING_ARRAY:
        relationOperator = stringRelationOperator;
        break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        relationOperator = realRelationOperator;
        break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        relationOperator = complexRelationOperator;
        break;
    default:
        break;
    }
    if (relationOperator) {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * B.getColumns();
                Cp[m] = relationOperator(classA, ptrA, ptrB, m, j);
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
relationOperator(const ArrayOf& A, const ArrayOf& B, const std::wstring& operatorName,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    bool& needToOverload)
{
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    Dimensions dimsC;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                ArrayOf res = ArrayOf::emptyConstructor(dimsB);
                res.promoteType(NLS_LOGICAL);
                return res;
            }
            ArrayOf res = ArrayOf::emptyConstructor(dimsA);
            res.promoteType(NLS_LOGICAL);
            return res;
        }
        if (!(SameSizeCheck(dimsA, dimsB))) {
            Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + operatorName);
        }
        ArrayOf res = ArrayOf::emptyConstructor(dimsB);
        res.promoteType(NLS_LOGICAL);
        return res;
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return matrix_matrix_operator(
            A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
    }
    if (A.isScalar() || B.isScalar()) {
        if (A.isScalar()) {
            return scalar_matrix_operator(
                A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
        }
        return matrix_scalar_operator(
            A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
    }
    if (A.isVector() || B.isVector()) {
        if (A.isRowVector() && B.isColumnVector()) {
            dimsC = Dimensions(
                std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
            return vector_row_column_operator(
                dimsC, A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
        }
        if (A.isColumnVector() && B.isRowVector()) {
            dimsC = Dimensions(
                std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
            return vector_column_row_operator(
                dimsC, A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
        }
        if ((A.isRowVector() && B.isRowVector()) || (A.isColumnVector() && B.isColumnVector())) {
            Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + operatorName);
        } else {
            if (dimsA[1] == dimsB[1]) {
                if (A.isVector()) {
                    return vector_column_matrix_operator(A, B, realRelationOperator,
                        complexRelationOperator, stringRelationOperator);
                }
                return matrix_vector_column_operator(
                    A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
            }
            if ((dimsA[0] == dimsB[0]) && (dimsA[0] != 1)) {
                if (A.isVector()) {
                    return vector_matrix_operator(A, B, realRelationOperator,
                        complexRelationOperator, stringRelationOperator);
                }
                return matrix_vector_operator(
                    A, B, realRelationOperator, complexRelationOperator, stringRelationOperator);
            }
            Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + operatorName);
        }
    } else {
        Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + operatorName);
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
