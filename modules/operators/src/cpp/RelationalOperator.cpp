//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "nlsConfig.h"
#include "Equals.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "ClassName.hpp"
#include "FindCommonClass.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
matrix_matrix_operator(ArrayOf& A, ArrayOf& B,
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
        switch (classA) {
        case NLS_STRING_ARRAY: {
            Cp[i] = (*stringRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_LOGICAL: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_UINT8: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_INT8: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_UINT16: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_INT16: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_UINT32: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_INT32: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_UINT64: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_INT64: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_SINGLE: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_DOUBLE: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_SCOMPLEX: {
            Cp[i] = (*complexRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_DCOMPLEX: {
            Cp[i] = (*complexRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        case NLS_CHAR: {
            Cp[i] = (*realRelationOperator)(classA, ptrA, ptrB, i, i);
        } break;
        default: {
        } break;
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
        switch (classA) {
        case NLS_STRING_ARRAY: {
            Cp[i] = stringRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_LOGICAL: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_UINT8: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_INT8: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_UINT16: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_INT16: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_UINT32: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_INT32: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_UINT64: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_INT64: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_SINGLE: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_DOUBLE: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_SCOMPLEX: {
            Cp[i] = complexRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_DCOMPLEX: {
            Cp[i] = complexRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        case NLS_CHAR: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, 0, i);
        } break;
        default: {
        } break;
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)Clen; i++) {
        switch (classA) {
        case NLS_STRING_ARRAY: {
            Cp[i] = stringRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_LOGICAL: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_UINT8: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_INT8: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_UINT16: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_INT16: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_UINT32: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_INT32: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_UINT64: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_INT64: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_SINGLE: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_DOUBLE: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_SCOMPLEX: {
            Cp[i] = complexRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_DCOMPLEX: {
            Cp[i] = complexRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        case NLS_CHAR: {
            Cp[i] = realRelationOperator(classA, ptrA, ptrB, i, 0);
        } break;
        default: {
        } break;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
vector_row_column_operator(const Dimensions& outputDimensions, ArrayOf& A, ArrayOf& B,
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
    indexType m = 0;
    for (indexType i = 0; i < nbElementsA; i++) {
        for (indexType j = 0; j < nbElementsB; j++) {
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, i, j);
            } break;
            default: {
            } break;
            }
            m++;
        }
    }
    return ArrayOf(NLS_LOGICAL, outputDimensions, Cp, false);
}
//=============================================================================
static ArrayOf
vector_column_row_operator(const Dimensions& outputDimensions, ArrayOf& A, ArrayOf& B,
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
    indexType m = 0;
    indexType elementCountA = A.getElementCount();
    indexType elementCountB = B.getElementCount();
    for (indexType i = 0; i < elementCountB; i++) {
        for (indexType j = 0; j < elementCountA; j++) {
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, i);
            } break;
            default: {
            } break;
            }
            m++;
        }
    }
    return ArrayOf(NLS_LOGICAL, outputDimensions, Cp, false);
}
//=============================================================================
static ArrayOf
vector_matrix_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    indexType q = 0;
    Dimensions dimsC = B.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * A.getRows();
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, q, m);
            } break;
            default: {
            } break;
            }
        }
        q++;
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_vector_operator(ArrayOf& A, ArrayOf& B,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* ptrA, void* ptrB, indexType idxA, indexType idxB))
{
    indexType q = 0;
    Dimensions dimsC = A.getDimensions();
    indexType Clen = dimsC.getElementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    NelsonType classA = A.getDataClass();
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * A.getRows();
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, q);
            } break;
            default: {
            } break;
            }
        }
        q++;
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
vector_column_matrix_operator(ArrayOf& A, ArrayOf& B,
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
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * A.getColumns();
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, j, m);
            } break;
            default: {
            } break;
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsC, Cp, false);
}
//=============================================================================
static ArrayOf
matrix_vector_column_operator(ArrayOf& A, ArrayOf& B,
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
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * B.getColumns();
            switch (classA) {
            case NLS_STRING_ARRAY: {
                Cp[m] = stringRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_LOGICAL: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_UINT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_INT8: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_UINT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_INT16: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_UINT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_INT32: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_UINT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_INT64: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_SINGLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_DOUBLE: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_SCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_DCOMPLEX: {
                Cp[m] = complexRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            case NLS_CHAR: {
                Cp[m] = realRelationOperator(classA, ptrA, ptrB, m, j);
            } break;
            default: {
            } break;
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
    ArrayOf _A = A;
    ArrayOf _B = B;
    needToOverload = false;
    if (_A.isSparse() || _B.isSparse()) {
        needToOverload = true;
        return {};
    }
    bool asStringArray = (_A.isStringArray() && _B.isStringArray())
        || (_A.isStringArray() && _B.isRowVectorCharacterArray())
        || (_B.isStringArray() && _A.isRowVectorCharacterArray());
    if ((_A.isReferenceType() || _B.isReferenceType()) && !asStringArray) {
        needToOverload = true;
        return {};
    }
    NelsonType classCommon = FindCommonType(_A, _B);
    if (asStringArray) {
        if (!_A.isStringArray()) {
            _A = ArrayOf::toStringArray(_A, needToOverload);
        }
        if (!_B.isStringArray()) {
            _B = ArrayOf::toStringArray(_B, needToOverload);
        }
    } else {
        _A.promoteType(classCommon);
        _B.promoteType(classCommon);
    }

    Dimensions dimsA = _A.getDimensions();
    Dimensions dimsB = _B.getDimensions();
    Dimensions dimsC;
    if (_A.isEmpty() || _B.isEmpty()) {
        if (_A.isScalar() || _B.isScalar()) {
            if (_A.isScalar()) {
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
            _A, _B, realRelationOperator, complexRelationOperator, stringRelationOperator);
    }
    if (_A.isScalar() || _B.isScalar()) {
        if (_A.isScalar()) {
            return scalar_matrix_operator(
                _A, _B, realRelationOperator, complexRelationOperator, stringRelationOperator);
        }
        return matrix_scalar_operator(
            _A, _B, realRelationOperator, complexRelationOperator, stringRelationOperator);
    }
    if (_A.isVector() || _B.isVector()) {
        if (_A.isRowVector() && _B.isColumnVector()) {
            dimsC = Dimensions(
                std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
            return vector_row_column_operator(dimsC, _A, _B, realRelationOperator,
                complexRelationOperator, stringRelationOperator);
        }
        if (_A.isColumnVector() && _B.isRowVector()) {
            dimsC = Dimensions(
                std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
            return vector_column_row_operator(dimsC, _A, _B, realRelationOperator,
                complexRelationOperator, stringRelationOperator);
        }
        if ((_A.isRowVector() && _B.isRowVector())
            || (_A.isColumnVector() && _B.isColumnVector())) {
            Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + operatorName);
        } else {
            if (dimsA[1] == dimsB[1]) {
                if (A.isVector()) {
                    return vector_column_matrix_operator(_A, _B, realRelationOperator,
                        complexRelationOperator, stringRelationOperator);
                }
                return matrix_vector_column_operator(
                    _A, _B, realRelationOperator, complexRelationOperator, stringRelationOperator);
            }
            if ((dimsA[0] == dimsB[0]) && (dimsA[0] != 1)) {
                if (_A.isVector()) {
                    return vector_matrix_operator(_A, _B, realRelationOperator,
                        complexRelationOperator, stringRelationOperator);
                }
                return matrix_vector_operator(
                    _A, _B, realRelationOperator, complexRelationOperator, stringRelationOperator);
            }
            Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + operatorName);
        }
    } else {
        Error(_W("Size mismatch on arguments to arithmetic operator") + L" " + operatorName);
    }
    needToOverload = true;
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
