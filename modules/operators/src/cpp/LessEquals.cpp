//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "LessEquals.hpp"
#include "RelationOperator.hpp"
#include "complex_abs.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
logical
realComparatorLessEquals(
    NelsonType commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB)
{
    switch (commonClass) {
    case NLS_LOGICAL: {
        auto* ptrA = static_cast<logical*>(vptrA);
        auto* ptrB = static_cast<logical*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_UINT8: {
        auto* ptrA = static_cast<uint8*>(vptrA);
        auto* ptrB = static_cast<uint8*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_INT8: {
        int8* ptrA = static_cast<int8*>(vptrA);
        int8* ptrB = static_cast<int8*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_UINT16: {
        auto* ptrA = static_cast<uint16*>(vptrA);
        auto* ptrB = static_cast<uint16*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_INT16: {
        auto* ptrA = static_cast<int16*>(vptrA);
        auto* ptrB = static_cast<int16*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_UINT32: {
        auto* ptrA = static_cast<uint32*>(vptrA);
        auto* ptrB = static_cast<uint32*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_INT32: {
        auto* ptrA = static_cast<int32*>(vptrA);
        auto* ptrB = static_cast<int32*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_UINT64: {
        auto* ptrA = static_cast<uint64*>(vptrA);
        auto* ptrB = static_cast<uint64*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_INT64: {
        auto* ptrA = static_cast<int64*>(vptrA);
        auto* ptrB = static_cast<int64*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_SINGLE: {
        auto* ptrA = static_cast<single*>(vptrA);
        auto* ptrB = static_cast<single*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_DOUBLE: {
        auto* ptrA = static_cast<double*>(vptrA);
        auto* ptrB = static_cast<double*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    case NLS_CHAR: {
        auto* ptrA = static_cast<charType*>(vptrA);
        auto* ptrB = static_cast<charType*>(vptrB);
        return static_cast<Nelson::logical>(ptrA[idxA] <= ptrB[idxB]);
    } break;
    default: {
    } break;
    }
    return 0;
}
//=============================================================================
logical
complexComparatorLessEquals(
    NelsonType commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB)
{
    switch (commonClass) {
    case NLS_SCOMPLEX: {
        auto* ptrA = static_cast<single*>(vptrA);
        auto* ptrB = static_cast<single*>(vptrB);
        return static_cast<Nelson::logical>(complex_abs<single>(ptrA[2 * idxA], ptrA[2 * idxA + 1])
            <= complex_abs<single>(ptrB[2 * idxB], ptrB[2 * idxB + 1]));
    } break;
    case NLS_DCOMPLEX: {
        auto* ptrA = static_cast<double*>(vptrA);
        auto* ptrB = static_cast<double*>(vptrB);
        return static_cast<Nelson::logical>(complex_abs<double>(ptrA[2 * idxA], ptrA[2 * idxA + 1])
            <= complex_abs<double>(ptrB[2 * idxB], ptrB[2 * idxB + 1]));
    } break;
    default: {
    } break;
    }
    return 0;
}
//=============================================================================
logical
stringArrayComparatorLessEquals(
    NelsonType commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB)
{
    if (commonClass == NLS_STRING_ARRAY) {
        auto* ptrA = static_cast<ArrayOf*>(vptrA);
        auto* ptrB = static_cast<ArrayOf*>(vptrB);
        if (ptrA[idxA].isCharacterArray() && ptrB[idxB].isCharacterArray()) {
            return static_cast<Nelson::logical>(
                ptrA[idxA].getContentAsWideString() <= ptrB[idxB].getContentAsWideString());
        }
    }
    return 0;
}
//=============================================================================
ArrayOf
LessEquals(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    void* ptrA = const_cast<void*>(A.getDataPointer());
    void* ptrB = const_cast<void*>(B.getDataPointer());
    if (ptrA == ptrB) {
        Dimensions dimsA = A.getDimensions();
        logical* res = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        memset(res, 1, dimsA.getElementCount());
        return ArrayOf(NLS_LOGICAL, dimsA, res);
    }
    return relationOperator(A, B, L"<=", &realComparatorLessEquals, &complexComparatorLessEquals,
        &stringArrayComparatorLessEquals, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
