//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsMissing.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
isMissing(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    Dimensions dimsA = A.getDimensions();
    logical* resultAslogical = nullptr;

    switch (A.getDataClass()) {
    case NLS_STRING_ARRAY: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* elements = (ArrayOf*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(!elements[k].isCharacterArray());
        }
    } break;
    case NLS_CELL_ARRAY: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* elements = (ArrayOf*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(
                elements[k].isCharacterArray() && elements[k].isEmpty());
        }
    } break;
    case NLS_DOUBLE: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (double*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(std::isnan(ptrA[k]));
        }
    } break;
    case NLS_SINGLE: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (single*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(std::isnan(ptrA[k]));
        }
    } break;
    case NLS_SCOMPLEX: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (single*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount() * 2;
        ompIndexType q = 0;
        for (ompIndexType k = 0; k < elementCount; k = k + 2) {
            resultAslogical[q]
                = static_cast<Nelson::logical>(std::isnan(ptrA[k]) || std::isnan(ptrA[k + 1]));
            q++;
        }
    } break;
    case NLS_DCOMPLEX: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (double*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount() * 2;
        ompIndexType q = 0;
        for (ompIndexType k = 0; k < elementCount; k = k + 2) {
            resultAslogical[q]
                = static_cast<Nelson::logical>(std::isnan(ptrA[k]) || std::isnan(ptrA[k + 1]));
            q++;
        }
    } break;
    case NLS_CHAR: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (charType*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(ptrA[k] == L' ');
        }
    } break;
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), true));
    } break;
    default: {
        needToOverload = true;
        return {};
    } break;
    }
    return ArrayOf(NLS_LOGICAL, dimsA, resultAslogical);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
