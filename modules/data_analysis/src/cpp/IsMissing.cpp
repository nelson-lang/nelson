//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "omp_for_loop.hpp"
#include "IsMissing.hpp"
#include "Error.hpp"
#include "NelsonException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
isMissing(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    Dimensions dimsA = A.dimensions();
    logical* resultAslogical = nullptr;

    switch (A.dataClass()) {
    case NLS_MISSING_ARRAY: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
        ompIndexType elementCount = dimsA.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = true;
        }
    } break;
    case NLS_STRING_ARRAY: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
        auto* elements = (ArrayOf*)A.dataPointer();
        ompIndexType elementCount = dimsA.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(!elements[k].isCharacterArray());
        }
    } break;
    case NLS_CELL_ARRAY: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
        auto* elements = (ArrayOf*)A.dataPointer();
        ompIndexType elementCount = dimsA.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(
                elements[k].isCharacterArray() && elements[k].isEmpty());
        }
    } break;
    case NLS_DOUBLE: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
        auto* ptrA = (double*)A.dataPointer();
        ompIndexType elementCount = dimsA.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(std::isnan(ptrA[k]));
        }
    } break;
    case NLS_SINGLE: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
        auto* ptrA = (single*)A.dataPointer();
        ompIndexType elementCount = dimsA.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(std::isnan(ptrA[k]));
        }
    } break;
    case NLS_SCOMPLEX: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
        auto* ptrA = (single*)A.dataPointer();
        ompIndexType elementCount = dimsA.elementCount() * 2;
        ompIndexType q = 0;
        for (ompIndexType k = 0; k < elementCount; k = k + 2) {
            resultAslogical[q]
                = static_cast<Nelson::logical>(std::isnan(ptrA[k]) || std::isnan(ptrA[k + 1]));
            q++;
        }
    } break;
    case NLS_DCOMPLEX: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
        auto* ptrA = (double*)A.dataPointer();
        ompIndexType elementCount = dimsA.elementCount() * 2;
        ompIndexType q = 0;
        for (ompIndexType k = 0; k < elementCount; k = k + 2) {
            resultAslogical[q]
                = static_cast<Nelson::logical>(std::isnan(ptrA[k]) || std::isnan(ptrA[k + 1]));
            q++;
        }
    } break;
    case NLS_CHAR: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
        auto* ptrA = (charType*)A.dataPointer();
        ompIndexType elementCount = dimsA.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
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
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), true));
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
