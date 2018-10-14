//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "GreaterThan.hpp"
#include "RelationOperator.hpp"
#include "complex_abs.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
logical
realComparatorGreaterThan(
    Class commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB)
{
    switch (commonClass) {
    case NLS_LOGICAL: {
        logical* ptrA = (logical*)vptrA;
        logical* ptrB = (logical*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_UINT8: {
        uint8* ptrA = (uint8*)vptrA;
        uint8* ptrB = (uint8*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_INT8: {
        int8* ptrA = (int8*)vptrA;
        int8* ptrB = (int8*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_UINT16: {
        uint16* ptrA = (uint16*)vptrA;
        uint16* ptrB = (uint16*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_INT16: {
        int16* ptrA = (int16*)vptrA;
        int16* ptrB = (int16*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_UINT32: {
        uint32* ptrA = (uint32*)vptrA;
        uint32* ptrB = (uint32*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_INT32: {
        int32* ptrA = (int32*)vptrA;
        int32* ptrB = (int32*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_UINT64: {
        uint64* ptrA = (uint64*)vptrA;
        uint64* ptrB = (uint64*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_INT64: {
        int64* ptrA = (int64*)vptrA;
        int64* ptrB = (int64*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_SINGLE: {
        single* ptrA = (single*)vptrA;
        single* ptrB = (single*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_DOUBLE: {
        double* ptrA = (double*)vptrA;
        double* ptrB = (double*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    case NLS_CHAR: {
        charType* ptrA = (charType*)vptrA;
        charType* ptrB = (charType*)vptrB;
        return (ptrA[idxA] > ptrB[idxB]);
    } break;
    }
    return false;
}
//=============================================================================
logical
complexComparatorGreaterThan(
    Class commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB)
{
    switch (commonClass) {
    case NLS_SCOMPLEX: {
        single* ptrA = (single*)vptrA;
        single* ptrB = (single*)vptrB;
        return complex_abs<single>(ptrA[2 * idxA], ptrA[2 * idxA + 1])
            > complex_abs<single>(ptrB[2 * idxB], ptrB[2 * idxB + 1]);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrA = (double*)vptrA;
        double* ptrB = (double*)vptrB;
        return complex_abs<double>(ptrA[2 * idxA], ptrA[2 * idxA + 1])
            > complex_abs<double>(ptrB[2 * idxB], ptrB[2 * idxB + 1]);
    } break;
    }
    return false;
}
//=============================================================================
logical
stringArrayComparatorGreaterThan(
    Class commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB)
{
    if (commonClass == NLS_STRING_ARRAY) {
        ArrayOf* ptrA = (ArrayOf*)vptrA;
        ArrayOf* ptrB = (ArrayOf*)vptrB;
        if (ptrA[idxA].isCharacterArray() && ptrB[idxB].isCharacterArray()) {
            return ptrA[idxA].getContentAsWideString() > ptrB[idxB].getContentAsWideString();
        }
    }
    return false;
}
//=============================================================================
ArrayOf
GreaterThan(ArrayOf& A, ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    return relationOperator(A, B, L">", &realComparatorGreaterThan, &complexComparatorGreaterThan,
        &stringArrayComparatorGreaterThan, needToOverload);
}
//=============================================================================
}
//=============================================================================
