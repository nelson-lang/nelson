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
#include "nlsConfig.h"
#include "AbsoluteValue.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
absoluteValueRealTemplate(T* ptrA, indexType N, T* ptrRes)
{
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        ptrRes[i] = std::abs(ptrA[i]);
    }
}
//=============================================================================
template <class T>
void
absoluteValueComplexTemplate(T* ptrA, indexType N, T* ptrRes)
{
    auto* matCplxA = reinterpret_cast<std::complex<T>*>(ptrA);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        ptrRes[i] = std::abs(matCplxA[i]);
    }
}
//=============================================================================
ArrayOf
AbsoluteValue(const ArrayOf& arrayIn, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (arrayIn.isSparse()) {
        needToOverload = true;
        return ArrayOf();
    }
    switch (arrayIn.getDataClass()) {
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    default: {
        needToOverload = true;
        return ArrayOf();
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        res = arrayIn;
        res.ensureSingleOwner();
    } break;
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = arrayIn;
        res.ensureSingleOwner();
        res.promoteType(NLS_DOUBLE);
    } break;
    case NLS_INT8: {
        Dimensions dimsRes = arrayIn.getDimensions();
        int8* ptrRes
            = (int8*)ArrayOf::allocateArrayOf(arrayIn.getDataClass(), dimsRes.getElementCount());
        res = ArrayOf(arrayIn.getDataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<int8>(
            (int8*)arrayIn.getDataPointer(), dimsRes.getElementCount(), ptrRes);
    } break;
    case NLS_INT16: {
        Dimensions dimsRes = arrayIn.getDimensions();
        int16* ptrRes
            = (int16*)ArrayOf::allocateArrayOf(arrayIn.getDataClass(), dimsRes.getElementCount());
        res = ArrayOf(arrayIn.getDataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<int16>(
            (int16*)arrayIn.getDataPointer(), dimsRes.getElementCount(), ptrRes);
    } break;
    case NLS_INT32: {
        Dimensions dimsRes = arrayIn.getDimensions();
        int32* ptrRes
            = (int32*)ArrayOf::allocateArrayOf(arrayIn.getDataClass(), dimsRes.getElementCount());
        res = ArrayOf(arrayIn.getDataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<int32>(
            (int32*)arrayIn.getDataPointer(), dimsRes.getElementCount(), ptrRes);
    } break;
    case NLS_INT64: {
        Dimensions dimsRes = arrayIn.getDimensions();
        int64* ptrRes
            = (int64*)ArrayOf::allocateArrayOf(arrayIn.getDataClass(), dimsRes.getElementCount());
        res = ArrayOf(arrayIn.getDataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<int64>(
            (int64*)arrayIn.getDataPointer(), dimsRes.getElementCount(), ptrRes);
    } break;
    case NLS_SINGLE: {
        Dimensions dimsRes = arrayIn.getDimensions();
        single* ptrRes
            = (single*)ArrayOf::allocateArrayOf(arrayIn.getDataClass(), dimsRes.getElementCount());
        res = ArrayOf(arrayIn.getDataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<single>(
            (single*)arrayIn.getDataPointer(), dimsRes.getElementCount(), ptrRes);
    } break;
    case NLS_DOUBLE: {
        Dimensions dimsRes = arrayIn.getDimensions();
        double* ptrRes
            = (double*)ArrayOf::allocateArrayOf(arrayIn.getDataClass(), dimsRes.getElementCount());
        res = ArrayOf(arrayIn.getDataClass(), dimsRes, ptrRes);
        absoluteValueRealTemplate<double>(
            (double*)arrayIn.getDataPointer(), dimsRes.getElementCount(), ptrRes);
    } break;
    case NLS_DCOMPLEX: {
        Dimensions dimsRes = arrayIn.getDimensions();
        double* ptrRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptrRes);
        absoluteValueComplexTemplate<double>(
            (double*)arrayIn.getDataPointer(), dimsRes.getElementCount(), ptrRes);
    } break;
    case NLS_SCOMPLEX: {
        Dimensions dimsRes = arrayIn.getDimensions();
        single* ptrRes = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dimsRes.getElementCount());
        res = ArrayOf(NLS_SINGLE, dimsRes, ptrRes);
        absoluteValueComplexTemplate<single>(
            (single*)arrayIn.getDataPointer(), dimsRes.getElementCount(), ptrRes);
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
