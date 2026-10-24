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
#include "CumProd.hpp"
#include "CumulativeLines.hpp"
#include "IntegerOperations.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
TIntegerCumprod(const T* sp, T* dp, T tMin, T tMax, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool reverse)
{
    // Accumulate in the narrow integer type with exact per-step saturation. A double
    // accumulator loses precision above 2^53: (double)INT64_MAX rounds up to 2^63 and
    // the cast back overflows.
    (void)tMin;
    (void)tMax;
    forEachCumulativeLine(
        planes, planesize, linesize, reverse, [&](ompIndexType idx, ompIndexType step) {
            T accum = (T)1;
            for (ompIndexType k = 0; k < linesize; k++, idx += step) {
                accum = scalar_scalar_integer_times<T>(accum, sp[idx]);
                dp[idx] = accum;
            }
        });
}
//=============================================================================
template <class T>
void
TRealCumprod(const T* sp, T* dp, ompIndexType planes, ompIndexType planesize, ompIndexType linesize,
    bool withNaN, bool reverse)
{
    forEachCumulativeLine(
        planes, planesize, linesize, reverse, [&](ompIndexType idx, ompIndexType step) {
            T accum = 1;
            for (ompIndexType k = 0; k < linesize; k++, idx += step) {
                const T value = sp[idx];
                if (withNaN || !std::isnan(value)) {
                    accum *= value;
                }
                dp[idx] = accum;
            }
        });
}
//=============================================================================
template <class T>
void
TComplexCumprod(const T* sp, T* dp, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool withNaN, bool reverse)
{
    forEachCumulativeLine(
        planes, planesize, linesize, reverse, [&](ompIndexType idx, ompIndexType step) {
            T accum_r = 1;
            T accum_i = 0;
            for (ompIndexType k = 0; k < linesize; k++, idx += step) {
                T el_r = sp[2 * idx];
                T el_i = sp[2 * idx + 1];
                if (!withNaN) {
                    if (std::isnan(el_r)) {
                        el_r = 1;
                    }
                    if (std::isnan(el_i)) {
                        el_i = 0;
                    }
                }
                const T tmp_r = accum_r * el_r - accum_i * el_i;
                const T tmp_i = accum_r * el_i + accum_i * el_r;
                dp[2 * idx] = tmp_r;
                dp[2 * idx + 1] = tmp_i;
                accum_r = tmp_r;
                accum_i = tmp_i;
            }
        });
}
//=============================================================================
static inline void
LogicalCumprod(const logical* sp, double* dp, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool reverse)
{
    forEachCumulativeLine(
        planes, planesize, linesize, reverse, [&](ompIndexType idx, ompIndexType step) {
            double accum = 1;
            for (ompIndexType k = 0; k < linesize; k++, idx += step) {
                accum *= sp[idx] ? 1. : 0.;
                dp[idx] = accum;
            }
        });
}
//=============================================================================
ArrayOf
CumProd(const ArrayOf& A, indexType n, bool withNaN, bool reverse, bool& needOverload)
{
    ArrayOf res;
    needOverload = true;
    if (A.isComplex() && A.isEmpty() && !A.isSparse()) {
        // the running result of no elements is an empty real array
        needOverload = false;
        return decomplexify(A);
    }
    if (A.isSparse()) {
        return {};
    }
    Dimensions dimsA = A.dimensions();
    indexType workDim;
    if (n == 0) {
        indexType l = 0;
        while (dimsA.at(l, false) == 1) {
            l++;
        }
        workDim = l;
    } else {
        workDim = n - 1;
    }
    Dimensions dimsRes = dimsA;
    ompIndexType planecount;
    ompIndexType planesize = 1;
    ompIndexType linesize = dimsA[workDim];
    for (indexType l = 0; l < workDim; l++) {
        planesize *= (ompIndexType)dimsA[l];
    }
    planecount = 1;
    for (ompIndexType l = workDim + 1; l < (ompIndexType)dimsA.length(); l++) {
        planecount *= (ompIndexType)dimsA[l];
    }
    switch (A.dataClass()) {
    case NLS_SCOMPLEX: {
        single* ptrCplxSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, dimsRes.elementCount());
        res = ArrayOf(NLS_SCOMPLEX, dimsRes, ptrCplxSingle);
        const single* ptrIn = (const single*)A.dataPointer();
        TComplexCumprod<single>(
            ptrIn, ptrCplxSingle, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_DCOMPLEX: {
        double* ptrCplxDouble
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimsRes.elementCount());
        res = ArrayOf(NLS_DCOMPLEX, dimsRes, ptrCplxDouble);
        const double* ptrIn = (const double*)A.dataPointer();
        TComplexCumprod<double>(
            ptrIn, ptrCplxDouble, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_INT8: {
        int8* ptrInt8 = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, dimsRes.elementCount());
        res = ArrayOf(NLS_INT8, dimsRes, ptrInt8);
        const int8* ptrIn = (const int8*)A.dataPointer();
        TIntegerCumprod<int8>(ptrIn, ptrInt8, std::numeric_limits<int8>::min(),
            std::numeric_limits<int8>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT16: {
        int16* ptrInt16 = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, dimsRes.elementCount());
        res = ArrayOf(NLS_INT16, dimsRes, ptrInt16);
        const int16* ptrIn = (const int16*)A.dataPointer();
        TIntegerCumprod<int16>(ptrIn, ptrInt16, std::numeric_limits<int16>::min(),
            std::numeric_limits<int16>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT32: {
        int32* ptrInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, dimsRes.elementCount());
        res = ArrayOf(NLS_INT32, dimsRes, ptrInt32);
        const int32* ptrIn = (const int32*)A.dataPointer();
        TIntegerCumprod<int32>(ptrIn, ptrInt32, std::numeric_limits<int32>::min(),
            std::numeric_limits<int32>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT64: {
        int64* ptrInt64 = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, dimsRes.elementCount());
        res = ArrayOf(NLS_INT64, dimsRes, ptrInt64);
        const int64* ptrIn = (const int64*)A.dataPointer();
        TIntegerCumprod<int64>(ptrIn, ptrInt64, std::numeric_limits<int64>::min(),
            std::numeric_limits<int64>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT8: {
        uint8* ptrUInt8 = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dimsRes.elementCount());
        res = ArrayOf(NLS_UINT8, dimsRes, ptrUInt8);
        const uint8* ptrIn = (const uint8*)A.dataPointer();
        TIntegerCumprod<uint8>(ptrIn, ptrUInt8, std::numeric_limits<uint8>::min(),
            std::numeric_limits<uint8>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT16: {
        uint16* ptrUInt16 = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, dimsRes.elementCount());
        res = ArrayOf(NLS_UINT16, dimsRes, ptrUInt16);
        const uint16* ptrIn = (const uint16*)A.dataPointer();
        TIntegerCumprod<uint16>(ptrIn, ptrUInt16, std::numeric_limits<uint16>::min(),
            std::numeric_limits<uint16>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT32: {
        uint32* ptrUInt32 = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, dimsRes.elementCount());
        res = ArrayOf(NLS_UINT32, dimsRes, ptrUInt32);
        const uint32* ptrIn = (const uint32*)A.dataPointer();
        TIntegerCumprod<uint32>(ptrIn, ptrUInt32, std::numeric_limits<uint32>::min(),
            std::numeric_limits<uint32>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT64: {
        uint64* ptrUInt64 = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, dimsRes.elementCount());
        res = ArrayOf(NLS_UINT64, dimsRes, ptrUInt64);
        const uint64* ptrIn = (const uint64*)A.dataPointer();
        TIntegerCumprod<uint64>(ptrIn, ptrUInt64, std::numeric_limits<uint64>::min(),
            std::numeric_limits<uint64>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_LOGICAL: {
        double* ptrDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptrDouble);
        const logical* ptrIn = (const logical*)A.dataPointer();
        LogicalCumprod(ptrIn, ptrDouble, planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_DOUBLE: {
        double* ptrDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptrDouble);
        const double* ptrIn = (const double*)A.dataPointer();
        TRealCumprod<double>(ptrIn, ptrDouble, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_SINGLE: {
        single* ptrSingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dimsRes.elementCount());
        res = ArrayOf(NLS_SINGLE, dimsRes, ptrSingle);
        const single* ptrIn = (const single*)A.dataPointer();
        TRealCumprod<single>(ptrIn, ptrSingle, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    default:
        break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson5
//=============================================================================
