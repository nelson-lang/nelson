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
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
TIntegerCumprod(const T* sp, T* dp, T tMin, T tMax, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool reverse)
{
    double accum = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(* : accum)
#endif
    for (ompIndexType i = 0; i < planes; i++) {
        for (ompIndexType j = 0; j < planesize; j++) {
            accum = 1;
            if (reverse) {
                for (ompIndexType k = linesize - 1; k >= 0; --k) {
                    accum *= (double)sp[i * planesize * linesize + j + k * planesize];
                    if (accum < (double)tMin) {
                        dp[i * planesize * linesize + j + k * planesize] = tMin;
                    } else if (accum > (double)tMax) {
                        dp[i * planesize * linesize + j + k * planesize] = tMax;
                    } else {
                        dp[i * planesize * linesize + j + k * planesize] = (T)accum;
                    }
                }

            } else {
                for (ompIndexType k = 0; k < linesize; k++) {
                    accum *= sp[i * planesize * linesize + j + k * planesize];
                    if (accum < (double)tMin) {
                        dp[i * planesize * linesize + j + k * planesize] = tMin;
                    } else if (accum > (double)tMax) {
                        dp[i * planesize * linesize + j + k * planesize] = tMax;
                    } else {
                        dp[i * planesize * linesize + j + k * planesize] = (T)accum;
                    }
                }
            }
        }
    }
}
//=============================================================================
template <class T>
void
TRealCumprod(const T* sp, T* dp, ompIndexType planes, ompIndexType planesize, ompIndexType linesize,
    bool withNaN, bool reverse)
{
    T accum = 1;
    if (withNaN) {
#if WITH_OPENMP
#pragma omp parallel for reduction(* : accum)
#endif
        for (ompIndexType i = 0; i < planes; i++) {
            for (ompIndexType j = 0; j < planesize; j++) {
                accum = 1;
                if (reverse) {
                    for (ompIndexType k = linesize - 1; k >= 0; --k) {
                        accum *= sp[i * planesize * linesize + j + k * planesize];
                        dp[i * planesize * linesize + j + k * planesize] = accum;
                    }

                } else {
                    for (ompIndexType k = 0; k < linesize; k++) {
                        accum *= sp[i * planesize * linesize + j + k * planesize];
                        dp[i * planesize * linesize + j + k * planesize] = accum;
                    }
                }
            }
        }
    } else {
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : accum)
#endif
        for (ompIndexType i = 0; i < planes; i++) {
            for (ompIndexType j = 0; j < planesize; j++) {
                accum = 1;
                if (reverse) {
                    for (ompIndexType k = linesize - 1; k >= 0; --k) {
                        T val = sp[i * planesize * linesize + j + k * planesize];
                        if (!std::isnan(val)) {
                            accum *= sp[i * planesize * linesize + j + k * planesize];
                        } else {
                            accum *= 1;
                        }
                        dp[i * planesize * linesize + j + k * planesize] = accum;
                    }

                } else {
                    for (ompIndexType k = 0; k < linesize; k++) {
                        T val = sp[i * planesize * linesize + j + k * planesize];
                        if (!std::isnan(val)) {
                            accum *= sp[i * planesize * linesize + j + k * planesize];
                        } else {
                            accum *= 1;
                        }
                        dp[i * planesize * linesize + j + k * planesize] = accum;
                    }
                }
            }
        }
    }
}
//=============================================================================
template <class T>
void
TComplexCumprod(const T* sp, T* dp, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool withNaN, bool reverse)
{
    T accum_r = 0;
    T accum_i = 0;

    if (withNaN) {
#if WITH_OPENMP
#pragma omp parallel for reduction(* : accum_r, accum_i)
#endif
        for (ompIndexType i = 0; i < planes; i++) {
            for (ompIndexType j = 0; j < planesize; j++) {
                accum_r = 1;
                accum_i = 0;
                if (reverse) {
                    for (ompIndexType k = linesize - 1; k >= 0; --k) {
                        T el_r = sp[2 * (i * planesize * linesize + j + k * planesize)];
                        T el_i = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                        T tmp_r = accum_r * el_r - accum_i * el_i;
                        T tmp_i = accum_r * el_i + accum_i * el_r;
                        dp[2 * (i * planesize * linesize + j + k * planesize)] = tmp_r;
                        dp[2 * (i * planesize * linesize + j + k * planesize) + 1] = tmp_i;
                        accum_r = tmp_r;
                        accum_i = tmp_i;
                    }

                } else {
                    for (ompIndexType k = 0; k < linesize; k++) {
                        T el_r = sp[2 * (i * planesize * linesize + j + k * planesize)];
                        T el_i = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                        T tmp_r = accum_r * el_r - accum_i * el_i;
                        T tmp_i = accum_r * el_i + accum_i * el_r;
                        dp[2 * (i * planesize * linesize + j + k * planesize)] = tmp_r;
                        dp[2 * (i * planesize * linesize + j + k * planesize) + 1] = tmp_i;
                        accum_r = tmp_r;
                        accum_i = tmp_i;
                    }
                }
            }
        }
    } else {
#if WITH_OPENMP
#pragma omp parallel for reduction(* : accum_r, accum_i)
#endif
        for (ompIndexType i = 0; i < planes; i++) {
            for (ompIndexType j = 0; j < planesize; j++) {
                accum_r = 1;
                accum_i = 0;
                if (reverse) {
                    for (ompIndexType k = linesize - 1; k >= 0; --k) {
                        T el_r = sp[2 * (i * planesize * linesize + j + k * planesize)];
                        if (std::isnan(el_r)) {
                            el_r = 1;
                        }
                        T el_i = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                        if (std::isnan(el_i)) {
                            el_i = 0;
                        }
                        T tmp_r = accum_r * el_r - accum_i * el_i;
                        T tmp_i = accum_r * el_i + accum_i * el_r;
                        dp[2 * (i * planesize * linesize + j + k * planesize)] = tmp_r;
                        dp[2 * (i * planesize * linesize + j + k * planesize) + 1] = tmp_i;
                        accum_r = tmp_r;
                        accum_i = tmp_i;
                    }
                } else {
                    for (ompIndexType k = 0; k < linesize; k++) {
                        T el_r = sp[2 * (i * planesize * linesize + j + k * planesize)];
                        if (std::isnan(el_r)) {
                            el_r = 1;
                        }
                        T el_i = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                        if (std::isnan(el_i)) {
                            el_i = 0;
                        }
                        T tmp_r = accum_r * el_r - accum_i * el_i;
                        T tmp_i = accum_r * el_i + accum_i * el_r;
                        dp[2 * (i * planesize * linesize + j + k * planesize)] = tmp_r;
                        dp[2 * (i * planesize * linesize + j + k * planesize) + 1] = tmp_i;
                        accum_r = tmp_r;
                        accum_i = tmp_i;
                    }
                }
            }
        }
    }
}
//=============================================================================
static inline void
LogicalCumprod(const logical* sp, double* dp, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool reverse)
{
    double accum = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : accum)
#endif
    for (ompIndexType i = 0; i < planes; i++) {
        for (ompIndexType j = 0; j < planesize; j++) {
            accum = 1;
            if (reverse) {
                for (ompIndexType k = linesize - 1; k >= 0; --k) {
                    accum *= sp[i * planesize * linesize + j + k * planesize] ? 1. : 0.;
                    dp[i * planesize * linesize + j + k * planesize] = accum;
                }
            } else {
                for (ompIndexType k = 0; k < linesize; k++) {
                    accum *= sp[i * planesize * linesize + j + k * planesize] ? 1. : 0.;
                    dp[i * planesize * linesize + j + k * planesize] = accum;
                }
            }
        }
    }
}
//=============================================================================
ArrayOf
CumProd(const ArrayOf& A, indexType n, bool withNaN, bool reverse, bool& needOverload)
{
    ArrayOf res;
    needOverload = true;
    if (A.isSparse()) {
        return {};
    }
    Dimensions dimsA = A.getDimensions();
    indexType workDim;
    if (n == 0) {
        indexType l = 0;
        while (dimsA.getAt(l, false) == 1) {
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
    for (ompIndexType l = workDim + 1; l < (ompIndexType)dimsA.getLength(); l++) {
        planecount *= (ompIndexType)dimsA[l];
    }
    switch (A.getDataClass()) {
    case NLS_SCOMPLEX: {
        single* ptrCplxSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, dimsRes.getElementCount());
        res = ArrayOf(NLS_SCOMPLEX, dimsRes, ptrCplxSingle);
        const single* ptrIn = (const single*)A.getDataPointer();
        TComplexCumprod<single>(
            ptrIn, ptrCplxSingle, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_DCOMPLEX: {
        double* ptrCplxDouble
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimsRes.getElementCount());
        res = ArrayOf(NLS_DCOMPLEX, dimsRes, ptrCplxDouble);
        const double* ptrIn = (const double*)A.getDataPointer();
        TComplexCumprod<double>(
            ptrIn, ptrCplxDouble, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_INT8: {
        int8* ptrInt8 = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, dimsRes.getElementCount());
        res = ArrayOf(NLS_INT8, dimsRes, ptrInt8);
        const int8* ptrIn = (const int8*)A.getDataPointer();
        TIntegerCumprod<int8>(ptrIn, ptrInt8, std::numeric_limits<int8>::min(),
            std::numeric_limits<int8>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT16: {
        int16* ptrInt16 = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, dimsRes.getElementCount());
        res = ArrayOf(NLS_INT16, dimsRes, ptrInt16);
        const int16* ptrIn = (const int16*)A.getDataPointer();
        TIntegerCumprod<int16>(ptrIn, ptrInt16, std::numeric_limits<int16>::min(),
            std::numeric_limits<int16>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT32: {
        int32* ptrInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, dimsRes.getElementCount());
        res = ArrayOf(NLS_INT32, dimsRes, ptrInt32);
        const int32* ptrIn = (const int32*)A.getDataPointer();
        TIntegerCumprod<int32>(ptrIn, ptrInt32, std::numeric_limits<int32>::min(),
            std::numeric_limits<int32>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT64: {
        int64* ptrInt64 = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, dimsRes.getElementCount());
        res = ArrayOf(NLS_INT64, dimsRes, ptrInt64);
        const int64* ptrIn = (const int64*)A.getDataPointer();
        TIntegerCumprod<int64>(ptrIn, ptrInt64, std::numeric_limits<int64>::min(),
            std::numeric_limits<int64>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT8: {
        uint8* ptrUInt8 = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dimsRes.getElementCount());
        res = ArrayOf(NLS_UINT8, dimsRes, ptrUInt8);
        const uint8* ptrIn = (const uint8*)A.getDataPointer();
        TIntegerCumprod<uint8>(ptrIn, ptrUInt8, std::numeric_limits<uint8>::min(),
            std::numeric_limits<uint8>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT16: {
        uint16* ptrUInt16
            = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, dimsRes.getElementCount());
        res = ArrayOf(NLS_UINT16, dimsRes, ptrUInt16);
        const uint16* ptrIn = (const uint16*)A.getDataPointer();
        TIntegerCumprod<uint16>(ptrIn, ptrUInt16, std::numeric_limits<uint16>::min(),
            std::numeric_limits<uint16>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT32: {
        uint32* ptrUInt32
            = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, dimsRes.getElementCount());
        res = ArrayOf(NLS_UINT32, dimsRes, ptrUInt32);
        const uint32* ptrIn = (const uint32*)A.getDataPointer();
        TIntegerCumprod<uint32>(ptrIn, ptrUInt32, std::numeric_limits<uint32>::min(),
            std::numeric_limits<uint32>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT64: {
        uint64* ptrUInt64
            = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, dimsRes.getElementCount());
        res = ArrayOf(NLS_UINT64, dimsRes, ptrUInt64);
        const uint64* ptrIn = (const uint64*)A.getDataPointer();
        TIntegerCumprod<uint64>(ptrIn, ptrUInt64, std::numeric_limits<uint64>::min(),
            std::numeric_limits<uint64>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_LOGICAL: {
        double* ptrDouble
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptrDouble);
        const logical* ptrIn = (const logical*)A.getDataPointer();
        LogicalCumprod(ptrIn, ptrDouble, planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_DOUBLE: {
        double* ptrDouble
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptrDouble);
        const double* ptrIn = (const double*)A.getDataPointer();
        TRealCumprod<double>(ptrIn, ptrDouble, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_SINGLE: {
        single* ptrSingle
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dimsRes.getElementCount());
        res = ArrayOf(NLS_SINGLE, dimsRes, ptrSingle);
        const single* ptrIn = (const single*)A.getDataPointer();
        TRealCumprod<single>(ptrIn, ptrSingle, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson5
//=============================================================================
