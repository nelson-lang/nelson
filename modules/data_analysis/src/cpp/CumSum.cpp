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
#include "CumSum.hpp"
#include "CumulativeLines.hpp"
#include "IntegerOperations.hpp"
#include "Decomplexify.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static bool
sparseCumSumValueIsNaN(const T& value)
{
    return std::isnan(value);
}
//=============================================================================
template <>
bool
sparseCumSumValueIsNaN<doublecomplex>(const doublecomplex& value)
{
    return std::isnan(value.real()) || std::isnan(value.imag());
}
//=============================================================================
template <class T>
static ArrayOf
SparseCumSumTemplate(
    NelsonType dataClass, const ArrayOf& A, indexType workDim, bool withNaN, bool reverse)
{
    indexType rows = A.rows();
    indexType cols = A.columns();
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;

    if (workDim == 0) {
        for (indexType col = 0; col < cols; ++col) {
            T accum = T(0);
            if (reverse) {
                for (indexType row = rows; row > 0; --row) {
                    T value = spmatSRC->coeff(row - 1, col);
                    if (withNaN || !sparseCumSumValueIsNaN<T>(value)) {
                        accum += value;
                    }
                    if (accum != T(0)) {
                        triplets.emplace_back(row - 1, col, accum);
                    }
                }
            } else {
                for (indexType row = 0; row < rows; ++row) {
                    T value = spmatSRC->coeff(row, col);
                    if (withNaN || !sparseCumSumValueIsNaN<T>(value)) {
                        accum += value;
                    }
                    if (accum != T(0)) {
                        triplets.emplace_back(row, col, accum);
                    }
                }
            }
        }
    } else {
        for (indexType row = 0; row < rows; ++row) {
            T accum = T(0);
            if (reverse) {
                for (indexType col = cols; col > 0; --col) {
                    T value = spmatSRC->coeff(row, col - 1);
                    if (withNaN || !sparseCumSumValueIsNaN<T>(value)) {
                        accum += value;
                    }
                    if (accum != T(0)) {
                        triplets.emplace_back(row, col - 1, accum);
                    }
                }
            } else {
                for (indexType col = 0; col < cols; ++col) {
                    T value = spmatSRC->coeff(row, col);
                    if (withNaN || !sparseCumSumValueIsNaN<T>(value)) {
                        accum += value;
                    }
                    if (accum != T(0)) {
                        triplets.emplace_back(row, col, accum);
                    }
                }
            }
        }
    }

    auto* spmatDST = new Eigen::SparseMatrix<T, 0, signedIndexType>(rows, cols);
    if (!triplets.empty()) {
        spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    }
    return makeSparseResult(dataClass, A.dimensions(), spmatDST);
}
//=============================================================================
static ArrayOf
SparseCumSum(ArrayOf A, indexType n, bool withNaN, bool reverse, bool& needOverload)
{
    needOverload = false;
    Dimensions dimsA = A.dimensions();
    indexType workDim = 0;
    if (n == 0) {
        workDim = dimsA.rows() == 1 ? 1 : 0;
    } else {
        workDim = n - 1;
    }
    if (workDim > 1) {
        Error(L"Nelson:data_analysis:sparseMatrixSupportsCumsumAlong",
            L"Sparse matrix supports %s along dimensions 1 or 2.", L"cumsum");
    }
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        A.promoteType(NLS_DOUBLE);
        return SparseCumSumTemplate<double>(NLS_DOUBLE, A, workDim, withNaN, reverse);
    case NLS_DOUBLE:
        return SparseCumSumTemplate<double>(NLS_DOUBLE, A, workDim, withNaN, reverse);
    case NLS_DCOMPLEX:
        return SparseCumSumTemplate<doublecomplex>(NLS_DCOMPLEX, A, workDim, withNaN, reverse);
    default:
        needOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
void
TIntegerCumsum(const T* sp, T* dp, T tMin, T tMax, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool reverse)
{
    // Accumulate in the narrow integer type with exact per-step saturation. A double
    // accumulator loses precision above 2^53: (double)INT64_MAX rounds up to 2^63 and
    // the cast back overflows.
    (void)tMin;
    (void)tMax;
    forEachCumulativeLine(
        planes, planesize, linesize, reverse, [&](ompIndexType idx, ompIndexType step) {
            T accum = (T)0;
            for (ompIndexType k = 0; k < linesize; k++, idx += step) {
                accum = scalar_scalar_integer_addition<T>(accum, sp[idx]);
                dp[idx] = accum;
            }
        });
}
//=============================================================================
template <class T>
void
TRealCumsum(const T* sp, T* dp, ompIndexType planes, ompIndexType planesize, ompIndexType linesize,
    bool withNaN, bool reverse)
{
    forEachCumulativeLine(
        planes, planesize, linesize, reverse, [&](ompIndexType idx, ompIndexType step) {
            T accum = 0;
            for (ompIndexType k = 0; k < linesize; k++, idx += step) {
                const T value = sp[idx];
                if (withNaN || !std::isnan(value)) {
                    accum += value;
                }
                dp[idx] = accum;
            }
        });
}
//=============================================================================
template <class T>
void
TComplexCumsum(const T* sp, T* dp, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool withNaN, bool reverse)
{
    forEachCumulativeLine(
        planes, planesize, linesize, reverse, [&](ompIndexType idx, ompIndexType step) {
            T accum_r = 0;
            T accum_i = 0;
            for (ompIndexType k = 0; k < linesize; k++, idx += step) {
                const T value_r = sp[2 * idx];
                const T value_i = sp[2 * idx + 1];
                if (withNaN || !std::isnan(value_r)) {
                    accum_r += value_r;
                }
                if (withNaN || !std::isnan(value_i)) {
                    accum_i += value_i;
                }
                dp[2 * idx] = accum_r;
                dp[2 * idx + 1] = accum_i;
            }
        });
}
//=============================================================================
static inline void
LogicalCumsum(const logical* sp, double* dp, ompIndexType planes, ompIndexType planesize,
    ompIndexType linesize, bool reverse)
{
    forEachCumulativeLine(
        planes, planesize, linesize, reverse, [&](ompIndexType idx, ompIndexType step) {
            double accum = 0;
            for (ompIndexType k = 0; k < linesize; k++, idx += step) {
                accum += sp[idx] ? 1. : 0.;
                dp[idx] = accum;
            }
        });
}
//=============================================================================
ArrayOf
CumSum(const ArrayOf& A, indexType n, bool withNaN, bool reverse, bool& needOverload)
{
    ArrayOf res;
    needOverload = true;
    if (A.isComplex() && A.isEmpty() && !A.isSparse()) {
        // the running result of no elements is an empty real array
        needOverload = false;
        return decomplexify(A);
    }
    if (A.isSparse()) {
        return SparseCumSum(A, n, withNaN, reverse, needOverload);
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
        TComplexCumsum<single>(
            ptrIn, ptrCplxSingle, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_DCOMPLEX: {
        double* ptrCplxDouble
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimsRes.elementCount());
        res = ArrayOf(NLS_DCOMPLEX, dimsRes, ptrCplxDouble);
        const double* ptrIn = (const double*)A.dataPointer();
        TComplexCumsum<double>(
            ptrIn, ptrCplxDouble, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_INT8: {
        int8* ptrInt8 = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, dimsRes.elementCount());
        res = ArrayOf(NLS_INT8, dimsRes, ptrInt8);
        const int8* ptrIn = (const int8*)A.dataPointer();
        TIntegerCumsum<int8>(ptrIn, ptrInt8, std::numeric_limits<int8>::min(),
            std::numeric_limits<int8>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT16: {
        int16* ptrInt16 = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, dimsRes.elementCount());
        res = ArrayOf(NLS_INT16, dimsRes, ptrInt16);
        const int16* ptrIn = (const int16*)A.dataPointer();
        TIntegerCumsum<int16>(ptrIn, ptrInt16, std::numeric_limits<int16>::min(),
            std::numeric_limits<int16>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT32: {
        int32* ptrInt32 = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, dimsRes.elementCount());
        res = ArrayOf(NLS_INT32, dimsRes, ptrInt32);
        const int32* ptrIn = (const int32*)A.dataPointer();
        TIntegerCumsum<int32>(ptrIn, ptrInt32, std::numeric_limits<int32>::min(),
            std::numeric_limits<int32>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_INT64: {
        int64* ptrInt64 = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, dimsRes.elementCount());
        res = ArrayOf(NLS_INT64, dimsRes, ptrInt64);
        const int64* ptrIn = (const int64*)A.dataPointer();
        TIntegerCumsum<int64>(ptrIn, ptrInt64, std::numeric_limits<int64>::min(),
            std::numeric_limits<int64>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT8: {
        uint8* ptrUInt8 = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dimsRes.elementCount());
        res = ArrayOf(NLS_UINT8, dimsRes, ptrUInt8);
        const uint8* ptrIn = (const uint8*)A.dataPointer();
        TIntegerCumsum<uint8>(ptrIn, ptrUInt8, std::numeric_limits<uint8>::min(),
            std::numeric_limits<uint8>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT16: {
        uint16* ptrUInt16 = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, dimsRes.elementCount());
        res = ArrayOf(NLS_UINT16, dimsRes, ptrUInt16);
        const uint16* ptrIn = (const uint16*)A.dataPointer();
        TIntegerCumsum<uint16>(ptrIn, ptrUInt16, std::numeric_limits<uint16>::min(),
            std::numeric_limits<uint16>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT32: {
        uint32* ptrUInt32 = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, dimsRes.elementCount());
        res = ArrayOf(NLS_UINT32, dimsRes, ptrUInt32);
        const uint32* ptrIn = (const uint32*)A.dataPointer();
        TIntegerCumsum<uint32>(ptrIn, ptrUInt32, std::numeric_limits<uint32>::min(),
            std::numeric_limits<uint32>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_UINT64: {
        uint64* ptrUInt64 = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, dimsRes.elementCount());
        res = ArrayOf(NLS_UINT64, dimsRes, ptrUInt64);
        const uint64* ptrIn = (const uint64*)A.dataPointer();
        TIntegerCumsum<uint64>(ptrIn, ptrUInt64, std::numeric_limits<uint64>::min(),
            std::numeric_limits<uint64>::max(), planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_LOGICAL: {
        double* ptrDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptrDouble);
        const logical* ptrIn = (const logical*)A.dataPointer();
        LogicalCumsum(ptrIn, ptrDouble, planecount, planesize, linesize, reverse);
        needOverload = false;
    } break;
    case NLS_DOUBLE: {
        double* ptrDouble = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptrDouble);
        const double* ptrIn = (const double*)A.dataPointer();
        TRealCumsum<double>(ptrIn, ptrDouble, planecount, planesize, linesize, withNaN, reverse);
        needOverload = false;
    } break;
    case NLS_SINGLE: {
        single* ptrSingle = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dimsRes.elementCount());
        res = ArrayOf(NLS_SINGLE, dimsRes, ptrSingle);
        const single* ptrIn = (const single*)A.dataPointer();
        TRealCumsum<single>(ptrIn, ptrSingle, planecount, planesize, linesize, withNaN, reverse);
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
