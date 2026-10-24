//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <algorithm>
#include <vector>
#include "Types.hpp"
#include "SortHelpers.hpp"
#include "Sort.hpp"
#include "nlsBuildConfig.h"
#include "ParallelSort.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const T& a, const T& b)
    {
        return a > b;
    }
} comparisonIntegerGreater;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const T& a, const T& b)
    {
        return a < b;
    }
} comparisonIntegerLess;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const SortRealEntry<T>& a, const SortRealEntry<T>& b)
    {
        return a.x == b.x ? a.n < b.n : a.x > b.x;
    }
} comparisonIntegerGreaterRealEntry;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const SortRealEntry<T>& a, const SortRealEntry<T>& b)
    {
        return a.x == b.x ? a.n < b.n : a.x < b.x;
    }
} comparisonIntegerLessRealEntry;
//=============================================================================
template <class T>
void
IntegerSortWithIndex(const T* sp, T* dp, double* ip, indexType planes, indexType planesize,
    indexType linesize, bool isVector, bool ascend)
{
    std::vector<SortRealEntry<T>> buf;
    buf.reserve(linesize);
    if (isVector) {
        for (indexType i = 0; i < linesize; i++) {
            SortRealEntry<T> entry;
            entry.x = sp[i];
            entry.n = (double)i + 1;
            buf.push_back(entry);
        }
        if (ascend) {
            parallelSort(buf.begin(), buf.end(), comparisonIntegerLessRealEntry);
        } else {
            parallelSort(buf.begin(), buf.end(), comparisonIntegerGreaterRealEntry);
        }
        for (indexType i = 0; i < linesize; i++) {
            dp[i] = buf[i].x;
            ip[i] = buf[i].n;
        }
    } else {
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                buf.clear();
                for (indexType k = 0; k < linesize; k++) {
                    SortRealEntry<T> entry;
                    entry.x = sp[i * planesize * linesize + j + k * planesize];
                    entry.n = (double)k + 1;
                    buf.push_back(entry);
                }
                if (!ascend) {
                    parallelSort(buf.begin(), buf.end(), comparisonIntegerGreaterRealEntry);
                } else {
                    parallelSort(buf.begin(), buf.end(), comparisonIntegerLessRealEntry);
                }
                for (indexType k = 0; k < linesize; k++) {
                    dp[i * planesize * linesize + j + k * planesize] = buf[k].x;
                    ip[i * planesize * linesize + j + k * planesize] = buf[k].n;
                }
            }
        }
    }
}
//=============================================================================
template <class T>
void
IntegerSortWithoutIndex(const T* sp, T* dp, indexType planes, indexType planesize,
    indexType linesize, bool isVector, bool ascend)
{
    if (isVector) {
        if (ascend) {
            parallelSort(dp, dp + linesize, comparisonIntegerLess);
        } else {
            parallelSort(dp, dp + linesize, comparisonIntegerGreater);
        }
    } else {
        std::vector<T> buf;
        buf.reserve(linesize);
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                buf.clear();
                for (indexType k = 0; k < linesize; k++) {
                    buf.push_back(sp[i * planesize * linesize + j + k * planesize]);
                }
                if (!ascend) {
                    parallelSort(buf.begin(), buf.end(), comparisonIntegerGreater);
                } else {
                    parallelSort(buf.begin(), buf.end(), comparisonIntegerLess);
                }

                for (indexType k = 0; k < linesize; k++) {
                    dp[i * planesize * linesize + j + k * planesize] = buf[k];
                }
            }
        }
    }
}
//=============================================================================
// 'ComparisonMethod', 'abs' for signed integers: the exact magnitude (as an
// unsigned 64-bit value, so intmin is larger than intmax) orders the elements,
// a positive value before the negative one of the same magnitude (ascending
// order); equal values keep their original order in both directions.
template <class T> struct SortIntegerAbsoluteEntry
{
    uint64 magnitude;
    bool negative;
    T x;
    double n;
};
//=============================================================================
template <class T>
static inline uint64
integerMagnitude(T value)
{
    if (value < (T)0) {
        return static_cast<uint64>(-(value + (T)1)) + 1ULL;
    }
    return static_cast<uint64>(value);
}
//=============================================================================
template <class T>
ArrayOfVector
sortIntegerByAbsolute(const ArrayOf& arrayIn, NelsonType dataClass, bool withIndex,
    indexType linesize, indexType planecount, indexType planesize, Dimensions& outDim, bool ascend)
{
    indexType n = outDim.elementCount();
    T* ptrValue = (T*)ArrayOf::allocateArrayOf(dataClass, n, stringVector(), false);
    ArrayOf sortedValues(dataClass, outDim, ptrValue);
    double* ptrIndex = nullptr;
    ArrayOf indexValues;
    if (withIndex) {
        ptrIndex = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, n, stringVector(), false);
        indexValues = ArrayOf(NLS_DOUBLE, outDim, ptrIndex);
    }
    const T* sp = (const T*)arrayIn.dataPointer();
    std::vector<SortIntegerAbsoluteEntry<T>> buf(linesize);
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            for (indexType k = 0; k < linesize; k++) {
                T value = sp[i * planesize * linesize + j + k * planesize];
                buf[k] = { integerMagnitude<T>(value), value < (T)0, value, (double)k + 1 };
            }
            if (ascend) {
                std::stable_sort(buf.begin(), buf.end(),
                    [](const SortIntegerAbsoluteEntry<T>& a, const SortIntegerAbsoluteEntry<T>& b) {
                        return (a.magnitude != b.magnitude) ? a.magnitude < b.magnitude
                                                            : a.negative < b.negative;
                    });
            } else {
                std::stable_sort(buf.begin(), buf.end(),
                    [](const SortIntegerAbsoluteEntry<T>& a, const SortIntegerAbsoluteEntry<T>& b) {
                        return (a.magnitude != b.magnitude) ? a.magnitude > b.magnitude
                                                            : a.negative > b.negative;
                    });
            }
            for (indexType k = 0; k < linesize; k++) {
                indexType pos = i * planesize * linesize + j + k * planesize;
                ptrValue[pos] = buf[k].x;
                if (withIndex) {
                    ptrIndex[pos] = buf[k].n;
                }
            }
        }
    }
    ArrayOfVector res;
    res.push_back(sortedValues);
    if (withIndex) {
        res.push_back(indexValues);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOfVector
sortInteger(const ArrayOf& arrayIn, NelsonType dataClass, bool withIndex, indexType linesize,
    indexType planecount, indexType planesize, Dimensions& outDim, indexType dim, bool ascend)
{
    ArrayOfVector res;
    ArrayOf sortedValues, indexValues;
    bool isVector = arrayIn.isVector();
    if (withIndex) {
        T* ptrValue = nullptr;
        if (isVector) {
            sortedValues = ArrayOf(arrayIn);
            sortedValues.ensureSingleOwner();
            ptrValue = (T*)sortedValues.dataPointer();
        } else {
            ptrValue = (T*)ArrayOf::allocateArrayOf(
                dataClass, outDim.elementCount(), stringVector(), false);
            sortedValues = ArrayOf(dataClass, outDim, ptrValue);
        }
        double* ptrIndex = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, outDim.elementCount(), stringVector(), false);
        if (isVector) {
            ompIndexType elementCount = outDim.elementCount();
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < elementCount; ++k) {
                ptrIndex[k] = (double)1;
            }
        }
        indexValues = ArrayOf(NLS_DOUBLE, outDim, ptrIndex);
        IntegerSortWithIndex<T>((const T*)arrayIn.dataPointer(), (T*)ptrValue, (double*)ptrIndex,
            planecount, planesize, linesize, isVector, ascend);
        res.push_back(sortedValues);
        res.push_back(indexValues);
    } else {
        T* ptrValue = nullptr;
        if (isVector) {
            sortedValues = ArrayOf(arrayIn);
            sortedValues.ensureSingleOwner();
            ptrValue = (T*)sortedValues.dataPointer();
        } else {
            ptrValue = (T*)ArrayOf::allocateArrayOf(
                dataClass, outDim.elementCount(), stringVector(), false);
            sortedValues = ArrayOf(dataClass, outDim, ptrValue);
        }
        IntegerSortWithoutIndex<T>((const T*)arrayIn.dataPointer(), (T*)ptrValue, planecount,
            planesize, linesize, isVector, ascend);
        res.push_back(sortedValues);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
