//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
//=============================================================================
namespace Nelson {
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const T& a, const T& b)
    {
        if (std::isnan(a) && !std::isnan(b))
            return true;
        return a > b;
    }
} comparisonRealGreater;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const T& a, const T& b)
    {
        if (std::isnan(b) && !std::isnan(a)) {
            return true;
        }
        return a < b;
    }
} comparisonRealLess;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const SortRealEntry<T>& a, const SortRealEntry<T>& b)
    {
        return comparisonRealGreater(a.x, b.x);
    }
} comparisonRealGreaterRealEntry;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const SortRealEntry<T>& a, const SortRealEntry<T>& b)
    {
        return comparisonRealLess(a.x, b.x);
    }
} comparisonRealLessRealEntry;
//=============================================================================
template <class T>
void
RealEntrySortByPlacement(
    std::vector<SortRealEntry<T>>& buf, bool ascend, MISSING_PLACEMENT placement)
{
    switch (placement) {
    case MISSING_PLACEMENT::AUTO_PLACEMENT: {
        if (ascend) {
            parallelSort(buf.begin(), buf.end(), comparisonRealLessRealEntry);
        } else {
            auto it = std::partition(
                buf.begin(), buf.end(), [](const SortRealEntry<T>& i) { return std::isnan(i.x); });

            parallelSort(
                buf.begin(), buf.end(), [](const SortRealEntry<T>& a, const SortRealEntry<T>& b) {
                    if (std::isnan(a.x) && std::isnan(b.x)) {
                        return a.n < b.n;
                    }
                    return a.n != a.n;
                });

            parallelSort(it, buf.end(), comparisonRealGreaterRealEntry);
        }
    } break;
    case MISSING_PLACEMENT::FIRST_PLACEMENT: {
        auto it = std::partition(
            buf.begin(), buf.end(), [](const SortRealEntry<T>& i) { return std::isnan(i.x); });
        parallelSort(
            buf.begin(), buf.end(), [](const SortRealEntry<T>& a, const SortRealEntry<T>& b) {
                if (std::isnan(a.x) && std::isnan(b.x)) {
                    return a.n < b.n;
                }
                return a.n != a.n;
            });
        if (ascend) {
            parallelSort(it, buf.end(), comparisonRealLessRealEntry);
        } else {
            parallelSort(it, buf.end(), comparisonRealGreaterRealEntry);
        }
    } break;
    case MISSING_PLACEMENT::LAST_PLACEMENT: {
        auto it = std::partition(
            buf.begin(), buf.end(), [](const SortRealEntry<T>& i) { return !std::isnan(i.x); });
        parallelSort(
            buf.begin(), buf.end(), [](const SortRealEntry<T>& a, const SortRealEntry<T>& b) {
                if (std::isnan(a.x) && std::isnan(b.x)) {
                    return a.n < b.n;
                }
                return a.n != a.n;
            });
        if (ascend) {
            parallelSort(buf.begin(), it, comparisonRealLessRealEntry);
        } else {
            parallelSort(buf.begin(), it, comparisonRealGreaterRealEntry);
        }
    } break;
    }
}
//=============================================================================
template <class T>
void
RealSortWithIndex(const T* sp, T* dp, double* ip, indexType planes, indexType planesize,
    indexType linesize, bool isVector, bool ascend, MISSING_PLACEMENT placement)
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

        RealEntrySortByPlacement<T>(buf, ascend, placement);

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

                RealEntrySortByPlacement<T>(buf, ascend, placement);

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
RealSortWithoutIndex(const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize,
    bool isVector, bool ascend, MISSING_PLACEMENT placement)
{
    if (isVector) {
        switch (placement) {
        case MISSING_PLACEMENT::AUTO_PLACEMENT: {
            if (ascend) {
                parallelSort(dp, dp + linesize, comparisonRealLess);
            } else {
                T* pt = std::partition(dp, dp + linesize, [](const T& i) { return std::isnan(i); });
                parallelSort(pt, dp + linesize, comparisonRealGreater);
            }
        } break;
        case MISSING_PLACEMENT::FIRST_PLACEMENT: {
            T* pt = std::partition(dp, dp + linesize, [](const T& i) { return std::isnan(i); });
            if (ascend) {
                parallelSort(pt, dp + linesize, comparisonRealLess);
            } else {
                parallelSort(pt, dp + linesize, comparisonRealGreater);
            }
        } break;
        case MISSING_PLACEMENT::LAST_PLACEMENT: {
            T* pt = std::partition(dp, dp + linesize, [](const T& i) { return !std::isnan(i); });
            if (ascend) {
                parallelSort(dp, pt, comparisonRealLess);
            } else {
                parallelSort(dp, pt, comparisonRealGreater);
            }
        } break;
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

                switch (placement) {
                case MISSING_PLACEMENT::AUTO_PLACEMENT: {
                    if (ascend) {
                        parallelSort(buf.begin(), buf.end(), comparisonRealLess);
                    } else {
                        auto it = std::partition(
                            buf.begin(), buf.end(), [](const T& i) { return std::isnan(i); });
                        parallelSort(it, buf.end(), comparisonRealGreater);
                    }
                } break;
                case MISSING_PLACEMENT::FIRST_PLACEMENT: {
                    auto it = std::partition(
                        buf.begin(), buf.end(), [](const T& i) { return std::isnan(i); });
                    if (ascend) {
                        parallelSort(it, buf.end(), comparisonRealLess);
                    } else {
                        parallelSort(it, buf.end(), comparisonRealGreater);
                    }
                } break;
                case MISSING_PLACEMENT::LAST_PLACEMENT: {
                    auto it = std::partition(
                        buf.begin(), buf.end(), [](const T& i) { return !std::isnan(i); });
                    if (ascend) {
                        parallelSort(it, buf.end(), comparisonRealLess);
                    } else {
                        parallelSort(it, buf.end(), comparisonRealGreater);
                    }
                } break;
                }
                for (indexType k = 0; k < linesize; k++) {
                    dp[i * planesize * linesize + j + k * planesize] = buf[k];
                }
            }
        }
    }
}
//=============================================================================
template <class T>
ArrayOfVector
sortReal(const ArrayOf& arrayIn, NelsonType dataClass, bool withIndex, indexType linesize,
    indexType planecount, indexType planesize, Dimensions& outDim, indexType dim, bool ascend,
    MISSING_PLACEMENT placement)
{
    ArrayOfVector res;
    ArrayOf sortedValues, indexValues;
    bool isVector = arrayIn.isVector();
    if (withIndex) {
        T* ptrValue = nullptr;
        if (isVector) {
            sortedValues = ArrayOf(arrayIn);
            sortedValues.ensureSingleOwner();
            ptrValue = (T*)sortedValues.getDataPointer();
        } else {
            ptrValue = (T*)ArrayOf::allocateArrayOf(
                dataClass, outDim.getElementCount(), stringVector(), false);
            sortedValues = ArrayOf(dataClass, outDim, ptrValue);
        }
        double* ptrIndex = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, outDim.getElementCount(), stringVector(), false);
        if (isVector) {
            ompIndexType elementCount = (ompIndexType)outDim.getElementCount();
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < elementCount; ++k) {
                ptrIndex[k] = (double)1;
            }
        }
        indexValues = ArrayOf(NLS_DOUBLE, outDim, ptrIndex);
        RealSortWithIndex<T>((const T*)arrayIn.getDataPointer(), (T*)ptrValue, (double*)ptrIndex,
            planecount, planesize, linesize, isVector, ascend, placement);
        res.push_back(sortedValues);
        res.push_back(indexValues);
    } else {
        T* ptrValue = nullptr;
        if (isVector) {
            sortedValues = ArrayOf(arrayIn);
            sortedValues.ensureSingleOwner();
            ptrValue = (T*)sortedValues.getDataPointer();
        } else {
            ptrValue = (T*)ArrayOf::allocateArrayOf(
                dataClass, outDim.getElementCount(), stringVector(), false);
            sortedValues = ArrayOf(dataClass, outDim, ptrValue);
        }
        RealSortWithoutIndex<T>((const T*)arrayIn.getDataPointer(), (T*)ptrValue, planecount,
            planesize, linesize, isVector, ascend, placement);
        res.push_back(sortedValues);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
