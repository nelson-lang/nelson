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
#include "SortStringHelpers.hpp"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class StringEntry
{
public:
    double n;
    std::wstring x;
    bool isMissing;
};
//=============================================================================
bool
comparisonStringLessPlacementFirst(const StringEntry& a, const StringEntry& b)
{
    return a.x.compare(b.x) < 0;
}
//=============================================================================
bool
comparisonStringLessPlacementLast(const StringEntry& a, const StringEntry& b)
{
    if (a.isMissing) {
        return false;
    }
    if (b.isMissing) {
        return true;
    }
    return a.x.compare(b.x) < 0;
}
//=============================================================================
bool
comparisonStringGreaterPlacementFirst(const StringEntry& a, const StringEntry& b)
{
    return a.x.compare(b.x) > 0;
}
//=============================================================================
bool
comparisonStringGreaterPlacementLast(const StringEntry& a, const StringEntry& b)
{
    return a.x.compare(b.x) > 0;
}
//=============================================================================
using PROC_Comparison = bool (*)(const StringEntry&, const StringEntry&);
//=============================================================================
static PROC_Comparison
getComparisonMethod(MISSING_PLACEMENT placement, bool ascend, bool& missingFirst)
{
    missingFirst = false;
    PROC_Comparison ptrComparison;
    switch (placement) {
    case MISSING_PLACEMENT::AUTO_PLACEMENT: {
        if (ascend) {
            ptrComparison = comparisonStringLessPlacementLast;
        } else {
            ptrComparison = comparisonStringGreaterPlacementFirst;
            missingFirst = true;
        }
    } break;
    case MISSING_PLACEMENT::FIRST_PLACEMENT: {
        if (ascend) {
            ptrComparison = comparisonStringLessPlacementFirst;
        } else {
            ptrComparison = comparisonStringGreaterPlacementFirst;
        }
        missingFirst = true;
    } break;
    case MISSING_PLACEMENT::LAST_PLACEMENT: {
        if (ascend) {
            ptrComparison = comparisonStringLessPlacementLast;
        } else {
            ptrComparison = comparisonStringGreaterPlacementLast;
        }
    } break;
    }
    return ptrComparison;
}
//=============================================================================
static ArrayOfVector
sortStringWithIndex(const ArrayOf& arrayIn, indexType linesize, indexType planecount,
    indexType planesize, Dimensions& outDim, indexType dim, MISSING_PLACEMENT placement,
    bool ascend)
{
    ArrayOfVector res;
    auto* arg = (ArrayOf*)(arrayIn.getDataPointer());
    ArrayOf* ptrValue = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, outDim.getElementCount(), stringVector(), false);
    ArrayOf sortedValues = ArrayOf(NLS_STRING_ARRAY, outDim, ptrValue);
    double* ptrIndex = (double*)ArrayOf::allocateArrayOf(
        NLS_DOUBLE, outDim.getElementCount(), stringVector(), false);
    ArrayOf indexValues = ArrayOf(NLS_DOUBLE, outDim, ptrIndex);

    bool missingFirst = false;
    PROC_Comparison ptrComparison = getComparisonMethod(placement, ascend, missingFirst);

    std::vector<StringEntry> buf;
    buf.reserve(linesize);
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            buf.clear();
            for (indexType k = 0; k < linesize; k++) {
                StringEntry ce;
                ArrayOf param = arg[i * planesize * linesize + j + k * planesize];
                if (param.isNumeric()) {
                    ce.isMissing = true;
                    ce.x = L"";
                } else {
                    ce.isMissing = false;
                    ce.x = param.getContentAsWideString();
                }
                ce.n = (double)k + 1;
                buf.push_back(ce);
            }
            if (missingFirst) {
                auto it = std::partition(
                    buf.begin(), buf.end(), [](const StringEntry& i) { return i.isMissing; });

                parallelSort(
                    buf.begin(), buf.end(), [](const StringEntry& a, const StringEntry& b) {
                        if (a.isMissing && b.isMissing) {
                            return a.n < b.n;
                        }
                        return a.n != a.n;
                    });
                parallelSort(it, buf.end(), ptrComparison);
            } else {
                parallelSort(buf.begin(), buf.end(), ptrComparison);
            }

            for (indexType k = 0; k < linesize; k++) {
                if (buf[k].isMissing) {
                    ptrValue[i * planesize * linesize + j + k * planesize]
                        = ArrayOf::emptyConstructor();
                } else {
                    ptrValue[i * planesize * linesize + j + k * planesize]
                        = ArrayOf::characterArrayConstructor(buf[k].x);
                }
                ptrIndex[i * planesize * linesize + j + k * planesize] = buf[k].n;
            }
        }
    }
    res.push_back(sortedValues);
    res.push_back(indexValues);
    return res;
}
//=============================================================================

static ArrayOfVector
sortStringWithoutIndex(const ArrayOf& arrayIn, indexType linesize, indexType planecount,
    indexType planesize, Dimensions& outDim, indexType dim, MISSING_PLACEMENT placement,
    bool ascend)
{
    ArrayOfVector res;
    auto* arg = (ArrayOf*)(arrayIn.getDataPointer());
    ArrayOf* ptrValue = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, outDim.getElementCount(), stringVector(), false);
    ArrayOf sortedValues = ArrayOf(NLS_STRING_ARRAY, outDim, ptrValue);

    bool missingFirst = false;
    PROC_Comparison ptrComparison = getComparisonMethod(placement, ascend, missingFirst);

    std::vector<StringEntry> buf;

    buf.reserve(linesize);
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            buf.clear();
            for (indexType k = 0; k < linesize; k++) {
                StringEntry ce;
                ce.n = 0;
                ArrayOf param = arg[i * planesize * linesize + j + k * planesize];
                if (param.isNumeric()) {
                    ce.isMissing = true;
                    ce.x = L"";
                } else {
                    ce.isMissing = false;
                    ce.x = param.getContentAsWideString();
                }
                buf.push_back(ce);
            }
            if (missingFirst) {
                auto it = std::partition(
                    buf.begin(), buf.end(), [](const StringEntry& i) { return i.isMissing; });

                parallelSort(it, buf.end(), ptrComparison);
            } else {
                parallelSort(buf.begin(), buf.end(), ptrComparison);
            }

            for (indexType k = 0; k < linesize; k++) {
                if (buf[k].isMissing) {
                    ptrValue[i * planesize * linesize + j + k * planesize]
                        = ArrayOf::emptyConstructor();
                } else {
                    ptrValue[i * planesize * linesize + j + k * planesize]
                        = ArrayOf::characterArrayConstructor(buf[k].x);
                }
            }
        }
    }
    res.push_back(sortedValues);
    return res;
}
//=============================================================================
ArrayOfVector
sortString(const ArrayOf& arrayIn, bool withIndex, indexType linesize, indexType planecount,
    indexType planesize, Dimensions& outDim, indexType dim, MISSING_PLACEMENT placement,
    bool ascend)
{
    if (withIndex) {
        return sortStringWithIndex(
            arrayIn, linesize, planecount, planesize, outDim, dim, placement, ascend);
    }
    return sortStringWithoutIndex(
        arrayIn, linesize, planecount, planesize, outDim, dim, placement, ascend);
}
//=============================================================================
}
//=============================================================================
