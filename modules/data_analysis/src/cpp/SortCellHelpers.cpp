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
#include <vector>
#include "SortCellHelpers.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class CellEntry
{
public:
    double n;
    std::wstring x;
};
//=============================================================================
struct
{
    bool
    operator()(const CellEntry& a, const CellEntry& b)
    {
        const int comparison = a.x.compare(b.x);
        return comparison == 0 ? a.n < b.n : comparison < 0;
    }
} comparisonCellLess;
//=============================================================================
struct
{
    bool
    operator()(const std::wstring& a, const std::wstring& b)
    {
        return a.compare(b) < 0;
    }
} comparisonWideStringLess;
//=============================================================================
static ArrayOfVector
sortCellWithIndex(const ArrayOf& arrayIn, indexType linesize, indexType planecount,
    indexType planesize, Dimensions& outDim, indexType dim, bool& needToOverload)
{
    ArrayOfVector res;
    auto* arg = (ArrayOf*)(arrayIn.dataPointer());
    indexType elementCount = arrayIn.elementCount();
    for (indexType k = 0; k < elementCount; k++) {
        if (!arg[k].isCharacterArray()) {
            needToOverload = true;
            return res;
        }
    }
    ArrayOf* ptrValue = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_CELL_ARRAY, outDim.elementCount(), stringVector(), false);
    ArrayOf sortedValues = ArrayOf(NLS_CELL_ARRAY, outDim, ptrValue);
    double* ptrIndex = (double*)ArrayOf::allocateArrayOf(
        NLS_DOUBLE, outDim.elementCount(), stringVector(), false);
    ArrayOf indexValues = ArrayOf(NLS_DOUBLE, outDim, ptrIndex);

    std::vector<CellEntry> buf;
    buf.reserve(linesize);
    for (indexType i = 0; i < planecount; i++) {
        buf.clear();
        for (indexType j = 0; j < planesize; j++) {
            for (indexType k = 0; k < linesize; k++) {
                CellEntry ce;
                ce.x = arg[i * planesize * linesize + j + k * planesize].asWideString();
                ce.n = (double)k + 1;
                buf.push_back(ce);
            }
            parallelSort(buf, comparisonCellLess);
            for (indexType k = 0; k < linesize; k++) {
                ptrValue[i * planesize * linesize + j + k * planesize]
                    = ArrayOf::characterArray(buf[k].x);
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
sortCellWithoutIndex(const ArrayOf& arrayIn, indexType linesize, indexType planecount,
    indexType planesize, Dimensions& outDim, indexType dim, bool& needToOverload)
{
    ArrayOfVector res;
    if (arrayIn.isVector()) {
        wstringVector strs;
        strs.reserve(outDim.elementCount());
        auto* arg = (ArrayOf*)(arrayIn.dataPointer());
        indexType elementCount = arrayIn.elementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!arg[k].isCharacterArray()) {
                needToOverload = true;
                return res;
            }
            strs.push_back(arg[k].asWideString());
        }
        parallelSort(strs, comparisonWideStringLess);
        ArrayOf* ptrValue = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, outDim.elementCount(), stringVector(), false);
        ArrayOf sortedValues = ArrayOf(NLS_CELL_ARRAY, outDim, ptrValue);
        ompIndexType elementCountOut = outDim.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCountOut; ++k) {
            ptrValue[k] = ArrayOf::characterArray(strs[k]);
        }
        res.push_back(sortedValues);
    } else {
        auto* arg = (ArrayOf*)(arrayIn.dataPointer());
        indexType elementCount = arrayIn.elementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!arg[k].isCharacterArray()) {
                needToOverload = true;
                return res;
            }
        }
        ArrayOf* ptrValue = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, outDim.elementCount(), stringVector(), false);
        ArrayOf sortedValues = ArrayOf(NLS_CELL_ARRAY, outDim, ptrValue);

        wstringVector buf;
        for (indexType i = 0; i < planecount; i++) {
            buf.clear();
            buf.reserve(linesize);
            for (indexType j = 0; j < planesize; j++) {
                for (indexType k = 0; k < linesize; k++) {
                    buf.push_back(arg[i * planesize * linesize + j + k * planesize].asWideString());
                }
                parallelSort(buf, comparisonWideStringLess);

                for (indexType k = 0; k < linesize; k++) {
                    ptrValue[i * planesize * linesize + j + k * planesize]
                        = ArrayOf::characterArray(buf[k]);
                }
            }
        }
        res.push_back(sortedValues);
    }
    return res;
}
//=============================================================================
ArrayOfVector
sortCell(const ArrayOf& arrayIn, bool withIndex, indexType linesize, indexType planecount,
    indexType planesize, Dimensions& outDim, indexType dim, bool& needToOverload)
{
    if (withIndex) {
        return sortCellWithIndex(
            arrayIn, linesize, planecount, planesize, outDim, dim, needToOverload);
    }
    return sortCellWithoutIndex(
        arrayIn, linesize, planecount, planesize, outDim, dim, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
