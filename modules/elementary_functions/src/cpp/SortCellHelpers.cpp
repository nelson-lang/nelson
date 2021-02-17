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
#include <algorithm>
#include <vector>
#include "SortCellHelpers.hpp"
#include "nlsConfig.h"
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
        return a.x.compare(b.x) < 0;
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
    auto* arg = (ArrayOf*)(arrayIn.getDataPointer());
    indexType elementCount = arrayIn.getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
        if (!arg[k].isCharacterArray()) {
            needToOverload = true;
            return res;
        }
    }
    ArrayOf* ptrValue = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_CELL_ARRAY, outDim.getElementCount(), stringVector(), false);
    ArrayOf sortedValues = ArrayOf(NLS_CELL_ARRAY, outDim, ptrValue);
    double* ptrIndex = (double*)ArrayOf::allocateArrayOf(
        NLS_DOUBLE, outDim.getElementCount(), stringVector(), false);
    ArrayOf indexValues = ArrayOf(NLS_DOUBLE, outDim, ptrIndex);

    std::vector<CellEntry> buf;
    buf.reserve(linesize);
    for (indexType i = 0; i < planecount; i++) {
        buf.clear();
        for (indexType j = 0; j < planesize; j++) {
            for (indexType k = 0; k < linesize; k++) {
                CellEntry ce;
                ce.x = arg[i * planesize * linesize + j + k * planesize].getContentAsWideString();
                ce.n = (double)k + 1;
                buf.push_back(ce);
            }
            std::sort(buf.begin(), buf.end(), comparisonCellLess);
            for (indexType k = 0; k < linesize; k++) {
                ptrValue[i * planesize * linesize + j + k * planesize]
                    = ArrayOf::characterArrayConstructor(buf[k].x);
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
        strs.reserve(outDim.getElementCount());
        auto* arg = (ArrayOf*)(arrayIn.getDataPointer());
        indexType elementCount = arrayIn.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!arg[k].isCharacterArray()) {
                needToOverload = true;
                return res;
            }
            strs.push_back(arg[k].getContentAsWideString());
        }
        std::sort(strs.begin(), strs.end(), comparisonWideStringLess);
        ArrayOf* ptrValue = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, outDim.getElementCount(), stringVector(), false);
        ArrayOf sortedValues = ArrayOf(NLS_CELL_ARRAY, outDim, ptrValue);
        ompIndexType elementCountOut = outDim.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCountOut; ++k) {
            ptrValue[k] = ArrayOf::characterArrayConstructor(strs[k]);
        }
        res.push_back(sortedValues);
    } else {
        auto* arg = (ArrayOf*)(arrayIn.getDataPointer());
        indexType elementCount = arrayIn.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!arg[k].isCharacterArray()) {
                needToOverload = true;
                return res;
            }
        }
        ArrayOf* ptrValue = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, outDim.getElementCount(), stringVector(), false);
        ArrayOf sortedValues = ArrayOf(NLS_CELL_ARRAY, outDim, ptrValue);

        wstringVector buf;
        for (indexType i = 0; i < planecount; i++) {
            buf.clear();
            buf.reserve(linesize);
            for (indexType j = 0; j < planesize; j++) {
                for (indexType k = 0; k < linesize; k++) {
                    buf.push_back(
                        arg[i * planesize * linesize + j + k * planesize].getContentAsWideString());
                }
                std::sort(buf.begin(), buf.end(), comparisonWideStringLess);

                for (indexType k = 0; k < linesize; k++) {
                    ptrValue[i * planesize * linesize + j + k * planesize]
                        = ArrayOf::characterArrayConstructor(buf[k]);
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
}
//=============================================================================
