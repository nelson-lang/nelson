//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UniqueString.hpp"
#include "UniqueHelpers.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class UniqueStringEntry
{
public:
    ompIndexType n;
    std::wstring x;
    bool isNaN;
};
//=============================================================================
bool
operator<(const UniqueStringEntry& a, const UniqueStringEntry& b)
{
    if (a.isNaN || b.isNaN) {
        return false;
    }
    return (a.x < b.x);
}
//=============================================================================
bool
operator==(const UniqueStringEntry& a, const UniqueStringEntry& b)
{
    return (a.isNaN && b.isNaN) || (a.x == b.x);
}
//=============================================================================
static ArrayOfVector
UniqueStringOneLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();
    rows = input.getElementCount();
    cols = 1;

    const ArrayOf* dp = (const ArrayOf*)input.getDataPointer();
    ompIndexType len = input.getElementCount();
    std::vector<UniqueStringEntry> values(len);
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < len; ++k) {
        values[k].isNaN = !dp[k].isCharacterArray();
        values[k].n = k;
        if (!values[k].isNaN) {
            values[k].x = dp[k].getContentAsWideString();
        }
    }
    parallelSort(values);
    values.erase(std::unique(values.begin(), values.end()), values.end());

    ompIndexType nbElements = values.size();
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbElements);
    ArrayOf NaN = ArrayOf::doubleConstructor(std::nan(""));
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        elements[k] = values[k].isNaN ? NaN : ArrayOf::characterArrayConstructor(values[k].x);
    }
    ArrayOfVector retval;
    Dimensions dimsOut = isRowVector ? Dimensions(1, nbElements) : Dimensions(nbElements, 1);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    retval << ArrayOf(NLS_STRING_ARRAY, dimsOut, elements);
    return retval;
}
//=============================================================================
static ArrayOfVector
UniqueStringTwoLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();
    rows = input.getElementCount();
    cols = 1;

    const ArrayOf* dp = (const ArrayOf*)input.getDataPointer();
    ompIndexType len = input.getElementCount();
    std::vector<UniqueStringEntry> values(len);
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < len; ++k) {
        values[k].isNaN = !dp[k].isCharacterArray();
        values[k].n = k;
        if (!values[k].isNaN) {
            values[k].x = dp[k].getContentAsWideString();
        }
    }
    parallelSort(values);
    values.erase(std::unique(values.begin(), values.end()), values.end());

    ompIndexType nbElements = values.size();
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nbElements);
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbElements);
    ArrayOf NaN = ArrayOf::doubleConstructor(std::nan(""));

    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        elements[k] = values[k].isNaN ? NaN : ArrayOf::characterArrayConstructor(values[k].x);
        mp[k] = (double)(values[k].n + 1);
    }
    ArrayOfVector retval;
    Dimensions dimsOut = isRowVector ? Dimensions(1, nbElements) : Dimensions(nbElements, 1);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    retval << ArrayOf(NLS_STRING_ARRAY, dimsOut, elements);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(nbElements, 1), mp);
    return retval;
}
//=============================================================================
static ArrayOfVector
UniqueStringThreeLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();
    rows = input.getElementCount();
    cols = 1;

    const ArrayOf* dp = (const ArrayOf*)input.getDataPointer();
    ompIndexType len = input.getElementCount();
    std::vector<UniqueStringEntry> values(len);
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < len; ++k) {
        values[k].isNaN = !dp[k].isCharacterArray();
        values[k].n = k;
        if (!values[k].isNaN) {
            values[k].x = dp[k].getContentAsWideString();
        }
    }
    parallelSort(values);
    ompIndexType cnt = len > 0 ? 1 : 0;
    for (ompIndexType i = 1; i < len; ++i) {
        if (!(values[i] == values[i - 1])) {
            cnt++;
        }
    }

    double* np = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    ArrayOf* op = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, cnt);
    if (len > 0) {
        op[0] = dp[values[0].n];
        cnt = 1;
        np[values[0].n] = 1;
        mp[0] = (double)(values[0].n + 1);
        for (int i = 1; i < len; i++) {
            if (!(values[i] == values[i - 1])) {
                op[cnt] = dp[values[i].n];
                mp[cnt] = (double)(values[i].n + 1);
                cnt++;
            }
            np[values[i].n] = (double)cnt;
        }
    }
    ArrayOfVector retval;
    Dimensions dimsOut = isRowVector ? Dimensions(1, cnt) : Dimensions(cnt, 1);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    retval << ArrayOf(NLS_STRING_ARRAY, dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 1), np);
    return retval;
}
//=============================================================================
ArrayOfVector
UniqueString(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueStringOneLhs(input);
    }
    if (nLhs == 2) {
        return UniqueStringTwoLhs(input);
    }
    return UniqueStringThreeLhs(input);
}
//=============================================================================
}
//=============================================================================
