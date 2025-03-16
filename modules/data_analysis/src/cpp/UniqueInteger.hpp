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
#include "nlsBuildConfig.h"
#include "ArrayOf.hpp"
#include "UniqueHelpers.hpp"
#include <algorithm>
#include <vector>
#if WITH_OPENMP
#include <omp.h>
#endif
#include "ParallelSort.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> class UniqueIntegerEntry
{
public:
    ompIndexType n;
    T value;
};
//=============================================================================
template <class T>
bool
operator<(const UniqueIntegerEntry<T>& a, const UniqueIntegerEntry<T>& b)
{
    return a.value < b.value;
}
//=============================================================================
template <class T>
bool
operator==(const UniqueIntegerEntry<T>& a, const UniqueIntegerEntry<T>& b)
{
    return a.value == b.value;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueIntegerOneLhs(const ArrayOf& input)
{
    ompIndexType len = input.getElementCount();
    const T* dp = static_cast<const T*>(input.getDataPointer());
    ArrayOfVector retval;
    std::vector<T> values(dp, dp + len);
    parallelSort(values);
    values.erase(std::unique(values.begin(), values.end()), values.end());

    len = static_cast<ompIndexType>(values.size());
    T* op = static_cast<T*>(ArrayOf::allocateArrayOf(input.getDataClass(), len));

    std::copy(values.begin(), values.end(), op);

    Dimensions dimsOut = input.isRowVector() ? Dimensions(1, len) : Dimensions(len, 1);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    retval << ArrayOf(input.getDataClass(), dimsOut, op);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueIntegerTwoLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();
    rows = input.getElementCount();
    cols = 1;

    const T* dp = (const T*)input.getDataPointer();
    ompIndexType len = input.getElementCount();
    ArrayOfVector retval;
    std::vector<UniqueIntegerEntry<T>> values(len);
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < len; ++k) {
        values[k].n = k + 1;
        values[k].value = dp[k];
    }
    parallelSort(values);
    values.erase(std::unique(values.begin(), values.end()), values.end());

    len = values.size();
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
    T* op = (T*)ArrayOf::allocateArrayOf(input.getDataClass(), len);

    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < len; ++k) {
        op[k] = values[k].value;
        mp[k] = (double)values[k].n;
    }

    Dimensions dimsOut = isRowVector ? Dimensions(1, len) : Dimensions(len, 1);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    retval << ArrayOf(input.getDataClass(), dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 1), mp);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueIntegerThreeLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();
    rows = input.getElementCount();
    cols = 1;

    const T* dp = (const T*)input.getDataPointer();
    ompIndexType len = input.getElementCount();
    std::vector<UniqueIntegerEntry<T>> values(len);
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < len; ++k) {
        values[k].n = k;
        values[k].value = dp[k];
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
    T* op = (T*)ArrayOf::allocateArrayOf(input.getDataClass(), cnt);
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
        if (input.getRows() == input.getColumns()) {
            dimsOut = Dimensions(0, 1);
        } else {
            dimsOut = Dimensions(input.getRows() != 0 ? 1 : 0, input.getColumns() != 0 ? 1 : 0);
        }
    }
    retval << ArrayOf(input.getDataClass(), dimsOut, op);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 1), np);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueInteger(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueIntegerOneLhs<T>(input);
    }
    if (nLhs == 2) {
        return UniqueIntegerTwoLhs<T>(input);
    }
    return UniqueIntegerThreeLhs<T>(input);
}
//=============================================================================
}
//=============================================================================
