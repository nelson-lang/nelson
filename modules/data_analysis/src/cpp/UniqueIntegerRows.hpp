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
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> class UniqueIntegerRowsEntry
{
public:
    ompIndexType n;
    ompIndexType len;
    ompIndexType stride;
    const T* data;
};
//=============================================================================
template <class T>
bool
operator<(const UniqueIntegerRowsEntry<T>& a, const UniqueIntegerRowsEntry<T>& b)
{
    for (int i = 0; i < a.len; i++) {
        if (a.data[i * a.stride] < b.data[i * b.stride]) {
            return true;
        } else if (a.data[i * a.stride] > b.data[i * b.stride]) {
            return false;
        }
    }
    return false;
}
//=============================================================================
template <class T>
bool
operator==(const UniqueIntegerRowsEntry<T>& a, const UniqueIntegerRowsEntry<T>& b)
{
    if (a.len != b.len || a.stride != b.stride) {
        return false;
    }

    const T* a_data = a.data;
    const T* b_data = b.data;
    const ompIndexType stride = a.stride;

    for (ompIndexType i = 0; i < a.len; ++i) {
        if (a_data[i * stride] != b_data[i * stride]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueIntegerRowsOneLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();

    const T* dp = (const T*)input.getDataPointer();

    indexType len = rows;
    NelsonType cls(input.getDataClass());
    int cnt;
    std::vector<UniqueIntegerRowsEntry<T>> sp(len);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + k };
    }
    parallelSort(sp);
    cnt = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : cnt)
#endif

    for (int i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            cnt++;
        }
    }

    int tcnt = cnt;

    T* op = (T*)ArrayOf::allocateArrayOf(cls, cnt * cols);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        op[0 + j * tcnt] = sp[0].data[0 + j * rows];
    }
    cnt = 1;

    for (ompIndexType i = 1; i < (ompIndexType)len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
                op[cnt + j * tcnt] = sp[i].data[0 + j * rows];
            }
            cnt++;
        }
    }

    ArrayOfVector retval;
    Dimensions dimsOut = (isRowVector) ? Dimensions(1, cnt * cols) : Dimensions(cnt, cols);
    retval << ArrayOf(cls, dimsOut, op);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueIntegerRowsTwoLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();

    const T* dp = (const T*)input.getDataPointer();

    indexType len = rows;
    NelsonType cls(input.getDataClass());
    int i;
    int cnt;
    std::vector<UniqueIntegerRowsEntry<T>> sp(len);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + k };
    }

    parallelSort(sp);
    cnt = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : cnt)
#endif

    for (int i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            cnt++;
        }
    }

    int tcnt = cnt;

    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    T* op = (T*)ArrayOf::allocateArrayOf(cls, cnt * cols);
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        op[0 + j * tcnt] = sp[0].data[0 + j * rows];
    }
    i = 1;
    cnt = 1;
    mp[0] = (double)(sp[0].n + 1);
    for (i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
                op[cnt + j * tcnt] = sp[i].data[0 + j * rows];
            }
            mp[cnt] = (double)(sp[i].n + 1);
            cnt++;
        }
    }
    ArrayOfVector retval;
    Dimensions dimsOut = (isRowVector) ? Dimensions(1, cnt * cols) : Dimensions(cnt, cols);
    retval << ArrayOf(cls, dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueIntegerRowsThreeLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();

    const T* dp = (const T*)input.getDataPointer();

    indexType len = rows;
    NelsonType cls(input.getDataClass());
    int i;
    int cnt;
    std::vector<UniqueIntegerRowsEntry<T>> sp(len);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + k };
    }
    parallelSort(sp);
    cnt = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : cnt)
#endif

    for (int i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            cnt++;
        }
    }

    int tcnt = cnt;
    double* np = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    T* op = (T*)ArrayOf::allocateArrayOf(cls, cnt * cols);
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        op[0 + j * tcnt] = sp[0].data[0 + j * rows];
    }
    i = 1;
    cnt = 1;
    np[sp[0].n] = 1;
    mp[0] = (double)(sp[0].n + 1);
    for (i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
                op[cnt + j * tcnt] = sp[i].data[0 + j * rows];
            }
            mp[cnt] = (double)(sp[i].n + 1);
            cnt++;
        }
        np[sp[i].n] = cnt;
    }
    ArrayOfVector retval;
    Dimensions dimsOut = (isRowVector) ? Dimensions(1, cnt * cols) : Dimensions(cnt, cols);
    retval << ArrayOf(cls, dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 1), np);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueIntegerRows(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueIntegerRowsOneLhs<T>(input);
    }
    if (nLhs == 2) {
        return UniqueIntegerRowsTwoLhs<T>(input);
    }
    return UniqueIntegerRowsThreeLhs<T>(input);
}
//=============================================================================
}
//=============================================================================
