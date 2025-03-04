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
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "complex_abs.hpp"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> class UniqueComplexRowsEntry
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
operator<(const UniqueComplexRowsEntry<T>& a, const UniqueComplexRowsEntry<T>& b)
{
    ompIndexType i = 0;
    while (i < a.len) {
        T a_abs = complex_abs(a.data[2 * i * a.stride], a.data[2 * i * a.stride + 1]);
        T b_abs = complex_abs(b.data[2 * i * b.stride], b.data[2 * i * b.stride + 1]);
        if (a_abs < b_abs) {
            return true;
        } else if (a_abs > b_abs) {
            return false;
        }
        i++;
    }
    return false;
}
//=============================================================================
template <class T>
bool
operator==(const UniqueComplexRowsEntry<T>& a, const UniqueComplexRowsEntry<T>& b)
{
    ompIndexType i = 0;
    while (i < a.len) {
        if ((a.data[2 * i * a.stride] != b.data[2 * i * b.stride])
            || (a.data[2 * i * a.stride + 1] != b.data[2 * i * b.stride + 1])) {
            return false;
        }
        i++;
    }
    return true;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueComplexRowsOneLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    ompIndexType cols = input.getColumns();

    const T* dp = (const T*)input.getDataPointer();

    ompIndexType len = rows;
    NelsonType cls(input.getDataClass());
    int cnt;
    std::vector<UniqueComplexRowsEntry<T>> sp(len);

    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + 2 * k };
    }
    parallelSort(sp);
    cnt = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : cnt) if (len > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            cnt++;
        }
    }

    int tcnt = cnt;

    T* op = (T*)ArrayOf::allocateArrayOf(cls, cnt * cols);

    OMP_PARALLEL_FOR_LOOP(cols)
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        op[0 + j * 2 * tcnt] = sp[0].data[0 + j * 2 * rows];
        op[1 + j * 2 * tcnt] = sp[0].data[1 + j * 2 * rows];
    }
    cnt = 1;

    for (ompIndexType i = 1; i < (ompIndexType)len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
                op[2 * cnt + j * 2 * tcnt] = sp[i].data[0 + j * 2 * rows];
                op[2 * cnt + j * 2 * tcnt + 1] = sp[i].data[1 + j * 2 * rows];
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
UniqueComplexRowsTwoLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();

    const T* dp = (const T*)input.getDataPointer();

    indexType len = rows;
    NelsonType cls(input.getDataClass());
    int cnt;
    std::vector<UniqueComplexRowsEntry<T>> sp(len);

    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + 2 * k };
    }
    parallelSort(sp);
    cnt = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : cnt) if (len > OMP_DEFAULT_THRESHOLD)
#endif

    for (int i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            cnt++;
        }
    }

    int tcnt = cnt;

    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    T* op = (T*)ArrayOf::allocateArrayOf(cls, cnt * cols);
    OMP_PARALLEL_FOR_LOOP(cols)
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        op[0 + j * 2 * tcnt] = sp[0].data[0 + j * 2 * rows];
        op[1 + j * 2 * tcnt] = sp[0].data[1 + j * 2 * rows];
    }
    cnt = 1;
    mp[0] = (double)(sp[0].n + 1);
    for (int i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
                op[2 * cnt + j * 2 * tcnt] = sp[i].data[0 + j * 2 * rows];
                op[2 * cnt + j * 2 * tcnt + 1] = sp[i].data[1 + j * 2 * rows];
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
UniqueComplexRowsThreeLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();

    const T* dp = (const T*)input.getDataPointer();

    indexType len = rows;
    NelsonType cls(input.getDataClass());
    int cnt;
    std::vector<UniqueComplexRowsEntry<T>> sp(len);

    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + 2 * k };
    }
    parallelSort(sp);
    cnt = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : cnt) if (len > OMP_DEFAULT_THRESHOLD)
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
    OMP_PARALLEL_FOR_LOOP(cols)
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        op[0 + j * 2 * tcnt] = sp[0].data[0 + j * 2 * rows];
        op[1 + j * 2 * tcnt] = sp[0].data[1 + j * 2 * rows];
    }
    cnt = 1;
    np[sp[0].n] = 1;
    mp[0] = (double)(sp[0].n + 1);
    for (int i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
                op[2 * cnt + j * 2 * tcnt] = sp[i].data[0 + j * 2 * rows];
                op[2 * cnt + j * 2 * tcnt + 1] = sp[i].data[1 + j * 2 * rows];
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
UniqueComplexRows(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueComplexRowsOneLhs<T>(input);
    }
    if (nLhs == 2) {
        return UniqueComplexRowsTwoLhs<T>(input);
    }
    return UniqueComplexRowsThreeLhs<T>(input);
}
//=============================================================================
}
//=============================================================================
