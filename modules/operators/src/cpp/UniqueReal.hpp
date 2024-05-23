//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Unique.hpp"
#include "nlsBuildConfig.h"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> class UniqueEntryReal
{
public:
    ompIndexType n;
    T v;
};
//=============================================================================
template <class T>
bool
operator<(const UniqueEntryReal<T>& a, const UniqueEntryReal<T>& b)
{
    if (std::isnan(b.v) && !std::isnan(a.v)) {
        return true;
    }
    return a.v < b.v;
}
//=============================================================================
template <class T>
bool
operator==(const UniqueEntryReal<T>& a, const UniqueEntryReal<T>& b)
{
    return a.v == b.v;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueRealOneLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType len = input.getElementCount();
    const T* dp = static_cast<const T*>(input.getDataPointer());
    std::vector<UniqueEntryReal<T>> values(len);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        values[k].v = dp[k];
    }
    std::sort(values.begin(), values.end());
    values.erase(std::unique(values.begin(), values.end()), values.end());

    ArrayOfVector retval;
    NelsonType cls(input.getDataClass());
    T* op = (T*)ArrayOf::allocateArrayOf(cls, values.size());
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)values.size(); k++) {
        op[k] = values[k].v;
    }
    Dimensions dimsOut = isRowVector ? Dimensions(1, values.size()) : Dimensions(values.size(), 1);
    retval << ArrayOf(cls, dimsOut, op);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueRealTwoLhs(const ArrayOf& input)
{
    //    bool isRowVector = input.isRowVector();
    //    ompIndexType rows = input.getRows();
    //    indexType cols = input.getColumns();
    //    rows = input.getElementCount();
    //    cols = 1;
    //
    //    const T* dp = (const T*)input.getDataPointer();
    //
    //    indexType len = rows;
    //    NelsonType cls(input.getDataClass());
    //    int i;
    //    int cnt;
    //    std::vector<UniqueEntryReal<T>> sp(len);
    //
    // #if WITH_OPENMP
    // #pragma omp parallel for
    // #endif
    //    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
    //        sp[k] = { k, (ompIndexType)cols, rows, dp + k };
    //    }
    //    std::sort(sp.begin(), sp.end());
    //    cnt = 1;
    //
    // #if WITH_OPENMP
    // #pragma omp parallel for reduction(+ : cnt)
    // #endif
    //    for (int i = 1; i < len; ++i) {
    //        if (!(sp[i] == sp[i - 1])) {
    //            cnt++;
    //        }
    //    }
    //
    //    int tcnt = cnt;
    //    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    //    T* op = (T*)ArrayOf::allocateArrayOf(cls, cnt * cols);
    // #if WITH_OPENMP
    // #pragma omp parallel for
    // #endif
    //    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
    //        op[0 + j * tcnt] = sp[0].data[0 + j * rows];
    //    }
    //    i = 1;
    //    cnt = 1;
    //    mp[0] = (double)(sp[0].n + 1);
    //    for (i = 1; i < len; ++i) {
    //        if (!(sp[i] == sp[i - 1])) {
    //            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
    //                op[cnt + j * tcnt] = sp[i].data[0 + j * rows];
    //            }
    //            mp[cnt] = (double)(sp[i].n + 1);
    //            cnt++;
    //        }
    //    }
    //    ArrayOfVector retval;
    //    Dimensions dimsOut = (isRowVector) ? Dimensions(1, cnt * cols) : Dimensions(cnt, cols);
    //    retval << ArrayOf(cls, dimsOut, op);
    //    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    //    return retval;
    return {};
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueRealThreeLhs(const ArrayOf& input)
{
    //    bool isRowVector = input.isRowVector();
    //    ompIndexType rows = input.getRows();
    //    indexType cols = input.getColumns();
    //    rows = input.getElementCount();
    //    cols = 1;
    //
    //    const T* dp = (const T*)input.getDataPointer();
    //
    //    indexType len = rows;
    //    NelsonType cls(input.getDataClass());
    //    int i;
    //    int cnt;
    //    std::vector<UniqueEntryReal<T>> sp(len);
    //
    // #if WITH_OPENMP
    // #pragma omp parallel for
    // #endif
    //    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
    //        sp[k] = { k, (ompIndexType)cols, rows, dp + k };
    //    }
    //    std::sort(sp.begin(), sp.end());
    //    cnt = 1;
    //
    // #if WITH_OPENMP
    // #pragma omp parallel for reduction(+ : cnt)
    // #endif
    //    for (int i = 1; i < len; ++i) {
    //        if (!(sp[i] == sp[i - 1])) {
    //            cnt++;
    //        }
    //    }
    //
    //    int tcnt = cnt;
    //    double* np = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
    //    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    //    T* op = (T*)ArrayOf::allocateArrayOf(cls, cnt * cols);
    // #if WITH_OPENMP
    // #pragma omp parallel for
    // #endif
    //    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
    //        op[0 + j * tcnt] = sp[0].data[0 + j * rows];
    //    }
    //    i = 1;
    //    cnt = 1;
    //    np[sp[0].n] = 1;
    //    mp[0] = (double)(sp[0].n + 1);
    //    for (i = 1; i < len; ++i) {
    //        if (!(sp[i] == sp[i - 1])) {
    //            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
    //                op[cnt + j * tcnt] = sp[i].data[0 + j * rows];
    //            }
    //            mp[cnt] = (double)(sp[i].n + 1);
    //            cnt++;
    //        }
    //        np[sp[i].n] = cnt;
    //    }
    //    ArrayOfVector retval;
    //    Dimensions dimsOut = (isRowVector) ? Dimensions(1, cnt * cols) : Dimensions(cnt, cols);
    //    retval << ArrayOf(cls, dimsOut, op);
    //    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    //    retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 1), np);
    //    return retval;
    return {};
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueReal(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueRealOneLhs<T>(input);
    }
    if (nLhs == 2) {
        return UniqueRealTwoLhs<T>(input);
    }
    return UniqueRealThreeLhs<T>(input);
}
//=============================================================================
}
//=============================================================================
