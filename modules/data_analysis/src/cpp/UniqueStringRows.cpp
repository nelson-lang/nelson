//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UniqueStringRows.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "StringHelpers.hpp"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class UniqueStringRowsEntry
{
public:
    ompIndexType n;
    ompIndexType len;
    ompIndexType stride;
    const ArrayOf* data;
};
//=============================================================================
bool
operator<(const UniqueStringRowsEntry& a, const UniqueStringRowsEntry& b)
{
    for (int i = 0; i < a.len; i++) {
        ArrayOf A = a.data[i * a.stride];
        ArrayOf B = b.data[i * b.stride];
        bool AisNaN = !A.isCharacterArray();
        bool BisNaN = !B.isCharacterArray();
        if (!AisNaN && !BisNaN) {
            std::wstring Astr = A.getContentAsWideString();
            std::wstring Bstr = B.getContentAsWideString();
            if (Astr < Bstr) {
                return true;
            } else if (Astr > Bstr) {
                return false;
            }
        } else if (AisNaN && !BisNaN) {
            return true;
        } else if (!AisNaN && BisNaN) {
            return false;
        }
    }
    return false;
}
//=============================================================================
bool
operator==(const UniqueStringRowsEntry& a, const UniqueStringRowsEntry& b)
{
    if (a.len != b.len || a.stride != b.stride) {
        return false;
    }

    const ArrayOf* a_data = a.data;
    const ArrayOf* b_data = b.data;
    const ompIndexType stride = a.stride;

    for (ompIndexType i = 0; i < a.len; ++i) {
        ArrayOf A = a_data[i * stride];
        ArrayOf B = b_data[i * stride];
        bool AisNaN = !A.isCharacterArray();
        bool BisNaN = !B.isCharacterArray();

        if ((AisNaN && !BisNaN) || (BisNaN && !AisNaN)) {
            return false;
        }
        if (!AisNaN && !BisNaN) {
            if (A.getContentAsWideString() != B.getContentAsWideString()) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
static ArrayOfVector
UniqueStringRowsOneLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();

    const ArrayOf* dp = (const ArrayOf*)input.getDataPointer();

    indexType len = rows;
    NelsonType cls(input.getDataClass());
    int cnt;
    std::vector<UniqueStringRowsEntry> sp(len);

    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + k };
    }

    parallelSort(sp);

    cnt = 1;
    for (indexType i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            cnt++;
        }
    }
    int tcnt = cnt;
    ArrayOf* op = (ArrayOf*)ArrayOf::allocateArrayOf(cls, cnt * cols);

    OMP_PARALLEL_FOR_LOOP(cols)
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
    Dimensions dimsOut = isRowVector ? Dimensions(1, cnt * cols) : Dimensions(cnt, cols);
    retval << ArrayOf(cls, dimsOut, op);
    return retval;
}
//=============================================================================
static ArrayOfVector
UniqueStringRowsTwoLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();

    const ArrayOf* dp = (const ArrayOf*)input.getDataPointer();

    indexType len = rows;
    NelsonType cls(input.getDataClass());
    int cnt;
    std::vector<UniqueStringRowsEntry> sp(len);

    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + k };
    }

    parallelSort(sp);

    cnt = 1;
    for (indexType i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            cnt++;
        }
    }

    int tcnt = cnt;
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    ArrayOf* op = (ArrayOf*)ArrayOf::allocateArrayOf(cls, cnt * cols);
    OMP_PARALLEL_FOR_LOOP(cols)
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        op[0 + j * tcnt] = sp[0].data[0 + j * rows];
    }

    cnt = 1;
    mp[0] = (double)(sp[0].n + 1);
    for (int i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
                op[cnt + j * tcnt] = sp[i].data[0 + j * rows];
            }
            mp[cnt] = (double)sp[i].n + 1;
            cnt++;
        }
    }
    ArrayOfVector retval;
    Dimensions dimsOut = isRowVector ? Dimensions(1, cnt * cols) : Dimensions(cnt, cols);
    retval << ArrayOf(cls, dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    return retval;
}
//=============================================================================
static ArrayOfVector
UniqueStringRowsThreeLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();

    const ArrayOf* dp = (const ArrayOf*)input.getDataPointer();

    indexType len = rows;
    NelsonType cls(input.getDataClass());
    int cnt;
    std::vector<UniqueStringRowsEntry> sp(len);

    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        sp[k] = { k, (ompIndexType)cols, rows, dp + k };
    }
    parallelSort(sp);
    cnt = 1;
    for (indexType i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            cnt++;
        }
    }

    int tcnt = cnt;
    double* np = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    ArrayOf* op = (ArrayOf*)ArrayOf::allocateArrayOf(cls, cnt * cols);
    OMP_PARALLEL_FOR_LOOP(cols)
    for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
        op[0 + j * tcnt] = sp[0].data[0 + j * rows];
    }

    cnt = 1;
    np[sp[0].n] = 1;
    mp[0] = (double)(sp[0].n + 1);
    for (int i = 1; i < len; ++i) {
        if (!(sp[i] == sp[i - 1])) {
            for (ompIndexType j = 0; j < (ompIndexType)cols; j++) {
                op[cnt + j * tcnt] = sp[i].data[0 + j * rows];
            }
            mp[cnt] = (double)sp[i].n + 1;
            cnt++;
        }
        np[sp[i].n] = cnt;
    }
    ArrayOfVector retval;
    Dimensions dimsOut = isRowVector ? Dimensions(1, cnt * cols) : Dimensions(cnt, cols);
    retval << ArrayOf(cls, dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 1), np);
    return retval;
}
//=============================================================================
ArrayOfVector
UniqueStringRows(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueStringRowsOneLhs(input);
    }
    if (nLhs == 2) {
        return UniqueStringRowsTwoLhs(input);
    }
    return UniqueStringRowsThreeLhs(input);
}
//=============================================================================
}
//=============================================================================
