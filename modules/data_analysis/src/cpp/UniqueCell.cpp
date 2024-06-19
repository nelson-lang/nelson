//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UniqueCell.hpp"
#include "UniqueHelpers.hpp"
#include "nlsBuildConfig.h"
#include "Exception.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOfVector
UniqueCellOneLhs(const ArrayOf& input)
{
    ArrayOfVector retval;
    bool isRowVector = input.isRowVector();
    indexType len(input.getElementCount());
    wstringVector values;
    try {
        values = input.getContentAsWideStringVector(false);
    } catch (Exception&) {
        Error(_W("The input must be a cell array containing character vectors."),
            L"Nelson:UNIQUE:InputClass");
    }
    parallelSort(values);
    values.erase(std::unique(values.begin(), values.end()), values.end());
    Dimensions dimsOut = isRowVector ? Dimensions(1, values.size()) : Dimensions(values.size(), 1);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    ArrayOf cell;
    if (!values.empty()) {
        cell = ArrayOf::toCellArrayOfCharacterColumnVectors(values);
        cell.reshape(dimsOut);
    } else {
        ArrayOf* op = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, 0);
        cell = ArrayOf(NLS_CELL_ARRAY, dimsOut, op);
    }
    retval << cell;
    return retval;
}
//=============================================================================
class UniqueCellString
{
public:
    ompIndexType n;
    std::wstring x;
};
//=============================================================================
bool
operator<(const UniqueCellString& a, const UniqueCellString& b)
{
    return (a.x < b.x);
}
//=============================================================================
bool
operator==(const UniqueCellString& a, const UniqueCellString& b)
{
    return (a.x == b.x);
}
//=============================================================================
static ArrayOfVector
UniqueCellTwoLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType len(input.getElementCount());
    std::vector<UniqueCellString> cellsEntry(len);
    ArrayOf* sp = (ArrayOf*)input.getDataPointer();
    bool isNotCellString = false;
#if WITH_OPENMP
#pragma omp parallel for shared(isNotCellString)
#endif
    for (ompIndexType i = 0; i < len; i++) {
        if (sp[i].isCharacterArray()) {
            cellsEntry[i].x = sp[i].getContentAsWideString();
        } else {
            isNotCellString = true;
        }
        cellsEntry[i].n = i;
    }
    if (isNotCellString) {
        Error(_W("The input must be a cell array containing character vectors."),
            L"Nelson:UNIQUE:InputClass");
    }
    parallelSort(cellsEntry);
    cellsEntry.erase(std::unique(cellsEntry.begin(), cellsEntry.end()), cellsEntry.end());

    ompIndexType cnt = cellsEntry.size();

    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    ArrayOf* op = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, cnt);
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < cnt; i++) {
        op[i] = sp[cellsEntry[i].n];
        mp[i] = (double)(cellsEntry[i].n + 1);
    }
    ArrayOfVector retval;
    Dimensions dimsOut = isRowVector ? Dimensions(1, cnt) : Dimensions(cnt, 1);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    retval << ArrayOf(NLS_CELL_ARRAY, dimsOut, op);
    retval.push_back(ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp));
    return retval;
}
//=============================================================================
static ArrayOfVector
UniqueCellThreeLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    indexType len(input.getElementCount());
    std::vector<UniqueCellString> buf(len);
    ArrayOf* sp = (ArrayOf*)input.getDataPointer();
    for (int i = 0; i < len; i++) {
        if (sp[i].isCharacterArray()) {
            buf[i].x = sp[i].getContentAsWideString();
        } else {
            Error(_W("The input must be a cell array containing character vectors."),
                L"Nelson:UNIQUE:InputClass");
        }
        buf[i].n = i;
    }
    parallelSort(buf);
    int cnt = len > 0 ? 1 : 0;
    for (int i = 1; i < len; ++i) {
        if (!(buf[i] == buf[i - 1])) {
            cnt++;
        }
    }
    double* np = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    ArrayOf* op = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, cnt);
    if (len > 0) {
        op[0] = sp[buf[0].n];
        cnt = 1;
        np[buf[0].n] = 1;
        mp[0] = (double)(buf[0].n + 1);
        for (int i = 1; i < len; i++) {
            if (!(buf[i] == buf[i - 1])) {
                op[cnt] = sp[buf[i].n];
                mp[cnt] = (double)(buf[i].n + 1);
                cnt++;
            }
            np[buf[i].n] = cnt;
        }
    }
    ArrayOfVector retval;
    Dimensions dimsOut = isRowVector ? Dimensions(1, cnt) : Dimensions(cnt, 1);
    if (len == 0) {
        dimsOut = getUniqueEmptyDimensions(input);
    }
    retval << ArrayOf(NLS_CELL_ARRAY, dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 1), np);
    return retval;
}
//=============================================================================
ArrayOfVector
UniqueCell(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueCellOneLhs(input);
    }
    if (nLhs == 2) {
        return UniqueCellTwoLhs(input);
    }
    return UniqueCellThreeLhs(input);
}
//=============================================================================
}
//=============================================================================
