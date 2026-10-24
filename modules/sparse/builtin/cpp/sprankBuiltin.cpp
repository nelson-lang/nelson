//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sprankBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include <Eigen/Sparse>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
template <class T>
void
appendSparsePattern(const ArrayOf& value, std::vector<std::vector<signedIndexType>>& rowsToColumns)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    for (signedIndexType col = 0; col < matrix->outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(*matrix, col); it; ++it) {
            if (it.value() == T(0)) {
                continue;
            }
            rowsToColumns[static_cast<size_t>(it.row())].push_back(it.col());
        }
    }
}
//=============================================================================
template <class T>
void
appendDensePattern(const ArrayOf& value, std::vector<std::vector<signedIndexType>>& rowsToColumns)
{
    const T* data = static_cast<const T*>(value.dataPointer());
    for (indexType col = 0; col < value.columns(); ++col) {
        for (indexType row = 0; row < value.rows(); ++row) {
            if (data[row + col * value.rows()] != T(0)) {
                rowsToColumns[static_cast<size_t>(row)].push_back(
                    static_cast<signedIndexType>(col));
            }
        }
    }
}
//=============================================================================
std::vector<std::vector<signedIndexType>>
densePattern(const ArrayOf& value)
{
    std::vector<std::vector<signedIndexType>> rowsToColumns(static_cast<size_t>(value.rows()));
    switch (value.dataClass()) {
    case NLS_LOGICAL:
        appendDensePattern<logical>(value, rowsToColumns);
        break;
    case NLS_SINGLE:
        appendDensePattern<single>(value, rowsToColumns);
        break;
    case NLS_DOUBLE:
        appendDensePattern<double>(value, rowsToColumns);
        break;
    case NLS_SCOMPLEX:
        appendDensePattern<singlecomplex>(value, rowsToColumns);
        break;
    case NLS_DCOMPLEX:
        appendDensePattern<doublecomplex>(value, rowsToColumns);
        break;
    default:
        OverloadRequired("sprank");
    }
    return rowsToColumns;
}
//=============================================================================
std::vector<std::vector<signedIndexType>>
sparsePattern(const ArrayOf& value)
{
    std::vector<std::vector<signedIndexType>> rowsToColumns(static_cast<size_t>(value.rows()));
    switch (value.dataClass()) {
    case NLS_LOGICAL:
        appendSparsePattern<logical>(value, rowsToColumns);
        break;
    case NLS_DOUBLE:
        appendSparsePattern<double>(value, rowsToColumns);
        break;
    case NLS_SINGLE:
        appendSparsePattern<single>(value, rowsToColumns);
        break;
    case NLS_DCOMPLEX:
        appendSparsePattern<doublecomplex>(value, rowsToColumns);
        break;
    case NLS_SCOMPLEX:
        appendSparsePattern<singlecomplex>(value, rowsToColumns);
        break;
    default:
        OverloadRequired("sprank");
    }
    return rowsToColumns;
}
//=============================================================================
bool
findAugmentingPath(signedIndexType row,
    const std::vector<std::vector<signedIndexType>>& rowsToColumns,
    std::vector<signedIndexType>& columnMatch, std::vector<signedIndexType>& seenColumns,
    signedIndexType stamp)
{
    for (signedIndexType col : rowsToColumns[static_cast<size_t>(row)]) {
        if (seenColumns[static_cast<size_t>(col)] == stamp) {
            continue;
        }
        seenColumns[static_cast<size_t>(col)] = stamp;
        signedIndexType matchedRow = columnMatch[static_cast<size_t>(col)];
        if (matchedRow < 0
            || findAugmentingPath(matchedRow, rowsToColumns, columnMatch, seenColumns, stamp)) {
            columnMatch[static_cast<size_t>(col)] = row;
            return true;
        }
    }
    return false;
}
//=============================================================================
indexType
structuralRank(const ArrayOf& value)
{
    std::vector<std::vector<signedIndexType>> rowsToColumns
        = value.isSparse() ? sparsePattern(value) : densePattern(value);
    std::vector<signedIndexType> columnMatch(static_cast<size_t>(value.columns()), -1);
    std::vector<signedIndexType> seenColumns(static_cast<size_t>(value.columns()), 0);
    indexType rank = 0;
    signedIndexType stamp = 1;
    for (indexType row = 0; row < value.rows(); ++row) {
        if (findAugmentingPath(static_cast<signedIndexType>(row), rowsToColumns, columnMatch,
                seenColumns, stamp++)) {
            ++rank;
        }
    }
    return rank;
}
//=============================================================================
bool
isFloatingPointOrLogicalClass(const ArrayOf& value)
{
    switch (value.dataClass()) {
    case NLS_LOGICAL:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        return true;
    default:
        return false;
    }
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sprankBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].is2D()) {
        Error(L"Nelson:validators:mustBeMatrix", L"%s must be a 2-D matrix.", L"Value");
    }
    if (!argIn[0].isNumeric() && !argIn[0].isLogical()) {
        Error(L"Nelson:types:numericOrLogical2DExpected",
            L"A numeric or logical 2-D matrix expected.");
    }
    if (!isFloatingPointOrLogicalClass(argIn[0])) {
        Error(L"Nelson:types:floatingPointOrLogical2DExpected",
            L"A floating-point or logical 2-D matrix expected.");
    }
    return ArrayOf::doubleScalar(static_cast<double>(structuralRank(argIn[0])));
}
//=============================================================================
