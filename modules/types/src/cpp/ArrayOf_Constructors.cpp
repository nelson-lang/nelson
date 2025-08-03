//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Constructors
//=============================================================================
ArrayOf::ArrayOf(const ArrayOf& copy) { copyObject(copy); }
//=============================================================================
ArrayOf
ArrayOf::getDiagonal(int64 diagonalOrder)
{
    if (!is2D()) {
        Error(_W("Input must be 2-D"));
    }
    if (isSparse()) {
        Error(_W("Sparse matrix not managed."));
    }
    if (isEmpty(true)) {
        Dimensions dims(0, 0);
        ArrayOf res = ArrayOf::emptyConstructor(dims);
        res.promoteType(dp->dataClass);
        return res;
    }
    indexType rows = dp->getRows();
    indexType cols = dp->getColumns();
    int64 outLen;
    Dimensions outDims;
    int64 srcIndex;
    void* qp;
    if (diagonalOrder < 0) {
        if (diagonalOrder < -(int64)(rows)) {
            diagonalOrder = -(int64)(rows);
        }

        outLen = (rows + diagonalOrder) < cols ? (rows + diagonalOrder) : cols;
        outLen = (outLen < 0) ? 0 : outLen;
        if (outLen == 0) {
            Dimensions dims(0, 1);
            ArrayOf res = ArrayOf::emptyConstructor(dims);
            res.promoteType(dp->dataClass);
            return res;
        }
        outDims[0] = outLen;
        outDims[1] = 1;
        qp = allocateArrayOf(dp->dataClass, outLen, dp->fieldNames);
        for (int64 i = 0; i < outLen; i++) {
            srcIndex = -diagonalOrder + i * (rows + 1);
            copyElements(srcIndex, qp, i, 1);
        }
    } else {
        if (diagonalOrder > (int64)cols) {
            diagonalOrder = (int64)cols;
        }
        outLen = rows < (cols - diagonalOrder) ? rows : (cols - diagonalOrder);
        outLen = (outLen < 0) ? 0 : outLen;
        if (outLen == 0) {
            Dimensions dims(0, 1);
            ArrayOf res = ArrayOf::emptyConstructor(dims);
            res.promoteType(dp->dataClass);
            return res;
        }
        outDims[0] = outLen;
        outDims[1] = 1;
        qp = allocateArrayOf(dp->dataClass, outLen, dp->fieldNames);
        for (int64 i = 0; i < outLen; i++) {
            srcIndex = diagonalOrder * rows + i * (rows + 1);
            copyElements(srcIndex, qp, i, 1);
        }
    }
    return ArrayOf(dp->dataClass, outDims, qp, dp->sparse, dp->fieldNames);
}
//=============================================================================
ArrayOf
ArrayOf::diagonalConstructor(ArrayOf src, int64 diagonalOrder)
{
    ArrayOf retval;
    if (!src.isVector() && !src.isEmpty()) {
        Error(_W("Argument to diagonal constructor must by a vector!"));
    }
    indexType length = src.getElementCount();
    indexType M = length + abs(diagonalOrder);
    Dimensions dims(M, M);
    void* rp = allocateArrayOf(src.dp->dataClass, dims.getElementCount(), src.dp->fieldNames, true);
    if (diagonalOrder < 0) {
        OMP_PARALLEL_FOR_LOOP(length)
        for (ompIndexType i = 0; i < (ompIndexType)length; i++) {
            indexType dstIndex = -diagonalOrder + i * (M + 1);
            src.copyElements(i, rp, dstIndex, 1);
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(length)
        for (ompIndexType i = 0; i < (ompIndexType)length; i++) {
            indexType dstIndex = diagonalOrder * M + i * (M + 1);
            src.copyElements(i, rp, dstIndex, 1);
        }
    }
    return ArrayOf(src.dp->dataClass, dims, rp, false, src.dp->fieldNames);
}
//=============================================================================
ArrayOf
ArrayOf::emptyCell(const Dimensions& dim)
{
    if (dim.getElementCount() == 0) {
        return ArrayOf(NLS_CELL_ARRAY, dim, nullptr, false);
    }
    Error(_W("Invalid dimensions."));
    return {};
}
//=============================================================================
ArrayOf
ArrayOf::emptyConstructor(const Dimensions& dim, bool bIsSparse)
{
    if (dim.getElementCount() == 0) {
        return ArrayOf(NLS_DOUBLE, dim, nullptr, bIsSparse);
    }
    Error(_W("Invalid dimensions."));

    return {};
}
//=============================================================================
ArrayOf
ArrayOf::emptyConstructor(indexType m, indexType n, bool bIsSparse)
{
    if (((m == 0) && (n == 0)) || ((m == 0) && (n != 0)) || ((m != 0) && (n == 0))) {
        Dimensions dim(m, n);
        return ArrayOf(NLS_DOUBLE, dim, nullptr, bIsSparse);
    }
    Error(_W("Invalid dimensions."));

    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
