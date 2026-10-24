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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf::ArrayOf(const ArrayOf& copy) { copyObject(copy); }
//=============================================================================
ArrayOf
ArrayOf::diagonal(int64 diagonalOrder)
{
    materializeScalar();
    if (!is2D()) {
        Error(L"Nelson:types:invalidDimensions:input2D", L"Input must be 2-D.");
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported:sparseMatrixNotSupported",
            L"Sparse matrix not supported.");
    }
    if (isEmpty(true)) {
        Dimensions dims(0, 0);
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(dp->dataClass);
        return res;
    }
    indexType rows = dp->rows();
    indexType cols = dp->columns();
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
            ArrayOf res = ArrayOf::empty(dims);
            res.promoteType(dp->dataClass);
            return res;
        }
        outDims[0] = outLen;
        outDims[1] = 1;
        qp = allocateArrayOf(dp->dataClass, outLen, dp->fieldNames());
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
            ArrayOf res = ArrayOf::empty(dims);
            res.promoteType(dp->dataClass);
            return res;
        }
        outDims[0] = outLen;
        outDims[1] = 1;
        qp = allocateArrayOf(dp->dataClass, outLen, dp->fieldNames());
        for (int64 i = 0; i < outLen; i++) {
            srcIndex = diagonalOrder * rows + i * (rows + 1);
            copyElements(srcIndex, qp, i, 1);
        }
    }
    return ArrayOf(dp->dataClass, outDims, qp, dp->sparse, dp->fieldNames());
}
//=============================================================================
ArrayOf
ArrayOf::diagonalMatrix(ArrayOf src, int64 diagonalOrder)
{
    src.materializeScalar();
    ArrayOf retval;
    if (!src.isVector() && !src.isEmpty()) {
        Error(L"Nelson:types:invalidDimensions:argumentDiagonalConstructorByAVector",
            L"Argument to diagonal constructor must by a vector!");
    }
    indexType length = src.elementCount();
    indexType M = length + abs(diagonalOrder);
    Dimensions dims(M, M);
    void* rp = allocateArrayOf(src.dp->dataClass, dims.elementCount(), src.dp->fieldNames(), true);
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
    return ArrayOf(src.dp->dataClass, dims, rp, false, src.dp->fieldNames());
}
//=============================================================================
ArrayOf
ArrayOf::emptyCell(const Dimensions& dim)
{
    if (dim.elementCount() == 0) {
        return ArrayOf(NLS_CELL_ARRAY, dim, (void*)nullptr, false);
    }
    Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    return {};
}
//=============================================================================
ArrayOf
ArrayOf::empty(const Dimensions& dim, bool bIsSparse)
{
    if (dim.elementCount() == 0) {
        return ArrayOf(NLS_DOUBLE, dim, (void*)nullptr, bIsSparse);
    }
    Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");

    return {};
}
//=============================================================================
ArrayOf
ArrayOf::empty(indexType m, indexType n, bool bIsSparse)
{
    if (((m == 0) && (n == 0)) || ((m == 0) && (n != 0)) || ((m != 0) && (n == 0))) {
        Dimensions dim(m, n);
        return ArrayOf(NLS_DOUBLE, dim, (void*)nullptr, bIsSparse);
    }
    Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");

    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
