//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "Data.hpp"
#include "Error.hpp"
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
    indexType rows = dp->dimensions.getRows();
    indexType cols = dp->dimensions.getColumns();
    int64 outLen;
    Dimensions outDims;
    int64 srcIndex;
    void* qp;
    if (diagonalOrder < 0) {
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
    if (!src.isVector()) {
        Error(_W("Argument to diagonal constructor must by a vector!"));
    }
    indexType length = src.getLength();
    indexType M = 0;
    M = length + abs(diagonalOrder);
    Dimensions dims;
    dims[0] = M;
    dims[1] = M;
    void* rp = allocateArrayOf(src.dp->dataClass, dims.getElementCount(), src.dp->fieldNames, true);
    indexType i = 0;
    indexType dstIndex = 0;
    if (diagonalOrder < 0) {
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < length; i++) {
            dstIndex = -diagonalOrder + i * (M + 1);
            src.copyElements(i, rp, dstIndex, 1);
        }
    } else {
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < length; i++) {
            dstIndex = diagonalOrder * M + i * (M + 1);
            src.copyElements(i, rp, dstIndex, 1);
        }
    }
    return ArrayOf(src.dp->dataClass, dims, rp, false, src.dp->fieldNames);
}
//=============================================================================
ArrayOf
ArrayOf::emptyConstructor(const Dimensions& dim, bool bIsSparse)
{
    if (dim.getElementCount() == 0) {
        return ArrayOf(NLS_DOUBLE, dim, nullptr, bIsSparse);
    }
    Error(_W("Invalid dimensions."));

    return ArrayOf();
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

    return ArrayOf();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
