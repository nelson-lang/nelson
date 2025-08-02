//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ConcatenateNdArray.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ConcatenateNdArray(ArrayOfMatrix& m, NelsonType destClass)
{
    Dimensions mat_dims;
    Dimensions row_dims;
    Dimensions retDims;
    auto i = m.begin();
    bool firstNonzeroColumn = true;
    bool firstNonzeroRow = true;
    while (i != m.end()) {
        ArrayOfVector ptr = ArrayOfVector(*i);
        for (const auto& d : ptr) {
            if (!d.isEmpty()) {
                if (firstNonzeroColumn) {
                    row_dims = d.getDimensions();
                    firstNonzeroColumn = false;
                } else {
                    row_dims[1] += d.getDimensions()[1];
                }
            }
        }
        if (!firstNonzeroColumn) {
            if (firstNonzeroRow) {
                mat_dims = row_dims;
                firstNonzeroRow = false;
            } else {
                mat_dims[0] += row_dims[0];
            }
            firstNonzeroColumn = true;
        }
        ++i;
    }
    retDims = mat_dims;
    void* dstPtr
        = ArrayOf::allocateArrayOf(destClass, retDims.getElementCount(), stringVector(), true);
    indexType row_corner = 0;
    indexType column_corner = 0;
    indexType dim_count(mat_dims.getLength());
    Dimensions aptr(dim_count);
    Dimensions bptr(dim_count);
    indexType row_count;
    i = m.begin();
    while (i != m.end()) {
        ArrayOfVector ptr = *i;
        row_count = 0;
        for (auto d : ptr) {
            if (!d.isEmpty()) {
                aptr.zeroOut();
                indexType dstIndex;
                indexType srcIndex;
                bptr = d.getDimensions();
                row_count = d.getDimensions()[0];
                srcIndex = 0;
                while (aptr.inside(bptr)) {
                    dstIndex = mat_dims.mapPoint(aptr);
                    dstIndex += row_corner + column_corner * mat_dims[0];
                    d.copyElements(srcIndex, dstPtr, dstIndex, row_count);
                    aptr.incrementModulo(bptr, 1);
                    srcIndex += row_count;
                }
                column_corner += bptr[1];
            }
        }
        column_corner = 0;
        row_corner += row_count;
        ++i;
    }
    return ArrayOf(destClass, retDims, dstPtr, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
