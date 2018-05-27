//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "ConcatenateNdArray.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ConcatenateNdArray(ArrayOfMatrix& m, Class destClass)
{
    Dimensions mat_dims;
    Dimensions row_dims;
    Dimensions retDims;
    ArrayOfMatrix::iterator i = m.begin();
    bool firstNonzeroColumn = true;
    bool firstNonzeroRow = true;
    while (i != m.end()) {
        ArrayOfVector ptr = (ArrayOfVector)*i;
        for (sizeType j = 0; j < (sizeType)ptr.size(); j++) {
            const ArrayOf& d = ptr[j];
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
    void* dstPtr = ArrayOf::allocateArrayOf(destClass, retDims.getElementCount());
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
        for (size_t j = 0; j < ptr.size(); j++) {
            ArrayOf d(ptr[j]);
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
}
//=============================================================================
