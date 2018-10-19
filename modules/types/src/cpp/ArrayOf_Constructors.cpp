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
ArrayOf::diagonalConstructor(ArrayOf src, int diagonalOrder)
{
    ArrayOf retval;
    if (!src.isVector()) {
        Error(_W("Argument to diagonal constructor must by a vector!"));
    }
    indexType length = src.getLength();
    indexType M = 0;
    // Calculate the size of the output matrix (square of size outLen +
    // abs(diagonalOrder)).
    M = length + abs(diagonalOrder);
    Dimensions dims;
    dims[0] = M;
    dims[1] = M;
    // Allocate space for the output
    void* rp = allocateArrayOf(src.dp->dataClass, dims.getElementCount(), src.dp->fieldNames);
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
ArrayOf::emptyConstructor(Dimensions& dim, bool bIsSparse)
{
    if (dim.getElementCount() == 0) {
        return ArrayOf(NLS_DOUBLE, dim, NULL, bIsSparse);
    } else {
        Error(_W("Invalid dimensions."));
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
ArrayOf::emptyConstructor(indexType m, indexType n, bool bIsSparse)
{
    if (((m == 0) && (n == 0)) || ((m == 0) && (n != 0)) || ((m != 0) && (n == 0))) {
        Dimensions dim(m, n);
        return ArrayOf(NLS_DOUBLE, dim, NULL, bIsSparse);
    } else {
        Error(_W("Invalid dimensions."));
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
