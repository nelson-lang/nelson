//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// [M, I] = max(A, [], dim) or min(A, [], dim) for an empty A (dim == 0: first
// non-singleton dimension). A reduction over a dimension of length 0 has no
// value to return, so M keeps the size of A; any other operating dimension is
// reduced to 1 and M stays empty. I is an empty double array of the size of M.
inline ArrayOfVector
ExtremumOfEmpty(const ArrayOf& A, indexType dim, int nLhs)
{
    Dimensions outDim(A.dimensions());
    indexType workDim = 0;
    if (dim == 0) {
        while (workDim < outDim.length() && outDim.dimensionLength(workDim) == 1) {
            workDim++;
        }
    } else {
        workDim = dim - 1;
    }
    if (workDim < outDim.length() && outDim.dimensionLength(workDim) != 0) {
        outDim.setDimensionLength(workDim, 1);
        outDim.simplify();
    }
    // the extremum of no elements is an empty real array
    ArrayOf M(decomplexify(A));
    M.reshape(outDim);
    ArrayOfVector retval;
    retval << M;
    if (nLhs > 1) {
        retval << ArrayOf::empty(outDim);
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
