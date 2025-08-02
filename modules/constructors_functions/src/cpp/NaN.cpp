//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "omp_for_loop.hpp"
#include "NaN.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
NaN()
{
    return NaN(1, 1);
}
//=============================================================================
ArrayOf
NaN(indexType m, indexType n)
{
    Dimensions dims(m, n);
    return NaN(dims);
}
//=============================================================================
ArrayOf
NaN(Dimensions& dims)
{
    dims.simplify();
    indexType nbElements = dims.getElementCount();
    double* mat = nullptr;
    if (nbElements != 0) {
        mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, nbElements, stringVector(), false));
        double value = std::nan("NaN");
        if (nbElements == 1) {
            mat[0] = value;
        } else {
#if WITH_OPENMP
            OMP_PARALLEL_FOR_LOOP(nbElements)
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
                mat[k] = value;
            }
#else
            std::fill_n(mat, nbElements, value);
#endif
        }
    }
    return ArrayOf(NLS_DOUBLE, dims, mat, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
