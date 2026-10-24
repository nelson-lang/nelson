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
    return NaN(dims, NLS_DOUBLE);
}
//=============================================================================
ArrayOf
NaN(Dimensions& dims, NelsonType destinationClass)
{
    dims.simplify();
    indexType nbElements = dims.elementCount();
    if (destinationClass == NLS_SINGLE) {
        single* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<single*>(
                ArrayOf::allocateArrayOf(NLS_SINGLE, nbElements, stringVector(), false));
            single value = std::nanf("NaN");
            std::fill_n(mat, nbElements, value);
        }
        return ArrayOf(NLS_SINGLE, dims, mat, false);
    }
    double* mat = nullptr;
    if (nbElements != 0) {
        mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, nbElements, stringVector(), false));
        double value = std::nan("NaN");
        std::fill_n(mat, nbElements, value);
    }
    return ArrayOf(NLS_DOUBLE, dims, mat, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
