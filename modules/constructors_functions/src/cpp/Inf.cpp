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
#include "Inf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Inf()
{
    return Inf(1, 1);
}
//=============================================================================
ArrayOf
Inf(indexType m, indexType n)
{
    Dimensions dims(m, n);
    return Inf(dims);
}
//=============================================================================
ArrayOf
Inf(Dimensions& dims)
{
    return Inf(dims, NLS_DOUBLE);
}
//=============================================================================
ArrayOf
Inf(Dimensions& dims, NelsonType destinationClass)
{
    dims.simplify();
    indexType nbElements = dims.elementCount();
    if (destinationClass == NLS_SINGLE) {
        single* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<single*>(
                ArrayOf::allocateArrayOf(NLS_SINGLE, nbElements, stringVector(), false));
            constexpr single value = std::numeric_limits<single>::infinity();
            std::fill_n(mat, nbElements, value);
        }
        return ArrayOf(NLS_SINGLE, dims, mat, false);
    }
    double* mat = nullptr;
    if (nbElements != 0) {
        mat = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, nbElements, stringVector(), false));
        constexpr double value = std::numeric_limits<double>::infinity();
        std::fill_n(mat, nbElements, value);
    }
    return ArrayOf(NLS_DOUBLE, dims, mat, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
