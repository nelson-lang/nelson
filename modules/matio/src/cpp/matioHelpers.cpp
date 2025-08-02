//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Dimensions
getMatVarDimensions(matvar_t* matVariable)
{
    Dimensions dims;
    if (matVariable) {
        for (int d = 0; d < matVariable->rank; d++) {
            dims[d] = matVariable->dims[d];
        }
    }
    return dims;
}
//=============================================================================
size_t*
convertDimensionsForMatVar(const Dimensions& variableDims, indexType& rank)
{
    rank = variableDims.getLength();
    size_t* dims = nullptr;
    try {
        dims = new size_t[rank];
    } catch (const std::bad_alloc&) {
        rank = 0;
        return nullptr;
    }
    Dimensions _dims(variableDims);
    for (indexType k = 0; k < rank; k++) {
        dims[k] = _dims[k];
    }
    return dims;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
