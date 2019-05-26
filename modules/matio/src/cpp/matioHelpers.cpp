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
