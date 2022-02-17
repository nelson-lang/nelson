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
#include "h5ReadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Dimensions
getDimensions(hid_t space_id, int& rank)
{
    Dimensions dims;
    if (space_id > 0) {
        rank = H5Sget_simple_extent_ndims(space_id);
        if (rank > 0) {
            hsize_t* len = new_with_exception<hsize_t>((size_t)rank, false);
            int ret = H5Sget_simple_extent_dims(space_id, len, nullptr);
            hsize_t i = rank - 1;
            hsize_t j = 0;
            while (i > j) {
                hsize_t temp = len[i];
                len[i] = len[j];
                len[j] = temp;
                i--;
                j++;
            }
            for (indexType i = 0; i < static_cast<indexType>(rank); i++) {
                dims[i] = (indexType)len[i];
            }
            delete[] len;
        } else if (rank == 0) {
            dims[0] = 1;
            dims[1] = 1;
        }
    } else {
        rank = -1;
    }
    if (dims.getLength() == 1) {
        dims[1] = 1;
    }
    if (dims.getLength() == 0) {
        dims[0] = 0;
        dims[1] = 0;
    }
    return dims;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
