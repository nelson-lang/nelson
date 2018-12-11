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
#include "h5ReadAttributeHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Dimensions
getDimensions(hid_t space_id) {
    Dimensions dims;
    indexType rank = H5Sget_simple_extent_ndims(space_id);
    if (rank > 0) {
        hsize_t* len = new_with_exception<hsize_t>(rank, false);
        int ret = H5Sget_simple_extent_dims(space_id, len, NULL);
        hsize_t i = rank - 1;
        hsize_t j = 0;
        while (i > j) {
            hsize_t temp = len[i];
            len[i] = len[j];
            len[j] = temp;
            i--;
            j++;
        }
        for (indexType i = 0; i < rank; i++) {
            dims[i] = len[i];
        }
        if (dims.getLength() == 1) {
            dims[1] = 1;
        }
        delete[] len;
    }
    return dims;
}
//=============================================================================
}  // namespace Nelson
//=============================================================================
