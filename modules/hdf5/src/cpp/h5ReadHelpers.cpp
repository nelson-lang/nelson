//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadHelpers.hpp"
#include "NewWithException.hpp"
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
            H5Sget_simple_extent_dims(space_id, len, nullptr);
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
