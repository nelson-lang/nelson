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
#include "h5ReadOpaqueDataset.hpp"
#include "h5ReadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadBitfieldDataset(hid_t dset_id, hid_t type_id, hid_t dspace_id, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5Aget_storage_size(dset_id);
    hsize_t sizeType = H5Tget_size(type_id);
    int rank;
    Dimensions dims = getDimensions(dspace_id, rank);
    Class outputClass;
    hid_t dataType;
    switch (sizeType) {
    case 1: {
        outputClass = NLS_UINT8;
        dataType = H5Tcopy(H5T_NATIVE_B8);
    } break;
    case 2: {
        outputClass = NLS_UINT16;
        dataType = H5Tcopy(H5T_NATIVE_B16);
    } break;
    case 4: {
        outputClass = NLS_UINT32;
        dataType = H5Tcopy(H5T_NATIVE_B32);
    } break;
    case 8: {
        outputClass = NLS_UINT64;
        dataType = H5Tcopy(H5T_NATIVE_B64);
    } break;
    default: {
        error = _W("Type not managed.");
        return res;
    } break;
    }
    if (dims.isEmpty(false)) {
        res = ArrayOf::emptyConstructor(dims);
        res.promoteType(outputClass);
    } else {
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(
                outputClass, dims.getElementCount(), stringVector(), false);
        } catch (Exception& e) {
            error = e.getMessage();
            return ArrayOf();
        }
        hsize_t* h5_dims = nullptr;
        hsize_t* h5_maxdims = nullptr;
        try {
            h5_dims = (hsize_t*)new_with_exception<hsize_t>(rank * sizeof(hsize_t), false);
        } catch (Exception&) {
            throw;
        }
        try {
            h5_maxdims = (hsize_t*)new_with_exception<hsize_t>(rank * sizeof(hsize_t), false);
        } catch (Exception&) {
            throw;
        }

        if (H5Sget_simple_extent_dims(dspace_id, h5_dims, h5_maxdims) < 0) {
            delete[] h5_dims;
            delete[] h5_maxdims;
            Error("Impossible to read dimensions and maximum size of dataset.");
        }
        hid_t memspace = H5Screate_simple(rank, h5_dims, NULL);
        delete[] h5_dims;
        delete[] h5_maxdims;
        if (H5Dread(dset_id, dataType, memspace, dspace_id, H5P_DEFAULT, ptr) < 0) {
            res = ArrayOf(outputClass, dims, ptr);
            H5Sclose(memspace);
            res = ArrayOf();
            error = _W("Cannot read data set.");
        } else {
            res = ArrayOf(outputClass, dims, ptr);
            H5Sclose(memspace);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
