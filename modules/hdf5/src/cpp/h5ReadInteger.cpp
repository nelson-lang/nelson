//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "h5ReadInteger.hpp"
#include "h5ReadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadInteger(hid_t dset_id, hid_t type_id, hid_t dspace_id, bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5I_INVALID_HID;
    if (asAttribute) {
        storageSize = H5Aget_storage_size(dset_id);
    } else {
        storageSize = H5Dget_storage_size(dset_id);
    }
    hsize_t sizeType = H5Tget_size(type_id);
    int rank;
    Dimensions dims = getDimensions(dspace_id, rank);
    if ((rank == 0) && (storageSize == 0)) {
        dims = Dimensions(0, 0);
    }
    Class outputClass;
    hid_t dataType;
    switch (sizeType) {
    case 1: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
            dataType = H5Tcopy(H5T_NATIVE_UINT8);
        } else {
            outputClass = NLS_INT8;
            dataType = H5Tcopy(H5T_NATIVE_INT8);
        }
    } break;
    case 2: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT16;
            dataType = H5Tcopy(H5T_NATIVE_UINT16);
        } else {
            outputClass = NLS_INT16;
            dataType = H5Tcopy(H5T_NATIVE_INT16);
        }
    } break;
    case 4: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT32;
            dataType = H5Tcopy(H5T_NATIVE_UINT32);
        } else {
            outputClass = NLS_INT32;
            dataType = H5Tcopy(H5T_NATIVE_INT32);
        }
    } break;
    case 8: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT64;
            dataType = H5Tcopy(H5T_NATIVE_UINT64);
        } else {
            outputClass = NLS_INT64;
            dataType = H5Tcopy(H5T_NATIVE_INT64);
        }
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
        herr_t status = H5I_INVALID_HID;
        hid_t memspace = H5I_INVALID_HID;
        if (asAttribute) {
            status = H5Aread(dset_id, dataType, ptr);
        } else {
            hsize_t* h5_dims = nullptr;
            hsize_t* h5_maxdims = nullptr;
            try {
                h5_dims = (hsize_t*)new_with_exception<hsize_t>(rank * sizeof(hsize_t), false);
            } catch (Exception& e) {
                error = e.getMessage();
                return ArrayOf();
            }
            try {
                h5_maxdims = (hsize_t*)new_with_exception<hsize_t>(rank * sizeof(hsize_t), false);
            } catch (Exception& e) {
                delete[] h5_dims;
                error = e.getMessage();
                return ArrayOf();
            }
            if (H5Sget_simple_extent_dims(dspace_id, h5_dims, h5_maxdims) < 0) {
                delete[] h5_dims;
                delete[] h5_maxdims;
                error = _W("Impossible to read dimensions and maximum size of data set.");
                return ArrayOf();
            }
            memspace = H5Screate_simple(rank, h5_dims, NULL);
            delete[] h5_dims;
            delete[] h5_maxdims;

            status = H5Dread(dset_id, dataType, memspace, dspace_id, H5P_DEFAULT, ptr);
        }
        if (status < 0) {
            ArrayOf(outputClass, dims, ptr);
            if (asAttribute) {
                error = _W("Cannot read attribute.");
            } else {
                H5Sclose(memspace);
                error = _W("Cannot read data set.");
            }
            res = ArrayOf();
        } else {
            res = ArrayOf(outputClass, dims, ptr);
            if (!asAttribute) {
                H5Sclose(memspace);
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
