//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadFloat.hpp"
#include "h5ReadHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadFloat(hid_t dset_id, hid_t type_id, hid_t dspace_id, bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize;
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
    NelsonType outputClass;
    if (sizeType == 4) {
        outputClass = NLS_SINGLE;
    } else if (sizeType == 8) {
        outputClass = NLS_DOUBLE;
    } else {
        error = _W("Type not managed.");
        return res;
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
            return {};
        }

        hid_t memspace = H5I_INVALID_HID;
        if (!asAttribute) {
            hsize_t* h5_dims = nullptr;
            hsize_t* h5_maxdims = nullptr;
            try {
                h5_dims
                    = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
            } catch (Exception& e) {
                error = e.getMessage();
                return {};
            }
            try {
                h5_maxdims
                    = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
            } catch (Exception& e) {
                error = e.getMessage();
                return {};
            }
            if (H5Sget_simple_extent_dims(dspace_id, h5_dims, h5_maxdims) < 0) {
                delete[] h5_dims;
                delete[] h5_maxdims;
                Error("Impossible to read dimensions and maximum size of data set.");
                return {};
            }
            memspace = H5Screate_simple(rank, h5_dims, nullptr);
            delete[] h5_dims;
            delete[] h5_maxdims;
        }

        herr_t status = H5I_INVALID_HID;
        if (asAttribute) {
            status = H5Aread(dset_id, type_id, ptr);
        } else {
            status = H5Dread(dset_id, type_id, memspace, dspace_id, H5P_DEFAULT, ptr);
        }
        if (status < 0) {
            ArrayOf(outputClass, dims, ptr);
            res = ArrayOf();
            if (asAttribute) {
                error = _W("Cannot read attribute.");
            } else {
                H5Sclose(memspace);
                error = _W("Cannot read data set.");
            }
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
