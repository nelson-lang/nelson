//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadBitfield.hpp"
#include "h5ReadHelpers.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadBitfield(hid_t dset_id, hid_t type_id, hid_t dspace_id, bool asAttribute, std::wstring& error)
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
    Dimensions dims = dimensions(dspace_id, rank);
    if ((rank == 0) && (storageSize == 0)) {
        dims = Dimensions(0, 0);
    }
    NelsonType outputClass;
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
        error = _W("Type not supported.");
        return res;
    } break;
    }
    if (dims.isEmpty(false)) {
        res = ArrayOf::empty(dims);
        res.promoteType(outputClass);
    } else {
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(outputClass, dims.elementCount(), stringVector(), false);
        } catch (NelsonException& e) {
            error = e.message();
            return {};
        }
        hid_t memspace = H5I_INVALID_HID;
        herr_t status = H5I_INVALID_HID;
        if (asAttribute) {
            status = H5Aread(dset_id, dataType, ptr);
        } else {
            hsize_t* h5_dims = nullptr;
            hsize_t* h5_maxdims = nullptr;
            try {
                h5_dims
                    = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
            } catch (NelsonException&) {
                throw;
            }
            try {
                h5_maxdims
                    = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
            } catch (NelsonException&) {
                throw;
            }

            if (H5Sget_simple_extent_dims(dspace_id, h5_dims, h5_maxdims) < 0) {
                delete[] h5_dims;
                delete[] h5_maxdims;
                Error("Nelson:hdf5:readDimensionsFailed",
                    "Impossible to read dimensions and maximum size of data set.");
                return {};
            }
            memspace = H5Screate_simple(rank, h5_dims, nullptr);
            delete[] h5_dims;
            delete[] h5_maxdims;
            status = H5Dread(dset_id, dataType, memspace, dspace_id, H5P_DEFAULT, ptr);
        }
        if (status < 0) {
            ArrayOf(outputClass, dims, ptr);
            res = ArrayOf();
            if (asAttribute) {
                error = resolveMessageTemplate(
                    L"Nelson:hdf5:cannotReadAttribute", L"Cannot read attribute.");
            } else {
                error = resolveMessageTemplate(
                    L"Nelson:hdf5:cannotReadDataSet", L"Cannot read data set.");
                H5Sclose(memspace);
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
