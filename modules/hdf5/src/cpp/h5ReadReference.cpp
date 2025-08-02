//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadReference.hpp"
#include "h5ReadHelpers.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
h5ReadReferenceFloat(hid_t dset2, hid_t space2, hid_t mtype, bool asAttribute, std::wstring& error)
{
    ArrayOf element;
    hsize_t sizeSType = H5Tget_size(mtype);
    NelsonType outputClass;
    hid_t dataType;
    switch (sizeSType) {
    case 4: {
        outputClass = NLS_SINGLE;
        dataType = H5Tcopy(H5T_NATIVE_FLOAT);
    } break;
    case 8: {
        outputClass = NLS_DOUBLE;
        dataType = H5Tcopy(H5T_NATIVE_DOUBLE);
    } break;
    default: {
        error = _W("Type not managed.");
        return {};
    } break;
    }
    hssize_t npoints = H5Sget_select_npoints(space2);
    int rank = 1;
    Dimensions dims2((indexType)npoints, 1);
    hid_t memspace = H5Screate_simple(rank, (hsize_t*)&npoints, nullptr);
    void* rdata2 = ArrayOf::allocateArrayOf(outputClass, (indexType)npoints, stringVector(), false);
    element = ArrayOf(outputClass, dims2, rdata2);
    herr_t status = H5Dread(dset2, dataType, memspace, space2, H5P_DEFAULT, rdata2);
    H5Sclose(memspace);
    if (status < 0) {
        error = _W("Cannot read attribute.");
        return {};
    }
    return element;
}
//=============================================================================
static ArrayOf
h5ReadReferenceInteger(
    hid_t dset2, hid_t space2, hid_t mtype, bool asAttribute, std::wstring& error)
{
    ArrayOf element;
    hsize_t sizeType = H5Tget_size(mtype);
    NelsonType outputClass;
    hid_t dataType;
    switch (sizeType) {
    case 1: {
        if (H5Tget_sign(mtype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
            dataType = H5Tcopy(H5T_NATIVE_UINT8);
        } else {
            outputClass = NLS_INT8;
            dataType = H5Tcopy(H5T_NATIVE_INT8);
        }
    } break;
    case 2: {
        if (H5Tget_sign(mtype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT16;
            dataType = H5Tcopy(H5T_NATIVE_UINT16);
        } else {
            outputClass = NLS_INT16;
            dataType = H5Tcopy(H5T_NATIVE_INT16);
        }
    } break;
    case 4: {
        if (H5Tget_sign(mtype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT32;
            dataType = H5Tcopy(H5T_NATIVE_UINT32);
        } else {
            outputClass = NLS_INT32;
            dataType = H5Tcopy(H5T_NATIVE_INT32);
        }
    } break;
    case 8: {
        if (H5Tget_sign(mtype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT64;
            dataType = H5Tcopy(H5T_NATIVE_UINT64);
        } else {
            outputClass = NLS_INT64;
            dataType = H5Tcopy(H5T_NATIVE_INT64);
        }
    } break;
    default: {
        error = _W("Type not managed.");
        return {};
    } break;
    }
    hssize_t npoints = H5Sget_select_npoints(space2);
    int rank = 1;
    Dimensions dims2((indexType)npoints, 1);
    hid_t memspace = H5Screate_simple(rank, (hsize_t*)&npoints, nullptr);
    void* rdata2 = ArrayOf::allocateArrayOf(outputClass, (indexType)npoints, stringVector(), false);
    element = ArrayOf(outputClass, dims2, rdata2);
    herr_t status = H5Dread(dset2, dataType, memspace, space2, H5P_DEFAULT, rdata2);
    H5Sclose(memspace);
    if (status < 0) {
        error = _W("Cannot read attribute.");
        return {};
    }
    return element;
}
//=============================================================================
ArrayOf
h5ReadReference(hid_t attr_id, hid_t type, hid_t aspace, bool asAttribute, std::wstring& error)
{
    hsize_t storageSize = H5I_INVALID_HID;
    if (asAttribute) {
        storageSize = H5Aget_storage_size(attr_id);
    } else {
        storageSize = H5Dget_storage_size(attr_id);
    }
    hsize_t sizeType = H5Tget_size(type);
    int rank;
    Dimensions dims = getDimensions(aspace, rank);
    if ((rank == 0) && (storageSize == 0)) {
        dims = Dimensions(0, 0);
    }
    ArrayOf* elements = nullptr;
    try {
        elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return {};
    }

    hdset_reg_ref_t* rdata = nullptr;
    try {
        rdata = new_with_exception<hdset_reg_ref_t>(dims.getElementCount(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return {};
    }
    herr_t status = H5I_INVALID_HID;
    if (asAttribute) {
        status = H5Aread(attr_id, H5T_STD_REF_DSETREG, rdata);
    } else {
        status = H5Dread(attr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    }
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    indexType elementCount = dims.getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
#if H5_VERS_MAJOR <= 1 && H5_VERS_MINOR < 9
        hid_t dset2 = H5Rdereference(attr_id, H5R_DATASET_REGION, &rdata[k]);
#else
        hid_t dset2 = H5Rdereference2(attr_id, H5P_DEFAULT, H5R_DATASET_REGION, &rdata[k]);
#endif
        hid_t space2 = H5Rget_region(attr_id, H5R_DATASET_REGION, &rdata[k]);
        hid_t mtype = H5Dget_type(dset2);
        switch (H5Tget_class(mtype)) {
        case H5T_INTEGER: {
            elements[k] = h5ReadReferenceInteger(dset2, space2, mtype, asAttribute, error);
        } break;
        case H5T_FLOAT: {
            elements[k] = h5ReadReferenceFloat(dset2, space2, mtype, asAttribute, error);
        } break;
        default: {
            error = _W("Type not managed.");
        } break;
        }
        H5Tclose(mtype);
        H5Sclose(space2);
        H5Dclose(dset2);
        if (!error.empty()) {
            delete[] rdata;
            return {};
        }
    }
    delete[] rdata;
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
