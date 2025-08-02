//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadVlen.hpp"
#include "h5ReadHelpers.hpp"
#include "NewWithException.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
h5ReadVlenOpaque(
    hid_t attr_id, hid_t stype, const hvl_t* rdata, const Dimensions& dims, std::wstring& error)
{
    ArrayOf* elements = nullptr;
    NelsonType outputClass = NLS_UINT8;
    try {
        elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return {};
    }
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    indexType elementCount = dims.getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
        Dimensions dimsVector(rdata[k].len, 1);
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(outputClass, rdata[k].len, stringVector(), false);
            elements[k] = ArrayOf(outputClass, dimsVector, ptr);
        } catch (Exception& e) {
            error = e.getMessage();
            return {};
        }
        memcpy(ptr, rdata[k].p, elements[k].getElementSize() * rdata[k].len);
    }
    return res;
}
//=============================================================================
static ArrayOf
h5ReadVlenFloat(
    hid_t attr_id, hid_t stype, const hvl_t* rdata, const Dimensions& dims, std::wstring& error)
{
    ArrayOf* elements = nullptr;
    hsize_t sizeSType = H5Tget_size(stype);
    NelsonType outputClass;
    switch (sizeSType) {
    case 4: {
        outputClass = NLS_SINGLE;
    } break;
    case 8: {
        outputClass = NLS_DOUBLE;
    } break;
    default: {
        error = _W("Type not managed.");
        return {};
    } break;
    }
    try {
        elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return {};
    }
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    indexType elementCount = dims.getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
        Dimensions dimsVector(rdata[k].len, 1);
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(outputClass, rdata[k].len, stringVector(), false);
            elements[k] = ArrayOf(outputClass, dimsVector, ptr);
        } catch (Exception& e) {
            error = e.getMessage();
            return {};
        }
        memcpy(ptr, rdata[k].p, elements[k].getElementSize() * rdata[k].len);
    }
    return res;
}
//=============================================================================
static ArrayOf
h5ReadVlenInteger(
    hid_t attr_id, hid_t stype, const hvl_t* rdata, const Dimensions& dims, std::wstring& error)
{
    ArrayOf* elements = nullptr;
    hsize_t sizeSType = H5Tget_size(stype);
    NelsonType outputClass;
    switch (sizeSType) {
    case 1: {
        if (H5Tget_sign(stype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    case 2: {
        if (H5Tget_sign(stype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT16;
        } else {
            outputClass = NLS_INT16;
        }
    } break;
    case 4: {
        if (H5Tget_sign(stype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT32;
        } else {
            outputClass = NLS_INT32;
        }
    } break;
    case 8: {
        if (H5Tget_sign(stype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT64;
        } else {
            outputClass = NLS_INT64;
        }
    } break;
    default: {
        error = _W("Type not managed.");
        return {};
    } break;
    }
    try {
        elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return {};
    }
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    indexType elementCount = dims.getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
        Dimensions dimsVector(rdata[k].len, 1);
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(outputClass, rdata[k].len, stringVector(), false);
            elements[k] = ArrayOf(outputClass, dimsVector, ptr);
        } catch (Exception& e) {
            error = e.getMessage();
            return {};
        }
        memcpy(ptr, rdata[k].p, elements[k].getElementSize() * rdata[k].len);
    }
    return res;
}
//=============================================================================
static ArrayOf
h5ReadVlenBitfield(
    hid_t attr_id, hid_t stype, const hvl_t* rdata, const Dimensions& dims, std::wstring& error)
{
    return h5ReadVlenInteger(attr_id, stype, rdata, dims, error);
}
//=============================================================================
ArrayOf
h5ReadVlen(hid_t attr_id, hid_t type, hid_t aspace, bool asAttribute, std::wstring& error)
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
    hvl_t* rdata = nullptr;
    try {
        rdata = (hvl_t*)new_with_exception<hvl_t>(dims.getElementCount(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return {};
    }
    hid_t memspace = H5I_INVALID_HID;
    if (!asAttribute) {
        hsize_t* h5_dims = nullptr;
        hsize_t* h5_maxdims = nullptr;
        try {
            h5_dims = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
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
        if (H5Sget_simple_extent_dims(aspace, h5_dims, h5_maxdims) < 0) {
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
        status = H5Aread(attr_id, type, rdata);
    } else {
        status = H5Dread(attr_id, type, memspace, aspace, H5P_DEFAULT, rdata);
    }

    if (status < 0) {
        if (asAttribute) {
            error = _W("Cannot read attribute.");
        } else {
            H5Sclose(memspace);
            error = _W("Cannot read data set.");
        }
        return {};
    }

    hid_t stype = H5Tget_super(type);
    switch (H5Tget_class(stype)) {
    case H5T_INTEGER: {
        return h5ReadVlenInteger(attr_id, stype, rdata, dims, error);
    } break;
    case H5T_FLOAT: {
        return h5ReadVlenFloat(attr_id, stype, rdata, dims, error);
    } break;
    case H5T_BITFIELD: {
        return h5ReadVlenBitfield(attr_id, stype, rdata, dims, error);
    } break;
    case H5T_OPAQUE: {
        return h5ReadVlenOpaque(attr_id, stype, rdata, dims, error);
    } break;
    default: {
        error = _W("Type not managed.");
    } break;
    }
    if (!asAttribute) {
        H5Sclose(memspace);
    }
    H5Tclose(stype);
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
