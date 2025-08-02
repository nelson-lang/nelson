//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadArray.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
h5ReadArrayFloat(hid_t attr_id, hid_t type, hid_t stype, const Dimensions& dimsOutput,
    bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    NelsonType outputClass;
    void* ptrVoid = nullptr;
    if (H5Tequal(type, H5T_NATIVE_FLOAT) == 0) {
        outputClass = NLS_SINGLE;
    } else if (H5Tequal(type, H5T_NATIVE_DOUBLE) == 0) {
        outputClass = NLS_DOUBLE;
    } else {
        error = _W("Type not managed.");
        return {};
    }
    if (dimsOutput.isEmpty(false)) {
        res = ArrayOf::emptyConstructor(dimsOutput);
        res.promoteType(outputClass);
    } else {
        ptrVoid = ArrayOf::allocateArrayOf(
            outputClass, dimsOutput.getElementCount(), stringVector(), false);
    }
    herr_t status = H5I_INVALID_HID;
    if (asAttribute) {
        status = H5Aread(attr_id, type, ptrVoid);
    } else {
        status = H5Dread(attr_id, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, ptrVoid);
    }
    if (status < 0) {
        if (asAttribute) {
            error = _W("Cannot read attribute.");
        } else {
            error = _W("Cannot read data set.");
        }
        ArrayOf(outputClass, dimsOutput, ptrVoid);
        res = ArrayOf();
    } else {
        res = ArrayOf(outputClass, dimsOutput, ptrVoid);
    }
    return res;
}
//=============================================================================
static ArrayOf
h5ReadArrayInteger(hid_t attr_id, hid_t type, hid_t stype, const Dimensions& dimsOutput,
    bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    NelsonType outputClass;
    void* ptrVoid = nullptr;
    if (H5Tequal(type, H5T_NATIVE_INT64) == 0) {
        outputClass = NLS_INT64;
    } else if (H5Tequal(type, H5T_NATIVE_INT32) == 0) {
        outputClass = NLS_INT32;
    } else if (H5Tequal(type, H5T_NATIVE_INT16) == 0) {
        outputClass = NLS_INT16;
    } else if (H5Tequal(type, H5T_NATIVE_INT8) == 0) {
        outputClass = NLS_INT8;
    } else if (H5Tequal(type, H5T_NATIVE_UINT64) == 0) {
        outputClass = NLS_UINT64;
    } else if (H5Tequal(type, H5T_NATIVE_UINT32) == 0) {
        outputClass = NLS_UINT32;
    } else if (H5Tequal(type, H5T_NATIVE_UINT16) == 0) {
        outputClass = NLS_UINT16;
    } else if (H5Tequal(type, H5T_NATIVE_UINT8) == 0) {
        outputClass = NLS_UINT8;
    } else {
        outputClass = NLS_INT8;
    }
    if (dimsOutput.isEmpty(false)) {
        res = ArrayOf::emptyConstructor(dimsOutput);
        res.promoteType(outputClass);
    } else {
        ptrVoid = ArrayOf::allocateArrayOf(
            outputClass, dimsOutput.getElementCount(), stringVector(), false);
    }
    herr_t status = H5I_INVALID_HID;

    if (asAttribute) {
        status = H5Aread(attr_id, type, ptrVoid);
    } else {
        status = H5Dread(attr_id, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, ptrVoid);
    }

    if (status < 0) {
        if (asAttribute) {
            error = _W("Cannot read attribute.");
        } else {
            error = _W("Cannot read data set.");
        }
        ArrayOf(outputClass, dimsOutput, ptrVoid);
        res = ArrayOf();
    } else {
        res = ArrayOf(outputClass, dimsOutput, ptrVoid);
    }
    return res;
}
//=============================================================================
ArrayOf
h5ReadArray(hid_t attr_id, hid_t type, hid_t aspace, bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5I_INVALID_HID;
    if (asAttribute) {
        storageSize = H5Aget_storage_size(attr_id);
    } else {
        storageSize = H5Dget_storage_size(attr_id);
    }
    hsize_t sizeType = H5Tget_size(type);
    size_t numVal = (size_t)(storageSize / sizeType);

    int ndims = H5Tget_array_ndims(type);
    hsize_t* dimsAsHsize;
    try {
        dimsAsHsize = new_with_exception<hsize_t>((size_t)ndims, false);
    } catch (Exception& e) {
        error = e.getMessage();
        H5Sclose(aspace);
        return res;
    }
    H5Tget_array_dims(type, dimsAsHsize);
    Dimensions dimsOutput;
    size_t i = (size_t)ndims - 1;
    hsize_t j = 0;
    while (i > j) {
        hsize_t temp = dimsAsHsize[i];
        dimsAsHsize[i] = dimsAsHsize[j];
        dimsAsHsize[j] = temp;
        i--;
        j++;
    }
    for (indexType k = 0; k < ndims; k++) {
        dimsOutput[k] = (indexType)dimsAsHsize[k];
    }
    delete[] dimsAsHsize;
    dimsOutput[ndims] = numVal;

    hid_t stype = H5Tget_super(type);
    switch (H5Tget_class(stype)) {
    case H5T_INTEGER: {
        res = h5ReadArrayInteger(attr_id, type, stype, dimsOutput, asAttribute, error);
    } break;
    case H5T_FLOAT: {
        res = h5ReadArrayFloat(attr_id, type, stype, dimsOutput, asAttribute, error);
    } break;
    default: {
        error = _W("Type not managed.");
    } break;
    }
    H5Tclose(stype);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
