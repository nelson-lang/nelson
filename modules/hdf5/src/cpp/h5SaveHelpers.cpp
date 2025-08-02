//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5SaveHelpers.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveClassAttribute(hid_t fid, const std::string& location, const ArrayOf& variableValue)
{
    std::string value = ClassName(variableValue);
    if (value == "sparsedouble") {
        value = "double";
    }
    if (value == "sparselogical") {
        value = "logical";
    }
    return h5SaveStringAttribute(fid, location, NELSON_CLASS_STR, value);
}
//=============================================================================
bool
h5SaveDimensionsAttribute(hid_t fid, const std::string& location, const Dimensions& dims)
{
    bool bSuccess = false;
    hid_t obj_id = H5Oopen(fid, location.c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        return false;
    }
    void* ptr = ArrayOf::allocateArrayOf(NLS_UINT64, dims.getLength(), stringVector(), false);
    ArrayOf dimsAsArray;
    if (dims.isScalar()) {
        dimsAsArray = ArrayOf(NLS_UINT64, Dimensions(1, 2), ptr);
    } else {
        dimsAsArray = ArrayOf(NLS_UINT64, Dimensions(1, dims.getLength()), ptr);
    }
    auto* ptrAsUint64 = (uint64*)dimsAsArray.getDataPointer();
    if (dims.isScalar()) {
        ptrAsUint64[0] = 1;
        ptrAsUint64[1] = 1;
    } else {
        Dimensions _dims(dims);
        auto* ptrAsUint64 = (uint64*)dimsAsArray.getDataPointer();
        for (indexType k = 0; k < _dims.getLength(); k++) {
            ptrAsUint64[k] = static_cast<uint64>(_dims[k]);
        }
    }

    hid_t type_id = H5Tcopy(H5T_NATIVE_UINT64);
    void* buffer = nullptr;
    hid_t dspace_id = H5I_INVALID_HID;

    hsize_t* dimsAsHsize_t = nullptr;
    try {
        dimsAsHsize_t = new_with_exception<hsize_t>(2, true);
    } catch (Exception&) {
        return false;
    }
    dimsAsHsize_t[0] = 1;
    dimsAsHsize_t[1] = dims.getLength();
    dspace_id = H5Screate_simple((int)2, dimsAsHsize_t, dimsAsHsize_t);
    delete[] dimsAsHsize_t;

    hid_t att_id
        = H5Acreate(obj_id, NELSON_DIMENSIONS_STR, type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT);
    if (att_id > 0) {
        buffer = const_cast<void*>(dimsAsArray.getDataPointer());
        herr_t status = H5Awrite(att_id, type_id, buffer);
        if (status < 0) {
            bSuccess = false;
        } else {
            bSuccess = true;
        }
        H5Aclose(att_id);
    }
    H5Sclose(dspace_id);
    H5Oclose(obj_id);
    return bSuccess;
}
//=============================================================================
bool
h5SaveEmptyAttribute(hid_t fid, const std::string& location, bool isEmpty)
{
    return h5SaveUint8Attribute(fid, location, NELSON_EMPTY_STR, isEmpty ? uint8(1) : uint8(0));
}
//=============================================================================
bool
h5SaveComplexAttribute(hid_t fid, const std::string& location, bool isComplex)
{
    return h5SaveUint8Attribute(fid, location, NELSON_COMPLEX_STR, isComplex ? uint8(1) : uint8(0));
}
//=============================================================================
bool
h5SaveSparseAttribute(hid_t fid, const std::string& location, bool isSparse)
{
    return h5SaveUint8Attribute(fid, location, NELSON_SPARSE_STR, isSparse ? uint8(1) : uint8(0));
}
//=============================================================================
bool
h5SaveStringAttribute(hid_t fid, const std::string& location, const std::string& attributeName,
    const std::string& content)
{
    bool bSuccess = false;
    hid_t obj_id = H5Oopen(fid, location.c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        return false;
    }
    herr_t status;
    hid_t dspace_id = H5Screate(H5S_SCALAR);
    hid_t type_id = H5Tcopy(H5T_C_S1);
    H5Tset_size(type_id, content.length());
    H5Tset_strpad(type_id, H5T_STR_NULLTERM);
    void* buffer = const_cast<void*>(static_cast<const void*>(content.c_str()));
    hid_t att_id
        = H5Acreate(obj_id, attributeName.c_str(), type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT);
    if (att_id > 0) {
        status = H5Awrite(att_id, type_id, buffer);
        H5Aclose(att_id);
        bSuccess = true;
    }
    H5Oclose(obj_id);
    H5Sclose(dspace_id);
    return bSuccess;
}
//=============================================================================
bool
h5SaveUint8Attribute(
    hid_t fid, const std::string& location, const std::string& attributeName, uint8 content)
{
    bool bSuccess = false;
    hid_t obj_id = H5Oopen(fid, location.c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        return false;
    }
    herr_t status;
    hid_t dspace_id = H5Screate(H5S_SCALAR);
    hid_t type_id = H5Tcopy(H5T_NATIVE_UINT8);
    hid_t att_id
        = H5Acreate(obj_id, attributeName.c_str(), type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT);
    if (att_id > 0) {
        void* buffer = &content;
        status = H5Awrite(att_id, type_id, buffer);
        if (status < 0) {
            bSuccess = false;
        } else {
            bSuccess = true;
        }
        H5Aclose(att_id);
    }
    H5Oclose(obj_id);
    H5Sclose(dspace_id);
    return bSuccess;
}
//=============================================================================
bool
h5SaveUint64Attribute(
    hid_t fid, const std::string& location, const std::string& attributeName, uint64 content)
{
    bool bSuccess = false;
    hid_t obj_id = H5Oopen(fid, location.c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        return false;
    }
    herr_t status;
    hid_t dspace_id = H5Screate(H5S_SCALAR);
    hid_t type_id = H5Tcopy(H5T_NATIVE_UINT64);
    hid_t att_id
        = H5Acreate(obj_id, attributeName.c_str(), type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT);
    if (att_id > 0) {
        void* buffer = &content;
        status = H5Awrite(att_id, type_id, buffer);
        if (status < 0) {
            bSuccess = false;
        } else {
            bSuccess = true;
        }
        H5Aclose(att_id);
    }
    H5Oclose(obj_id);
    H5Sclose(dspace_id);
    return bSuccess;
}
//=============================================================================
bool
h5LDeleteIfExists(hid_t fid, const std::string& location)
{
    htri_t exists = H5Lexists(fid, location.c_str(), H5P_DEFAULT);
    if (exists) {
        herr_t status = H5Ldelete(fid, location.c_str(), H5P_DEFAULT);
        return status >= 0 ? true : false;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
