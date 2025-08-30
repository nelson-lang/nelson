//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5SaveString.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5SaveVariable.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
h5SaveCharacterEmptyMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue);
//=============================================================================
static bool
h5SaveCharacterMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression);
//=============================================================================
bool
h5SaveStringArray(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    h5LDeleteIfExists(fid, h5path);
    hid_t gcpl = H5Pcreate(H5P_GROUP_CREATE);
    hid_t group = H5Gcreate(fid, h5path.c_str(), H5P_DEFAULT, gcpl, H5P_DEFAULT);
    herr_t status = H5Gclose(group);
    if (status < 0) {
        return false;
    }
    Dimensions dims = VariableValue.getDimensions();
    auto* elements = (ArrayOf*)VariableValue.getDataPointer();
    indexType elementCount = dims.getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
        ArrayOf element = elements[k];
        std::string name = std::to_string(k);
        if (element.isCharacterArray()) {
            bSuccess = h5SaveCharacterArray(
                fid, h5path + std::string("/"), name, element, useCompression);
        } else {
            ArrayOf v = ArrayOf::doubleConstructor(std::nan(""));
            bSuccess = h5SaveVariable(fid, h5path + std::string("/"), name, v, useCompression);
        }
        if (!bSuccess) {
            return false;
        }
    }
    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveDimensionsAttribute(fid, h5path, dims);
    if (!bSuccess) {
        return false;
    }
    if (dims.isEmpty(false)) {
        bSuccess = h5SaveEmptyAttribute(fid, h5path);
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveCharacterArray(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    if (VariableValue.isEmpty(false)) {
        bSuccess = h5SaveCharacterEmptyMatrix(fid, location, variableName, VariableValue);
    } else {
        bSuccess
            = h5SaveCharacterMatrix(fid, location, variableName, VariableValue, useCompression);
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveCharacterEmptyMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    h5LDeleteIfExists(fid, h5path);

    uint16 value = 0;
    hid_t type_id = H5Tcopy(H5T_NATIVE_UINT16);
    hsize_t dimsAsHsize_t[1];
    dimsAsHsize_t[0] = 1;
    hid_t dspace_id = H5Screate_simple((int)1, dimsAsHsize_t, dimsAsHsize_t);
    if (dspace_id < 0) {
        return false;
    }

    hid_t dataset_id
        = H5Dcreate(fid, h5path.c_str(), type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dataset_id < 0) {
        return false;
    }
    herr_t status = H5Dwrite(dataset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);

    H5Dclose(dataset_id);
    H5Sclose(dspace_id);
    if (status < 0) {
        return false;
    }
    bSuccess = h5SaveEmptyAttribute(fid, h5path);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (bSuccess) {
        bSuccess = h5SaveDimensionsAttribute(fid, h5path, VariableValue.getDimensions());
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveCharacterMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    h5LDeleteIfExists(fid, h5path);

    hid_t dspace_id = H5I_INVALID_HID;
    hid_t type_id = H5Tcopy(H5T_NATIVE_UINT16);
    Dimensions dimsValue = VariableValue.getDimensions();

    hsize_t* dimsAsHsize_t = nullptr;
    indexType nbElementsSizeData;
    if (dimsValue.isScalar()) {
        try {
            dimsAsHsize_t = new_with_exception<hsize_t>(1, true);
        } catch (Exception&) {
            return false;
        }
        nbElementsSizeData = 1;
        dimsAsHsize_t[0] = 1;
        dspace_id = H5Screate_simple((int)1, dimsAsHsize_t, dimsAsHsize_t);
    } else {
        try {
            dimsAsHsize_t = new_with_exception<hsize_t>(dimsValue.getLength(), true);
        } catch (Exception&) {
            return false;
        }
        nbElementsSizeData = dimsValue.getLength();
        for (indexType k = 1; k <= nbElementsSizeData; k++) {
            dimsAsHsize_t[k - 1] = (hsize_t)dimsValue[nbElementsSizeData - k];
        }
        dspace_id = H5Screate_simple((int)dimsValue.getLength(), dimsAsHsize_t, dimsAsHsize_t);
    }
    delete[] dimsAsHsize_t;
    void* buffer = nullptr;
    ArrayOf asUint16;
    if (sizeof(charType) == sizeof(uint16)) {
        buffer = const_cast<void*>(VariableValue.getDataPointer());
    } else {
        asUint16 = VariableValue;
        asUint16.promoteType(NLS_UINT16);
        buffer = const_cast<void*>(asUint16.getDataPointer());
    }
    hid_t plist = setCompression(dimsValue, useCompression);
    hid_t dataset_id
        = H5Dcreate(fid, h5path.c_str(), type_id, dspace_id, H5P_DEFAULT, plist, H5P_DEFAULT);
    herr_t status = H5Dwrite(dataset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
    H5Pclose(plist);
    H5Dclose(dataset_id);
    H5Sclose(dspace_id);
    if (status < 0) {
        bSuccess = false;
    } else {
        bSuccess = true;
    }
    if (bSuccess) {
        bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
        if (!bSuccess) {
            return false;
        }
        bSuccess = h5SaveDimensionsAttribute(fid, h5path, dimsValue);
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
