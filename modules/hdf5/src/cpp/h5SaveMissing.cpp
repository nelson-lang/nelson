//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Sparse>
#include "h5SaveMissing.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
h5SaveMissingEmptyMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue);
//=============================================================================
static bool
h5SaveMissingMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression);
//=============================================================================
bool
h5SaveMissing(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    if (VariableValue.isEmpty(false)) {
        bSuccess = h5SaveMissingEmptyMatrix(fid, location, variableName, VariableValue);
    } else {
        bSuccess = h5SaveMissingMatrix(fid, location, variableName, VariableValue, useCompression);
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveMissingEmptyMatrix(hid_t fid, const std::string& location, const std::string& variableName,
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

    double value = 0;
    hid_t type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
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
        if (bSuccess) {
            bSuccess = h5SaveDimensionsAttribute(fid, h5path, VariableValue.getDimensions());
        }
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveMissingMatrix(hid_t fid, const std::string& location, const std::string& variableName,
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
    hid_t type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
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

    hid_t plist = setCompression(dimsValue, useCompression);
    hid_t dataset_id
        = H5Dcreate(fid, h5path.c_str(), type_id, dspace_id, H5P_DEFAULT, plist, H5P_DEFAULT);
    H5Pclose(plist);
    H5Dclose(dataset_id);
    H5Sclose(dspace_id);
    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveDimensionsAttribute(fid, h5path, dimsValue);
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
