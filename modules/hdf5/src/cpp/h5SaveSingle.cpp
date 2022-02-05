//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "h5SaveSingle.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
h5SaveSingleEmptyMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue);
//=============================================================================
static bool
h5SaveSingleMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression);
//=============================================================================
bool
h5SaveSingle(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    if (VariableValue.isEmpty(false)) {
        bSuccess = h5SaveSingleEmptyMatrix(fid, location, variableName, VariableValue);
    } else {
        bSuccess = h5SaveSingleMatrix(fid, location, variableName, VariableValue, useCompression);
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveSingleEmptyMatrix(hid_t fid, const std::string& location, const std::string& variableName,
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

    single value = 0;
    hid_t type_id = H5Tcopy(H5T_NATIVE_FLOAT);
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
        if (VariableValue.isComplex()) {
            bSuccess = h5SaveComplexAttribute(fid, h5path);
        }
        if (bSuccess) {
            bSuccess = h5SaveDimensionsAttribute(fid, h5path, VariableValue.getDimensions());
        }
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveSingleMatrix(hid_t fid, const std::string& location, const std::string& variableName,
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
    hid_t type_id = H5Tcopy(H5T_NATIVE_FLOAT);
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
    void* buffer = const_cast<void*>(VariableValue.getDataPointer());
    if (VariableValue.isComplex()) {
        typedef struct complex_type
        {
            single r;
            single i;
        } complex_type;
        hid_t compoundId = H5Tcreate(H5T_COMPOUND, sizeof(singlecomplex));
        H5Tinsert(compoundId, "real", HOFFSET(complex_type, r), H5T_NATIVE_FLOAT);
        H5Tinsert(compoundId, "imag", HOFFSET(complex_type, i), H5T_NATIVE_FLOAT);

        hid_t dataset_id = H5Dcreate(
            fid, h5path.c_str(), compoundId, dspace_id, H5P_DEFAULT, plist, H5P_DEFAULT);
        herr_t status = H5Dwrite(dataset_id, compoundId, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
        H5Pclose(plist);
        H5Dclose(dataset_id);
        H5Sclose(dspace_id);
        if (status < 0) {
            bSuccess = false;
        } else {
            bSuccess = h5SaveComplexAttribute(fid, h5path);
        }
    } else {
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
