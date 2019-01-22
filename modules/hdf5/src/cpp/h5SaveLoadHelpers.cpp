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
#include "h5SaveLoadHelpers.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isNelsonH5File(hid_t fid)
{
    bool bSuccess = false;
    hid_t obj_id = H5Oopen(fid, "/", H5P_DEFAULT);
    if (obj_id < 0) {
        H5Fclose(fid);
        Error(_W("Specified HDF5 object location could not be opened."));
    }
    htri_t exists = H5Aexists(obj_id, NELSON_SCHEMA_STR);
    if (exists > 0) {
        H5Aclose(exists);
        bSuccess = true;
    }
    H5Oclose(obj_id);
    return bSuccess;
}
//=============================================================================
int32
getNelsonH5Schema(hid_t fid)
{
    int32 schema = -1;
    hid_t obj_id = H5Oopen(fid, "/", H5P_DEFAULT);
    if (obj_id < 0) {
        return schema;
    }
    hid_t attr_id = H5Aopen_name(obj_id, NELSON_SCHEMA_STR);
    if (attr_id < 0) {
        H5Oclose(obj_id);
        return schema;
    }
    herr_t status = H5Aread(attr_id, H5T_NATIVE_INT32, &schema);
    if (status < 0) {
        schema = -1;
    }
    H5Aclose(attr_id);
    H5Oclose(obj_id);
    return schema;
}
//=============================================================================
bool
addSchemaFormat(hid_t obj_id)
{
    bool bSuccess = true;
    herr_t status;
    void* buffer = nullptr;
    hid_t att_id = H5I_INVALID_HID;
    hid_t dspace_id = H5Screate(H5S_SCALAR);
    hid_t type_id = H5Tcopy(H5T_NATIVE_INT32);
    att_id = H5Acreate(obj_id, NELSON_SCHEMA_STR, type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT);
    if (att_id < 0) {
        bSuccess = false;
    } else {
        int32 schema_version = NELSON_SCHEMA;
        buffer = &schema_version;
        status = H5Awrite(att_id, type_id, buffer);
        if (status < 0) {
            bSuccess = false;
        }
        H5Aclose(att_id);
    }
    H5Sclose(dspace_id);
    return bSuccess;
}
//=============================================================================
bool
updateNelsonH5Header(hid_t fid)
{
    bool bSuccess = false;
    hid_t obj_id = H5Oopen(fid, "/", H5P_DEFAULT);
    if (obj_id < 0) {
        H5Fclose(fid);
        Error(_W("Specified HDF5 object location could not be opened."));
    }
    if (!addSchemaFormat(obj_id)) {
        H5Oclose(obj_id);
        return false;
    }
    H5Oclose(obj_id);
    return bSuccess;
}
//=============================================================================
};
//=============================================================================
