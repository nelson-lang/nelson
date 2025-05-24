//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include "FileSystemWrapper.hpp"
#include "HDF5_helpers.hpp"
#include "h5WriteAttribute.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "h5WriteHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
h5WriteAttribute(const std::wstring& filename, const std::wstring& location,
    const std::wstring& attributeName, ArrayOf& attributeValue, const std::wstring& textEncoding)
{
    if (filename.empty()) {
        Error(_W("Valid filename expected."));
    }
    if (location.empty()) {
        Error(_W("Valid location expected."));
    }
    if (attributeName.empty()) {
        Error(_W("Valid attribute name expected."));
    }
    if (!(textEncoding == L"system" || textEncoding == L"UTF-8")) {
        Error(_W("Valid text encoding expected."));
    }
    hid_t fid = H5I_INVALID_HID;
    FileSystemWrapper::Path hdf5_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(hdf5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
    }
    if (!fileExistPreviously) {
        Error(_W("file does not exist."));
    } else {
        if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str()))
            Error(_W("HDF5 format file expected."));
        else {
            fid = H5Fopen(
                wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
        }
    }
    if (fid == H5I_INVALID_HID) {
        Error(_W("Open file failed."));
    }
    if (location != L"/" && !H5Lexists(fid, wstring_to_utf8(location).c_str(), H5P_DEFAULT)) {
        H5Fclose(fid);
        Error(_W("Specified HDF5 object location does not exist."));
    }
    hid_t obj_id = H5Oopen(fid, wstring_to_utf8(location).c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        H5Fclose(fid);
        Error(_W("Specified HDF5 object location could not be opened."));
    }

    htri_t exists = H5Aexists(obj_id, wstring_to_utf8(attributeName).c_str());
    if (exists > 0) {
        if (H5Adelete(obj_id, wstring_to_utf8(attributeName).c_str()) < 0) {
            H5Oclose(obj_id);
            H5Fclose(fid);
            Error(_W("Could not delete existing attribute."));
        }
    } else if (exists < 0) {
        H5Oclose(obj_id);
        H5Fclose(fid);
        Error(_W("Could not check if attribute exists."));
    }

    if (attributeValue.isSparse()) {
        attributeValue.makeDense();
    }
    hid_t type_id = H5I_INVALID_HID;
    hid_t dspace_id = H5I_INVALID_HID;
    std::wstring error;
    void* buffer = h5WriteNelsonToHdf5(attributeValue, type_id, dspace_id, error);
    if (!error.empty()) {
        H5Aclose(exists);
        H5Oclose(obj_id);
        H5Fclose(fid);
        Error(error);
    }
    hid_t att_id = H5I_INVALID_HID;
    if (H5Aexists(obj_id, wstring_to_utf8(attributeName).c_str()))
        att_id = H5Aopen(obj_id, wstring_to_utf8(attributeName).c_str(), H5P_DEFAULT);
    else {
        att_id = H5Acreate(obj_id, wstring_to_utf8(attributeName).c_str(), type_id, dspace_id,
            H5P_DEFAULT, H5P_DEFAULT);
    }
    herr_t status = H5I_INVALID_HID;
    if (att_id > 0) {
        status = H5Awrite(att_id, type_id, buffer);
    }
    H5Sclose(dspace_id);
    H5Aclose(att_id);
    H5Oclose(obj_id);
    H5Fclose(fid);
    if (status < 0) {
        Error(_W("Cannot write attribute."));
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
