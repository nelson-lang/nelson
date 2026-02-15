//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "PredefinedErrorMessages.hpp"
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
        raiseError(L"Nelson:hdf5:ERROR_VALID_FILENAME_EXPECTED", ERROR_VALID_FILENAME_EXPECTED);
    }
    if (location.empty()) {
        raiseError(L"Nelson:hdf5:ERROR_VALID_LOCATION_EXPECTED", ERROR_VALID_LOCATION_EXPECTED);
    }
    if (attributeName.empty()) {
        raiseError(L"Nelson:hdf5:ERROR_VALID_ATTRIBUTE_NAME_EXPECTED",
            ERROR_VALID_ATTRIBUTE_NAME_EXPECTED);
    }
    if (!(textEncoding == L"system" || textEncoding == L"UTF-8")) {
        raiseError(
            L"Nelson:hdf5:ERROR_VALID_TEXT_ENCODING_EXPECTED", ERROR_VALID_TEXT_ENCODING_EXPECTED);
    }
    hid_t fid = H5I_INVALID_HID;
    FileSystemWrapper::Path hdf5_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(hdf5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            raiseError2(L"nelson:io:permissionDenied");
        }
    }
    if (!fileExistPreviously) {
        raiseError(L"Nelson:hdf5:ERROR_FILE_DOES_NOT_EXIST", ERROR_FILE_DOES_NOT_EXIST);
    } else {
        if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str())) {
            raiseError(
                L"Nelson:hdf5:ERROR_HDF5_FORMAT_FILE_EXPECTED", ERROR_HDF5_FORMAT_FILE_EXPECTED);
        } else {
            fid = H5Fopen(
                wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
        }
    }
    if (fid == H5I_INVALID_HID) {
        raiseError(L"Nelson:hdf5:ERROR_OPEN_FILE_FAILED", ERROR_OPEN_FILE_FAILED);
    }
    if (location != L"/" && !H5Lexists(fid, wstring_to_utf8(location).c_str(), H5P_DEFAULT)) {
        H5Fclose(fid);
        raiseError(L"Nelson:hdf5:ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_DOES_NOT_EXIST",
            ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_DOES_NOT_EXIST);
    }
    hid_t obj_id = H5Oopen(fid, wstring_to_utf8(location).c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        H5Fclose(fid);
        raiseError(L"Nelson:hdf5:ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_COULD_NOT_BE_OPENED",
            ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_COULD_NOT_BE_OPENED);
    }

    htri_t exists = H5Aexists(obj_id, wstring_to_utf8(attributeName).c_str());
    if (exists > 0) {
        if (H5Adelete(obj_id, wstring_to_utf8(attributeName).c_str()) < 0) {
            H5Oclose(obj_id);
            H5Fclose(fid);
            raiseError(L"Nelson:hdf5:ERROR_COULD_NOT_DELETE_EXISTING_ATTRIBUTE",
                ERROR_COULD_NOT_DELETE_EXISTING_ATTRIBUTE);
        }
    } else if (exists < 0) {
        H5Oclose(obj_id);
        H5Fclose(fid);
        raiseError(L"Nelson:hdf5:ERROR_COULD_NOT_CHECK_IF_ATTRIBUTE_EXISTS",
            ERROR_COULD_NOT_CHECK_IF_ATTRIBUTE_EXISTS);
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
        Error(error, L"Nelson:hdf5:ERROR_H5WRITE_NELSON_TO_HDF5");
    }
    hid_t att_id = H5I_INVALID_HID;
    if (H5Aexists(obj_id, wstring_to_utf8(attributeName).c_str())) {
        att_id = H5Aopen(obj_id, wstring_to_utf8(attributeName).c_str(), H5P_DEFAULT);
    } else {
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
        raiseError(L"Nelson:hdf5:ERROR_CANNOT_WRITE_ATTRIBUTE", ERROR_CANNOT_WRITE_ATTRIBUTE);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
