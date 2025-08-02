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
#include "h5WriteDataset.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "h5WriteHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
h5WriteDataset(const std::wstring& filename, const std::wstring& location, ArrayOf& data)
{
    if (filename.empty()) {
        Error(_W("Valid filename expected."));
    }
    if (location.empty()) {
        Error(_W("Valid location expected."));
    }
    FileSystemWrapper::Path hdf5_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(hdf5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
    }
    hid_t h5obj = H5I_INVALID_HID;
    if (!fileExistPreviously) {
        h5obj
            = H5Fcreate(wstring_to_utf8(filename).c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    } else {
        if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str())) {
            Error(_W("HDF5 format file expected."));
        }
        h5obj = H5Fopen(wstring_to_utf8(filename).c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
    }
    if (h5obj < 0) {
        Error(_W("Cannot open HDF5 file expected."));
    }
    htri_t exists = H5Lexists(h5obj, wstring_to_utf8(location).c_str(), H5P_DEFAULT);
    herr_t status;
    if (exists) {
        status = H5Ldelete(h5obj, wstring_to_utf8(location).c_str(), H5P_DEFAULT);
    }
    hid_t dspace_id = H5I_INVALID_HID;
    hid_t type_id = H5I_INVALID_HID;
    std::wstring error;
    void* buffer = h5WriteNelsonToHdf5(data, type_id, dspace_id, error);
    if (!error.empty()) {
        status = H5Fclose(h5obj);
        Error(error);
    }
    hid_t dataset_id = H5Dcreate(h5obj, wstring_to_utf8(location).c_str(), type_id, dspace_id,
        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dataset_id < 0) {
        H5Sclose(dspace_id);
        H5Fclose(h5obj);
        Error(_W("Cannot create data set."));
    }
    status = H5Dwrite(dataset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
    H5Dclose(dataset_id);
    H5Sclose(dspace_id);
    H5Fclose(h5obj);
    if (status < 0) {
        Error(_W("Cannot write data set."));
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
