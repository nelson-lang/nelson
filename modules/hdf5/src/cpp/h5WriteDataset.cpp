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
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "HDF5_helpers.hpp"
#include "h5WriteDataset.hpp"
#include "Exception.hpp"
#include "Error.hpp"
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
    boost::filesystem::path hdf5_filename(filename);
    bool fileExistPreviously = false;
    try {
        fileExistPreviously = boost::filesystem::exists(hdf5_filename)
            && !boost::filesystem::is_directory(hdf5_filename);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            Error(_W("Permission denied."));
        }
        fileExistPreviously = false;
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
