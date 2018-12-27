//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "Exception.hpp"
#include "characters_encoding.hpp"
#include "h5Read.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5Read(const std::wstring& filename, const std::wstring& dataSetName,
    const boost::container::vector<double> &start, const boost::container::vector<double> &count,
    const boost::container::vector<double> &stride) {
    ArrayOf res;
    if (start.size() != count.size()) {
        Error(_W("start and count parameters must have same rank."));
	}
    if (stride.size() != 0 && start.size() != stride.size()) {
        Error(_W("start, count, stride parameters must have same rank."));
	}
    if (filename.empty()) {
        Error(_W("Valid filename expected."));
    }
    if (dataSetName.empty()) {
        Error(_W("Valid data set name expected."));
    }
    hid_t fid = H5I_INVALID_HID;
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
    if (!fileExistPreviously) {
        Error(_W("file does not exist."));
    }
    if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str()))
        Error(_W("HDF5 format file expected."));
    else {
        fid = H5Fopen(
            wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    }
    if (fid == H5I_INVALID_HID) {
        Error(_W("Impossible to open hdf5 file."));
    }

	hid_t dset_id = H5Dopen(fid, wstring_to_utf8(dataSetName).c_str(), H5P_DEFAULT);
    if (dset_id < 0) {
        H5Fclose(fid);
        Error("Impossible to read data set");
    }

    hid_t dspace_id = H5Dget_space(dset_id);
    if (dspace_id < 0) {
        H5Dclose(dset_id);
        H5Fclose(fid);
        Error("Impossible to read data set");
    }

    int rank = H5Sget_simple_extent_ndims(dspace_id);
    if (rank < 0) {
        H5Dclose(dset_id);
        H5Dclose(dspace_id);
        H5Fclose(fid);
        Error("Impossible to read data set");
    }

	if (start.size() != 0) {
        if (start.size() != rank) {
            H5Dclose(dset_id);
            H5Dclose(dspace_id);
            H5Fclose(fid);
            Error("start, count, stride must have same rank than data set.");
        }
    }

    hsize_t* h5_dims = nullptr;
    hsize_t* h5_maxdims = nullptr;
    try {
        h5_dims = (hsize_t*)new_with_exception<hsize_t>(rank * sizeof(hsize_t), false);
    } catch (Exception &e)
    {
        H5Dclose(dset_id);
        H5Dclose(dspace_id);
        H5Fclose(fid);
        throw;
	}
    try {
        h5_maxdims = (hsize_t*)new_with_exception<hsize_t>(rank * sizeof(hsize_t), false);
    } catch (Exception& e) {
        H5Dclose(dset_id);
        H5Dclose(dspace_id);
        H5Fclose(fid);
        throw;
	} 

	if (H5Sget_simple_extent_dims(dspace_id, h5_dims, h5_maxdims) < 0) {
        delete[] h5_dims;
        delete[] h5_maxdims;
        H5Dclose(dset_id);
        H5Dclose(dspace_id);
        H5Fclose(fid);
		Error("Impossible to read dimensions and maximum size of dataset.");
    }
    hid_t type_id = H5Dget_type(dset_id);
	if (start.empty() && count.empty() && stride.empty()) {
       // herr_t status = H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0])
    } else {
        boost::container::vector<double> _stride;
        if (stride.empty()) {
            _stride.reserve((size_t)rank);
            for (indexType k = 0; k < rank; k++) {
                _stride.push_back(1);
            }
        } else {
            _stride = stride;
        }

		/*
        hsize_t* hstart;
        hsize_t* hcount;
        hsize_t* hstride;
        hsize_t* hblock;

        herr_t selection_result
            = H5Sselect_hyperslab(dspace_id, H5S_SELECT_SET, hstart, hstride, hcount, hblock);
        if (selection_result < 0) {
            H5Dclose(dset_id);
            H5Dclose(dspace_id);
            H5Fclose(fid);
            Error("Impossible to read data.");
        }

        hsize_t* hmem = alloc_hsize(mat_dims, ALLOC_HSIZE_DEFAULT, false);
        hid_t memspace_id = H5Screate_simple(rank, hmem, hmem);
        free(hmem);
        if (memspace_id < 0) {
        }

        if (H5Sselect_valid(dspace_id) <= 0) {
            H5Sclose(memspace_id);
            H5Dclose(dset_id);
            H5Dclose(dspace_id);
            H5Fclose(fid);
            Error(_W("Selected dataspace is not valid."));
        }
		*/
	}

    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
