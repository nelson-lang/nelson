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
#include "HDF5_helpers.hpp"
#include "Exception.hpp"
#include "characters_encoding.hpp"
#include "h5create.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static hid_t
nelsonClassToHdf5DataType(Class dataType)
{
    hid_t datatype;
    switch (dataType) {
    case NLS_DOUBLE: {
        datatype = H5Tcopy(H5T_NATIVE_DOUBLE);
    } break;
    case NLS_SINGLE: {
        datatype = H5Tcopy(H5T_NATIVE_FLOAT);
    } break;
    case NLS_UINT64: {
        datatype = H5Tcopy(H5T_STD_U64LE);
    } break;
    case NLS_UINT32: {
        datatype = H5Tcopy(H5T_STD_U32LE);
    } break;
    case NLS_UINT16: {
        datatype = H5Tcopy(H5T_STD_U16LE);
    } break;
    case NLS_UINT8: {
        datatype = H5Tcopy(H5T_STD_U8LE);
    } break;
    case NLS_INT64: {
        datatype = H5Tcopy(H5T_STD_I64LE);
    } break;
    case NLS_INT32: {
        datatype = H5Tcopy(H5T_STD_I32LE);
    } break;
    case NLS_INT16: {
        datatype = H5Tcopy(H5T_STD_I16LE);
    } break;
    case NLS_INT8: {
        datatype = H5Tcopy(H5T_STD_I8LE);
    } break;
    default: {
        Error(_("Type not managed."));
    } break;
    }
    return datatype;
}
//=============================================================================
static herr_t
setFillValue(ArrayOf fillvalue, Class dataType, hid_t dcpl)
{
    herr_t status = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
    switch (dataType) {
    case NLS_DOUBLE: {
        hid_t fillType = H5T_NATIVE_DOUBLE;
        double value = fillvalue.getContentAsDoubleScalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_SINGLE: {
        hid_t fillType = H5T_NATIVE_FLOAT;
        single value = fillvalue.getContentAsSingleScalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_UINT64: {
        hid_t fillType = H5T_STD_U64LE;
        uint64 value = fillvalue.getContentAsUnsignedInt64Scalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_UINT32: {
        hid_t fillType = H5T_STD_U32LE;
        uint32 value = fillvalue.getContentAsUnsignedInteger32Scalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_UINT16: {
        hid_t fillType = H5T_STD_U16LE;
        uint16 value = fillvalue.getContentAsUnsignedInteger16Scalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_UINT8: {
        hid_t fillType = H5T_STD_U8LE;
        uint8 value = fillvalue.getContentAsUnsignedInteger8Scalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_INT64: {
        hid_t fillType = H5T_STD_I64LE;
        int64 value = fillvalue.getContentAsInteger64Scalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_INT32: {
        hid_t fillType = H5T_STD_I32LE;
        int32 value = fillvalue.getContentAsInteger32Scalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_INT16: {
        hid_t fillType = H5T_STD_I16LE;
        int16 value = fillvalue.getContentAsInteger16Scalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    case NLS_INT8: {
        hid_t fillType = H5T_STD_I8LE;
        int8 value = fillvalue.getContentAsInteger8Scalar();
        status = H5Pset_fill_value(dcpl, fillType, &value);
    } break;
    default: {
        Error(_("Type not managed."));
    } break;
    }
    return status;
}
//=============================================================================
void
hdf5Create(const std::wstring& filename, const std::wstring& dataSetName,
    boost::container::vector<double> sizeData, Class dataType,
    boost::container::vector<double> chunksize, int deflate, ArrayOf fillvalue, bool fletcher32,
    bool shuffle, const std::wstring& textEncoding)
{
    if (deflate < 0 || deflate > 9) {
        Error(_W("Valid deflate value expected."));
    }
    if (filename.empty()) {
        Error(_W("Valid filename expected."));
    }
    if (dataSetName.empty()) {
        Error(_W("Valid data set name expected."));
    }
    if ((fletcher32 || shuffle || (deflate > 0)) && chunksize.empty()) {
        Error(_W("ChunkSize required."));
    }
    if (!fillvalue.isEmpty()) {
        if (fillvalue.getDataClass() != dataType) {
            Error(_W("FillValue and dataset class must be same."));
        }
    }
    if (!chunksize.empty()) {
        if (chunksize.size() != sizeData.size()) {
            Error(_W("Length ChunkSize and Size must be equal."));
        }
        for (indexType k = 0; k < chunksize.size(); k++) {
            if (chunksize[k] - sizeData[k] > 0) {
                Error(_W("ChunkSize larger than Size."));
            }
        }
    } else {
        bool haveZeros = false;
        for (indexType k = 0; k < sizeData.size(); k++) {
            if (sizeData[k] < 0.5) {
                haveZeros = true;
                break;
            }
        }
        if (haveZeros) {
            Error(_W("ChunkSize expected."));
        }
    }

    hid_t datatype = nelsonClassToHdf5DataType(dataType);

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
        fid = H5Fcreate(wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_TRUNC,
            H5P_DEFAULT, H5P_DEFAULT);
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
    hid_t dset = H5Dopen1(fid, wstring_to_utf8(dataSetName).c_str());
    if (dset > 0) {
        H5Dclose(dset);
        H5Fclose(fid);
        Error(_W("dataset already exists."));
    }

    /* Create the data space for the dataset. */
    hsize_t* sizeDataAsHsize_t = new_with_exception<hsize_t>(sizeData.size(), true);
    indexType nbElementsSizeData = sizeData.size();
    for (indexType k = 1; k <= nbElementsSizeData; k++) {
        if (std::isinf(sizeData[nbElementsSizeData - k]) || sizeData[nbElementsSizeData - k] == 0) {
            sizeDataAsHsize_t[k - 1] = 0;
        } else {
            sizeDataAsHsize_t[k - 1] = (hsize_t)sizeData[nbElementsSizeData - k];
        }
    }
    hsize_t* maxdimsAsHsize_t = new_with_exception<hsize_t>(sizeData.size(), true);
    for (indexType k = 1; k <= nbElementsSizeData; k++) {
        if (std::isinf(sizeData[nbElementsSizeData - k]) || sizeData[nbElementsSizeData - k] == 0) {
            maxdimsAsHsize_t[k - 1] = H5S_UNLIMITED;
        } else {
            maxdimsAsHsize_t[k - 1] = (hsize_t)sizeData[nbElementsSizeData - k];
        }
    }
    hid_t space_id = H5Screate_simple((int)sizeData.size(), sizeDataAsHsize_t, maxdimsAsHsize_t);
    delete[] sizeDataAsHsize_t;
    delete[] maxdimsAsHsize_t;
    if (space_id == H5I_INVALID_HID) {
        H5Fclose(fid);
        Error(_W("H5Screate_simple fails."));
    }

    hid_t lcpl = H5Pcreate(H5P_LINK_CREATE);
    H5Pset_create_intermediate_group(lcpl, 1);

    herr_t status;

    hid_t dcpl = H5Pcreate(H5P_DATASET_CREATE);
    if (shuffle) {
        status = H5Pset_shuffle(dcpl);
    }
    if (!chunksize.empty()) {
        hsize_t* dimsChunk = new_with_exception<hsize_t>(chunksize.size(), true);
        for (indexType k = 0; k < chunksize.size(); k++) {
            dimsChunk[k] = (hsize_t)chunksize[k];
        }
        if (H5Pset_layout(dcpl, H5D_CHUNKED) == H5I_INVALID_HID) {
            delete[] dimsChunk;
            H5Pclose(dcpl);
            H5Fclose(fid);
            Error(_W("H5Pset_layout fails."));
        }
        if (H5Pset_chunk(dcpl, (int)sizeData.size(), dimsChunk) == H5I_INVALID_HID) {
            delete[] dimsChunk;
            H5Pclose(dcpl);
            H5Fclose(fid);
            Error(_W("H5Pset_chunk fails."));
        }
        delete[] dimsChunk;
    } else {
        if (H5Pset_layout(dcpl, H5D_CONTIGUOUS) == H5I_INVALID_HID) {
            H5Pclose(dcpl);
            H5Fclose(fid);
            Error(_W("H5Pset_layout fails."));
        }
    }
    if (deflate != 0) {
        status = H5Pset_deflate(dcpl, deflate);
    }
    if (!fillvalue.isEmpty()) {
        status = setFillValue(fillvalue, dataType, dcpl);
    }
    if (fletcher32) {
        status = H5Pset_fletcher32(dcpl);
    }

    hid_t dapl = H5P_DEFAULT;
    hid_t dset_id = H5Dcreate(
        fid, wstring_to_utf8(dataSetName).c_str(), datatype, space_id, lcpl, dcpl, dapl);
    if (dset_id == H5I_INVALID_HID) {
        H5Pclose(lcpl);
        H5Pclose(dcpl);
        H5Sclose(space_id);
        H5Fclose(fid);
        if (!fileExistPreviously) {
            try {
                boost::filesystem::path p = filename;
                boost::filesystem::remove(p);
            } catch (const boost::filesystem::filesystem_error&) {
            }
        }
        Error(_W("H5Dcreate fails."));
    }
    status = H5Pclose(lcpl);
    status = H5Pclose(dcpl);
    status = H5Dclose(dset_id);
    status = H5Sclose(space_id);
    status = H5Fclose(fid);
}
//=============================================================================
}  // namespace Nelson
//=============================================================================
