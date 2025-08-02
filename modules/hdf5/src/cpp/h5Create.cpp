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
#include "characters_encoding.hpp"
#include "h5Create.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static hid_t
nelsonClassToHdf5DataType(NelsonType dataType)
{
    hid_t datatype = H5I_INVALID_HID;
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
setFillValue(const ArrayOf& fillvalue, NelsonType dataType, hid_t dcpl)
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
        uint64 value = fillvalue.getContentAsUnsignedInteger64Scalar();
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
h5Create(const std::wstring& filename, const std::wstring& dataSetName,
    const std::vector<double>& sizeData, NelsonType dataType, const std::vector<double>& chunksize,
    int deflate, const ArrayOf& fillvalue, bool fletcher32, bool shuffle,
    const std::wstring& textEncoding)
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
            Error(_W("FillValue and data set class must be same."));
        }
    }
    if (!chunksize.empty()) {
        if (chunksize.size() != sizeData.size()) {
            Error(_W("Length ChunkSize and Size must be equal."));
        }
        for (size_t k = 0; k < chunksize.size(); k++) {
            if (chunksize[k] - sizeData[k] > 0) {
                Error(_W("ChunkSize larger than Size."));
            }
        }
    } else {
        bool haveZeros = false;
        for (double k : sizeData) {
            if (k < 0.5) {
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
    FileSystemWrapper::Path hdf5_filename(filename);

    bool permissionDenied = false;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(hdf5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
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
    htri_t exists = H5Lexists(fid, wstring_to_utf8(dataSetName).c_str(), H5P_DEFAULT);
    if (exists) {
        H5Fclose(fid);
        Error(_W("data set already exists."));
    }

    /* Create the data space for the data set. */
    hsize_t* sizeDataAsHsize_t = nullptr;
    try {
        sizeDataAsHsize_t = new_with_exception<hsize_t>(sizeData.size(), true);
    } catch (Exception&) {
        H5Fclose(fid);
        throw;
    }
    indexType nbElementsSizeData = sizeData.size();
    for (indexType k = 1; k <= nbElementsSizeData; k++) {
        if (std::isinf(sizeData[nbElementsSizeData - k]) || sizeData[nbElementsSizeData - k] == 0) {
            sizeDataAsHsize_t[k - 1] = 0;
        } else {
            sizeDataAsHsize_t[k - 1] = (hsize_t)sizeData[nbElementsSizeData - k];
        }
    }
    hsize_t* maxdimsAsHsize_t = nullptr;
    try {
        maxdimsAsHsize_t = new_with_exception<hsize_t>(sizeData.size(), true);
    } catch (Exception&) {
        H5Fclose(fid);
        throw;
    }
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
        hsize_t* dimsChunk = nullptr;
        try {
            dimsChunk = new_with_exception<hsize_t>(chunksize.size(), true);
        } catch (Exception&) {
            H5Pclose(dcpl);
            H5Fclose(fid);
            throw;
        }
        for (size_t k = 0; k < chunksize.size(); k++) {
            dimsChunk[k] = (hsize_t)chunksize[k];
        }
        if (H5Pset_layout(dcpl, H5D_CHUNKED) == H5I_INVALID_HID) {
            delete[] dimsChunk;
            H5Pclose(dcpl);
            H5Fclose(fid);
            Error(_W("H5Pset_layout fails."));
            return;
        }
        if (H5Pset_chunk(dcpl, (int)sizeData.size(), dimsChunk) == H5I_INVALID_HID) {
            delete[] dimsChunk;
            H5Pclose(dcpl);
            H5Fclose(fid);
            Error(_W("H5Pset_chunk fails."));
            return;
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
            FileSystemWrapper::Path _p = filename;
            FileSystemWrapper::Path::remove(_p);
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
} // namespace Nelson
//=============================================================================
