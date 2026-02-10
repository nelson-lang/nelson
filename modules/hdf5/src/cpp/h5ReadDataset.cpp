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
#include "characters_encoding.hpp"
#include "h5ReadDataset.hpp"
#include "h5ReadFloat.hpp"
#include "h5ReadInteger.hpp"
#include "h5ReadOpaque.hpp"
#include "h5ReadBitfield.hpp"
#include "h5ReadString.hpp"
#include "h5ReadEnum.hpp"
#include "h5ReadCompound.hpp"
#include "h5ReadVlen.hpp"
#include "h5ReadArray.hpp"
#include "h5ReadReference.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadDataset(const std::wstring& filename, const std::wstring& dataSetName)
{
    ArrayOf res;
    if (filename.empty()) {
        raiseError(L"Nelson:hdf5:ERROR_VALID_FILENAME_EXPECTED", ERROR_VALID_FILENAME_EXPECTED);
    }
    if (dataSetName.empty()) {
        raiseError(
            L"Nelson:hdf5:ERROR_VALID_DATA_SET_NAME_EXPECTED", ERROR_VALID_DATA_SET_NAME_EXPECTED);
    }
    hid_t fid = H5I_INVALID_HID;
    FileSystemWrapper::Path hdf5_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(hdf5_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            raiseError(L"Nelson:hdf5:ERROR_PERMISSION_DENIED", ERROR_PERMISSION_DENIED);
        }
    }

    if (!fileExistPreviously) {
        raiseError(L"Nelson:hdf5:ERROR_FILE_DOES_NOT_EXIST", ERROR_FILE_DOES_NOT_EXIST);
    }
    if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str())) {
        raiseError(L"Nelson:hdf5:ERROR_HDF5_FORMAT_FILE_EXPECTED", ERROR_HDF5_FORMAT_FILE_EXPECTED);
    } else {
        fid = H5Fopen(
            wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    }
    if (fid == H5I_INVALID_HID) {
        raiseError(
            L"Nelson:hdf5:ERROR_IMPOSSIBLE_TO_OPEN_HDF5_FILE", ERROR_IMPOSSIBLE_TO_OPEN_HDF5_FILE);
    }

    hid_t dset_id = H5Dopen(fid, wstring_to_utf8(dataSetName).c_str(), H5P_DEFAULT);
    if (dset_id < 0) {
        H5Fclose(fid);
        raiseError(
            L"Nelson:hdf5:ERROR_IMPOSSIBLE_TO_READ_DATA_SET", ERROR_IMPOSSIBLE_TO_READ_DATA_SET);
    }

    hid_t dspace_id = H5Dget_space(dset_id);
    if (dspace_id < 0) {
        H5Dclose(dset_id);
        H5Fclose(fid);
        raiseError(
            L"Nelson:hdf5:ERROR_IMPOSSIBLE_TO_READ_DATA_SET", ERROR_IMPOSSIBLE_TO_READ_DATA_SET);
    }

    hid_t type_id = H5Dget_type(dset_id);
    std::wstring errorMessage;
    switch (H5Tget_class(type_id)) {
    case H5T_STRING: {
        res = h5ReadString(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_INTEGER: {
        res = h5ReadInteger(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_FLOAT: {
        res = h5ReadFloat(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_BITFIELD: {
        res = h5ReadBitfield(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_OPAQUE: {
        res = h5ReadOpaque(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_COMPOUND: {
        res = h5ReadCompound(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_REFERENCE: {
        res = h5ReadReference(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_ENUM: {
        res = h5ReadEnum(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_VLEN: {
        res = h5ReadVlen(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_ARRAY: {
        res = h5ReadArray(dset_id, type_id, dspace_id, false, errorMessage);
    } break;
    case H5T_TIME: {
        /* The time datatype, H5T_TIME,
        has not been fully implemented and is not supported.If H5T_TIME is used,
        the resulting data will be readable
        and modifiable only on the originating computing platform;
        it will not be portable to other platforms. */
        errorMessage = _W("Type not managed.");
    } break;
    case H5T_NCLASSES:
    default: {
        errorMessage = _W("Type not managed.");
    } break;
    }
    if (!errorMessage.empty()) {
        Error(errorMessage, L"Nelson:hdf5:h5ReadDataset");
    }
    H5Tclose(type_id);
    H5Dclose(dset_id);
    H5Sclose(dspace_id);
    H5Fclose(fid);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
