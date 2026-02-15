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
#include "h5ReadAttribute.hpp"
#include "h5ReadString.hpp"
#include "h5ReadInteger.hpp"
#include "h5ReadFloat.hpp"
#include "h5ReadBitfield.hpp"
#include "h5ReadOpaque.hpp"
#include "h5ReadEnum.hpp"
#include "h5ReadArray.hpp"
#include "h5ReadCompound.hpp"
#include "h5ReadVlen.hpp"
#include "h5ReadReference.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadAttribute(
    const std::wstring& filename, const std::wstring& location, const std::wstring& attributeName)
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
    }
    if (!H5Fis_hdf5(wstring_to_utf8(hdf5_filename.wstring()).c_str())) {
        raiseError(L"Nelson:hdf5:ERROR_HDF5_FORMAT_FILE_EXPECTED", ERROR_HDF5_FORMAT_FILE_EXPECTED);
    } else {
        fid = H5Fopen(
            wstring_to_utf8(hdf5_filename.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    }
    hid_t obj_id = H5Oopen(fid, wstring_to_utf8(location).c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        H5Fclose(fid);
        raiseError(L"Nelson:hdf5:ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_COULD_NOT_BE_OPENED",
            ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_COULD_NOT_BE_OPENED);
    }

    hid_t attr_id = H5Aopen_by_name(obj_id, wstring_to_utf8(location).c_str(),
        wstring_to_utf8(attributeName).c_str(), H5P_DEFAULT, H5P_DEFAULT);
    if (attr_id < 0) {
        H5Oclose(obj_id);
        H5Fclose(fid);
        raiseError(L"Nelson:hdf5:ERROR_ATTRIBUTE_NAME_NOT_FOUND", ERROR_ATTRIBUTE_NAME_NOT_FOUND);
    }
    hid_t type = H5Aget_type(attr_id);
    if (type < 0) {
        H5Aclose(attr_id);
        H5Oclose(obj_id);
        H5Fclose(fid);
        raiseError(L"Nelson:hdf5:ERROR_ATTRIBUTE_HAVE_AN_INVALID_TYPE",
            ERROR_ATTRIBUTE_HAVE_AN_INVALID_TYPE);
    }
    hid_t aspace = H5Aget_space(attr_id);
    ArrayOf res;
    std::wstring errorMessage;
    switch (H5Tget_class(type)) {
    case H5T_STRING: {
        res = h5ReadString(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_INTEGER: {
        res = h5ReadInteger(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_FLOAT: {
        res = h5ReadFloat(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_BITFIELD: {
        res = h5ReadBitfield(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_OPAQUE: {
        res = h5ReadOpaque(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_COMPOUND: {
        res = h5ReadCompound(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_REFERENCE: {
        res = h5ReadReference(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_ENUM: {
        res = h5ReadEnum(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_VLEN: {
        res = h5ReadVlen(attr_id, type, aspace, true, errorMessage);
    } break;
    case H5T_ARRAY: {
        res = h5ReadArray(attr_id, type, aspace, true, errorMessage);
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
    H5Sclose(aspace);
    H5Tclose(type);
    H5Aclose(attr_id);
    H5Oclose(obj_id);
    H5Fclose(fid);
    if (!errorMessage.empty()) {
        Error(errorMessage, L"Nelson:hdf5:ERROR_h5ReadAttribute");
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
