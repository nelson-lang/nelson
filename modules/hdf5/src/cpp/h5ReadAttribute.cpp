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
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
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
#include "Exception.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadAttribute(
    const std::wstring& filename, const std::wstring& location, const std::wstring& attributeName)
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
    hid_t obj_id = H5Oopen(fid, wstring_to_utf8(location).c_str(), H5P_DEFAULT);
    if (obj_id < 0) {
        H5Fclose(fid);
        Error(_W("Specified HDF5 object location could not be opened."));
    }

    hid_t attr_id = H5Aopen_name(obj_id, wstring_to_utf8(attributeName).c_str());
    if (attr_id < 0) {
        H5Oclose(obj_id);
        H5Fclose(fid);
        Error(_W("Attribute name not found."));
    }
    hid_t type = H5Aget_type(attr_id);
    if (type < 0) {
        H5Aclose(attr_id);
        H5Oclose(obj_id);
        H5Fclose(fid);
        Error(_W("Attribute have an invalid type."));
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
    H5Aclose(type);
    H5Aclose(attr_id);
    H5Oclose(obj_id);
    H5Fclose(fid);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
