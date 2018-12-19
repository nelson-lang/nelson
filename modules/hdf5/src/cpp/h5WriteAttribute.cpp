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
#include "h5WriteAttribute.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void*
createMatrix(const ArrayOf& attributeValue, hid_t& dspace_id)
{
    Dimensions dimsValue = attributeValue.getDimensions();
    hsize_t* dimsAsHsize_t = new_with_exception<hsize_t>(dimsValue.getLength(), true);
    indexType nbElementsSizeData = dimsValue.getLength();
    for (indexType k = 1; k <= nbElementsSizeData; k++) {
        dimsAsHsize_t[k - 1] = (hsize_t)dimsValue[nbElementsSizeData - k];
    }
    dspace_id = H5Screate_simple((int)dimsValue.getLength(), dimsAsHsize_t, dimsAsHsize_t);
    delete[] dimsAsHsize_t;
    void* buffer = (void*)attributeValue.getDataPointer();
    return buffer;
}
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
    hid_t dspace_id;
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
            H5Aclose(exists);
            H5Oclose(obj_id);
            H5Fclose(fid);
            Error(_W("Could not delete existing attribute."));
        }
    } else if (exists < 0) {
        H5Aclose(exists);
        H5Oclose(obj_id);
        H5Fclose(fid);
        Error(_W("Could not check if attribute exists."));
    }

    if (attributeValue.isSparse()) {
        attributeValue.makeDense();
    }
    void* buffer = nullptr;
    hid_t type_id;
    hid_t mem_type_id;
    switch (attributeValue.getDataClass()) {
    case NLS_CHAR: {
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
            type_id = H5Tcopy(H5T_C_S1);
        } else if (attributeValue.isRowVector()) {
            std::wstring value = attributeValue.getContentAsWideString();
            std::string value_utf8 = wstring_to_utf8(value);
            type_id = H5Tcopy(H5T_C_S1);
            H5Tset_size(type_id, value_utf8.length());
            H5Tset_strpad(type_id, H5T_STR_NULLTERM);
            mem_type_id = H5Tcopy(type_id);
            buffer = (void*)value_utf8.c_str();
        } else {
            H5Oclose(obj_id);
            H5Fclose(fid);
            Error(_W("row vector characters expected."));
        }
    } break;
    case NLS_DOUBLE: {
        type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
        mem_type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            double value = attributeValue.getContentAsDoubleScalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_SINGLE: {
        type_id = H5Tcopy(H5T_NATIVE_FLOAT);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            single value = attributeValue.getContentAsSingleScalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_SCOMPLEX: {
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
            type_id = H5Tcopy(H5T_NATIVE_FLOAT);
        } else {
            H5Aclose(exists);
            H5Oclose(obj_id);
            H5Fclose(fid);
            Error(_W("Complex number not supported."));
        }
    } break;
    case NLS_DCOMPLEX: {
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
            type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
        } else {
            H5Aclose(exists);
            H5Oclose(obj_id);
            H5Fclose(fid);
            Error(_W("Complex number not supported."));
        }
    } break;
    case NLS_STRING_ARRAY: {
        H5Aclose(exists);
        H5Oclose(obj_id);
        H5Fclose(fid);
        Error(_W("String class not supported."));
    } break;
    case NLS_INT8: {
        type_id = H5Tcopy(H5T_NATIVE_SCHAR);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            int8 value = attributeValue.getContentAsInteger8Scalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_UINT8: {
        type_id = H5Tcopy(H5T_NATIVE_UCHAR);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            uint8 value = attributeValue.getContentAsUnsignedInteger8Scalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_INT16: {
        type_id = H5Tcopy(H5T_NATIVE_SHORT);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            int16 value = attributeValue.getContentAsInteger16Scalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_UINT16: {
        type_id = H5Tcopy(H5T_NATIVE_USHORT);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            uint16 value = attributeValue.getContentAsUnsignedInteger16Scalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_INT32: {
        type_id = H5Tcopy(H5T_NATIVE_INT);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            mem_type_id = H5Tcopy(H5T_NATIVE_INT);
            int32 value = attributeValue.getContentAsInteger32Scalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_UINT32: {
        type_id = H5Tcopy(H5T_NATIVE_UINT);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            uint32 value = attributeValue.getContentAsUnsignedInteger32Scalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_INT64: {
        type_id = H5Tcopy(H5T_NATIVE_LLONG);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            int64 value = attributeValue.getContentAsInteger64Scalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    case NLS_UINT64: {
        type_id = H5Tcopy(H5T_NATIVE_ULLONG);
        if (attributeValue.isEmpty()) {
            dspace_id = H5Screate(H5S_NULL);
        } else if (attributeValue.isScalar()) {
            dspace_id = H5Screate(H5S_SCALAR);
            uint64 value = attributeValue.getContentAsUnsignedInt64Scalar();
            buffer = (void*)&value;
        } else {
            buffer = createMatrix(attributeValue, dspace_id);
        }
    } break;
    default: {
        H5Sclose(dspace_id);
        H5Aclose(exists);
        H5Oclose(obj_id);
        H5Fclose(fid);
        Error(_W("Type not managed."));
    } break;
    }
    hid_t att_id;
    if (H5Aexists(obj_id, wstring_to_utf8(attributeName).c_str()))
        att_id = H5Aopen(obj_id, wstring_to_utf8(attributeName).c_str(), H5P_DEFAULT);
    else {
        att_id = H5Acreate(obj_id, wstring_to_utf8(attributeName).c_str(), type_id, dspace_id,
            H5P_DEFAULT, H5P_DEFAULT);
    }
    herr_t status = H5Awrite(att_id, mem_type_id, buffer);

    H5Sclose(dspace_id);
    H5Aclose(exists);
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
