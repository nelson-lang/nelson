//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5WriteHelpers.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static single single_scalar = static_cast<single>(0);
static double double_scalar = static_cast<double>(0);
static int8 int8_scalar = static_cast<int8>(0);
static int16 int16_scalar = static_cast<int16>(0);
static int32 int32_scalar = static_cast<int32>(0);
static int64 int64_scalar = static_cast<int64>(0);
static uint8 uint8_scalar = static_cast<uint8>(0);
static uint16 uint16_scalar = static_cast<uint16>(0);
static uint32 uint32_scalar = static_cast<uint32>(0);
static uint64 uint64_scalar = static_cast<uint64>(0);
static std::string string_utf8;
//=============================================================================
void*
h5WriteNelsonToHdf5(ArrayOf& data, hid_t& type_id, hid_t& dspace_id, std::wstring& error)
{
    string_utf8.clear();
    void* buffer = nullptr;
    switch (data.getDataClass()) {
    case NLS_CHAR: {
        if (data.isEmpty() || data.isRowVector()) {
            type_id = H5Tcopy(H5T_C_S1);
        } else {
            type_id = H5I_INVALID_HID;
            dspace_id = H5I_INVALID_HID;
            error = _W("row vector characters expected.");
            return nullptr;
        }
    } break;
    case NLS_DOUBLE: {
        type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
    } break;
    case NLS_SINGLE: {
        type_id = H5Tcopy(H5T_NATIVE_FLOAT);
    } break;
    case NLS_SCOMPLEX: {
        if (!data.isEmpty()) {
            type_id = H5I_INVALID_HID;
            dspace_id = H5I_INVALID_HID;
            error = _W("Complex number not supported.");
            return nullptr;
        }
        type_id = H5Tcopy(H5T_NATIVE_FLOAT);
    } break;
    case NLS_DCOMPLEX: {
        if (!data.isEmpty()) {
            type_id = H5I_INVALID_HID;
            dspace_id = H5I_INVALID_HID;
            error = _W("Complex number not supported.");
            return nullptr;
        }
        type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
    } break;
    case NLS_STRING_ARRAY: {
        type_id = H5I_INVALID_HID;
        dspace_id = H5I_INVALID_HID;
        error = _W("String class not supported.");
        return nullptr;
    } break;
    case NLS_INT8: {
        type_id = H5Tcopy(H5T_NATIVE_SCHAR);
    } break;
    case NLS_UINT8: {
        type_id = H5Tcopy(H5T_NATIVE_UCHAR);
    } break;
    case NLS_INT16: {
        type_id = H5Tcopy(H5T_NATIVE_SHORT);
    } break;
    case NLS_UINT16: {
        type_id = H5Tcopy(H5T_NATIVE_USHORT);
    } break;
    case NLS_INT32: {
        type_id = H5Tcopy(H5T_NATIVE_INT);
    } break;
    case NLS_UINT32: {
        type_id = H5Tcopy(H5T_NATIVE_UINT);
    } break;
    case NLS_INT64: {
        type_id = H5Tcopy(H5T_NATIVE_LLONG);
    } break;
    case NLS_UINT64: {
        type_id = H5Tcopy(H5T_NATIVE_ULLONG);
    } break;
    default: {
        type_id = H5I_INVALID_HID;
        dspace_id = H5I_INVALID_HID;
        error = _W("Type not managed.");
        return nullptr;
    } break;
    }
    if (data.isSparse()) {
        data.makeDense();
    }
    if (data.isEmpty()) {
        dspace_id = H5Screate(H5S_NULL);
        switch (data.getDataClass()) {
        case NLS_CHAR: {
            H5Tset_strpad(type_id, H5T_STR_NULLTERM);
            buffer = (void*)string_utf8.c_str();
        } break;
        case NLS_DOUBLE: {
            buffer = &double_scalar;
        } break;
        case NLS_SINGLE: {
            buffer = &single_scalar;
        } break;
        case NLS_SCOMPLEX: {
            buffer = &single_scalar;
        } break;
        case NLS_DCOMPLEX: {
            buffer = &double_scalar;
        } break;
        case NLS_INT8: {
            buffer = &int8_scalar;
        } break;
        case NLS_UINT8: {
            buffer = &uint8_scalar;
        } break;
        case NLS_INT16: {
            buffer = &int16_scalar;
        } break;
        case NLS_UINT16: {
            buffer = &uint16_scalar;
        } break;
        case NLS_INT32: {
            buffer = &int32_scalar;
        } break;
        case NLS_UINT32: {
            buffer = &uint32_scalar;
        } break;
        case NLS_INT64: {
            buffer = &int64_scalar;
        } break;
        case NLS_UINT64: {
            buffer = &uint64_scalar;
        } break;
        default: {
        } break;
        }
    } else {
        if (data.isCharacterArray()) {
            std::wstring value = data.getContentAsWideString();
            string_utf8 = wstring_to_utf8(value);
            dspace_id = H5Screate(H5S_SCALAR);
            H5Tset_size(type_id, string_utf8.length());
            H5Tset_strpad(type_id, H5T_STR_NULLTERM);
            buffer = (void*)string_utf8.c_str();
        } else {
            Dimensions dimsValue = data.getDimensions();
            hsize_t* dimsAsHsize_t = nullptr;
            indexType nbElementsSizeData;
            if (dimsValue.isScalar()) {
                try {
                    dimsAsHsize_t = new_with_exception<hsize_t>(1, true);
                } catch (Exception& e) {
                    error = e.getMessage();
                    dspace_id = H5I_INVALID_HID;
                    return nullptr;
                }
                nbElementsSizeData = 1;
                dimsAsHsize_t[0] = 1;
                dspace_id = H5Screate_simple((int)1, dimsAsHsize_t, dimsAsHsize_t);
            } else {
                try {
                    dimsAsHsize_t = new_with_exception<hsize_t>(dimsValue.getLength(), true);
                } catch (Exception& e) {
                    error = e.getMessage();
                    dspace_id = H5I_INVALID_HID;
                    return nullptr;
                }
                nbElementsSizeData = dimsValue.getLength();
                for (indexType k = 1; k <= nbElementsSizeData; k++) {
                    dimsAsHsize_t[k - 1] = (hsize_t)dimsValue[nbElementsSizeData - k];
                }
                dspace_id
                    = H5Screate_simple((int)dimsValue.getLength(), dimsAsHsize_t, dimsAsHsize_t);
            }
            delete[] dimsAsHsize_t;
            buffer = const_cast<void*>(data.getDataPointer());
        }
    }
    return buffer;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
