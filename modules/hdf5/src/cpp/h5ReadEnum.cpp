//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadEnum.hpp"
#include "h5ReadHelpers.hpp"
#include "NewWithException.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadEnum(hid_t dset_id, hid_t type_id, hid_t dspace_id, bool asAttribute, std::wstring& error)
{
    hsize_t storageSize = H5I_INVALID_HID;
    if (asAttribute) {
        storageSize = H5Aget_storage_size(dset_id);
    } else {
        storageSize = H5Dget_storage_size(dset_id);
    }
    hsize_t sizeType = H5Tget_size(type_id);
    int rank;
    Dimensions dims = dimensions(dspace_id, rank);
    if ((rank == 0) && (storageSize == 0)) {
        dims = Dimensions(0, 0);
    }
    ArrayOf* elements;
    try {
        elements = new_with_exception<ArrayOf>(dims.elementCount(), false);
    } catch (NelsonException& e) {
        error = e.message();
        return {};
    }
    NelsonType outputClass;
    switch (sizeType) {
    case 1: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    case 2: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT16;
        } else {
            outputClass = NLS_INT16;
        }
    } break;
    case 4: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT32;
        } else {
            outputClass = NLS_INT32;
        }
    } break;
    case 8: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT64;
        } else {
            outputClass = NLS_INT64;
        }
    } break;
    default: {
        if (H5Tget_sign(type_id) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    }
    void* buffer = nullptr;
    try {
        buffer = ArrayOf::allocateArrayOf(outputClass, dims.elementCount(), stringVector(), true);
    } catch (NelsonException& e) {
        delete[] elements;
        error = e.message();
        return {};
    }
    herr_t status = H5I_INVALID_HID;
    if (asAttribute) {
        status = H5Aread(dset_id, type_id, buffer);
    } else {
        status = H5Dread(dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
    }
    if (status < 0) {
        delete[] elements;
        if (asAttribute) {
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadAttribute", L"Cannot read attribute.");
        } else {
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadDataSet", L"Cannot read data set.");
        }
        return {};
    }
    indexType pos = 0;
#define LENGTH_NAME_MAX 1024
    char name[LENGTH_NAME_MAX] = { 0 };

    uint8* ptrAsUINT8 = nullptr;
    int8* ptrAsINT8 = nullptr;
    uint16* ptrAsUINT16 = nullptr;
    int16* ptrAsINT16 = nullptr;
    uint32* ptrAsUINT32 = nullptr;
    int32* ptrAsINT32 = nullptr;
    uint64* ptrAsUINT64 = nullptr;
    int64* ptrAsINT64 = nullptr;
    switch (outputClass) {
    case NLS_UINT8: {
        ptrAsUINT8 = (uint8*)buffer;
    } break;
    case NLS_UINT16: {
        ptrAsUINT16 = (uint16*)buffer;
    } break;
    case NLS_UINT32: {
        ptrAsUINT32 = (uint32*)buffer;
    } break;
    case NLS_UINT64: {
        ptrAsUINT64 = (uint64*)buffer;
    } break;
    case NLS_INT8: {
        ptrAsINT8 = (int8*)buffer;
    } break;
    case NLS_INT16: {
        ptrAsINT16 = (int16*)buffer;
    } break;
    case NLS_INT32: {
        ptrAsINT32 = (int32*)buffer;
    } break;
    case NLS_INT64: {
        ptrAsINT64 = (int64*)buffer;
    } break;
    default: {
        delete[] elements;
        error = _W("Type not supported.");
        return {};
    } break;
    }
    indexType k = 0;
    indexType elementCount = dims.elementCount();
    for (indexType i = 0; i < elementCount; i++) {
        switch (outputClass) {
        case NLS_UINT8: {
            if (ptrAsUINT8) {
                status = H5Tenum_nameof(type_id, &ptrAsUINT8[i], name, LENGTH_NAME_MAX);
            }
        } break;
        case NLS_UINT16: {
            if (ptrAsUINT16) {
                status = H5Tenum_nameof(type_id, &ptrAsUINT16[i], name, LENGTH_NAME_MAX);
            }
        } break;
        case NLS_UINT32: {
            if (ptrAsUINT32) {
                status = H5Tenum_nameof(type_id, &ptrAsUINT32[i], name, LENGTH_NAME_MAX);
            }
        } break;
        case NLS_UINT64: {
            if (ptrAsUINT64) {
                status = H5Tenum_nameof(type_id, &ptrAsUINT64[i], name, LENGTH_NAME_MAX);
            }
        } break;
        case NLS_INT8: {
            if (ptrAsINT8) {
                status = H5Tenum_nameof(type_id, &ptrAsINT8[i], name, LENGTH_NAME_MAX);
            }
        } break;
        case NLS_INT16: {
            if (ptrAsINT16) {
                status = H5Tenum_nameof(type_id, &ptrAsINT16[i], name, LENGTH_NAME_MAX);
            }
        } break;
        case NLS_INT32: {
            if (ptrAsINT32) {
                status = H5Tenum_nameof(type_id, &ptrAsINT32[i], name, LENGTH_NAME_MAX);
            }
        } break;
        case NLS_INT64: {
            if (ptrAsINT64) {
                status = H5Tenum_nameof(type_id, &ptrAsINT64[i], name, LENGTH_NAME_MAX);
            }
        } break;
        default: {
        } break;
        }
        if (status < 0) {
            // The value is not in the enum definition. The name buffer is not valid,
            // so use the numeric value as text.
            std::string placeholder;
            switch (outputClass) {
            case NLS_UINT8:
                placeholder = ptrAsUINT8 ? std::to_string(ptrAsUINT8[i]) : "<unknown>";
                break;
            case NLS_UINT16:
                placeholder = ptrAsUINT16 ? std::to_string(ptrAsUINT16[i]) : "<unknown>";
                break;
            case NLS_UINT32:
                placeholder = ptrAsUINT32 ? std::to_string(ptrAsUINT32[i]) : "<unknown>";
                break;
            case NLS_UINT64:
                placeholder = ptrAsUINT64 ? std::to_string(ptrAsUINT64[i]) : "<unknown>";
                break;
            case NLS_INT8:
                placeholder = ptrAsINT8 ? std::to_string(ptrAsINT8[i]) : "<unknown>";
                break;
            case NLS_INT16:
                placeholder = ptrAsINT16 ? std::to_string(ptrAsINT16[i]) : "<unknown>";
                break;
            case NLS_INT32:
                placeholder = ptrAsINT32 ? std::to_string(ptrAsINT32[i]) : "<unknown>";
                break;
            case NLS_INT64:
                placeholder = ptrAsINT64 ? std::to_string(ptrAsINT64[i]) : "<unknown>";
                break;
            default:
                placeholder = "<unknown>";
                break;
            }
            elements[i] = ArrayOf::characterArray(placeholder);
        } else {
            elements[i] = ArrayOf::characterArray(name);
        }
    }
    if (ptrAsUINT8) {
        delete[] ptrAsUINT8;
    }
    if (ptrAsUINT16) {
        delete[] ptrAsUINT16;
    }
    if (ptrAsUINT32) {
        delete[] ptrAsUINT32;
    }
    if (ptrAsUINT64) {
        delete[] ptrAsUINT64;
    }
    if (ptrAsINT8) {
        delete[] ptrAsINT8;
    }
    if (ptrAsINT16) {
        delete[] ptrAsINT16;
    }
    if (ptrAsINT32) {
        delete[] ptrAsINT32;
    }
    if (ptrAsINT64) {
        delete[] ptrAsINT64;
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
