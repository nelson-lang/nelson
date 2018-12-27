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
#include "h5ReadEnumAttribute.hpp"
#include "h5ReadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadEnumAttribute(hid_t attr_id, hid_t type, hid_t aspace, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5Aget_storage_size(attr_id);
    hsize_t sizeType = H5Tget_size(type);
    int rank;
    Dimensions dims = getDimensions(aspace, rank);
    ArrayOf* elements;
    try {
        elements = new_with_exception<ArrayOf>(dims.getElementCount(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        H5Sclose(aspace);
        return res;
    }
    Class outputClass;
    switch (sizeType) {
    case 1: {
        if (H5Tget_sign(type) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    case 2: {
        if (H5Tget_sign(type) == H5T_SGN_NONE) {
            outputClass = NLS_UINT16;
        } else {
            outputClass = NLS_INT16;
        }
    } break;
    case 4: {
        if (H5Tget_sign(type) == H5T_SGN_NONE) {
            outputClass = NLS_UINT32;
        } else {
            outputClass = NLS_INT32;
        }
    } break;
    case 8: {
        if (H5Tget_sign(type) == H5T_SGN_NONE) {
            outputClass = NLS_UINT64;
        } else {
            outputClass = NLS_INT64;
        }
    } break;
    default: {
        if (H5Tget_sign(type) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    }
    void* buffer
        = ArrayOf::allocateArrayOf(outputClass, dims.getElementCount(), stringVector(), true);
    if (H5Aread(attr_id, type, buffer) < 0) {
        delete[] elements;
        error = _W("Cannot read attribute.");
        return res;
    }
    indexType pos = 0;
#define LENGTH_NAME_MAX 1024
    char name[LENGTH_NAME_MAX];

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
    default: { } break; }
    indexType k = 0;
    herr_t status;
    for (indexType i = 0; i < dims.getElementCount(); i++) {
        switch (outputClass) {
        case NLS_UINT8: {
            status = H5Tenum_nameof(type, &ptrAsUINT8[i], name, LENGTH_NAME_MAX);
        } break;
        case NLS_UINT16: {
            status = H5Tenum_nameof(type, &ptrAsUINT16[i], name, LENGTH_NAME_MAX);
        } break;
        case NLS_UINT32: {
            status = H5Tenum_nameof(type, &ptrAsUINT32[i], name, LENGTH_NAME_MAX);
        } break;
        case NLS_UINT64: {
            status = H5Tenum_nameof(type, &ptrAsUINT64[i], name, LENGTH_NAME_MAX);
        } break;
        case NLS_INT8: {
            status = H5Tenum_nameof(type, &ptrAsINT8[i], name, LENGTH_NAME_MAX);
        } break;
        case NLS_INT16: {
            status = H5Tenum_nameof(type, &ptrAsINT16[i], name, LENGTH_NAME_MAX);
        } break;
        case NLS_INT32: {
            status = H5Tenum_nameof(type, &ptrAsINT32[i], name, LENGTH_NAME_MAX);
        } break;
        case NLS_INT64: {
            status = H5Tenum_nameof(type, &ptrAsINT64[i], name, LENGTH_NAME_MAX);
        } break;
        default: { } break; }
        elements[i] = ArrayOf::characterArrayConstructor(name);
    }
    if (ptrAsUINT8)
        delete[] ptrAsUINT8;
    if (ptrAsUINT16)
        delete[] ptrAsUINT16;
    if (ptrAsUINT32)
        delete[] ptrAsUINT32;
    if (ptrAsUINT64)
        delete[] ptrAsUINT64;
    if (ptrAsINT8)
        delete[] ptrAsINT8;
    if (ptrAsINT16)
        delete[] ptrAsINT16;
    if (ptrAsINT32)
        delete[] ptrAsINT32;
    if (ptrAsINT64)
        delete[] ptrAsINT64;
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
