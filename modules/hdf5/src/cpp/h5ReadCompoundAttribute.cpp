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
#include <memory>
#include "h5ReadCompoundAttribute.hpp"
#include "h5ReadHelpers.hpp"
#include "h5ReadString.hpp"
#include "h5ReadFloat.hpp"
#include "h5ReadBitfieldAttribute.hpp"
#include "h5ReadOpaque.hpp"
#include "h5ReadEnum.hpp"
#include "h5ReadArrayAttribute.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
h5ReadCompoundAttributeOpaqueMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    uint8* ptrUINT8 = nullptr;
    try {
        ptrUINT8 = (uint8*)ArrayOf::allocateArrayOf(
            NLS_UINT8, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
    }
    ArrayOf fieldvalue = ArrayOf(NLS_UINT8, dims, ptrUINT8);
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        ptrUINT8[k] = ((uint8*)(data + offset + (sizeType * k)))[0];
    }
    return fieldvalue;
}
//=============================================================================
static ArrayOf
h5ReadCompoundAttributeBitfieldMember(hsize_t sizeType, hid_t mType, const char* data,
    size_t offset, const Dimensions& dims, std::wstring& error)
{
    Class outputClass;
    hsize_t msizeType = H5Tget_size(mType);
    switch (msizeType) {
    case 1: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    case 2: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT16;
        } else {
            outputClass = NLS_INT16;
        }
    } break;
    case 4: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT32;
        } else {
            outputClass = NLS_INT32;
        }
    } break;
    case 8: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT64;
        } else {
            outputClass = NLS_INT64;
        }
    } break;
    default: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    }
    void* ptrVoid = nullptr;
    try {
        ptrVoid
            = ArrayOf::allocateArrayOf(outputClass, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
    }
    ArrayOf fieldvalue = ArrayOf(outputClass, dims, ptrVoid);
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        switch (outputClass) {
        case NLS_UINT8: {
            uint8* ptrUINT8 = (uint8*)ptrVoid;
            ptrUINT8[k] = ((uint8*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT16: {
            uint16* ptrUINT16 = (uint16*)ptrVoid;
            ptrUINT16[k] = ((uint16*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT32: {
            uint32* ptrUINT32 = (uint32*)ptrVoid;
            ptrUINT32[k] = ((uint32*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT64: {
            uint64* ptrUINT64 = (uint64*)ptrVoid;
            ptrUINT64[k] = ((uint64*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT8: {
            int8* ptrINT8 = (int8*)ptrVoid;
            ptrINT8[k] = ((int8*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT16: {
            int16* ptrINT16 = (int16*)ptrVoid;
            ptrINT16[k] = ((int16*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT32: {
            int32* ptrINT32 = (int32*)ptrVoid;
            ptrINT32[k] = ((int32*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT64: {
            int64* ptrINT64 = (int64*)ptrVoid;
            ptrINT64[k] = ((int64*)(data + offset + (sizeType * k)))[0];
        } break;
        }
    }
    return fieldvalue;
}
//=============================================================================
static ArrayOf
h5ReadCompoundAttributeIntegerMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    Class outputClass;
    hsize_t msizeType = H5Tget_size(mType);
    switch (msizeType) {
    case 1: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    case 2: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT16;
        } else {
            outputClass = NLS_INT16;
        }
    } break;
    case 4: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT32;
        } else {
            outputClass = NLS_INT32;
        }
    } break;
    case 8: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT64;
        } else {
            outputClass = NLS_INT64;
        }
    } break;
    default: {
        if (H5Tget_sign(mType) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    }
    void* ptrVoid = nullptr;
    try {
        ptrVoid
            = ArrayOf::allocateArrayOf(outputClass, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
    }
    ArrayOf fieldvalue = ArrayOf(outputClass, dims, ptrVoid);
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        switch (outputClass) {
        case NLS_UINT8: {
            uint8* ptrUINT8 = (uint8*)ptrVoid;
            ptrUINT8[k] = ((uint8*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT16: {
            uint16* ptrUINT16 = (uint16*)ptrVoid;
            ptrUINT16[k] = ((uint16*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT32: {
            uint32* ptrUINT32 = (uint32*)ptrVoid;
            ptrUINT32[k] = ((uint32*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT64: {
            uint64* ptrUINT64 = (uint64*)ptrVoid;
            ptrUINT64[k] = ((uint64*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT8: {
            int8* ptrINT8 = (int8*)ptrVoid;
            ptrINT8[k] = ((int8*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT16: {
            int16* ptrINT16 = (int16*)ptrVoid;
            ptrINT16[k] = ((int16*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT32: {
            int32* ptrINT32 = (int32*)ptrVoid;
            ptrINT32[k] = ((int32*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT64: {
            int64* ptrINT64 = (int64*)ptrVoid;
            ptrINT64[k] = ((int64*)(data + offset + (sizeType * k)))[0];
        } break;
        }
    }
    return fieldvalue;
}
//=============================================================================
static ArrayOf
h5ReadCompoundAttributeStringMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    ArrayOf* ptrArrayOf = nullptr;
    try {
        ptrArrayOf = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
    }
    ArrayOf fieldvalue = ArrayOf(NLS_CELL_ARRAY, dims, ptrArrayOf);
    if (H5Tis_variable_str(mType)) {
        for (indexType k = 0; k < dims.getElementCount(); k++) {
            char** value = (char**)(data + offset + (sizeType * k));
            ptrArrayOf[k] = ArrayOf::characterArrayConstructor(std::string(value[0]));
        }
    } else {
        hsize_t lengthString = H5Tget_size(mType);
        for (indexType k = 0; k < dims.getElementCount(); k++) {
            char* value = (char*)(data + offset + (sizeType * k));
            std::string str;
            str.reserve(lengthString);
            for (indexType l = 0; l < lengthString; l++) {
                str.push_back(value[l]);
            }
            ptrArrayOf[k] = ArrayOf::characterArrayConstructor(str);
        }
    }
    return fieldvalue;
}
//=============================================================================
static ArrayOf
h5ReadCompoundAttributeFloatMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    ArrayOf fieldvalue;
    hsize_t sizeMembertype = H5Tget_size(mType);
    Class outputClass;
    switch (sizeMembertype) {
    case 4: {
        outputClass = NLS_SINGLE;
    } break;
    default:
    case 8: {
        outputClass = NLS_DOUBLE;
    } break;
    }
    void* ptrVoid = nullptr;
    try {
        ptrVoid
            = ArrayOf::allocateArrayOf(outputClass, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
    }
    fieldvalue = ArrayOf(outputClass, dims, ptrVoid);
    auto* ptrDouble = static_cast<double*>(ptrVoid);
    single* ptrSingle = (single*)ptrVoid;
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        if (outputClass == NLS_SINGLE) {
            single value = ((single*)(data + offset + (sizeType * k)))[0];
            ptrSingle[k] = value;
        } else {
            double value = ((double*)(data + offset + (sizeType * k)))[0];
            ptrDouble[k] = value;
        }
    }
    return fieldvalue;
}
//=============================================================================
ArrayOf
h5ReadCompoundAttribute(hid_t attr_id, hid_t type, hid_t aspace, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5Aget_storage_size(attr_id);
    hsize_t sizeType = H5Tget_size(type);
    int rank;
    Dimensions dims = getDimensions(aspace, rank);

    std::unique_ptr<char[]> data(new char[storageSize]);
    if (!data.get()) {
        error = _W("Cannot read attribute.");
        return ArrayOf();
    }
    if (H5Aread(attr_id, type, data.get()) < 0) {
        error = _W("Cannot read attribute.");
        return ArrayOf();
    }

    stringVector fieldnames;
    ArrayOfVector fieldvalues;

    int nmembers = H5Tget_nmembers(type);
    for (int mIndex = 0; mIndex < nmembers; mIndex++) {
        hid_t mType = H5Tget_member_type(type, mIndex);
        char* fieldname = H5Tget_member_name(type, mIndex);
        fieldnames.push_back(std::string(fieldname));
#if H5_VERS_MAJOR <= 1 && H5_VERS_MINOR < 9
        free(fieldname);
#else
        H5free_memory(fieldname);
#endif
        size_t offset = H5Tget_member_offset(type, (unsigned int)mIndex);
        ArrayOf fieldvalue;
        switch (H5Tget_member_class(type, mIndex)) {
        case H5T_INTEGER: {
            fieldvalue = h5ReadCompoundAttributeIntegerMember(
                sizeType, mType, data.get(), offset, dims, error);
        } break;
        case H5T_STRING: {
            fieldvalue = h5ReadCompoundAttributeStringMember(
                sizeType, mType, data.get(), offset, dims, error);
        } break;
        case H5T_FLOAT: {
            fieldvalue = h5ReadCompoundAttributeFloatMember(
                sizeType, mType, data.get(), offset, dims, error);
        } break;
        case H5T_BITFIELD: {
            fieldvalue = h5ReadCompoundAttributeBitfieldMember(
                sizeType, mType, data.get(), offset, dims, error);
        } break;
        case H5T_OPAQUE: {
            fieldvalue = h5ReadCompoundAttributeOpaqueMember(
                sizeType, mType, data.get(), offset, dims, error);
        } break;
        case H5T_TIME:
            /* The time datatype, H5T_TIME,
                has not been fully implemented and is not supported.If H5T_TIME is used,
                the resulting data will be readable
                and modifiable only on the originating computing platform;
                it will not be portable to other platforms. */
        case H5T_REFERENCE:
        case H5T_ENUM:
        case H5T_VLEN:
        case H5T_ARRAY:
        case H5T_NO_CLASS:
        default: {
            error = _W("Type not managed.");
        } break;
        }

        if (error.empty()) {
            fieldvalues.push_back(fieldvalue);
        } else {
            break;
        }
    }
    if (error.empty()) {
        res = ArrayOf::structScalarConstructor(fieldnames, fieldvalues);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
