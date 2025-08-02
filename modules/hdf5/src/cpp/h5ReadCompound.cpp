//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <memory>
#include "h5ReadCompound.hpp"
#include "h5ReadHelpers.hpp"
#include "h5ReadString.hpp"
#include "h5ReadFloat.hpp"
#include "h5ReadBitfield.hpp"
#include "h5ReadOpaque.hpp"
#include "h5ReadEnum.hpp"
#include "h5ReadArray.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
h5ReadCompoundOpaqueMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    uint8* ptrUINT8 = nullptr;
    try {
        ptrUINT8 = (uint8*)ArrayOf::allocateArrayOf(
            NLS_UINT8, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return {};
    }
    ArrayOf fieldvalue = ArrayOf(NLS_UINT8, dims, ptrUINT8);
    ompIndexType elementCount = dims.getElementCount();
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        ptrUINT8[k] = ((uint8*)(data + offset + (sizeType * k)))[0];
    }
    return fieldvalue;
}
//=============================================================================
static ArrayOf
h5ReadCompoundIntegerMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    NelsonType outputClass;
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
        return {};
    }
    ArrayOf fieldvalue = ArrayOf(outputClass, dims, ptrVoid);
    ompIndexType elementCount = dims.getElementCount();
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        switch (outputClass) {
        case NLS_UINT8: {
            auto* ptrUINT8 = static_cast<uint8*>(ptrVoid);
            ptrUINT8[k] = ((uint8*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT16: {
            auto* ptrUINT16 = static_cast<uint16*>(ptrVoid);
            ptrUINT16[k] = ((uint16*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT32: {
            auto* ptrUINT32 = static_cast<uint32*>(ptrVoid);
            ptrUINT32[k] = ((uint32*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_UINT64: {
            auto* ptrUINT64 = static_cast<uint64*>(ptrVoid);
            ptrUINT64[k] = ((uint64*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT8: {
            int8* ptrINT8 = static_cast<int8*>(ptrVoid);
            ptrINT8[k] = ((int8*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT16: {
            auto* ptrINT16 = static_cast<int16*>(ptrVoid);
            ptrINT16[k] = ((int16*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT32: {
            auto* ptrINT32 = static_cast<int32*>(ptrVoid);
            ptrINT32[k] = ((int32*)(data + offset + (sizeType * k)))[0];
        } break;
        case NLS_INT64: {
            auto* ptrINT64 = static_cast<int64*>(ptrVoid);
            ptrINT64[k] = ((int64*)(data + offset + (sizeType * k)))[0];
        } break;
        default: {
        } break;
        }
    }
    return fieldvalue;
}
//=============================================================================
static ArrayOf
h5ReadCompoundBitfieldMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    return h5ReadCompoundIntegerMember(sizeType, mType, data, offset, dims, error);
}
//=============================================================================
static ArrayOf
h5ReadCompoundStringMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    ArrayOf* ptrArrayOf = nullptr;
    try {
        ptrArrayOf = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return {};
    }
    ArrayOf fieldvalue = ArrayOf(NLS_CELL_ARRAY, dims, ptrArrayOf);
    ompIndexType elementCount = dims.getElementCount();
    if (H5Tis_variable_str(mType)) {
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            char** value = (char**)(data + offset + (sizeType * k));
            ptrArrayOf[k] = ArrayOf::characterArrayConstructor(std::string(value[0]));
        }
    } else {
        hsize_t lengthString = H5Tget_size(mType);
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            char* value = (char*)(data + offset + (sizeType * k));
            std::string str;
            str.reserve((size_t)lengthString);
            for (indexType l = 0; l < (indexType)lengthString; l++) {
                str.push_back(value[l]);
            }
            ptrArrayOf[k] = ArrayOf::characterArrayConstructor(str);
        }
    }
    return fieldvalue;
}
//=============================================================================
static ArrayOf
h5ReadCompoundFloatMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    ArrayOf fieldvalue;
    hsize_t sizeMembertype = H5Tget_size(mType);
    NelsonType outputClass;
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
        return {};
    }
    fieldvalue = ArrayOf(outputClass, dims, ptrVoid);
    auto* ptrDouble = static_cast<double*>(ptrVoid);
    auto* ptrSingle = static_cast<single*>(ptrVoid);
    ompIndexType elementCount = dims.getElementCount();
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
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
h5ReadCompound(hid_t attr_id, hid_t type, hid_t aspace, bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5I_INVALID_HID;
    if (asAttribute) {
        storageSize = H5Aget_storage_size(attr_id);
    } else {
        storageSize = H5Dget_storage_size(attr_id);
    }
    hsize_t sizeType = H5Tget_size(type);
    int rank;
    Dimensions dims = getDimensions(aspace, rank);

    std::unique_ptr<char[]> data(new char[size_t(storageSize * sizeType)]);
    if (!data.get()) {
        if (asAttribute) {
            error = _W("Cannot read attribute.");
        } else {
            error = _W("Cannot read data set.");
        }
        return {};
    }
    hid_t memspace = H5I_INVALID_HID;
    if (!asAttribute) {
        hsize_t* h5_dims = nullptr;
        hsize_t* h5_maxdims = nullptr;
        try {
            h5_dims = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
        } catch (Exception& e) {
            error = e.getMessage();
            return {};
        }
        try {
            h5_maxdims
                = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
        } catch (Exception& e) {
            error = e.getMessage();
            return {};
        }
        if (H5Sget_simple_extent_dims(aspace, h5_dims, h5_maxdims) < 0) {
            delete[] h5_dims;
            delete[] h5_maxdims;
            Error("Impossible to read dimensions and maximum size of data set.");
            return {};
        }
        memspace = H5Screate_simple(rank, h5_dims, nullptr);
        delete[] h5_dims;
        delete[] h5_maxdims;
    }

    herr_t h5readStatus = H5I_INVALID_HID;
    if (asAttribute) {
        h5readStatus = H5Aread(attr_id, type, data.get());
    } else {
        h5readStatus = H5Dread(attr_id, type, memspace, aspace, H5P_DEFAULT, data.get());
    }

    if (h5readStatus < 0) {
        if (asAttribute) {
            error = _W("Cannot read attribute.");
        } else {
            H5Sclose(memspace);
            error = _W("Cannot read data set.");
        }
        return {};
    }

    if (!asAttribute) {
        H5Sclose(memspace);
    }
    stringVector fieldnames;
    ArrayOfVector fieldvalues;

    int nmembers = H5Tget_nmembers(type);
    for (int mIndex = 0; mIndex < nmembers; mIndex++) {
        hid_t mType = H5Tget_member_type(type, mIndex);
        char* fieldname = H5Tget_member_name(type, mIndex);
        fieldnames.emplace_back(std::string(fieldname));
#if H5_VERS_MAJOR <= 1 && H5_VERS_MINOR < 9
        free(fieldname);
#else
        H5free_memory(fieldname);
#endif
        size_t offset = H5Tget_member_offset(type, (unsigned int)mIndex);
        ArrayOf fieldvalue;
        const char* pAsChar = data.get();
        switch (H5Tget_member_class(type, mIndex)) {
        case H5T_INTEGER: {
            fieldvalue = h5ReadCompoundIntegerMember(sizeType, mType, pAsChar, offset, dims, error);
        } break;
        case H5T_STRING: {
            fieldvalue = h5ReadCompoundStringMember(sizeType, mType, pAsChar, offset, dims, error);
        } break;
        case H5T_FLOAT: {
            fieldvalue = h5ReadCompoundFloatMember(sizeType, mType, pAsChar, offset, dims, error);
        } break;
        case H5T_BITFIELD: {
            fieldvalue
                = h5ReadCompoundBitfieldMember(sizeType, mType, pAsChar, offset, dims, error);
        } break;
        case H5T_OPAQUE: {
            fieldvalue = h5ReadCompoundOpaqueMember(sizeType, mType, pAsChar, offset, dims, error);
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
        H5Tclose(mType);
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
