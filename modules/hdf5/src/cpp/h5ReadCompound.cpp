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
#include <cstring>
#include "h5ReadCompound.hpp"
#include "h5ReadHelpers.hpp"
#include "h5ReadString.hpp"
#include "h5ReadFloat.hpp"
#include "h5ReadBitfield.hpp"
#include "h5ReadOpaque.hpp"
#include "h5ReadEnum.hpp"
#include "h5ReadArray.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Compound members are packed at arbitrary offsets. A cast and dereference would be a
// misaligned load (undefined behaviour), so the bytes are copied.
template <typename T>
static inline T
readUnalignedMember(const char* p)
{
    T v;
    std::memcpy(&v, p, sizeof(T));
    return v;
}
//=============================================================================
static ArrayOf
h5ReadCompoundOpaqueMember(hsize_t sizeType, hid_t mType, const char* data, size_t offset,
    const Dimensions& dims, std::wstring& error)
{
    uint8* ptrUINT8 = nullptr;
    try {
        ptrUINT8 = (uint8*)ArrayOf::allocateArrayOf(
            NLS_UINT8, dims.elementCount(), stringVector(), false);
    } catch (NelsonException& e) {
        error = e.message();
        return {};
    }
    ArrayOf fieldvalue = ArrayOf(NLS_UINT8, dims, ptrUINT8);
    ompIndexType elementCount = dims.elementCount();
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        ptrUINT8[k] = readUnalignedMember<uint8>(data + offset + (sizeType * k));
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
        ptrVoid = ArrayOf::allocateArrayOf(outputClass, dims.elementCount(), stringVector(), false);
    } catch (NelsonException& e) {
        error = e.message();
        return {};
    }
    ArrayOf fieldvalue = ArrayOf(outputClass, dims, ptrVoid);
    ompIndexType elementCount = dims.elementCount();
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        switch (outputClass) {
        case NLS_UINT8: {
            auto* ptrUINT8 = static_cast<uint8*>(ptrVoid);
            ptrUINT8[k] = readUnalignedMember<uint8>(data + offset + (sizeType * k));
        } break;
        case NLS_UINT16: {
            auto* ptrUINT16 = static_cast<uint16*>(ptrVoid);
            ptrUINT16[k] = readUnalignedMember<uint16>(data + offset + (sizeType * k));
        } break;
        case NLS_UINT32: {
            auto* ptrUINT32 = static_cast<uint32*>(ptrVoid);
            ptrUINT32[k] = readUnalignedMember<uint32>(data + offset + (sizeType * k));
        } break;
        case NLS_UINT64: {
            auto* ptrUINT64 = static_cast<uint64*>(ptrVoid);
            ptrUINT64[k] = readUnalignedMember<uint64>(data + offset + (sizeType * k));
        } break;
        case NLS_INT8: {
            int8* ptrINT8 = static_cast<int8*>(ptrVoid);
            ptrINT8[k] = readUnalignedMember<int8>(data + offset + (sizeType * k));
        } break;
        case NLS_INT16: {
            auto* ptrINT16 = static_cast<int16*>(ptrVoid);
            ptrINT16[k] = readUnalignedMember<int16>(data + offset + (sizeType * k));
        } break;
        case NLS_INT32: {
            auto* ptrINT32 = static_cast<int32*>(ptrVoid);
            ptrINT32[k] = readUnalignedMember<int32>(data + offset + (sizeType * k));
        } break;
        case NLS_INT64: {
            auto* ptrINT64 = static_cast<int64*>(ptrVoid);
            ptrINT64[k] = readUnalignedMember<int64>(data + offset + (sizeType * k));
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
            NLS_CELL_ARRAY, dims.elementCount(), stringVector(), false);
    } catch (NelsonException& e) {
        error = e.message();
        return {};
    }
    ArrayOf fieldvalue = ArrayOf(NLS_CELL_ARRAY, dims, ptrArrayOf);
    ompIndexType elementCount = dims.elementCount();
    if (H5Tis_variable_str(mType)) {
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            char* value = readUnalignedMember<char*>(data + offset + (sizeType * k));
            ptrArrayOf[k] = ArrayOf::characterArray(std::string(value));
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
            ptrArrayOf[k] = ArrayOf::characterArray(str);
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
        ptrVoid = ArrayOf::allocateArrayOf(outputClass, dims.elementCount(), stringVector(), false);
    } catch (NelsonException& e) {
        error = e.message();
        return {};
    }
    fieldvalue = ArrayOf(outputClass, dims, ptrVoid);
    auto* ptrDouble = static_cast<double*>(ptrVoid);
    auto* ptrSingle = static_cast<single*>(ptrVoid);
    ompIndexType elementCount = dims.elementCount();
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        if (outputClass == NLS_SINGLE) {
            single value = readUnalignedMember<single>(data + offset + (sizeType * k));
            ptrSingle[k] = value;
        } else {
            double value = readUnalignedMember<double>(data + offset + (sizeType * k));
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
    Dimensions dims = dimensions(aspace, rank);

    std::unique_ptr<char[]> data(new char[size_t(storageSize * sizeType)]);
    if (!data.get()) {
        if (asAttribute) {
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadAttribute", L"Cannot read attribute.");
        } else {
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadDataSet", L"Cannot read data set.");
        }
        return {};
    }
    hid_t memspace = H5I_INVALID_HID;
    if (!asAttribute) {
        hsize_t* h5_dims = nullptr;
        hsize_t* h5_maxdims = nullptr;
        try {
            h5_dims = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
        } catch (NelsonException& e) {
            error = e.message();
            return {};
        }
        try {
            h5_maxdims
                = (hsize_t*)new_with_exception<hsize_t>((size_t)rank * sizeof(hsize_t), false);
        } catch (NelsonException& e) {
            error = e.message();
            return {};
        }
        if (H5Sget_simple_extent_dims(aspace, h5_dims, h5_maxdims) < 0) {
            delete[] h5_dims;
            delete[] h5_maxdims;
            Error("Nelson:hdf5:readDimensionsFailed",
                "Impossible to read dimensions and maximum size of data set.");
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
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadAttribute", L"Cannot read attribute.");
        } else {
            H5Sclose(memspace);
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadDataSet", L"Cannot read data set.");
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
            // H5T_TIME is not fully implemented in HDF5 and is not portable across platforms.
        case H5T_REFERENCE:
        case H5T_ENUM:
        case H5T_VLEN:
        case H5T_ARRAY:
        case H5T_NO_CLASS:
        default: {
            error = _W("Type not supported.");
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
        res = ArrayOf::structScalar(fieldnames, fieldvalues);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
