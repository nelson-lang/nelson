//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
// Header only: matio uses it without linking the hdf5 module.
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include <cstdint>
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "PartialFileAccess.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// one value of a numeric, logical or char dataset (both halves of a complex value)
inline hid_t
h5HyperslabElementType(NelsonType type)
{
    switch (type) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX:
        return H5T_NATIVE_DOUBLE;
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
        return H5T_NATIVE_FLOAT;
    case NLS_INT8:
        return H5T_NATIVE_INT8;
    case NLS_UINT8:
    case NLS_LOGICAL:
        return H5T_NATIVE_UINT8;
    case NLS_INT16:
        return H5T_NATIVE_INT16;
    case NLS_UINT16:
    case NLS_CHAR:
        return H5T_NATIVE_UINT16;
    case NLS_INT32:
        return H5T_NATIVE_INT32;
    case NLS_UINT32:
        return H5T_NATIVE_UINT32;
    case NLS_INT64:
        return H5T_NATIVE_INT64;
    case NLS_UINT64:
        return H5T_NATIVE_UINT64;
    default:
        return H5I_INVALID_HID;
    }
}
//=============================================================================
// reads (write = false) or writes the strided block of a dataset; data holds the block values
inline bool
h5HyperslabTransfer(hid_t dataset, const PartialFileBlock& block, size_t rank, NelsonType type,
    void* data, indexType count, bool write)
{
    const hid_t elementType = h5HyperslabElementType(type);
    if (elementType == H5I_INVALID_HID || block.count.size() < rank) {
        return false;
    }
    const bool isComplex = type == NLS_DCOMPLEX || type == NLS_SCOMPLEX;
    hid_t fileType = H5Dget_type(dataset);
    const bool fileIsCompound = H5Tget_class(fileType) == H5T_COMPOUND;
    H5Tclose(fileType);
    hid_t fileSpace = H5Dget_space(dataset);
    if (fileIsCompound != isComplex
        || H5Sget_simple_extent_ndims(fileSpace) != static_cast<int>(rank)) {
        H5Sclose(fileSpace);
        return false;
    }
    // the file stores the dimensions in reverse order
    std::vector<hsize_t> hStart(rank), hStride(rank), hCount(rank);
    for (size_t k = 0; k < rank; k++) {
        hStart[k] = static_cast<hsize_t>(block.start[rank - 1 - k] - 1);
        hStride[k] = static_cast<hsize_t>(block.stride[rank - 1 - k]);
        hCount[k] = static_cast<hsize_t>(block.count[rank - 1 - k]);
    }
    bool ok = H5Sselect_hyperslab(
                  fileSpace, H5S_SELECT_SET, hStart.data(), hStride.data(), hCount.data(), nullptr)
        >= 0;
    hid_t memorySpace = H5Screate_simple(static_cast<int>(rank), hCount.data(), nullptr);
    hid_t memoryType = elementType;
    if (isComplex) {
        const size_t partSize = H5Tget_size(elementType);
        memoryType = H5Tcreate(H5T_COMPOUND, 2 * partSize);
        H5Tinsert(memoryType, "real", 0, elementType);
        H5Tinsert(memoryType, "imag", partSize, elementType);
    }
    // Nelson characters are wchar_t, the file holds 16-bit code units
    std::vector<uint16_t> chars;
    void* buffer = data;
    if (type == NLS_CHAR) {
        chars.resize(count);
        if (write) {
            for (indexType k = 0; k < count; k++) {
                chars[k] = static_cast<uint16_t>(static_cast<const charType*>(data)[k]);
            }
        }
        buffer = chars.data();
    }
    if (ok) {
        ok = write ? H5Dwrite(dataset, memoryType, memorySpace, fileSpace, H5P_DEFAULT, buffer) >= 0
                   : H5Dread(dataset, memoryType, memorySpace, fileSpace, H5P_DEFAULT, buffer) >= 0;
    }
    if (ok && !write && type == NLS_CHAR) {
        for (indexType k = 0; k < count; k++) {
            static_cast<charType*>(data)[k] = static_cast<charType>(chars[k]);
        }
    }
    if (isComplex) {
        H5Tclose(memoryType);
    }
    H5Sclose(memorySpace);
    H5Sclose(fileSpace);
    return ok;
}
//=============================================================================
// opens path in the file, then reads or writes the block; HDF5 errors stay silent
inline bool
h5HyperslabFileTransfer(const std::string& utf8filename, const std::string& path,
    const PartialFileBlock& block, size_t rank, NelsonType type, void* data, indexType count,
    bool write)
{
    bool ok = false;
    H5E_BEGIN_TRY
    {
        hid_t file
            = H5Fopen(utf8filename.c_str(), write ? H5F_ACC_RDWR : H5F_ACC_RDONLY, H5P_DEFAULT);
        if (file >= 0) {
            hid_t dataset = H5Dopen2(file, path.c_str(), H5P_DEFAULT);
            if (dataset >= 0) {
                ok = h5HyperslabTransfer(dataset, block, rank, type, data, count, write);
                H5Dclose(dataset);
            }
            H5Fclose(file);
        }
    }
    H5E_END_TRY
    return ok;
}
//=============================================================================
// reads a block into a new array of the block dimensions
inline bool
h5HyperslabRead(const std::string& utf8filename, const std::string& path,
    const PartialFileBlock& block, size_t rank, NelsonType type, ArrayOf& result)
{
    const indexType count = block.elementCount();
    void* data = ArrayOf::allocateArrayOf(type, count, stringVector(), false);
    ArrayOf owner(type, block.resultDimensions(), data);
    if (!h5HyperslabFileTransfer(utf8filename, path, block, rank, type, data, count, false)) {
        return false;
    }
    result = owner;
    return true;
}
//=============================================================================
inline bool
h5HyperslabWrite(const std::string& utf8filename, const std::string& path,
    const PartialFileBlock& block, size_t rank, const ArrayOf& data)
{
    return h5HyperslabFileTransfer(utf8filename, path, block, rank, data.dataClass(),
        const_cast<void*>(data.dataPointer()), block.elementCount(), true);
}
//=============================================================================
inline bool
h5UnlinkVariable(const std::string& utf8filename, const std::string& path)
{
    bool ok = false;
    H5E_BEGIN_TRY
    {
        hid_t file = H5Fopen(utf8filename.c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
        if (file >= 0) {
            ok = H5Ldelete(file, path.c_str(), H5P_DEFAULT) >= 0;
            H5Fclose(file);
        }
    }
    H5E_END_TRY
    return ok;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
