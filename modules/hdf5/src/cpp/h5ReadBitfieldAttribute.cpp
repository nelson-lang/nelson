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
#include "h5ReadBitfieldAttribute.hpp"
#include "h5ReadAttributeHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadBitfieldAttribute(hid_t attr_id, hid_t type, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5Aget_storage_size(attr_id);
    hsize_t sizeType = H5Tget_size(type);
    size_t numVal = storageSize / sizeType;
    hid_t aspace = H5Aget_space(attr_id);
    Dimensions dims = getDimensions(aspace);
    Class outputClass;
    void* ptrVoid = nullptr;
    hid_t dataType;
    switch (sizeType) {
    case 1: {
        outputClass = NLS_UINT8;
        dataType = H5Tcopy(H5T_NATIVE_B8);
    } break;
    case 2: {
        outputClass = NLS_UINT16;
        dataType = H5Tcopy(H5T_NATIVE_B16);
    } break;
    case 4: {
        outputClass = NLS_UINT32;
        dataType = H5Tcopy(H5T_NATIVE_B32);
    } break;
    case 8: {
        outputClass = NLS_UINT64;
        dataType = H5Tcopy(H5T_NATIVE_B64);
    } break;
    default: {
        outputClass = NLS_UINT8;
        dataType = H5Tcopy(H5T_NATIVE_B8);
    } break;
    }
    if (dims.isEmpty(false)) {
        res = ArrayOf::emptyConstructor(dims);
        res.promoteType(outputClass);
    } else {
        ptrVoid
            = ArrayOf::allocateArrayOf(outputClass, dims.getElementCount(), stringVector(), false);
    }
    if (H5Aread(attr_id, dataType, ptrVoid) < 0) {
        error = _W("Cannot read attribute.");
        res = ArrayOf(outputClass, dims, ptrVoid);
        res = ArrayOf();
    } else {
        res = ArrayOf(outputClass, dims, ptrVoid);
    }
    H5Sclose(aspace);
    return res;
}
//=============================================================================
}  // namespace Nelson
//=============================================================================
