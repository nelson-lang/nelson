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
#include "h5ReadVlenAttribute.hpp"
#include "h5ReadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
h5ReadVlenOpaqueAttribute(
    hid_t attr_id, hid_t stype, const hvl_t* rdata, const Dimensions& dims, std::wstring& error)
{
    ArrayOf* elements = nullptr;
    Class outputClass = NLS_UINT8;
    try {
        elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
    }
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        Dimensions dimsVector(rdata[k].len, 1);
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(outputClass, rdata[k].len, stringVector(), false);
            elements[k] = ArrayOf(outputClass, dimsVector, ptr);
        } catch (Exception& e) {
            error = e.getMessage();
            return ArrayOf();
        }
        memcpy(ptr, rdata[k].p, elements[k].getElementSize() * rdata[k].len);
    }
    return res;
}
//=============================================================================
static ArrayOf
h5ReadVlenFloatAttribute(
    hid_t attr_id, hid_t stype, const hvl_t* rdata, const Dimensions& dims, std::wstring& error)
{
    ArrayOf* elements = nullptr;
    hsize_t sizeSType = H5Tget_size(stype);
    Class outputClass;
    switch (sizeSType) {
    case 4: {
        outputClass = NLS_SINGLE;
    } break;
    case 8: {
        outputClass = NLS_DOUBLE;
    } break;
    default: {
        error = _W("Type not managed.");
        return ArrayOf();
    } break;
    }
    try {
        elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
    }
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        Dimensions dimsVector(rdata[k].len, 1);
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(outputClass, rdata[k].len, stringVector(), false);
            elements[k] = ArrayOf(outputClass, dimsVector, ptr);
        } catch (Exception& e) {
            error = e.getMessage();
            return ArrayOf();
        }
        memcpy(ptr, rdata[k].p, elements[k].getElementSize() * rdata[k].len);
    }
    return res;
}
//=============================================================================
static ArrayOf
h5ReadVlenIntegerAttribute(
    hid_t attr_id, hid_t stype, const hvl_t* rdata, const Dimensions& dims, std::wstring& error)
{
    ArrayOf* elements = nullptr;
    hsize_t sizeSType = H5Tget_size(stype);
    Class outputClass;
    switch (sizeSType) {
    case 1: {
        if (H5Tget_sign(stype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT8;
        } else {
            outputClass = NLS_INT8;
        }
    } break;
    case 2: {
        if (H5Tget_sign(stype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT16;
        } else {
            outputClass = NLS_INT16;
        }
    } break;
    case 4: {
        if (H5Tget_sign(stype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT32;
        } else {
            outputClass = NLS_INT32;
        }
    } break;
    case 8: {
        if (H5Tget_sign(stype) == H5T_SGN_NONE) {
            outputClass = NLS_UINT64;
        } else {
            outputClass = NLS_INT64;
        }
    } break;
    default: {
        error = _W("Type not managed.");
        return ArrayOf();
    } break;
    }
    try {
        elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
    }
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        Dimensions dimsVector(rdata[k].len, 1);
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(outputClass, rdata[k].len, stringVector(), false);
            elements[k] = ArrayOf(outputClass, dimsVector, ptr);
        } catch (Exception& e) {
            error = e.getMessage();
            return ArrayOf();
        }
        memcpy(ptr, rdata[k].p, elements[k].getElementSize() * rdata[k].len);
    }
    return res;
}
//=============================================================================
static ArrayOf
h5ReadVlenBitfieldAttribute(
    hid_t attr_id, hid_t stype, const hvl_t* rdata, const Dimensions& dims, std::wstring& error)
{
    return h5ReadVlenIntegerAttribute(attr_id, stype, rdata, dims, error);
}
//=============================================================================
ArrayOf
h5ReadVlenAttribute(hid_t attr_id, hid_t type, hid_t aspace, std::wstring& error)
{
    hsize_t storageSize = H5Aget_storage_size(attr_id);
    hsize_t sizeType = H5Tget_size(type);
    int rank;
    Dimensions dims = getDimensions(aspace, rank);
    hvl_t* rdata = nullptr; 
	try {
        rdata = (hvl_t*)new_with_exception<hvl_t>(dims.getElementCount(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return ArrayOf();
	}
    if (H5Aread(attr_id, type, rdata) < 0) {
        error = _W("Cannot read attribute.");
        return ArrayOf();
    }
    hid_t stype = H5Tget_super(type);
    switch (H5Tget_class(stype)) {
    case H5T_INTEGER: {
        return h5ReadVlenIntegerAttribute(attr_id, stype, rdata, dims, error);
    } break;
    case H5T_FLOAT: {
        return h5ReadVlenFloatAttribute(attr_id, stype, rdata, dims, error);
    } break;
    case H5T_BITFIELD: {
        return h5ReadVlenBitfieldAttribute(attr_id, stype, rdata, dims, error);
    } break;
    case H5T_OPAQUE: {
        return h5ReadVlenOpaqueAttribute(attr_id, stype, rdata, dims, error);
    } break;
    default: {
        error = _W("Type not managed.");
    } break;
    }
    H5Tclose(stype);
    return ArrayOf();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
