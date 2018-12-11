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
#include "h5ReadArrayAttribute.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
readArrayAttribute(hid_t attr_id, std::wstring& error)
{
    ArrayOf res;
    hid_t type = H5Aget_type(attr_id);
    if (type < 0) {
        error = _W("Attribute have an invalid type.");
        return res;
    }
    hsize_t storageSize = H5Aget_storage_size(attr_id);
    hsize_t sizeType = H5Tget_size(type);
    size_t numVal = storageSize / sizeType;
    hid_t aspace = H5Aget_space(attr_id);
    int ndims = H5Tget_array_ndims(type);
    hsize_t* dimsAsHsize;
    try {
        dimsAsHsize = new_with_exception<hsize_t>(ndims, false);
    } catch (Exception& e) {
        error = e.getMessage();
        H5Aclose(type);
        H5Sclose(aspace);
        return res;
    }
    H5Tget_array_dims(type, dimsAsHsize);
    Dimensions dimsOutput;
    size_t i = ndims - 1;
    hsize_t j = 0;
    while (i > j) {
        hsize_t temp = dimsAsHsize[i];
        dimsAsHsize[i] = dimsAsHsize[j];
        dimsAsHsize[j] = temp;
        i--;
        j++;
    }
    for (indexType k = 0; k < ndims; k++) {
        dimsOutput[k] = (indexType)dimsAsHsize[k];
    }
    delete[] dimsAsHsize;
    dimsOutput[ndims] = numVal;
    if (H5Tequal(type, H5T_INTEGER)) {
        hid_t nativeIntegerType = H5Tget_super(type);
        if (H5Tget_sign(type) == H5T_SGN_NONE) {
            Class outputClass;
            void* ptrVoid = nullptr;
            if (H5Tequal(nativeIntegerType, H5T_NATIVE_UINT64)) {
                uint64* ptr = nullptr;
                try {
                    ptr = new_with_exception<uint64>(dimsOutput.getElementCount(), false);
                } catch (Exception& e) {
                    H5Sclose(aspace);
                    H5Aclose(type);
                    error = e.getMessage();
                    return res;
                }
                ptrVoid = ptr;
                outputClass = NLS_UINT64;
            } else if (H5Tequal(nativeIntegerType, H5T_NATIVE_UINT32)) {
                uint32* ptr = nullptr;
                try {
                    ptr = new_with_exception<uint32>(dimsOutput.getElementCount(), false);
                } catch (Exception& e) {
                    H5Sclose(aspace);
                    H5Aclose(type);
                    error = e.getMessage();
                    return res;
                }
                ptrVoid = ptr;
                outputClass = NLS_UINT32;
            } else if (H5Tequal(nativeIntegerType, H5T_NATIVE_UINT16)) {
                uint16* ptr = nullptr;
                try {
                    ptr = new_with_exception<uint16>(dimsOutput.getElementCount(), false);
                } catch (Exception& e) {
                    H5Sclose(aspace);
                    H5Aclose(type);
                    error = e.getMessage();
                    return res;
                }
                ptrVoid = ptr;
                outputClass = NLS_UINT16;
            } else if (H5Tequal(nativeIntegerType, H5T_NATIVE_UINT8)) {
                uint8* ptr = nullptr;
                try {
                    ptr = new_with_exception<uint8>(dimsOutput.getElementCount(), false);
                } catch (Exception& e) {
                    H5Sclose(aspace);
                    H5Aclose(type);
                    error = e.getMessage();
                    return res;
                }
                ptrVoid = ptr;
                outputClass = NLS_UINT8;
            } else {
                H5Sclose(aspace);
                H5Aclose(type);
                error = _W("Integer type not managed.");
                return res;
            }
            H5Aread(attr_id, type, ptrVoid);
            res = ArrayOf(outputClass, dimsOutput, ptrVoid);
        } else {
            Class outputClass;
            void* ptrVoid = nullptr;
            if (H5Tequal(nativeIntegerType, H5T_NATIVE_INT64)) {
                int64* ptr = nullptr;
                try {
                    ptr = new_with_exception<int64>(dimsOutput.getElementCount(), false);
                } catch (Exception& e) {
                    H5Sclose(aspace);
                    H5Aclose(type);
                    error = e.getMessage();
                    return res;
                }
                ptrVoid = ptr;
                outputClass = NLS_INT64;
            } else if (H5Tequal(nativeIntegerType, H5T_NATIVE_INT32)) {
                int32* ptr = nullptr;
                try {
                    ptr = new_with_exception<int32>(dimsOutput.getElementCount(), false);
                } catch (Exception& e) {
                    H5Sclose(aspace);
                    H5Aclose(type);
                    error = e.getMessage();
                    return res;
                }
                ptrVoid = ptr;
                outputClass = NLS_INT32;
            } else if (H5Tequal(nativeIntegerType, H5T_NATIVE_INT16)) {
                int16* ptr = nullptr;
                try {
                    ptr = new_with_exception<int16>(dimsOutput.getElementCount(), false);
                } catch (Exception& e) {
                    H5Sclose(aspace);
                    H5Aclose(type);
                    error = e.getMessage();
                    return res;
                }
                ptrVoid = ptr;
                outputClass = NLS_INT16;
            } else if (H5Tequal(nativeIntegerType, H5T_NATIVE_INT8)) {
                int8* ptr = nullptr;
                try {
                    ptr = new_with_exception<int8>(dimsOutput.getElementCount(), false);
                } catch (Exception& e) {
                    H5Sclose(aspace);
                    H5Aclose(type);
                    error = e.getMessage();
                    return res;
                }
                ptrVoid = ptr;
                outputClass = NLS_INT8;
            } else {
                H5Sclose(aspace);
                H5Aclose(type);
                error = _W("Integer type not managed.");
                return res;
            }
            H5Aread(attr_id, type, ptrVoid);
            res = ArrayOf(outputClass, dimsOutput, ptrVoid);
        }
    } else if (H5Tequal(type, H5T_FLOAT)) {

    } else if (H5Tequal(type, H5T_STRING)) {

    } else if (H5Tequal(type, H5T_TIME)) {

    } else if (H5Tequal(type, H5T_BITFIELD)) {

    } else if (H5Tequal(type, H5T_OPAQUE)) {

    } else if (H5Tequal(type, H5T_COMPOUND)) {

    } else if (H5Tequal(type, H5T_REFERENCE)) {

    } else if (H5Tequal(type, H5T_ENUM)) {

    } else if (H5Tequal(type, H5T_VLEN)) {

    } else if (H5Tequal(type, H5T_ARRAY)) {
    }
    H5Aclose(type);
    H5Sclose(aspace);
    return res;
}
//=============================================================================
}  // namespace Nelson
//=============================================================================
