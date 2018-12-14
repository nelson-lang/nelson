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
#include "h5ReadOpaqueAttribute.hpp"
#include "h5ReadAttributeHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadOpaqueAttribute(hid_t attr_id, hid_t type, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5Aget_storage_size(attr_id);
    hsize_t sizeType = H5Tget_size(type);
    size_t numVal = storageSize / sizeType;
    hid_t aspace = H5Aget_space(attr_id);
    Dimensions dims = getDimensions(aspace);
    ArrayOf* elements;
    try {
        elements = new_with_exception<ArrayOf>(numVal, false);
    } catch (Exception& e) {
        error = e.getMessage();
        H5Sclose(aspace);
        return res;
    }
    uint8* temp;
    try {
        temp = new_with_exception<uint8>(storageSize, false);
    } catch (Exception& e) {
        H5Sclose(aspace);
        error = e.getMessage();
        return res;
    }
    if (H5Aread(attr_id, type, temp) < 0) {
        delete[] elements;
        delete[] temp;
        H5Sclose(aspace);
        error = _W("Cannot read attribute.");
        return res;
    }
    indexType pos = 0;
    for (indexType k = 0; k < numVal; k++) {
        Dimensions dimsElement(sizeType, 1);
        uint8* values;
        try {
            values = new_with_exception<uint8>(sizeType, false);
        } catch (Exception& e) {
            delete[] elements;
            delete[] temp;
            H5Sclose(aspace);
            error = e.getMessage();
            return res;
        }
        for (indexType l = 0; l < sizeType; l++) {
            values[l] = temp[pos];
            pos++;
        }
        elements[k] = ArrayOf(NLS_UINT8, dimsElement, values);
    }
    H5Sclose(aspace);
    delete[] temp;
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
}  // namespace Nelson
//=============================================================================
