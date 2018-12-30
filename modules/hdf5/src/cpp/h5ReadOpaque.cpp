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
#include "h5ReadOpaque.hpp"
#include "h5ReadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadOpaque(hid_t dset_id, hid_t type_id, hid_t dspace_id, bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5I_INVALID_HID;
    if (asAttribute) {
        storageSize = H5Aget_storage_size(dset_id);
    } else {
        storageSize = H5Dget_storage_size(dset_id);
    }
    hsize_t sizeType = H5Tget_size(type_id);
    int rank;
    Dimensions dims = getDimensions(dspace_id, rank);
    ArrayOf* elements;
    try {
        elements = new_with_exception<ArrayOf>(dims.getElementCount(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return res;
    }
    uint8* temp = nullptr;
    try {
        temp = new_with_exception<uint8>(sizeType * dims.getElementCount(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return res;
    }
	herr_t status = H5I_INVALID_HID;
    if (asAttribute) {
        status = H5Aread(dset_id, type_id, temp);
    } else {
        status = H5Dread(dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp);
    }
    if (status < 0) {
        delete[] elements;
        delete[] temp;
        if (asAttribute) {
            error = _W("Cannot read attribute.");
        } else {
			error = _W("Cannot read dataset.");
		}
        return res;
    }
    indexType pos = 0;
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        Dimensions dimsElement(sizeType, 1);
        uint8* values;
        try {
            values = new_with_exception<uint8>(sizeType, false);
        } catch (Exception& e) {
            delete[] elements;
            delete[] temp;
            error = e.getMessage();
            return res;
        }
        for (indexType l = 0; l < sizeType; l++) {
            values[l] = temp[pos];
            pos++;
        }
        elements[k] = ArrayOf(NLS_UINT8, dimsElement, values);
    }
    delete[] temp;
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
