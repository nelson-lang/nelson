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
#include "h5ReadStringDataset.hpp"
#include "h5ReadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
h5ReadStringDataset(hid_t dset_id, hid_t type_id, hid_t dspace_id, std::wstring& error)
{
    ArrayOf res;
    hsize_t sizeType = H5Tget_size(type_id);
    int rank;
    Dimensions dims = getDimensions(dspace_id, rank);
    bool isVlenString = H5Tis_variable_str(type_id);
    if (dims.isEmpty(false)) {
        res = ArrayOf::characterArrayConstructor("");
    } else if (dims.isScalar()) {
        if (isVlenString) {
        } else {
            hsize_t lenMax = sizeType + 1;
            char* temp;
            try {
                temp = new_with_exception<char>(lenMax, true);
            } catch (Exception& e) {
                error = e.getMessage();
                return res;
            }

            hid_t memtype = H5Tcopy(H5T_C_S1);
            hid_t hstatus = H5Tset_size(memtype, lenMax);
            if (H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp) < 0) {
                H5Tclose(memtype);
                delete[] temp;
                error = _W("Cannot read attribute.");
                return res;
            }
            indexType pos = 0;
            std::string str;
            str.reserve(sizeType);
            for (indexType l = 0; l < lenMax; l++) {
                str.push_back(temp[pos]);
                pos++;
            }
            res = ArrayOf::characterArrayConstructor(str);
            H5Tclose(memtype);
        }
    } else {
        ArrayOf* elements;
        try {
            elements = new_with_exception<ArrayOf>(dims.getElementCount(), false);
        } catch (Exception& e) {
            error = e.getMessage();
            return res;
        }

        if (isVlenString) {
        } else {
            hsize_t lenMax = sizeType + 1;
            char* temp;
            try {
                temp = new_with_exception<char>(lenMax * dims.getElementCount(), true);
            } catch (Exception& e) {
                error = e.getMessage();
                return res;
            }

			hid_t memtype = H5Tcopy(H5T_C_S1);
            hid_t hstatus = H5Tset_size(memtype, lenMax);
            if (H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp) < 0) {
                H5Tclose(memtype);
                delete[] elements;
                delete[] temp;
                error = _W("Cannot read attribute.");
                return res;
            }
            indexType pos = 0;
            for (indexType k = 0; k < dims.getElementCount(); k++) {
                std::string str;
                str.reserve(lenMax);
                for (indexType l = 0; l < lenMax; l++) {
                    if (temp[pos] != 0) {
                        str.push_back(temp[pos]);
                    }
                    pos++;
                }
                int nbSpaceToAdd = (int)(sizeType - str.length());
                for (int l = 0; l < nbSpaceToAdd; l++) {
                    str.push_back(' ');
				}
                elements[k] = ArrayOf::characterArrayConstructor(str);
            }
            H5Tclose(memtype);
        }
        res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
