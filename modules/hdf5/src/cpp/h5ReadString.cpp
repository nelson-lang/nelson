//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "h5ReadString.hpp"
#include "h5ReadHelpers.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
h5ReadStringVlen(hid_t attr_id, hid_t type, hid_t aspace, Dimensions& dims, int rank,
    bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5I_INVALID_HID;
    if (asAttribute) {
        storageSize = H5Aget_storage_size(attr_id);
    } else {
        storageSize = H5Dget_storage_size(attr_id);
    }
    hsize_t sizeType = H5Tget_size(type);
    ArrayOf* elements = nullptr;
    try {
        elements = new_with_exception<ArrayOf>(dims.getElementCount(), false);
    } catch (Exception& e) {
        error = e.getMessage();
        return res;
    }
    char** temp = nullptr;
    try {
        temp = new_with_exception<char*>(sizeType, true);
    } catch (Exception& e) {
        error = e.getMessage();
        return res;
    }
    hid_t memtype = H5Tcopy(H5T_C_S1);
    H5Tset_size(memtype, H5T_VARIABLE);

    herr_t status = H5I_INVALID_HID;
    if (asAttribute) {
        status = H5Aread(attr_id, memtype, temp);
    } else {
        status = H5Dread(attr_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp);
    }
    if (status < 0) {
        if (elements) {
            delete[] elements;
        }
        if (temp) {
            delete[] temp;
        }
        if (asAttribute) {
            error = _W("Cannot read attribute.");
        } else {
            error = _W("Cannot read data set.");
        }
        H5Tclose(memtype);
        return res;
    }
    indexType pos = 0;
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        std::string str = temp[k];
        elements[k] = ArrayOf::characterArrayConstructor(str);
    }
    herr_t err = H5Dvlen_reclaim(type, aspace, H5P_DEFAULT, temp);
    if (temp) {
        delete[] temp;
    }
    H5Tclose(memtype);
    res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    return res;
}
//=============================================================================
static ArrayOf
h5ReadStringNullTerm(hid_t attr_id, hid_t type, hid_t aspace, Dimensions& dims, int rank,
    bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    hsize_t storageSize = H5I_INVALID_HID;
    if (asAttribute) {
        storageSize = H5Aget_storage_size(attr_id);
    } else {
        storageSize = H5Dget_storage_size(attr_id);
    }
    hsize_t sizeType = H5Tget_size(type);
    ArrayOf* elements = nullptr;
    indexType nbElements;
    if (rank == 0) {
        nbElements = 1;
    } else {
        nbElements = dims.getElementCount();
    }
    if (nbElements > 1) {
        try {
            elements = new_with_exception<ArrayOf>(nbElements, false);
        } catch (Exception& e) {
            error = e.getMessage();
            return ArrayOf();
        }
    }
    char* temp = nullptr;
    try {
        temp = new_with_exception<char>((sizeType + 1) * nbElements, true);
    } catch (Exception& e) {
        if (elements) {
            delete[] elements;
        }
        error = e.getMessage();
        return res;
    }
    herr_t status = H5I_INVALID_HID;
    hid_t memtype = H5I_INVALID_HID;
    if (asAttribute) {
        status = H5Aread(attr_id, type, temp);
    } else {
        memtype = H5Tcopy(H5T_C_S1);
        H5Tset_size(memtype, sizeType + 1);
        status = H5Dread(attr_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp);
    }
    if (status < 0) {
        if (elements) {
            delete[] elements;
        }
        if (temp) {
            delete[] temp;
        }
        if (asAttribute) {
            error = _W("Cannot read attribute.");
        } else {
            H5Tclose(memtype);
            error = _W("Cannot read data set.");
        }
        return ArrayOf();
    }
    std::string str;
    str.reserve(sizeType);
    if (asAttribute) {
        if (nbElements > 1) {
            indexType pos = 0;
            for (indexType k = 0; k < nbElements; k++) {
                str.clear();
                for (indexType l = 0; l < sizeType; l++) {
                    str.push_back(temp[pos]);
                    pos++;
                }
                elements[k] = ArrayOf::characterArrayConstructor(str);
            }
        } else {
            for (indexType l = 0; l < sizeType; l++) {
                if (temp[l] != 0) {
                    str.push_back(temp[l]);
                } else {
                    break;
                }
            }
        }
    } else {
        indexType pos = 0;
        for (indexType k = 0; k < nbElements; k++) {
            str.clear();
            for (indexType l = 0; l < sizeType + 1; l++) {
                if (temp[pos] != 0) {
                    str.push_back(temp[pos]);
                } else {
                    if (l < sizeType && pos > 0) {
                        str.push_back(' ');
                    }
                }
                pos++;
            }
            if (nbElements > 1) {
                elements[k] = ArrayOf::characterArrayConstructor(str);
            }
        }
    }
    if (!asAttribute) {
        H5Tclose(memtype);
    }
    if (nbElements == 1) {
        res = ArrayOf::characterArrayConstructor(str);
    } else {
        res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return res;
}
//=============================================================================
ArrayOf
h5ReadString(hid_t attr_id, hid_t type, hid_t aspace, bool asAttribute, std::wstring& error)
{
    ArrayOf res;
    int rank = -1;
    Dimensions dims = getDimensions(aspace, rank);
    bool isVlenString = H5Tis_variable_str(type);
    if (isVlenString) {
        res = h5ReadStringVlen(attr_id, type, aspace, dims, rank, asAttribute, error);
    } else {
        res = h5ReadStringNullTerm(attr_id, type, aspace, dims, rank, asAttribute, error);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
