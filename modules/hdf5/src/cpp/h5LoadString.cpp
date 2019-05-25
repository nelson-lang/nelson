//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "h5LoadString.hpp"
#include "h5ReadInteger.hpp"
#include "h5LoadVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadStringArray(hid_t fid, const std::string& location, const std::string& variableName,
    bool isEmpty, const Dimensions& dims, ArrayOf& VariableValue)
{
    indexType nbElements = dims.getElementCount();
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        return false;
    }
    VariableValue = ArrayOf(NLS_STRING_ARRAY, dims, elements);
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    for (indexType k = 0; k < nbElements; k++) {
        std::string elementName = std::to_string(k);
        ArrayOf value;
        if (h5LoadVariable(fid, h5path, elementName, value)) {
            elements[k] = value;
        } else {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
h5LoadCharacterArray(hid_t fid, const std::string& location, const std::string& variableName,
    bool isEmpty, const Dimensions& dims, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (isEmpty) {
        VariableValue = ArrayOf(NLS_CHAR, dims, nullptr);
        bSuccess = true;
    } else {
        std::string h5path;
        if (location == "/") {
            h5path = location + variableName;
        } else {
            h5path = location + "/" + variableName;
        }
        std::wstring error;
        hid_t dset_id = H5Dopen(fid, h5path.c_str(), H5P_DEFAULT);
        if (dset_id < 0) {
            return false;
        }
        hid_t dspace_id = H5Dget_space(dset_id);
        if (dspace_id < 0) {
            H5Dclose(dset_id);
            return false;
        }
        hid_t type_id = H5Dget_type(dset_id);
        VariableValue = h5ReadInteger(dset_id, type_id, dspace_id, false, error);
        H5Tclose(type_id);
        H5Dclose(dset_id);
        H5Sclose(dspace_id);
        if (error.empty()) {
            VariableValue.promoteType(NLS_CHAR);
            bSuccess = true;
        } else {
            bSuccess = false;
        }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
