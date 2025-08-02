//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
