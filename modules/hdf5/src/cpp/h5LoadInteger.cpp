//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5LoadInteger.hpp"
#include "h5ReadInteger.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadInteger(hid_t fid, const std::string& location, const std::string& variableName,
    NelsonType destClass, bool isEmpty, const Dimensions& dims, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (isEmpty) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        VariableValue.promoteType(destClass);
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
        if (!error.empty()) {
            return false;
        }
        if (!VariableValue.getDimensions().equals(dims)) {
            return false;
        }
        VariableValue.promoteType(destClass);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
