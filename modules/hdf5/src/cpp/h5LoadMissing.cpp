//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5LoadMissing.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadMissing(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    const Dimensions& dims, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (isEmpty) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        VariableValue.promoteType(NLS_MISSING_ARRAY);
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
        H5Dclose(dset_id);
        H5Sclose(dspace_id);
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_MISSING_ARRAY, dims.getElementCount());
        for (size_t k = 0; k < dims.getElementCount(); ++k) {
            ptr[k] = std::nan("");
        }
        VariableValue = ArrayOf(NLS_MISSING_ARRAY, dims, ptr);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
