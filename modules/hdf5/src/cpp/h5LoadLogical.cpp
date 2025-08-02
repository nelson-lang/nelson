//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5LoadLogical.hpp"
#include "h5ReadInteger.hpp"
#include "SparseConstructors.hpp"
#include "h5LoadVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadLogical(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    const Dimensions& dims, bool isSparse, uint64 nzmax, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (isEmpty) {
        if (isSparse) {
            ArrayOf I = ArrayOf::emptyConstructor();
            ArrayOf J = ArrayOf::emptyConstructor();
            ArrayOf V = ArrayOf::emptyConstructor(dims);
            V.promoteType(NLS_LOGICAL);
            Dimensions _dims(dims);
            VariableValue
                = SparseConstructor(I, J, V, _dims[0], _dims[1], static_cast<indexType>(nzmax));
            bSuccess = true;
        } else {
            VariableValue = ArrayOf::emptyConstructor(dims);
            VariableValue.promoteType(NLS_LOGICAL);
            bSuccess = true;
        }
    } else {
        std::string h5path;
        if (location == "/") {
            h5path = location + variableName;
        } else {
            h5path = location + "/" + variableName;
        }
        if (isSparse) {
            ArrayOf I;
            if (!h5LoadVariable(fid, h5path, "ir", I)) {
                return false;
            }
            ArrayOf J;
            if (!h5LoadVariable(fid, h5path, "jc", J)) {
                return false;
            }
            ArrayOf V;
            if (!h5LoadVariable(fid, h5path, "data", V)) {
                return false;
            }
            V.promoteType(NLS_LOGICAL);
            Dimensions _dims(dims);
            VariableValue
                = SparseConstructor(I, J, V, _dims[0], _dims[1], static_cast<indexType>(nzmax));
            bSuccess = true;
        } else {
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
                VariableValue.promoteType(NLS_LOGICAL);
                bSuccess = true;
            } else {
                bSuccess = false;
            }
        }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
