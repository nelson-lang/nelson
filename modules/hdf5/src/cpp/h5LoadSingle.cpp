//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5LoadSingle.hpp"
#include "h5ReadFloat.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadSingle(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    bool isComplex, const Dimensions& dims, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (isEmpty) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        if (isComplex) {
            VariableValue.promoteType(NLS_SCOMPLEX);
        } else {
            VariableValue.promoteType(NLS_SINGLE);
        }
        bSuccess = true;
    } else {
        std::string h5path;
        if (location == "/") {
            h5path = location + variableName;
        } else {
            h5path = location + "/" + variableName;
        }
        if (isComplex) {
            using complex_type = struct complex_type
            {
                single r;
                single i;
            };
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
            hid_t compoundId = H5Tcreate(H5T_COMPOUND, sizeof(singlecomplex));
            H5Tinsert(compoundId, "real", HOFFSET(complex_type, r), H5T_NATIVE_FLOAT);
            H5Tinsert(compoundId, "imag", HOFFSET(complex_type, i), H5T_NATIVE_FLOAT);
            singlecomplex* pData = nullptr;
            try {
                pData = new singlecomplex[dims.getElementCount()];
            } catch (const std::bad_alloc&) {
                H5Dclose(dset_id);
                return false;
            }
            VariableValue = ArrayOf(NLS_SCOMPLEX, dims, pData);
            herr_t status = H5Dread(dset_id, compoundId, H5S_ALL, H5S_ALL, H5P_DEFAULT, pData);
            H5Tclose(type_id);
            H5Dclose(dset_id);
            H5Sclose(dspace_id);
            if (status < 0) {
                return false;
            }
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
            VariableValue = h5ReadFloat(dset_id, type_id, dspace_id, false, error);
            H5Tclose(type_id);
            H5Dclose(dset_id);
            H5Sclose(dspace_id);
            if (!error.empty()) {
                return false;
            }
            if (!VariableValue.getDimensions().equals(dims)) {
                return false;
            }
            VariableValue.promoteType(NLS_SINGLE);
            bSuccess = true;
        }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
