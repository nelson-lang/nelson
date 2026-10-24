//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadOpaque.hpp"
#include "h5ReadHelpers.hpp"
#include "NewWithException.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
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
    Dimensions dims = dimensions(dspace_id, rank);
    if ((rank == 0) && (storageSize == 0)) {
        dims = Dimensions(0, 0);
    }
    ArrayOf* elements;
    try {
        elements = new_with_exception<ArrayOf>(dims.elementCount(), false);
    } catch (NelsonException& e) {
        error = e.message();
        return res;
    }
    uint8* temp = nullptr;
    try {
        temp = new_with_exception<uint8>(((size_t)sizeType * (size_t)dims.elementCount()), false);
    } catch (NelsonException& e) {
        error = e.message();
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
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadAttribute", L"Cannot read attribute.");
        } else {
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadDataSet", L"Cannot read data set.");
        }
        return res;
    }
    indexType pos = 0;
    indexType elementCount = dims.elementCount();
    for (indexType k = 0; k < elementCount; k++) {
        Dimensions dimsElement((indexType)sizeType, 1);
        uint8* values;
        try {
            values = new_with_exception<uint8>((size_t)sizeType, false);
        } catch (NelsonException& e) {
            delete[] elements;
            delete[] temp;
            error = e.message();
            return res;
        }
        for (indexType l = 0; l < (indexType)sizeType; l++) {
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
