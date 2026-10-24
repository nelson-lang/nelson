//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5ReadString.hpp"
#include "h5ReadHelpers.hpp"
#include "NewWithException.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
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
        elements = new_with_exception<ArrayOf>(dims.elementCount(), false);
    } catch (NelsonException& e) {
        error = e.message();
        return res;
    }
    char** temp = nullptr;
    try {
        temp = new_with_exception<char*>((size_t)dims.elementCount(), true);
    } catch (NelsonException& e) {
        error = e.message();
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

        delete[] elements;

        delete[] temp;

        if (asAttribute) {
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadAttribute", L"Cannot read attribute.");
        } else {
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadDataSet", L"Cannot read data set.");
        }
        H5Tclose(memtype);
        return res;
    }
    indexType pos = 0;
    indexType elementCount = dims.elementCount();
    for (indexType k = 0; k < elementCount; k++) {
        std::string str;
        if (temp != nullptr) {
            str = temp[k];
        }
        elements[k] = ArrayOf::characterArray(str);
    }
    herr_t err = H5Dvlen_reclaim(type, aspace, H5P_DEFAULT, temp);

    delete[] temp;

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
        nbElements = dims.elementCount();
    }
    if (nbElements > 1) {
        try {
            elements = new_with_exception<ArrayOf>(nbElements, false);
        } catch (NelsonException& e) {
            error = e.message();
            return {};
        }
    }
    char* temp = nullptr;
    try {
        temp = new_with_exception<char>((size_t((sizeType + 1) * nbElements)), true);
    } catch (NelsonException& e) {

        delete[] elements;

        error = e.message();
        return res;
    }
    herr_t status = H5I_INVALID_HID;
    hid_t memtype = H5I_INVALID_HID;
    if (asAttribute) {
        status = H5Aread(attr_id, type, temp);
    } else {
        memtype = H5Tcopy(H5T_C_S1);
        H5Tset_size(memtype, (size_t)(sizeType + 1));
        status = H5Dread(attr_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, temp);
    }
    if (status < 0) {

        delete[] elements;

        delete[] temp;

        if (asAttribute) {
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadAttribute", L"Cannot read attribute.");
        } else {
            H5Tclose(memtype);
            error = resolveMessageTemplate(
                L"Nelson:hdf5:cannotReadDataSet", L"Cannot read data set.");
        }
        return {};
    }
    std::string str;
    str.reserve((size_t)sizeType);
    if (asAttribute) {
        if (nbElements > 1) {
            indexType pos = 0;
            for (indexType k = 0; k < nbElements; k++) {
                str.clear();
                for (indexType l = 0; l < (indexType)sizeType; l++) {
                    str.push_back(temp[pos]);
                    pos++;
                }
                elements[k] = ArrayOf::characterArray(str);
            }
        } else {
            for (indexType l = 0; l < (indexType)sizeType; l++) {
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
            for (indexType l = 0; l < (indexType)(sizeType + 1); l++) {
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
                elements[k] = ArrayOf::characterArray(str);
            }
        }
    }
    if (!asAttribute) {
        H5Tclose(memtype);
    }
    if (nbElements == 1) {
        res = ArrayOf::characterArray(str);
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
    Dimensions dims = dimensions(aspace, rank);
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
