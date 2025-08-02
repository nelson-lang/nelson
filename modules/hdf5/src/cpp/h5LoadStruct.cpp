//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5LoadStruct.hpp"
#include "h5LoadString.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5LoadVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadStruct(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    const Dimensions& dims, bool isObject, const std::string& objectClassname,
    ArrayOf& VariableValue)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    Dimensions dimsFieldnames = getNelsonDimensions(fid, h5path, "fieldnames");
    bool isEmptyFieldnames = isNelsonEmpty(fid, h5path, "fieldnames");
    stringVector fieldnames;
    ArrayOf fieldnamesArrayOf;
    if (h5LoadStringArray(
            fid, h5path, "fieldnames", isEmptyFieldnames, dimsFieldnames, fieldnamesArrayOf)) {
        fieldnames = fieldnamesArrayOf.getContentAsCStringVector(false);
    } else {
        return false;
    }
    indexType nbElements = dims.getElementCount();
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[nbElements * fieldnames.size()];
    } catch (const std::bad_alloc&) {
        return false;
    }
    VariableValue
        = ArrayOf(isObject ? NLS_CLASS_ARRAY : NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
    indexType length = dims.getElementCount();
    indexType offset = 0;
    for (indexType j = 0; j < length; j++) {
        for (indexType i = 0; i < (sizeType)fieldnames.size(); i++) {
            std::string name = std::to_string(offset);
            bool bSuccess = h5LoadVariable(fid, h5path + std::string("/"), name, elements[offset]);
            if (!bSuccess) {
                return false;
            }
            offset++;
        }
    }
    bSuccess = true;
    if (isObject) {
        VariableValue.setClassType(objectClassname);
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
