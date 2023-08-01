//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5SaveStruct.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5SaveVariable.hpp"
#include "h5SaveString.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveStruct(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    h5LDeleteIfExists(fid, h5path);

    hid_t gcpl = H5Pcreate(H5P_GROUP_CREATE);
    hid_t group = H5Gcreate(fid, h5path.c_str(), H5P_DEFAULT, gcpl, H5P_DEFAULT);
    herr_t status = H5Gclose(group);
    if (status < 0) {
        return false;
    }
    stringVector fNames = VariableValue.getFieldNames();
    Dimensions dimsNames(1, fNames.size());
    ArrayOf fieldnames = ArrayOf::stringArrayConstructor(fNames, dimsNames);
    bSuccess = h5SaveStringArray(
        fid, h5path + std::string("/"), FIELDNAMES_STR, fieldnames, useCompression);
    if (!bSuccess) {
        return false;
    }
    Dimensions dims = VariableValue.getDimensions();
    indexType length = dims.getElementCount();
    auto* elements = (ArrayOf*)VariableValue.getDataPointer();
    indexType offset = 0;
    for (indexType j = 0; j < length; j++)
        for (indexType i = 0; i < (indexType)fNames.size(); i++) {
            ArrayOf element = elements[offset];
            std::string name = std::to_string(offset);
            bSuccess
                = h5SaveVariable(fid, h5path + std::string("/"), name, element, useCompression);
            offset++;
            if (!bSuccess) {
                return false;
            }
        }
    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveDimensionsAttribute(fid, h5path, dims);
    if (!bSuccess) {
        return false;
    }
    if (dims.isEmpty(false)) {
        bSuccess = h5SaveEmptyAttribute(fid, h5path);
    }
    if (VariableValue.isClassType()) {
        bSuccess = h5SaveUint8Attribute(fid, h5path, NELSON_OBJECT_STR, uint8(1));
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
