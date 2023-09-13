//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5SaveFunctionHandle.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "h5SaveString.hpp"
#include "h5SaveVariable.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveFunctionHandle(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    function_handle fh = VariableValue.getContentAsFunctionHandle();
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
    stringVector fNames;
    fNames.push_back("name");
    fNames.push_back("anonymous");
    Dimensions dimsNames(1, fNames.size());
    ArrayOf fieldnames = ArrayOf::stringArrayConstructor(fNames, dimsNames);
    bSuccess = h5SaveStringArray(
        fid, h5path + std::string("/"), FIELDNAMES_STR, fieldnames, useCompression);
    if (!bSuccess) {
        return false;
    }
    ArrayOf nameElement = ArrayOf::characterArrayConstructor(fh.name);
    Dimensions dims(1, 1);
    std::string name = std::to_string(0);
    bSuccess = h5SaveVariable(fid, h5path + std::string("/"), name, nameElement, useCompression);
    if (!bSuccess) {
        return false;
    }
    name = std::to_string(1);
    std::string anonymousContent;
    if (fh.anonymousHandle) {
        AnonymousMacroFunctionDef* anonymousFunction
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        anonymousContent = anonymousFunction->getDefinition();
    }
    ArrayOf anonymousElement = ArrayOf::characterArrayConstructor(anonymousContent);
    bSuccess
        = h5SaveVariable(fid, h5path + std::string("/"), name, anonymousElement, useCompression);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveDimensionsAttribute(fid, h5path, dims);
    if (!bSuccess) {
        return false;
    }
    return h5SaveUint8Attribute(fid, h5path, NELSON_OBJECT_STR, uint8(1));
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
