//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "h5SaveCell.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveFunctionHandle(hid_t fid, const std::string& location, const std::string& variableName,
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

    function_handle fh = VariableValue.getContentAsFunctionHandle();
    if (!fh.anonymousHandle) {
        Error(_("Invalid function handle."));
    }
    AnonymousMacroFunctionDef* anonymousFunction
        = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
    std::string anonymousContent = anonymousFunction->getContent();
    bool isFunctionHandle = anonymousFunction->isFunctionHandle();

    ArrayOf isFunctionHandleArrayOf = ArrayOf::logicalConstructor(isFunctionHandle);
    bSuccess = h5SaveVariable(fid, h5path + std::string("/"), "is_function_handle",
        isFunctionHandleArrayOf, useCompression);
    if (!bSuccess) {
        return false;
    }

    ArrayOf anonymousElement = ArrayOf::characterArrayConstructor(anonymousContent);
    bSuccess = h5SaveVariable(
        fid, h5path + std::string("/"), "function_handle", anonymousElement, useCompression);
    if (!bSuccess) {
        return false;
    }

    if (!isFunctionHandle) {
        stringVector arguments = anonymousFunction->getArguments();
        stringVector names = anonymousFunction->getVariableNames();
        std::vector<ArrayOf> variables = anonymousFunction->getVariables();

        Dimensions dimsNames(1, names.size());
        ArrayOf fieldnames = ArrayOf::stringArrayConstructor(names, dimsNames);
        bSuccess = h5SaveStringArray(
            fid, h5path + std::string("/"), "variable_names", fieldnames, useCompression);
        if (!bSuccess) {
            return false;
        }

        Dimensions dimsArguments(1, arguments.size());
        ArrayOf argumentsArrayOf = ArrayOf::stringArrayConstructor(arguments, dimsArguments);
        bSuccess = h5SaveStringArray(
            fid, h5path + std::string("/"), "arguments", argumentsArrayOf, useCompression);
        if (!bSuccess) {
            return false;
        }

        Dimensions dimsVariables(1, variables.size());
        ArrayOf* cell = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dimsVariables.getElementCount(), stringVector(), false));
        ArrayOf cellArrayOf = ArrayOf(NLS_CELL_ARRAY, dimsVariables, cell);
        for (size_t k = 0; k < variables.size(); k++) {
            cell[k] = variables[k];
        }

        bSuccess
            = h5SaveCell(fid, h5path + std::string("/"), "variables", cellArrayOf, useCompression);
        if (!bSuccess) {
            return false;
        }
    }

    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (!bSuccess) {
        return false;
    }
    Dimensions dims(1, 1);
    bSuccess = h5SaveDimensionsAttribute(fid, h5path, dims);
    if (!bSuccess) {
        return false;
    }
    return h5SaveUint8Attribute(fid, h5path, NELSON_OBJECT_STR, uint8(1));
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
