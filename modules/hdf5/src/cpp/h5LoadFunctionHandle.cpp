//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "h5LoadFunctionHandle.hpp"
#include "h5LoadStruct.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "characters_encoding.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "h5LoadString.hpp"
#include "h5LoadCell.hpp"
#include "h5LoadLogical.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadFunctionHandle(hid_t fid, const std::string& location, const std::string& variableName,
    bool isEmpty, const Dimensions& dims, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }

    Dimensions dimsLogicalScalar(1, 1);
    ArrayOf isFunctionHandleArrayOf;
    bSuccess = h5LoadLogical(fid, h5path, "is_function_handle", isEmpty, dimsLogicalScalar, false,
        0, isFunctionHandleArrayOf);
    if (!bSuccess) {
        return false;
    }
    bool isFunctionHandle = isFunctionHandleArrayOf.getContentAsLogicalScalar();

    Dimensions dimsContent = getNelsonDimensions(fid, h5path, "function_handle");
    ArrayOf anonymousElement;
    bSuccess = h5LoadCharacterArray(
        fid, h5path, "function_handle", isEmpty, dimsContent, anonymousElement);
    if (!bSuccess) {
        return false;
    }
    std::string anonymousContent = anonymousElement.getContentAsCString();

    function_handle fh;

    if (!isFunctionHandle) {
        Dimensions dimsFieldnames = getNelsonDimensions(fid, h5path, "variable_names");
        bool isEmptyFieldnames = isNelsonEmpty(fid, h5path, "variable_names");
        stringVector fieldnames;
        ArrayOf fieldnamesArrayOf;
        bSuccess = h5LoadStringArray(
            fid, h5path, "variable_names", isEmptyFieldnames, dimsFieldnames, fieldnamesArrayOf);
        if (!bSuccess) {
            return false;
        }
        fieldnames = fieldnamesArrayOf.getContentAsCStringVector(false);

        Dimensions dimsArguments = getNelsonDimensions(fid, h5path, "arguments");
        bool isEmptyArguments = isNelsonEmpty(fid, h5path, "arguments");
        stringVector arguments;
        ArrayOf argumentsArrayOf;
        bSuccess = h5LoadStringArray(
            fid, h5path, "arguments", isEmptyArguments, dimsArguments, argumentsArrayOf);
        if (!bSuccess) {
            return false;
        }
        arguments = argumentsArrayOf.getContentAsCStringVector(false);

        ArrayOf cellArrayOf;
        Dimensions dimsVariables = getNelsonDimensions(fid, h5path, "variables");
        bSuccess = h5LoadCell(fid, h5path, "variables", isEmpty, dimsVariables, cellArrayOf);
        if (!bSuccess) {
            return false;
        }

        std::vector<ArrayOf> variables;
        variables.reserve(cellArrayOf.getElementCount());
        ArrayOf* cell = static_cast<ArrayOf*>(
            const_cast<void*>(static_cast<const void*>(cellArrayOf.getDataPointer())));
        for (indexType k = 0; k < cellArrayOf.getElementCount(); k++) {
            variables.push_back(cell[k]);
        }

        fh.anonymousHandle = reinterpret_cast<nelson_handle*>(
            new AnonymousMacroFunctionDef(anonymousContent, arguments, fieldnames, variables));
        VariableValue = ArrayOf::functionHandleConstructor(fh);
        bSuccess = true;
    } else {
        fh.anonymousHandle
            = reinterpret_cast<nelson_handle*>(new AnonymousMacroFunctionDef(anonymousContent));
        VariableValue = ArrayOf::functionHandleConstructor(fh);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
