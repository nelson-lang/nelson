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
h5LoadFunctionHandle(Evaluator* eval, hid_t fid, const std::string& location,
    const std::string& variableName, bool isEmpty, const Dimensions& dims, ArrayOf& VariableValue)
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
    bSuccess = h5LoadLogical(eval, fid, h5path, "is_function_handle", isEmpty, dimsLogicalScalar,
        false, 0, isFunctionHandleArrayOf);
    if (!bSuccess) {
        return false;
    }
    bool isFunctionHandle = isFunctionHandleArrayOf.asLogicalScalar();

    Dimensions dimsContent = getNelsonDimensions(fid, h5path, "function_handle");
    ArrayOf anonymousElement;
    bSuccess = h5LoadCharacterArray(
        fid, h5path, "function_handle", isEmpty, dimsContent, anonymousElement);
    if (!bSuccess) {
        return false;
    }
    std::string anonymousContent = anonymousElement.asUtf8String();

    function_handle fh;

    if (!isFunctionHandle) {
        Dimensions dimsFieldnames = getNelsonDimensions(fid, h5path, "variable_names");
        bool isEmptyFieldnames = isNelsonEmpty(fid, h5path, "variable_names");
        stringVector fieldnames;
        ArrayOf fieldnamesArrayOf;
        bSuccess = h5LoadStringArray(eval, fid, h5path, "variable_names", isEmptyFieldnames,
            dimsFieldnames, fieldnamesArrayOf);
        if (!bSuccess) {
            return false;
        }
        fieldnames = fieldnamesArrayOf.asUtf8StringVector(false);

        Dimensions dimsArguments = getNelsonDimensions(fid, h5path, "arguments");
        bool isEmptyArguments = isNelsonEmpty(fid, h5path, "arguments");
        stringVector arguments;
        ArrayOf argumentsArrayOf;
        bSuccess = h5LoadStringArray(
            eval, fid, h5path, "arguments", isEmptyArguments, dimsArguments, argumentsArrayOf);
        if (!bSuccess) {
            return false;
        }
        arguments = argumentsArrayOf.asUtf8StringVector(false);

        ArrayOf cellArrayOf;
        Dimensions dimsVariables = getNelsonDimensions(fid, h5path, "variables");
        bSuccess = h5LoadCell(eval, fid, h5path, "variables", isEmpty, dimsVariables, cellArrayOf);
        if (!bSuccess) {
            return false;
        }

        std::vector<ArrayOf> variables;
        variables.reserve(cellArrayOf.elementCount());
        ArrayOf* cell = static_cast<ArrayOf*>(
            const_cast<void*>(static_cast<const void*>(cellArrayOf.dataPointer())));
        for (indexType k = 0; k < cellArrayOf.elementCount(); k++) {
            variables.push_back(cell[k]);
        }

        auto* anonymousFunction
            = new AnonymousMacroFunctionDef(anonymousContent, arguments, fieldnames, variables);
        fh.anonymousHandle = reinterpret_cast<nelson_handle*>(anonymousFunction);
        VariableValue = ArrayOf::functionHandle(fh);
        // files saved before the body as written was stored have no "definition"
        std::string definitionPath = h5path + "/definition";
        if (H5Lexists(fid, definitionPath.c_str(), H5P_DEFAULT) > 0) {
            ArrayOf definition;
            Dimensions dimsDefinition = getNelsonDimensions(fid, h5path, "definition");
            if (h5LoadCharacterArray(fid, h5path, "definition",
                    isNelsonEmpty(fid, h5path, "definition"), dimsDefinition, definition)) {
                anonymousFunction->setDefinitionText(definition.asUtf8String());
            }
        }
        bSuccess = true;
    } else {
        fh.anonymousHandle
            = reinterpret_cast<nelson_handle*>(new AnonymousMacroFunctionDef(anonymousContent));
        VariableValue = ArrayOf::functionHandle(fh);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
