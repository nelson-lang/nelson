//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SaveMatioStruct.hpp"
#include "SaveMatioVariable.hpp"
#include "ClassdefParser.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioStruct(const std::string& variableName, const ArrayOf& variableValue, mat_ft matVersion)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    stringVector fieldnames = variableValue.getFieldNames();
    stringVector savedFieldnames = fieldnames;
    bool isClassdefObject = false;
    std::string className;
    if (variableValue.isClassType()) {
        className = variableValue.getClassType();
        isClassdefObject = ClassdefDefinitionManager::getInstance()->loadClass(className);
        if (isClassdefObject) {
            savedFieldnames.push_back(CLASSDEF_SERIALIZATION_NAME_FIELD);
            savedFieldnames.push_back(CLASSDEF_SERIALIZATION_HANDLE_FIELD);
        }
    }
    size_t nbFieldnames = savedFieldnames.size();
    indexType nbStructElements = nbFieldnames * variableDims.getElementCount() + 1;
    matvar_t** structElements = nullptr;
    try {
        structElements = new matvar_t*[nbStructElements];
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
    for (indexType i = 0; i < nbStructElements; ++i) {
        structElements[i] = nullptr;
    }

    auto* elements = (ArrayOf*)variableValue.getDataPointer();
    indexType elementCount = variableDims.getElementCount();
    for (indexType i = 0; i < elementCount; ++i) {
        for (indexType j = 0; j < static_cast<indexType>(nbFieldnames); ++j) {
            ArrayOf element;
            const std::string& fieldname = savedFieldnames[j];
            if (isClassdefObject && fieldname == CLASSDEF_SERIALIZATION_NAME_FIELD) {
                element = ArrayOf::characterArrayConstructor(className);
            } else if (isClassdefObject && fieldname == CLASSDEF_SERIALIZATION_HANDLE_FIELD) {
                element = ArrayOf::logicalConstructor(false);
            } else {
                element = elements[i * fieldnames.size() + j];
            }
            structElements[i * nbFieldnames + j]
                = SaveMatioVariable(fieldname, element, matVersion);
        }
    }
    matvar_t* matVariable = Mat_VarCreate(
        variableName.c_str(), MAT_C_STRUCT, MAT_T_STRUCT, (int)rank, dims, structElements, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
