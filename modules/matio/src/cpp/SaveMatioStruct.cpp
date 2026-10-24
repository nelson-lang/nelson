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
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
#define CLASSDEF_SERIALIZATION_EMPTY_DIMS_FIELD "__nelson_classdef_empty_dimensions__"
//=============================================================================
matvar_t*
SaveMatioStruct(Evaluator* eval, const std::string& variableName, const ArrayOf& variableValue,
    mat_ft matVersion)
{
    Dimensions variableDims = variableValue.dimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    stringVector fieldnames = variableValue.fieldNames();
    stringVector savedFieldnames = fieldnames;
    bool isClassdefObject = false;
    std::string className;
    if (variableValue.isClassType()) {
        className = variableValue.classType();
        isClassdefObject = classdefLoadClass(className);
        if (isClassdefObject) {
            if (variableDims.elementCount() == 0) {
                Dimensions markerDims(1, 1);
                delete[] dims;
                dims = convertDimensionsForMatVar(markerDims, rank);
                if (dims == nullptr) {
                    return nullptr;
                }
                savedFieldnames = { CLASSDEF_SERIALIZATION_NAME_FIELD,
                    CLASSDEF_SERIALIZATION_HANDLE_FIELD, CLASSDEF_SERIALIZATION_EMPTY_DIMS_FIELD };
            } else {
                savedFieldnames.push_back(CLASSDEF_SERIALIZATION_NAME_FIELD);
                savedFieldnames.push_back(CLASSDEF_SERIALIZATION_HANDLE_FIELD);
            }
        }
    }
    size_t nbFieldnames = savedFieldnames.size();
    bool isEmptyClassdefObject = isClassdefObject && variableDims.elementCount() == 0;
    indexType storedElementCount = isEmptyClassdefObject ? 1 : variableDims.elementCount();
    indexType nbStructElements = nbFieldnames * storedElementCount + 1;
    matvar_t** structElements = nullptr;
    try {
        structElements = new matvar_t*[nbStructElements];
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
    for (indexType i = 0; i < nbStructElements; ++i) {
        structElements[i] = nullptr;
    }

    auto* elements = (ArrayOf*)variableValue.dataPointer();
    indexType elementCount = variableDims.elementCount();
    if (isEmptyClassdefObject) {
        std::vector<double> dimensionValues;
        Dimensions originalDims(variableDims);
        for (indexType k = 0; k < originalDims.length(); ++k) {
            dimensionValues.push_back(static_cast<double>(originalDims[k]));
        }
        ArrayOf markerValues[3] = { ArrayOf::characterArray(className),
            ArrayOf::logicalScalar(false), ArrayOf::doubleRowVector(dimensionValues) };
        for (indexType j = 0; j < 3; ++j) {
            structElements[j]
                = SaveMatioVariable(eval, savedFieldnames[j], markerValues[j], matVersion);
        }
        matvar_t* matVariable = Mat_VarCreate(
            variableName.c_str(), MAT_C_STRUCT, MAT_T_STRUCT, (int)rank, dims, structElements, 0);
        delete[] dims;
        return matVariable;
    }
    for (indexType i = 0; i < elementCount; ++i) {
        for (indexType j = 0; j < static_cast<indexType>(nbFieldnames); ++j) {
            ArrayOf element;
            const std::string& fieldname = savedFieldnames[j];
            if (isClassdefObject && fieldname == CLASSDEF_SERIALIZATION_NAME_FIELD) {
                element = ArrayOf::characterArray(className);
            } else if (isClassdefObject && fieldname == CLASSDEF_SERIALIZATION_HANDLE_FIELD) {
                element = ArrayOf::logicalScalar(false);
            } else {
                element = elements[i * fieldnames.size() + j];
            }
            structElements[i * nbFieldnames + j]
                = SaveMatioVariable(eval, fieldname, element, matVersion);
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
