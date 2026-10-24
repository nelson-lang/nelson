//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SaveMatioHandle.hpp"
#include "SaveMatioStruct.hpp"
#include "SaveMatioVariable.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "Warning.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "matioHelpers.hpp"
#include <algorithm>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
#define CLASSDEF_ATTR_TRANSIENT "Transient"
#define CLASSDEF_SERIALIZATION_EMPTY_DIMS_FIELD "__nelson_classdef_empty_dimensions__"
//=============================================================================
static void
appendPersistentDynamicProperties(ClassdefHandleObject* object, stringVector& fieldnames)
{
    if (object == nullptr) {
        return;
    }
    for (const auto& propertyName : object->dynamicProperties()) {
        const ClassdefDynamicPropertyAttributes attributes
            = object->dynamicPropertyAttributes(propertyName);
        if (attributes.transient || attributes.dependent) {
            continue;
        }
        if (std::find(fieldnames.begin(), fieldnames.end(), propertyName) == fieldnames.end()) {
            fieldnames.push_back(propertyName);
        }
    }
}
//=============================================================================
matvar_t*
SaveMatioHandle(Evaluator* eval, const std::string& variableName, const ArrayOf& variableValue,
    mat_ft matVersion)
{
    const std::string className = variableValue.handleClassName();

    if (classdefLoadClass(className) && classdefIsHandleClass(className)) {
        Dimensions variableDims = variableValue.dimensions();
        indexType rank;
        size_t* dims = convertDimensionsForMatVar(variableDims, rank);
        if (dims == nullptr) {
            return nullptr;
        }

        stringVector fieldnames;
        for (const auto& property : classdefPropertyDefinitions(className)) {
            if (!classdefHasDependentProperty(className, property.name)
                && !classdefPropertyHasAttribute(property, CLASSDEF_ATTR_TRANSIENT)) {
                fieldnames.push_back(property.name);
            }
        }
        const auto* handles = static_cast<const HandleRef*>(variableValue.dataPointer());
        indexType elementCount = variableDims.elementCount();
        for (indexType i = 0; i < elementCount; ++i) {
            auto* object = handles == nullptr
                ? nullptr
                : dynamic_cast<ClassdefHandleObject*>(handles[i] ? handles[i]->object() : nullptr);
            appendPersistentDynamicProperties(object, fieldnames);
        }
        stringVector savedFieldnames = fieldnames;
        if (variableDims.elementCount() == 0) {
            savedFieldnames = { CLASSDEF_SERIALIZATION_NAME_FIELD,
                CLASSDEF_SERIALIZATION_HANDLE_FIELD, CLASSDEF_SERIALIZATION_EMPTY_DIMS_FIELD };
        } else {
            savedFieldnames.push_back(CLASSDEF_SERIALIZATION_NAME_FIELD);
            savedFieldnames.push_back(CLASSDEF_SERIALIZATION_HANDLE_FIELD);
        }

        size_t nbFieldnames = savedFieldnames.size();
        bool isEmptyClassdefObject = variableDims.elementCount() == 0;
        indexType storedElementCount = isEmptyClassdefObject ? 1 : variableDims.elementCount();
        indexType nbStructElements = nbFieldnames * storedElementCount + 1;
        matvar_t** structElements = nullptr;
        try {
            structElements = new matvar_t*[nbStructElements];
        } catch (const std::bad_alloc&) {
            delete[] dims;
            return nullptr;
        }
        for (indexType i = 0; i < nbStructElements; ++i) {
            structElements[i] = nullptr;
        }

        if (isEmptyClassdefObject) {
            std::vector<double> dimensionValues;
            Dimensions originalDims(variableDims);
            for (indexType k = 0; k < originalDims.length(); ++k) {
                dimensionValues.push_back(static_cast<double>(originalDims[k]));
            }
            ArrayOf markerValues[3] = { ArrayOf::characterArray(className),
                ArrayOf::logicalScalar(true), ArrayOf::doubleRowVector(dimensionValues) };
            for (indexType j = 0; j < 3; ++j) {
                structElements[j]
                    = SaveMatioVariable(eval, savedFieldnames[j], markerValues[j], matVersion);
            }
            Dimensions markerDims(1, 1);
            delete[] dims;
            dims = convertDimensionsForMatVar(markerDims, rank);
            if (dims == nullptr) {
                delete[] structElements;
                return nullptr;
            }
            matvar_t* matVariable = Mat_VarCreate(variableName.c_str(), MAT_C_STRUCT, MAT_T_STRUCT,
                (int)rank, dims, structElements, 0);
            delete[] dims;
            return matVariable;
        }

        for (indexType i = 0; i < elementCount; ++i) {
            auto* object = handles == nullptr
                ? nullptr
                : dynamic_cast<ClassdefHandleObject*>(handles[i] ? handles[i]->object() : nullptr);
            if (object == nullptr) {
                delete[] dims;
                delete[] structElements;
                return nullptr;
            }
            for (indexType j = 0; j < static_cast<indexType>(nbFieldnames); ++j) {
                ArrayOf element;
                const std::string& fieldname = savedFieldnames[j];
                if (fieldname == CLASSDEF_SERIALIZATION_NAME_FIELD) {
                    element = ArrayOf::characterArray(className);
                } else if (fieldname == CLASSDEF_SERIALIZATION_HANDLE_FIELD) {
                    element = ArrayOf::logicalScalar(true);
                } else {
                    ArrayOfVector propertyValue;
                    if (!object->hasProperty(fieldname)) {
                        element = ArrayOf::empty();
                    } else if (!object->invokeProperty(fieldname, propertyValue)
                        || propertyValue.empty()) {
                        delete[] dims;
                        delete[] structElements;
                        return nullptr;
                    } else {
                        element = propertyValue[0];
                    }
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

    /* handle have no equivalent in other software */
    Warning(L"Nelson:matio:handleNotSaved", L"handle not saved.");
    return SaveMatioStruct(eval, variableName, ArrayOf::emptyStruct(), matVersion);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
