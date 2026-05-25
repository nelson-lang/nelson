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
#include "Warning.hpp"
#include "i18n.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioHandle(const std::string& variableName, const ArrayOf& variableValue, mat_ft matVersion)
{
    const std::string className = variableValue.getHandleClassName();
    auto* manager = ClassdefDefinitionManager::getInstance();
    if (manager->loadClass(className) && manager->isHandleClass(className)) {
        Dimensions variableDims = variableValue.getDimensions();
        indexType rank;
        size_t* dims = convertDimensionsForMatVar(variableDims, rank);
        if (dims == nullptr) {
            return nullptr;
        }

        stringVector fieldnames;
        for (const auto& property : manager->propertyDefinitions(className)) {
            if (!manager->hasDependentProperty(className, property.name)) {
                fieldnames.push_back(property.name);
            }
        }
        stringVector savedFieldnames = fieldnames;
        savedFieldnames.push_back(CLASSDEF_SERIALIZATION_NAME_FIELD);
        savedFieldnames.push_back(CLASSDEF_SERIALIZATION_HANDLE_FIELD);

        size_t nbFieldnames = savedFieldnames.size();
        indexType nbStructElements = nbFieldnames * variableDims.getElementCount() + 1;
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

        const auto* handles = static_cast<const nelson_handle*>(variableValue.getDataPointer());
        indexType elementCount = variableDims.getElementCount();
        for (indexType i = 0; i < elementCount; ++i) {
            auto* object = handles == nullptr
                ? nullptr
                : dynamic_cast<ClassdefHandleObject*>(
                      HandleManager::getInstance()->getPointer(handles[i]));
            if (object == nullptr) {
                delete[] dims;
                delete[] structElements;
                return nullptr;
            }
            for (indexType j = 0; j < static_cast<indexType>(nbFieldnames); ++j) {
                ArrayOf element;
                const std::string& fieldname = savedFieldnames[j];
                if (fieldname == CLASSDEF_SERIALIZATION_NAME_FIELD) {
                    element = ArrayOf::characterArrayConstructor(className);
                } else if (fieldname == CLASSDEF_SERIALIZATION_HANDLE_FIELD) {
                    element = ArrayOf::logicalConstructor(true);
                } else {
                    ArrayOfVector propertyValue;
                    if (!object->invokeProperty(fieldname, propertyValue)
                        || propertyValue.empty()) {
                        delete[] dims;
                        delete[] structElements;
                        return nullptr;
                    }
                    element = propertyValue[0];
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

    /* handle have no equivalent in other software */
    Warning(_W("handle not saved."));
    return SaveMatioStruct(variableName, ArrayOf::emptyStructWithoutFields(), matVersion);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
