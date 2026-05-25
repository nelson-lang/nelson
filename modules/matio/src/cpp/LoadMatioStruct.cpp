//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <vector>
#include "LoadMatioStruct.hpp"
#include "LoadMatioVariable.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "Exception.hpp"
#include "HandleManager.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
findFieldIndex(const stringVector& fieldnames, const std::string& fieldname, indexType& index)
{
    for (indexType k = 0; k < static_cast<indexType>(fieldnames.size()); ++k) {
        if (fieldnames[k] == fieldname) {
            index = k;
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
loadClassdefStruct(const Dimensions& dims, const stringVector& fieldnames,
    const std::vector<ArrayOf>& loadedValues, ArrayOf& VariableValue)
{
    indexType classNameIndex = 0;
    indexType isHandleIndex = 0;
    if (!findFieldIndex(fieldnames, CLASSDEF_SERIALIZATION_NAME_FIELD, classNameIndex)
        || !findFieldIndex(fieldnames, CLASSDEF_SERIALIZATION_HANDLE_FIELD, isHandleIndex)
        || dims.getElementCount() == 0) {
        return false;
    }

    std::string className;
    bool isHandleObject = false;
    try {
        className = loadedValues[classNameIndex].getContentAsCString();
        isHandleObject = loadedValues[isHandleIndex].getContentAsLogicalScalar() != 0;
    } catch (const Exception&) {
        return false;
    }

    auto* manager = ClassdefDefinitionManager::getInstance();
    if (!manager->loadClass(className)) {
        return false;
    }

    stringVector objectFieldnames;
    std::vector<indexType> objectFieldIndexes;
    for (indexType k = 0; k < static_cast<indexType>(fieldnames.size()); ++k) {
        if (fieldnames[k] == CLASSDEF_SERIALIZATION_NAME_FIELD
            || fieldnames[k] == CLASSDEF_SERIALIZATION_HANDLE_FIELD) {
            continue;
        }
        objectFieldnames.push_back(fieldnames[k]);
        objectFieldIndexes.push_back(k);
    }

    indexType elementCount = dims.getElementCount();
    indexType sourceFieldCount = static_cast<indexType>(fieldnames.size());
    if (isHandleObject && manager->isHandleClass(className)) {
        nelson_handle* handles = static_cast<nelson_handle*>(
            ArrayOf::allocateArrayOf(NLS_HANDLE, elementCount, stringVector(), false));
        for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
            auto* object = new ClassdefHandleObject(className);
            for (indexType fieldIndex = 0;
                fieldIndex < static_cast<indexType>(objectFieldnames.size()); ++fieldIndex) {
                indexType sourceOffset
                    = elementIndex * sourceFieldCount + objectFieldIndexes[fieldIndex];
                object->setProperty(objectFieldnames[fieldIndex], loadedValues[sourceOffset]);
            }
            handles[elementIndex] = HandleManager::getInstance()->addHandle(object);
        }
        VariableValue = ArrayOf(NLS_HANDLE, dims, handles);
        return true;
    }

    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[elementCount * objectFieldnames.size()];
    } catch (const std::bad_alloc&) {
        return false;
    }
    for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
        for (indexType fieldIndex = 0; fieldIndex < static_cast<indexType>(objectFieldnames.size());
            ++fieldIndex) {
            indexType sourceOffset
                = elementIndex * sourceFieldCount + objectFieldIndexes[fieldIndex];
            indexType destinationOffset
                = elementIndex * static_cast<indexType>(objectFieldnames.size()) + fieldIndex;
            elements[destinationOffset] = loadedValues[sourceOffset];
        }
    }
    VariableValue = ArrayOf(NLS_CLASS_ARRAY, dims, elements, false, objectFieldnames);
    VariableValue.setClassType(className);
    return true;
}
//=============================================================================
bool
LoadMatioStruct(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    unsigned int nbFields = Mat_VarGetNumberOfFields(matVariable);
    char* const* names = Mat_VarGetStructFieldnames(matVariable);
    stringVector fieldnames;
    fieldnames.reserve((size_t)nbFields);
    for (unsigned int k = 0; k < nbFields; k++) {
        fieldnames.push_back(names[k]);
    }

    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyStructConstructor(fieldnames, dims);
        bSuccess = true;
    } else if (dims.equals(Dimensions(1, 1)) && fieldnames.empty()) {
        VariableValue = ArrayOf::emptyStructWithoutFields();
        bSuccess = true;
    } else {
        indexType nbElements = dims.getElementCount();
        std::vector<ArrayOf> loadedValues(nbElements * fieldnames.size());
        indexType offset = 0;
        for (indexType j = 0; j < nbElements; j++) {
            for (const auto& name : fieldnames) {
                matvar_t* fieldMatVar = Mat_VarGetStructFieldByName(matVariable, name.c_str(), j);
                bool bSuccess = LoadMatioVariable(fieldMatVar, true, loadedValues[offset]);
                if (!bSuccess) {
                    return false;
                }
                offset++;
            }
        }
        if (loadClassdefStruct(dims, fieldnames, loadedValues, VariableValue)) {
            return true;
        }

        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements * fieldnames.size()];
        } catch (const std::bad_alloc&) {
            return false;
        }
        for (indexType k = 0; k < nbElements * static_cast<indexType>(fieldnames.size()); ++k) {
            elements[k] = loadedValues[k];
        }
        VariableValue = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
