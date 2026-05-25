//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5SaveHandle.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "h5SaveStruct.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveHandle(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    if (!VariableValue.isHandle()) {
        return false;
    }
    const std::string className = VariableValue.getHandleClassName();
    auto* manager = ClassdefDefinitionManager::getInstance();
    if (!manager->loadClass(className) || !manager->isHandleClass(className)) {
        return false;
    }

    stringVector fieldnames;
    for (const auto& property : manager->propertyDefinitions(className)) {
        if (!manager->hasDependentProperty(className, property.name)) {
            fieldnames.push_back(property.name);
        }
    }
    Dimensions dims = VariableValue.getDimensions();
    indexType elementCount = dims.getElementCount();
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[elementCount * fieldnames.size()];
    } catch (const std::bad_alloc&) {
        return false;
    }

    const auto* handles = static_cast<const nelson_handle*>(VariableValue.getDataPointer());
    indexType offset = 0;
    for (indexType k = 0; k < elementCount; ++k) {
        auto* object = handles == nullptr
            ? nullptr
            : dynamic_cast<ClassdefHandleObject*>(
                  HandleManager::getInstance()->getPointer(handles[k]));
        if (object == nullptr) {
            delete[] elements;
            return false;
        }
        for (const auto& fieldname : fieldnames) {
            ArrayOfVector propertyValue;
            if (!object->invokeProperty(fieldname, propertyValue) || propertyValue.empty()) {
                delete[] elements;
                return false;
            }
            elements[offset++] = propertyValue[0];
        }
    }

    ArrayOf asObject(NLS_CLASS_ARRAY, dims, elements, false, fieldnames);
    asObject.setClassType(className);
    return h5SaveStruct(fid, location, variableName, asObject, useCompression);
}
//=============================================================================
};
//=============================================================================
