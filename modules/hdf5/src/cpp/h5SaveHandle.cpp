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
#include "HandleState.hpp"
#include "characters_encoding.hpp"
#include "h5SaveStruct.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
#define CLASSDEF_ATTR_TRANSIENT "Transient"
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
bool
h5SaveHandle(Evaluator* eval, hid_t fid, const std::string& location,
    const std::string& variableName, const ArrayOf& VariableValue, bool useCompression)
{
    if (!VariableValue.isHandle()) {
        return false;
    }
    const std::string className = VariableValue.handleClassName();

    if (!classdefLoadClass(className) || !classdefIsHandleClass(className)) {
        return false;
    }

    stringVector fieldnames;
    for (const auto& property : classdefPropertyDefinitions(className)) {
        if (!classdefHasDependentProperty(className, property.name)
            && !classdefPropertyHasAttribute(property, CLASSDEF_ATTR_TRANSIENT)) {
            fieldnames.push_back(property.name);
        }
    }
    Dimensions dims = VariableValue.dimensions();
    indexType elementCount = dims.elementCount();
    const auto* handles = static_cast<const HandleRef*>(VariableValue.dataPointer());
    for (indexType k = 0; k < elementCount; ++k) {
        auto* object = handles == nullptr
            ? nullptr
            : dynamic_cast<ClassdefHandleObject*>(handles[k] ? handles[k]->object() : nullptr);
        appendPersistentDynamicProperties(object, fieldnames);
    }
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[elementCount * fieldnames.size()];
    } catch (const std::bad_alloc&) {
        return false;
    }

    indexType offset = 0;
    for (indexType k = 0; k < elementCount; ++k) {
        auto* object = handles == nullptr
            ? nullptr
            : dynamic_cast<ClassdefHandleObject*>(handles[k] ? handles[k]->object() : nullptr);
        if (object == nullptr) {
            delete[] elements;
            return false;
        }
        for (const auto& fieldname : fieldnames) {
            ArrayOfVector propertyValue;
            if (!object->hasProperty(fieldname)) {
                elements[offset++] = ArrayOf::empty();
                continue;
            }
            if (!object->invokeProperty(fieldname, propertyValue) || propertyValue.empty()) {
                delete[] elements;
                return false;
            }
            elements[offset++] = propertyValue[0];
        }
    }

    ArrayOf asObject(NLS_CLASSDEF_ARRAY, dims, elements, false, fieldnames);
    asObject.setClassType(className);
    asObject.setClassdefLayout(classdefLayout(className));
    return h5SaveStruct(eval, fid, location, variableName, asObject, useCompression);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
