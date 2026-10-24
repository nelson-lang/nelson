//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserConstants.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
//=============================================================================
#define CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY "Cannot modify constant property: "
#define CLASSDEF_ERROR_WEAK_HANDLE_VALUE "WeakHandle property value must be a handle object: "
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
weakHandleSlotValue(const ClassdefWeakHandleSlot& slot);
static bool
isNonCopyableProperty(const ClassdefHandleObject& object, const std::string& className,
    const std::string& propertyName);
//=============================================================================
bool
ClassdefHandleObject::hasStoredProperty(const std::string& propertyName) const
{
    return _properties.find(propertyName) != _properties.end()
        || (_hasWeakHandleProperties
            && _weakHandleProperties.find(propertyName) != _weakHandleProperties.end());
}
//=============================================================================
ArrayOf
ClassdefHandleObject::getProperty(const std::string& propertyName)
{
    if (_hasWeakHandleProperties) {
        auto weak = _weakHandleProperties.find(propertyName);
        if (weak != _weakHandleProperties.end()) {
            return weakHandleSlotValue(weak->second);
        }
    }
    auto found = _properties.find(propertyName);
    if (found != _properties.end()) {
        return found->second;
    }
    return ArrayOf::empty();
}
//=============================================================================
void
ClassdefHandleObject::setProperty(const std::string& propertyName, const ArrayOf& value)
{
    if (classdefHasDependentProperty(category(), propertyName)
        || (hasDynamicProperty(propertyName)
            && dynamicPropertyAttributes(propertyName).dependent)) {
        return;
    }
    if (isWeakHandleProperty(propertyName)) {
        storeWeakHandleProperty(propertyName, value);
        return;
    }
    auto found = _properties.find(propertyName);
    if (found != _properties.end()) {
        if (classdefHasConstantProperty(category(), propertyName)) {
            RaiseError(CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY + propertyName,
                "Nelson:interpreter:cannotModifyConstantProperty");
        }
        found->second = value;
        return;
    }
    _properties.emplace(propertyName, value);
}
//=============================================================================
void
ClassdefHandleObject::setStoredProperty(const std::string& propertyName, const ArrayOf& value)
{
    if (isWeakHandleProperty(propertyName)) {
        storeWeakHandleProperty(propertyName, value);
        return;
    }
    auto found = _properties.find(propertyName);
    if (found != _properties.end()) {
        found->second = value;
        return;
    }
    _properties.emplace(propertyName, value);
}
//=============================================================================
void
ClassdefHandleObject::copyStoredPropertiesFrom(
    const ClassdefHandleObject& source, bool copyNonCopyable)
{
    if (copyNonCopyable) {
        _properties.clear();
        _weakHandleProperties.clear();
        _dynamicProperties.clear();
        _dynamicPropertyAttributes.clear();
    }
    for (const auto& propertyName : source._dynamicProperties) {
        const ClassdefDynamicPropertyAttributes attributes
            = source.dynamicPropertyAttributes(propertyName);
        if (!copyNonCopyable && attributes.nonCopyable) {
            continue;
        }
        if (!hasDynamicProperty(propertyName)) {
            _dynamicProperties.push_back(propertyName);
        }
        _dynamicPropertyAttributes[propertyName] = attributes;
    }
    for (const auto& item : source._properties) {
        if (!copyNonCopyable && isNonCopyableProperty(source, category(), item.first)) {
            continue;
        }
        _properties[item.first] = item.second;
    }
    for (const auto& item : source._weakHandleProperties) {
        if (!copyNonCopyable && isNonCopyableProperty(source, category(), item.first)) {
            continue;
        }
        _weakHandleProperties[item.first] = item.second;
    }
}
//=============================================================================
bool
ClassdefHandleObject::isWeakHandleProperty(const std::string& propertyName)
{
    return _hasWeakHandleProperties && classdefIsWeakHandleProperty(category(), propertyName);
}
//=============================================================================
void
ClassdefHandleObject::storeWeakHandleProperty(const std::string& propertyName, const ArrayOf& value)
{
    ClassdefWeakHandleSlot slot;
    slot.dims = value.dimensions();
    if (!value.isHandle()) {
        if (!value.isEmpty()) {
            RaiseError(CLASSDEF_ERROR_WEAK_HANDLE_VALUE + propertyName,
                "Nelson:class:weakHandleRequiresHandle");
        }
        slot.className = NLS_HANDLE_STR;
        _weakHandleProperties[propertyName] = std::move(slot);
        return;
    }
    slot.className = value.handleClassName();
    const auto* refs = static_cast<const HandleRef*>(value.dataPointer());
    const indexType count = value.elementCount();
    slot.targets.reserve(count);
    for (indexType k = 0; k < count; ++k) {
        slot.targets.emplace_back(refs != nullptr ? refs[k] : HandleRef());
    }
    _weakHandleProperties[propertyName] = std::move(slot);
}
//=============================================================================
ArrayOf
weakHandleSlotValue(const ClassdefWeakHandleSlot& slot)
{
    Dimensions dims = slot.dims;
    if (slot.targets.empty()) {
        return ArrayOf::emptyHandle(slot.className, dims);
    }
    auto* refs = static_cast<HandleRef*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, slot.targets.size(), stringVector(), false));
    for (size_t k = 0; k < slot.targets.size(); ++k) {
        // a destroyed target reads back as a deleted handle of the same class
        HandleRef ref = slot.targets[k].lock();
        refs[k] = ref ? ref : HandleManager::getInstance()->addInvalidHandleRef(slot.className);
    }
    return ArrayOf(NLS_HANDLE, dims, refs);
}
//=============================================================================
bool
isNonCopyableProperty(const ClassdefHandleObject& object, const std::string& className,
    const std::string& propertyName)
{
    if (object.hasDynamicProperty(propertyName)) {
        return object.dynamicPropertyAttributes(propertyName).nonCopyable;
    }
    ClassdefPropertyDefinition property;
    return classdefPropertyDefinition(className, propertyName, property)
        && classdefPropertyHasAttribute(property, CLASSDEF_ATTR_NON_COPYABLE);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
