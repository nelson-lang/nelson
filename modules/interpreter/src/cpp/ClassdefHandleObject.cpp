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
#include "Error.hpp"
#include "FunctionDef.hpp"
#include "HandleManager.hpp"
#include "Types.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
#define CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY "Cannot modify constant property: "
#define CLASSDEF_EVENT_DATA_CLASS "event.EventData"
#define CLASSDEF_EVENT_FIELD_AFFECTED_OBJECT "AffectedObject"
#define CLASSDEF_EVENT_FIELD_EVENT_NAME "EventName"
#define CLASSDEF_EVENT_FIELD_PROPERTY_NAME "PropertyName"
#define CLASSDEF_EVENT_FIELD_SOURCE "Source"
#define CLASSDEF_EVENT_OBJECT_BEING_DESTROYED "ObjectBeingDestroyed"
#define CLASSDEF_EVENT_PROPERTY_EVENT_CLASS "event.PropertyEvent"
#define CLASSDEF_LISTENER_CLASS "event.listener"
#define CLASSDEF_PROPERTY_EVENT_KEY_SEPARATOR ":"
//=============================================================================
static ArrayOf
makeHandleArray(nelson_handle handle)
{
    return ArrayOf::handleConstructor(handle);
}
//=============================================================================
static ArrayOf
makeDefaultEventData(const ArrayOf& source, const std::string& eventName)
{
    stringVector fieldnames = { CLASSDEF_EVENT_FIELD_SOURCE, CLASSDEF_EVENT_FIELD_EVENT_NAME };
    ArrayOfVector values;
    values << source;
    values << ArrayOf::characterArrayConstructor(eventName);
    return ArrayOf::classConstructor(CLASSDEF_EVENT_DATA_CLASS, fieldnames, values);
}
//=============================================================================
static std::string
propertyEventKey(const std::string& propertyName, const std::string& eventName)
{
    return propertyName + CLASSDEF_PROPERTY_EVENT_KEY_SEPARATOR + eventName;
}
//=============================================================================
static ArrayOf
makePropertyEventData(
    const ArrayOf& source, const std::string& propertyName, const std::string& eventName)
{
    stringVector fieldnames = { CLASSDEF_EVENT_FIELD_SOURCE, CLASSDEF_EVENT_FIELD_EVENT_NAME,
        CLASSDEF_EVENT_FIELD_PROPERTY_NAME, CLASSDEF_EVENT_FIELD_AFFECTED_OBJECT };
    ArrayOfVector values;
    values << source;
    values << ArrayOf::characterArrayConstructor(eventName);
    values << ArrayOf::characterArrayConstructor(propertyName);
    values << source;
    return ArrayOf::classConstructor(CLASSDEF_EVENT_PROPERTY_EVENT_CLASS, fieldnames, values);
}
//=============================================================================
static void
evaluateListenerCallback(
    Evaluator* eval, const ArrayOf& callback, const ArrayOf& source, const ArrayOf& eventData)
{
    if (!callback.isFunctionHandle()) {
        return;
    }
    function_handle functionHandle = callback.getContentAsFunctionHandle();
    if (functionHandle.anonymousHandle == nullptr) {
        return;
    }
    FunctionDef* functionDef = reinterpret_cast<FunctionDef*>(functionHandle.anonymousHandle);
    ArrayOfVector inputs;
    inputs << source;
    inputs << eventData;
    functionDef->evaluateFunction(eval, inputs, 0);
}
//=============================================================================
ClassdefHandleObject::ClassdefHandleObject(const std::string& className)
    : HandleGenericObject(className, this, false)
{
}
//=============================================================================
bool
ClassdefHandleObject::isProperty(const std::wstring& propertyName)
{
    return ClassdefDefinitionManager::getInstance()->hasProperty(
        getCategory(), wstring_to_utf8(propertyName));
}
//=============================================================================
bool
ClassdefHandleObject::isMethod(const std::wstring& methodName)
{
    return ClassdefDefinitionManager::getInstance()->hasMethod(
        getCategory(), wstring_to_utf8(methodName));
}
//=============================================================================
bool
ClassdefHandleObject::invokeProperty(const std::string& propertyName, ArrayOfVector& results)
{
    if (!hasProperty(propertyName)) {
        return false;
    }
    results.clear();
    results.push_back(getProperty(propertyName));
    return true;
}
//=============================================================================
wstringVector
ClassdefHandleObject::getProperties()
{
    wstringVector result;
    for (const auto& name : ClassdefDefinitionManager::getInstance()->properties(getCategory())) {
        result.push_back(utf8_to_wstring(name));
    }
    return result;
}
//=============================================================================
wstringVector
ClassdefHandleObject::getMethods()
{
    wstringVector result;
    for (const auto& name : ClassdefDefinitionManager::getInstance()->methods(getCategory())) {
        result.push_back(utf8_to_wstring(name));
    }
    return result;
}
//=============================================================================
bool
ClassdefHandleObject::hasProperty(const std::string& propertyName)
{
    return ClassdefDefinitionManager::getInstance()->hasProperty(getCategory(), propertyName);
}
//=============================================================================
bool
ClassdefHandleObject::hasStoredProperty(const std::string& propertyName) const
{
    return _properties.find(propertyName) != _properties.end();
}
//=============================================================================
ArrayOf
ClassdefHandleObject::getProperty(const std::string& propertyName)
{
    auto found = _properties.find(propertyName);
    if (found != _properties.end()) {
        return found->second;
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
void
ClassdefHandleObject::setProperty(const std::string& propertyName, const ArrayOf& value)
{
    if (ClassdefDefinitionManager::getInstance()->hasDependentProperty(
            getCategory(), propertyName)) {
        return;
    }
    if (_properties.find(propertyName) != _properties.end()
        && ClassdefDefinitionManager::getInstance()->hasConstantProperty(
            getCategory(), propertyName)) {
        Error(CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY + propertyName);
    }
    _properties[propertyName] = value;
}
//=============================================================================
bool
ClassdefHandleObject::hasEvent(const std::string& eventName)
{
    if (eventName == CLASSDEF_EVENT_OBJECT_BEING_DESTROYED
        && ClassdefDefinitionManager::getInstance()->isHandleClass(getCategory())) {
        return true;
    }
    stringVector eventNames = ClassdefDefinitionManager::getInstance()->events(getCategory());
    return std::find(eventNames.begin(), eventNames.end(), eventName) != eventNames.end();
}
//=============================================================================
void
ClassdefHandleObject::addListener(nelson_handle listenerHandle)
{
    if (std::find(_listenerHandles.begin(), _listenerHandles.end(), listenerHandle)
        == _listenerHandles.end()) {
        _listenerHandles.push_back(listenerHandle);
    }
}
//=============================================================================
void
ClassdefHandleObject::removeListener(nelson_handle listenerHandle)
{
    _listenerHandles.erase(
        std::remove(_listenerHandles.begin(), _listenerHandles.end(), listenerHandle),
        _listenerHandles.end());
}
//=============================================================================
void
ClassdefHandleObject::notifyEvent(Evaluator* eval, nelson_handle sourceHandle,
    const std::string& eventName, const ArrayOf& eventData)
{
    if (!hasEvent(eventName)) {
        return;
    }
    ArrayOf source = makeHandleArray(sourceHandle);
    ArrayOf payload = eventData.isEmpty() ? makeDefaultEventData(source, eventName) : eventData;

    std::vector<nelson_handle> listeners = _listenerHandles;
    for (nelson_handle listenerHandle : listeners) {
        HandleGenericObject* listenerObject
            = HandleManager::getInstance()->getPointer(listenerHandle);
        auto* listener = dynamic_cast<ClassdefListenerObject*>(listenerObject);
        if (listener == nullptr || !listener->enabled()
            || !listener->matches(sourceHandle, eventName)) {
            continue;
        }
        evaluateListenerCallback(eval, listener->callback(), source, payload);
    }
}
//=============================================================================
void
ClassdefHandleObject::notifyPropertyEvent(Evaluator* eval, nelson_handle sourceHandle,
    const std::string& propertyName, const std::string& eventName)
{
    if (!ClassdefDefinitionManager::getInstance()->isObservablePropertyEvent(
            getCategory(), propertyName, eventName)) {
        return;
    }

    ArrayOf source = makeHandleArray(sourceHandle);
    ArrayOf payload = makePropertyEventData(source, propertyName, eventName);
    const std::string listenerEventName = propertyEventKey(propertyName, eventName);

    std::vector<nelson_handle> listeners = _listenerHandles;
    for (nelson_handle listenerHandle : listeners) {
        HandleGenericObject* listenerObject
            = HandleManager::getInstance()->getPointer(listenerHandle);
        auto* listener = dynamic_cast<ClassdefListenerObject*>(listenerObject);
        if (listener == nullptr || !listener->enabled()
            || !listener->matches(sourceHandle, listenerEventName)) {
            continue;
        }
        evaluateListenerCallback(eval, listener->callback(), source, payload);
    }
}
//=============================================================================
void
ClassdefHandleObject::deleteListeners()
{
    std::vector<nelson_handle> listeners = _listenerHandles;
    _listenerHandles.clear();
    for (nelson_handle listenerHandle : listeners) {
        HandleGenericObject* listenerObject
            = HandleManager::getInstance()->getPointer(listenerHandle);
        auto* listener = dynamic_cast<ClassdefListenerObject*>(listenerObject);
        if (listener != nullptr) {
            delete listener;
        }
        HandleManager::getInstance()->removeHandle(listenerHandle);
    }
}
//=============================================================================
ArrayOf
ClassdefHandleObject::toStruct()
{
    stringVector fieldnames = ClassdefDefinitionManager::getInstance()->properties(getCategory());
    fieldnames.erase(std::remove_if(fieldnames.begin(), fieldnames.end(),
                         [&](const std::string& fieldname) {
                             return ClassdefDefinitionManager::getInstance()->hasDependentProperty(
                                 getCategory(), fieldname);
                         }),
        fieldnames.end());
    ArrayOfVector values;
    values.reserve(fieldnames.size());
    for (const auto& fieldname : fieldnames) {
        values.push_back(getProperty(fieldname));
    }
    return ArrayOf::structScalarConstructor(fieldnames, values);
}
//=============================================================================
ClassdefListenerObject::ClassdefListenerObject(
    nelson_handle sourceHandle, const std::string& eventName, const ArrayOf& callback)
    : HandleGenericObject(CLASSDEF_LISTENER_CLASS, this, false)
    , _sourceHandle(sourceHandle)
    , _eventName(eventName)
    , _callback(callback)
{
}
//=============================================================================
nelson_handle
ClassdefListenerObject::sourceHandle() const
{
    return _sourceHandle;
}
//=============================================================================
const std::string&
ClassdefListenerObject::eventName() const
{
    return _eventName;
}
//=============================================================================
const ArrayOf&
ClassdefListenerObject::callback() const
{
    return _callback;
}
//=============================================================================
bool
ClassdefListenerObject::enabled() const
{
    return _enabled;
}
//=============================================================================
bool
ClassdefListenerObject::matches(nelson_handle sourceHandle, const std::string& eventName) const
{
    return _enabled && _sourceHandle == sourceHandle && _eventName == eventName;
}
//=============================================================================
void
ClassdefListenerObject::detachFromSource()
{
    HandleGenericObject* sourceObject = HandleManager::getInstance()->getPointer(_sourceHandle);
    auto* source = dynamic_cast<ClassdefHandleObject*>(sourceObject);
    if (source != nullptr) {
        nelson_handle listenerHandle = HandleManager::getInstance()->findByPointerValue(this);
        if (listenerHandle != static_cast<nelson_handle>(-1)) {
            source->removeListener(listenerHandle);
        }
    }
    _enabled = false;
    _sourceHandle = 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
