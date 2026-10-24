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
#include "FunctionDef.hpp"
#include "HandleManager.hpp"
#include "Types.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define CLASSDEF_DYNAMIC_PROPERTY_ABORT_SET "AbortSet"
#define CLASSDEF_DYNAMIC_PROPERTY_DEPENDENT "Dependent"
#define CLASSDEF_DYNAMIC_PROPERTY_GET_ACCESS "GetAccess"
#define CLASSDEF_DYNAMIC_PROPERTY_GET_METHOD "GetMethod"
#define CLASSDEF_DYNAMIC_PROPERTY_GET_OBSERVABLE "GetObservable"
#define CLASSDEF_DYNAMIC_PROPERTY_HIDDEN "Hidden"
#define CLASSDEF_DYNAMIC_PROPERTY_NON_COPYABLE "NonCopyable"
#define CLASSDEF_DYNAMIC_PROPERTY_SET_ACCESS "SetAccess"
#define CLASSDEF_DYNAMIC_PROPERTY_SET_METHOD "SetMethod"
#define CLASSDEF_DYNAMIC_PROPERTY_SET_OBSERVABLE "SetObservable"
#define CLASSDEF_DYNAMIC_PROPERTY_VALIDATION_EXPRESSION "ValidationExpression"
#define CLASSDEF_ATTR_TRANSIENT "Transient"
#define CLASSDEF_EVENT_DATA_CLASS "event.EventData"
#define CLASSDEF_EVENT_FIELD_AFFECTED_OBJECT "AffectedObject"
#define CLASSDEF_EVENT_FIELD_EVENT_NAME "EventName"
#define CLASSDEF_EVENT_FIELD_PROPERTY_NAME "PropertyName"
#define CLASSDEF_EVENT_FIELD_SOURCE "Source"
#define CLASSDEF_EVENT_OBJECT_BEING_DESTROYED "ObjectBeingDestroyed"
#define CLASSDEF_EVENT_POST_GET "PostGet"
#define CLASSDEF_EVENT_POST_SET "PostSet"
#define CLASSDEF_EVENT_PRE_GET "PreGet"
#define CLASSDEF_EVENT_PRE_SET "PreSet"
#define CLASSDEF_EVENT_PROPERTY_EVENT_CLASS "event.PropertyEvent"
#define CLASSDEF_EVENT_DYNAMIC_PROPERTY_EVENT_CLASS "event.DynamicPropertyEvent"
#define CLASSDEF_LISTENER_CLASS "event.listener"
#define CLASSDEF_DYNAMIC_PROPERTY_CLASS "meta.DynamicProperty"
#define CLASSDEF_PROPERTY_EVENT_KEY_SEPARATOR ":"
//=============================================================================
static ArrayOf
makeHandleArray(nelson_handle handle)
{
    return ArrayOf::handle(handle);
}
//=============================================================================
static ArrayOf
makeDefaultEventData(const ArrayOf& source, const std::string& eventName)
{
    stringVector fieldnames = { CLASSDEF_EVENT_FIELD_SOURCE, CLASSDEF_EVENT_FIELD_EVENT_NAME };
    ArrayOfVector values;
    values << source;
    values << ArrayOf::characterArray(eventName);
    return ArrayOf::classArray(CLASSDEF_EVENT_DATA_CLASS, fieldnames, values);
}
//=============================================================================
static ArrayOf
fillEventDataFields(const ArrayOf& eventData, const ArrayOf& source, const std::string& eventName)
{
    if (!eventData.isClassType() || !eventData.isScalar()
        || !classdefIsA(eventData.classType(), CLASSDEF_EVENT_DATA_CLASS)) {
        return eventData;
    }
    stringVector fieldnames = eventData.fieldNames();
    ArrayOfVector values;
    values.reserve(fieldnames.size() + 2);
    bool hasSource = false;
    bool hasEventName = false;
    for (const auto& fieldname : fieldnames) {
        if (fieldname == CLASSDEF_EVENT_FIELD_SOURCE) {
            hasSource = true;
            values << source;
        } else if (fieldname == CLASSDEF_EVENT_FIELD_EVENT_NAME) {
            hasEventName = true;
            values << ArrayOf::characterArray(eventName);
        } else {
            values << eventData.field(fieldname);
        }
    }
    if (!hasSource) {
        fieldnames.push_back(CLASSDEF_EVENT_FIELD_SOURCE);
        values << source;
    }
    if (!hasEventName) {
        fieldnames.push_back(CLASSDEF_EVENT_FIELD_EVENT_NAME);
        values << ArrayOf::characterArray(eventName);
    }
    ArrayOf filled = ArrayOf::classdefArray(eventData.classType(), fieldnames, values);
    filled.setClassdefLayout(eventData.classdefLayout());
    return filled;
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
    values << ArrayOf::characterArray(eventName);
    values << ArrayOf::characterArray(propertyName);
    values << source;
    return ArrayOf::classArray(CLASSDEF_EVENT_PROPERTY_EVENT_CLASS, fieldnames, values);
}
//=============================================================================
static ArrayOf
makeDynamicPropertyEventData(
    const ArrayOf& source, const std::string& propertyName, const std::string& eventName)
{
    stringVector fieldnames = { CLASSDEF_EVENT_FIELD_SOURCE, CLASSDEF_EVENT_FIELD_EVENT_NAME,
        CLASSDEF_EVENT_FIELD_PROPERTY_NAME };
    ArrayOfVector values;
    values << source;
    values << ArrayOf::characterArray(eventName);
    values << ArrayOf::characterArray(propertyName);
    return ArrayOf::classArray(CLASSDEF_EVENT_DYNAMIC_PROPERTY_EVENT_CLASS, fieldnames, values);
}
//=============================================================================
static void
evaluateListenerCallback(
    Evaluator* eval, const ArrayOf& callback, const ArrayOf& source, const ArrayOf& eventData)
{
    if (!callback.isFunctionHandle()) {
        return;
    }
    function_handle functionHandle = callback.asFunctionHandle();
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
    // A class (or superclass) with a delete method must outlive its last
    // handle reference until that method has run.
    setFinalizerRequired(classdefHasMethodDefinition(className, CLASSDEF_FUNCTION_DELETE));
    _hasWeakHandleProperties = classdefIsWeakHandleProperty(className, {});
}
//=============================================================================
bool
ClassdefHandleObject::deleteInvoked() const
{
    return finalizerStarted();
}
//=============================================================================
void
ClassdefHandleObject::markDeleteInvoked()
{
    markFinalizerStarted();
}
//=============================================================================
void
ClassdefHandleObject::reserveStoredProperties(size_t count)
{
    _properties.reserve(count);
}
//=============================================================================
bool
ClassdefHandleObject::isProperty(const std::wstring& propertyName)
{
    return hasProperty(wstring_to_utf8(propertyName));
}
//=============================================================================
bool
ClassdefHandleObject::isMethod(const std::wstring& methodName)
{
    return classdefHasMethod(category(), wstring_to_utf8(methodName));
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
ClassdefHandleObject::properties()
{
    wstringVector result;
    for (const auto& name : classdefProperties(category())) {
        result.push_back(utf8_to_wstring(name));
    }
    for (const auto& name : _dynamicProperties) {
        if (!dynamicPropertyAttributes(name).hidden) {
            result.push_back(utf8_to_wstring(name));
        }
    }
    return result;
}
//=============================================================================
wstringVector
ClassdefHandleObject::methods()
{
    wstringVector result;
    for (const auto& name : classdefMethods(category())) {
        result.push_back(utf8_to_wstring(name));
    }
    return result;
}
//=============================================================================
bool
ClassdefHandleObject::hasProperty(const std::string& propertyName)
{
    return classdefHasProperty(category(), propertyName) || hasDynamicProperty(propertyName);
}
//=============================================================================
bool
ClassdefHandleObject::hasDynamicProperty(const std::string& propertyName) const
{
    return std::find(_dynamicProperties.begin(), _dynamicProperties.end(), propertyName)
        != _dynamicProperties.end();
}
//=============================================================================
void
ClassdefHandleObject::addDynamicProperty(const std::string& propertyName)
{
    if (!hasDynamicProperty(propertyName)) {
        _dynamicProperties.push_back(propertyName);
        _dynamicPropertyAttributes[propertyName] = {};
        _properties[propertyName] = ArrayOf::empty();
    }
}
//=============================================================================
bool
ClassdefHandleObject::removeDynamicProperty(const std::string& propertyName)
{
    auto found = std::find(_dynamicProperties.begin(), _dynamicProperties.end(), propertyName);
    if (found == _dynamicProperties.end()) {
        return false;
    }
    _dynamicProperties.erase(found);
    _dynamicPropertyAttributes.erase(propertyName);
    _properties.erase(propertyName);
    return true;
}
//=============================================================================
stringVector
ClassdefHandleObject::dynamicProperties() const
{
    return _dynamicProperties;
}
//=============================================================================
ClassdefDynamicPropertyAttributes
ClassdefHandleObject::dynamicPropertyAttributes(const std::string& propertyName) const
{
    auto found = _dynamicPropertyAttributes.find(propertyName);
    if (found != _dynamicPropertyAttributes.end()) {
        return found->second;
    }
    return {};
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyHidden(const std::string& propertyName, bool value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].hidden = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyNonCopyable(const std::string& propertyName, bool value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].nonCopyable = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyTransient(const std::string& propertyName, bool value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].transient = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyAbortSet(const std::string& propertyName, bool value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].abortSet = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyDependent(const std::string& propertyName, bool value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].dependent = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyGetObservable(const std::string& propertyName, bool value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].getObservable = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyGetAccess(
    const std::string& propertyName, const std::string& value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].getAccess = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyGetMethod(
    const std::string& propertyName, const ArrayOf& value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].getMethod = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertySetObservable(const std::string& propertyName, bool value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].setObservable = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertySetAccess(
    const std::string& propertyName, const std::string& value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].setAccess = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertySetMethod(
    const std::string& propertyName, const ArrayOf& value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].setMethod = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::setDynamicPropertyValidationExpression(
    const std::string& propertyName, const std::string& value)
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    _dynamicPropertyAttributes[propertyName].validationExpression = value;
    return true;
}
//=============================================================================
bool
ClassdefHandleObject::isDynamicPropertyObservableEvent(
    const std::string& propertyName, const std::string& eventName) const
{
    if (!hasDynamicProperty(propertyName)) {
        return false;
    }
    const ClassdefDynamicPropertyAttributes attributes = dynamicPropertyAttributes(propertyName);
    if (eventName == CLASSDEF_EVENT_PRE_GET || eventName == CLASSDEF_EVENT_POST_GET) {
        return attributes.getObservable;
    }
    if (eventName == CLASSDEF_EVENT_PRE_SET || eventName == CLASSDEF_EVENT_POST_SET) {
        return attributes.setObservable;
    }
    return false;
}
//=============================================================================
bool
ClassdefHandleObject::hasEvent(const std::string& eventName)
{
    if (eventName == CLASSDEF_EVENT_OBJECT_BEING_DESTROYED && classdefIsHandleClass(category())) {
        return true;
    }
    stringVector eventNames = classdefEvents(category());
    return std::find(eventNames.begin(), eventNames.end(), eventName) != eventNames.end();
}
//=============================================================================
void
ClassdefHandleObject::addListener(nelson_handle listenerHandle, bool bindToSource)
{
    if (std::find(_listenerHandles.begin(), _listenerHandles.end(), listenerHandle)
        == _listenerHandles.end()) {
        _listenerHandles.push_back(listenerHandle);
    }
    // a listener may observe ObjectBeingDestroyed even without a user delete
    setFinalizerRequired(true);
    if (bindToSource) {
        HandleRef ref = HandleManager::getInstance()->getHandleRef(listenerHandle);
        if (ref) {
            _boundListenerRefs.push_back(ref);
        }
    }
}
//=============================================================================
void
ClassdefHandleObject::removeListener(nelson_handle listenerHandle)
{
    _listenerHandles.erase(
        std::remove(_listenerHandles.begin(), _listenerHandles.end(), listenerHandle),
        _listenerHandles.end());
    _boundListenerRefs.erase(
        std::remove_if(_boundListenerRefs.begin(), _boundListenerRefs.end(),
            [listenerHandle](const HandleRef& ref) { return !ref || ref->id() == listenerHandle; }),
        _boundListenerRefs.end());
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
    ArrayOf payload = eventData.isEmpty() ? makeDefaultEventData(source, eventName)
                                          : fillEventDataFields(eventData, source, eventName);

    std::vector<nelson_handle> listeners = _listenerHandles;
    for (nelson_handle listenerHandle : listeners) {
        HandleGenericObject* listenerObject = HandleManager::getInstance()->pointer(listenerHandle);
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
ClassdefHandleObject::notifyDynamicPropertyEvent(Evaluator* eval, nelson_handle sourceHandle,
    const std::string& propertyName, const std::string& eventName)
{
    notifyEvent(eval, sourceHandle, eventName,
        makeDynamicPropertyEventData(makeHandleArray(sourceHandle), propertyName, eventName));
}
//=============================================================================
void
ClassdefHandleObject::notifyPropertyEvent(Evaluator* eval, nelson_handle sourceHandle,
    const std::string& propertyName, const std::string& eventName)
{
    if (!isDynamicPropertyObservableEvent(propertyName, eventName)
        && !classdefIsObservablePropertyEvent(category(), propertyName, eventName)) {
        return;
    }

    ArrayOf source = makeHandleArray(sourceHandle);
    ArrayOf payload = makePropertyEventData(source, propertyName, eventName);
    const std::string listenerEventName = propertyEventKey(propertyName, eventName);

    std::vector<nelson_handle> listeners = _listenerHandles;
    for (nelson_handle listenerHandle : listeners) {
        HandleGenericObject* listenerObject = HandleManager::getInstance()->pointer(listenerHandle);
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
        HandleRef ref = HandleManager::getInstance()->getHandleRef(listenerHandle);
        if (ref) {
            ref->invalidate(nullptr);
        }
    }
    _boundListenerRefs.clear();
}
//=============================================================================
void
ClassdefHandleObject::onDelete(Evaluator* eval)
{
    deleteListeners();
}
//=============================================================================
ArrayOf
ClassdefHandleObject::toStruct()
{
    stringVector fieldnames;
    for (const auto& property : classdefPropertyDefinitions(category())) {
        if (!classdefHasDependentProperty(category(), property.name)
            && !classdefPropertyHasAttribute(property, CLASSDEF_ATTR_TRANSIENT)) {
            fieldnames.push_back(property.name);
        }
    }
    for (const auto& propertyName : _dynamicProperties) {
        if (!dynamicPropertyAttributes(propertyName).transient) {
            fieldnames.push_back(propertyName);
        }
    }
    ArrayOfVector values;
    values.reserve(fieldnames.size());
    for (const auto& fieldname : fieldnames) {
        values.push_back(getProperty(fieldname));
    }
    return ArrayOf::structScalar(fieldnames, values);
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
    HandleGenericObject* sourceObject = HandleManager::getInstance()->pointer(_sourceHandle);
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
void
ClassdefListenerObject::onDelete(Evaluator* eval)
{
    detachFromSource();
}
//=============================================================================
ClassdefDynamicPropertyObject::ClassdefDynamicPropertyObject(nelson_handle sourceHandle,
    const std::string& definingClassName, const std::string& propertyName)
    : HandleGenericObject(CLASSDEF_DYNAMIC_PROPERTY_CLASS, this, false)
    , _sourceHandle(sourceHandle)
    , _definingClassName(definingClassName)
    , _propertyName(propertyName)
{
}
//=============================================================================
const std::string&
ClassdefDynamicPropertyObject::propertyName() const
{
    return _propertyName;
}
//=============================================================================
static ClassdefHandleObject*
dynamicPropertySource(nelson_handle sourceHandle)
{
    HandleGenericObject* sourceObject = HandleManager::getInstance()->pointer(sourceHandle);
    return dynamic_cast<ClassdefHandleObject*>(sourceObject);
}
//=============================================================================
bool
ClassdefDynamicPropertyObject::isProperty(const std::wstring& propertyName)
{
    static const wstringVector names = { L"Name", L"DefiningClass", L"Dynamic", L"Dependent",
        L"AbortSet", L"GetMethod", L"GetObservable", L"Hidden", L"NonCopyable", L"SetMethod",
        L"SetObservable", L"Transient", L"GetAccess", L"SetAccess", L"ValidationExpression" };
    return std::find(names.begin(), names.end(), propertyName) != names.end();
}
//=============================================================================
bool
ClassdefDynamicPropertyObject::invokeProperty(
    const std::string& propertyName, ArrayOfVector& results)
{
    results.clear();
    ClassdefDynamicPropertyAttributes attributes;
    ClassdefHandleObject* source = dynamicPropertySource(_sourceHandle);
    if (source != nullptr) {
        attributes = source->dynamicPropertyAttributes(_propertyName);
    }
    if (propertyName == "Name") {
        results << ArrayOf::characterArray(_propertyName);
    } else if (propertyName == "DefiningClass") {
        results << ArrayOf::characterArray(_definingClassName);
    } else if (propertyName == "Dynamic") {
        results << ArrayOf::logicalScalar(true);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_DEPENDENT) {
        results << ArrayOf::logicalScalar(attributes.dependent);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_ABORT_SET) {
        results << ArrayOf::logicalScalar(attributes.abortSet);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_METHOD) {
        results << attributes.getMethod;
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_OBSERVABLE) {
        results << ArrayOf::logicalScalar(attributes.getObservable);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_HIDDEN) {
        results << ArrayOf::logicalScalar(attributes.hidden);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_NON_COPYABLE) {
        results << ArrayOf::logicalScalar(attributes.nonCopyable);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_OBSERVABLE) {
        results << ArrayOf::logicalScalar(attributes.setObservable);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_METHOD) {
        results << attributes.setMethod;
    } else if (propertyName == "Transient") {
        results << ArrayOf::logicalScalar(attributes.transient);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_VALIDATION_EXPRESSION) {
        results << ArrayOf::characterArray(attributes.validationExpression);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_ACCESS) {
        results << ArrayOf::characterArray(attributes.getAccess);
    } else if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_ACCESS) {
        results << ArrayOf::characterArray(attributes.setAccess);
    } else {
        return false;
    }
    return true;
}
//=============================================================================
bool
ClassdefDynamicPropertyObject::setProperty(const std::string& propertyName, const ArrayOf& value)
{
    const bool isWritable = propertyName == CLASSDEF_DYNAMIC_PROPERTY_HIDDEN
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_ABORT_SET
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_DEPENDENT
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_ACCESS
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_METHOD
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_OBSERVABLE
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_NON_COPYABLE
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_ACCESS
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_METHOD
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_OBSERVABLE
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_VALIDATION_EXPRESSION
        || propertyName == "Transient";
    if (!isWritable) {
        return false;
    }
    const bool isMethodField = propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_METHOD
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_METHOD;
    if (isMethodField) {
        if (!value.isEmpty() && !value.isFunctionHandle()) {
            Error("Nelson:interpreter:invalidPropertyDescriptor",
                "Dynamic property descriptor method must be a function handle or empty.");
        }
        ClassdefHandleObject* source = dynamicPropertySource(_sourceHandle);
        if (source == nullptr) {
            return false;
        }
        if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_METHOD) {
            return source->setDynamicPropertyGetMethod(_propertyName, value);
        }
        return source->setDynamicPropertySetMethod(_propertyName, value);
    }
    const bool isAccessField = propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_ACCESS
        || propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_ACCESS;
    if (isAccessField) {
        if (!value.isRowVectorCharacterArray()) {
            Error("Nelson:interpreter:invalidPropertyAccess",
                "Dynamic property descriptor access must be public, private, or protected.");
        }
        std::string access = value.asUtf8String();
        if (access != "public" && access != "private" && access != "protected") {
            Error("Nelson:interpreter:invalidPropertyAccess",
                "Dynamic property descriptor access must be public, private, or protected.");
        }
        ClassdefHandleObject* source = dynamicPropertySource(_sourceHandle);
        if (source == nullptr) {
            return false;
        }
        if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_ACCESS) {
            return source->setDynamicPropertyGetAccess(_propertyName, access);
        }
        return source->setDynamicPropertySetAccess(_propertyName, access);
    }
    if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_VALIDATION_EXPRESSION) {
        std::string validationExpression;
        if (value.isEmpty()) {
            validationExpression.clear();
        } else if (value.isRowVectorCharacterArray()) {
            validationExpression = value.asUtf8String();
        } else {
            Error("Nelson:interpreter:invalidPropertyValidation",
                "Dynamic property validation expression must be a character vector or empty.");
        }
        if (!validationExpression.empty()
            && !classdefIsValidPropertyValidationExpression(validationExpression)) {
            RaiseError("Invalid dynamic property validation expression: " + validationExpression,
                "Nelson:interpreter:invalidPropertyValidation");
        }
        ClassdefHandleObject* source = dynamicPropertySource(_sourceHandle);
        if (source == nullptr) {
            return false;
        }
        return source->setDynamicPropertyValidationExpression(_propertyName, validationExpression);
    }
    if (!value.isLogical() || !value.isScalar()) {
        Error("Nelson:interpreter:invalidPropertyDescriptorValue",
            "Dynamic property descriptor value must be a scalar logical.");
    }
    ClassdefHandleObject* source = dynamicPropertySource(_sourceHandle);
    if (source == nullptr) {
        return false;
    }
    const bool logicalValue = value.asLogicalScalar();
    if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_HIDDEN) {
        return source->setDynamicPropertyHidden(_propertyName, logicalValue);
    }
    if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_ABORT_SET) {
        return source->setDynamicPropertyAbortSet(_propertyName, logicalValue);
    }
    if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_DEPENDENT) {
        return source->setDynamicPropertyDependent(_propertyName, logicalValue);
    }
    if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_GET_OBSERVABLE) {
        return source->setDynamicPropertyGetObservable(_propertyName, logicalValue);
    }
    if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_SET_OBSERVABLE) {
        return source->setDynamicPropertySetObservable(_propertyName, logicalValue);
    }
    if (propertyName == CLASSDEF_DYNAMIC_PROPERTY_NON_COPYABLE) {
        return source->setDynamicPropertyNonCopyable(_propertyName, logicalValue);
    }
    if (propertyName == "Transient") {
        return source->setDynamicPropertyTransient(_propertyName, logicalValue);
    }
    return false;
}
//=============================================================================
wstringVector
ClassdefDynamicPropertyObject::properties()
{
    return { L"Name", L"DefiningClass", L"Dynamic", L"Dependent", L"AbortSet", L"GetMethod",
        L"GetObservable", L"Hidden", L"NonCopyable", L"SetMethod", L"SetObservable", L"Transient",
        L"GetAccess", L"SetAccess", L"ValidationExpression" };
}
//=============================================================================
bool
ClassdefDynamicPropertyObject::removeFromSource()
{
    return removeFromSource(nullptr);
}
//=============================================================================
bool
ClassdefDynamicPropertyObject::removeFromSource(Evaluator* eval)
{
    auto* source = dynamicPropertySource(_sourceHandle);
    if (source == nullptr) {
        return false;
    }
    if (!source->hasDynamicProperty(_propertyName)) {
        return false;
    }
    source->notifyDynamicPropertyEvent(eval, _sourceHandle, _propertyName, "PropertyRemoved");
    return source->removeDynamicProperty(_propertyName);
}
//=============================================================================
void
ClassdefDynamicPropertyObject::onDelete(Evaluator* eval)
{
    removeFromSource(eval);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
