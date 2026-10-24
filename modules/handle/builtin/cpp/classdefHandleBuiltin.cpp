//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "classdefHandleBuiltin.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "FunctionDef.hpp"
#include "HandleManager.hpp"
#include "IsValidHandle.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include <memory>
//=============================================================================
using namespace Nelson;
//=============================================================================
#define CLASSDEF_ACCESSOR_GET "get"
#define CLASSDEF_ACCESSOR_SET "set"
#define CLASSDEF_ATTR_ABORT_SET "AbortSet"
#define CLASSDEF_ATTR_CONSTANT "Constant"
#define CLASSDEF_ATTR_DEPENDENT "Dependent"
#define CLASSDEF_ATTR_GET_OBSERVABLE "GetObservable"
#define CLASSDEF_ATTR_NON_COPYABLE "NonCopyable"
#define CLASSDEF_ATTR_SET_OBSERVABLE "SetObservable"
#define CLASSDEF_DYNAMICPROPS_STR "dynamicprops"
#define CLASSDEF_ERROR_CANNOT_GET_PROPERTY "Cannot get property: "
#define CLASSDEF_ERROR_CANNOT_LISTEN_EVENT "Cannot listen event: "
#define CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY "Cannot modify constant property: "
#define CLASSDEF_ERROR_CANNOT_NOTIFY_EVENT "Cannot notify event: "
#define CLASSDEF_ERROR_CANNOT_SET_PROPERTY "Cannot set property: "
#define CLASSDEF_ERROR_DYNAMIC_PROPERTY_EXISTS "Dynamic property already exists: "
#define CLASSDEF_ERROR_EXPECTED_VALID_HANDLE "Expected a valid handle."
#define CLASSDEF_ERROR_HANDLE_CLASS_NOT_FOUND "Handle class not found: "
#define CLASSDEF_ERROR_INVALID_CLASSDEF_HANDLE "Invalid classdef handle."
#define CLASSDEF_ERROR_INVALID_WEAK_REFERENCE "Invalid weak reference."
#define CLASSDEF_ERROR_INVALID_DYNAMIC_PROPERTY_NAME "Invalid dynamic property name: "
#define CLASSDEF_ERROR_INVALID_LISTENER_HANDLE "Invalid listener handle."
#define CLASSDEF_ERROR_NOT_DYNAMIC_PROPERTY "Not a dynamic property: "
#define CLASSDEF_ERROR_LISTENER_CALLBACK_FUNCTION_HANDLE                                           \
    "Listener callback must be a function handle."
#define CLASSDEF_ERROR_NO_SUCH_EVENT "No such event: "
#define CLASSDEF_ERROR_NO_SUCH_METHOD "No such method: "
#define CLASSDEF_ERROR_NO_SUCH_PROPERTY "No such property: "
#define CLASSDEF_ERROR_NO_SUCH_PROPERTY_EVENT "No such property event: "
#define CLASSDEF_ERROR_CANNOT_SET_DYNAMIC_DESCRIPTOR_FIELD                                         \
    "Cannot set dynamic property descriptor field: "
#define CLASSDEF_EVENT_POST_GET "PostGet"
#define CLASSDEF_EVENT_POST_SET "PostSet"
#define CLASSDEF_EVENT_PRE_GET "PreGet"
#define CLASSDEF_EVENT_PRE_SET "PreSet"
#define CLASSDEF_GENERATED_FUNCTION_PREFIX "__classdef_"
#define CLASSDEF_GENERATED_FUNCTION_SUFFIX "__"
#define CLASSDEF_PROPERTY_EVENT_DISPLAY_SEPARATOR "."
#define CLASSDEF_PROPERTY_EVENT_KEY_SEPARATOR ":"
//=============================================================================
namespace {
//=============================================================================
#define CLASSDEF_HANDLE_PLACEHOLDER_CLASS "nelson.lang.HandlePlaceholder"
//=============================================================================
ClassdefHandleObject*
getClassdefHandleObject(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    auto* object = dynamic_cast<ClassdefHandleObject*>(value.asHandleScalar());
    if (object == nullptr) {
        RaiseError(_W(CLASSDEF_ERROR_INVALID_CLASSDEF_HANDLE), L"Nelson:handle:invalidHandle");
    }
    return object;
}
//=============================================================================
nelson_handle
getHandleScalarId(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    const auto* qp = static_cast<const HandleRef*>(value.dataPointer());
    if (qp == nullptr || !qp[0] || !qp[0]->isValid()) {
        RaiseError(_W(CLASSDEF_ERROR_EXPECTED_VALID_HANDLE), L"Nelson:handle:invalidHandle");
    }
    return qp[0]->id();
}
//=============================================================================
ClassdefDynamicPropertyObject*
getClassdefDynamicPropertyObject(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    auto* object = dynamic_cast<ClassdefDynamicPropertyObject*>(value.asHandleScalar());
    if (object == nullptr) {
        Error(L"Nelson:handle:invalidDynamicPropertyHandle", L"Invalid dynamic property handle.");
    }
    return object;
}
//=============================================================================
std::string
propertyEventKey(const std::string& propertyName, const std::string& eventName)
{
    return propertyName + CLASSDEF_PROPERTY_EVENT_KEY_SEPARATOR + eventName;
}
//=============================================================================
bool
isIdentifierPart(char ch)
{
    return std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_';
}
//=============================================================================
bool
isValidIdentifierName(const std::string& name)
{
    if (name.empty() || std::isdigit(static_cast<unsigned char>(name.front())) != 0
        || !isIdentifierPart(name.front())) {
        return false;
    }
    for (char ch : name) {
        if (!isIdentifierPart(ch)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefAddDynamicPropertyBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    ClassdefHandleObject* object = getClassdefHandleObject(argIn[0]);
    if (!classdefIsA(object->category(), CLASSDEF_DYNAMICPROPS_STR)) {
        Error(L"Nelson:handle:notDynamicProps", L"Object must inherit from dynamicprops.");
    }
    const std::string propertyName = argIn[1].asUtf8String();
    if (!isValidIdentifierName(propertyName)) {
        RaiseError(_(CLASSDEF_ERROR_INVALID_DYNAMIC_PROPERTY_NAME) + propertyName,
            "Nelson:handle:invalidDynamicPropertyName");
    }
    if (object->hasProperty(propertyName)) {
        RaiseError(_(CLASSDEF_ERROR_DYNAMIC_PROPERTY_EXISTS) + propertyName,
            "Nelson:handle:dynamicPropertyExists");
    }
    nelson_handle sourceHandle = getHandleScalarId(argIn[0]);
    object->addDynamicProperty(propertyName);
    object->notifyDynamicPropertyEvent(eval, sourceHandle, propertyName, "PropertyAdded");
    retval << ArrayOf::handle(
        new ClassdefDynamicPropertyObject(sourceHandle, object->category(), propertyName));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefDynamicPropertyDeleteBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    const auto* qp = static_cast<const HandleRef*>(argIn[0].dataPointer());
    indexType elementCount = argIn[0].elementCount();
    for (indexType k = 0; qp != nullptr && k < elementCount; ++k) {
        HandleRef ref = qp[k];
        auto* property
            = ref ? dynamic_cast<ClassdefDynamicPropertyObject*>(ref->object()) : nullptr;
        if (property != nullptr) {
            ref->invalidate(eval);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefDynamicPropertyGetBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    ClassdefDynamicPropertyObject* property = getClassdefDynamicPropertyObject(argIn[0]);
    if (!property->invokeProperty(argIn[1].asUtf8String(), retval)) {
        RaiseError(_(CLASSDEF_ERROR_NO_SUCH_PROPERTY) + argIn[1].asUtf8String(),
            "Nelson:handle:noSuchProperty");
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefDynamicPropertySetBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    ClassdefDynamicPropertyObject* property = getClassdefDynamicPropertyObject(argIn[0]);
    const std::string propertyName = argIn[1].asUtf8String();
    if (!property->setProperty(propertyName, argIn[2])) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_DYNAMIC_DESCRIPTOR_FIELD) + propertyName,
            "Nelson:handle:cannotSetDescriptorField");
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefDynamicPropertyIsValidBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << IsValidHandle(argIn[0]);
    return retval;
}
//=============================================================================
static ArrayOfVector
createListener(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool bindToSource)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }

    if (argIn[0].isGraphicsObject()) {
        // Graphics objects have no property-change event delivery yet. The listener
        // is inert (it never fires) but is returned so addlistener(fig, 'Visible',
        // 'PostSet', cb) still works.
        ArrayOf graphicsCallback;
        std::string graphicsEventName;
        if (argIn.size() == 3) {
            graphicsCallback = argIn[2];
            graphicsEventName = argIn[1].asUtf8String();
        } else {
            if (!argIn[2].isRowVectorCharacterArray()) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (3),
                    L"string");
            }
            graphicsEventName = propertyEventKey(argIn[1].asUtf8String(), argIn[2].asUtf8String());
            graphicsCallback = argIn[3];
        }
        if (!graphicsCallback.isFunctionHandle()) {
            RaiseError(_W(CLASSDEF_ERROR_LISTENER_CALLBACK_FUNCTION_HANDLE),
                L"Nelson:handle:invalidCallback");
        }
        auto* graphicsListener = new ClassdefListenerObject(0, graphicsEventName, graphicsCallback);
        retval << ArrayOf::handle(graphicsListener);
        return retval;
    }

    nelson_handle sourceHandle = getHandleScalarId(argIn[0]);
    ClassdefHandleObject* source = getClassdefHandleObject(argIn[0]);
    std::string listenerEventName;
    ArrayOf callback;
    if (argIn.size() == 3) {
        callback = argIn[2];
        listenerEventName = argIn[1].asUtf8String();
        if (!source->hasEvent(listenerEventName)) {
            RaiseError(
                _(CLASSDEF_ERROR_NO_SUCH_EVENT) + listenerEventName, "Nelson:handle:noSuchEvent");
        }
        if (!classdefCanListenEvent(source->category(), listenerEventName,
                eval == nullptr ? std::string() : eval->getClassdefAccessContext())) {
            RaiseError(_(CLASSDEF_ERROR_CANNOT_LISTEN_EVENT) + listenerEventName,
                "Nelson:handle:cannotListenEvent");
        }
    } else {
        if (!argIn[2].isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (3),
                L"string");
        }
        std::string propertyName = argIn[1].asUtf8String();
        std::string eventName = argIn[2].asUtf8String();
        if (!source->isDynamicPropertyObservableEvent(propertyName, eventName)
            && !classdefIsObservablePropertyEvent(source->category(), propertyName, eventName)) {
            RaiseError(_(CLASSDEF_ERROR_NO_SUCH_PROPERTY_EVENT) + propertyName
                    + CLASSDEF_PROPERTY_EVENT_DISPLAY_SEPARATOR + eventName,
                "Nelson:handle:noSuchPropertyEvent");
        }
        listenerEventName = propertyEventKey(propertyName, eventName);
        callback = argIn[3];
    }
    if (!callback.isFunctionHandle()) {
        RaiseError(
            _W(CLASSDEF_ERROR_LISTENER_CALLBACK_FUNCTION_HANDLE), L"Nelson:handle:invalidCallback");
    }

    auto* listener = new ClassdefListenerObject(sourceHandle, listenerEventName, callback);
    ArrayOf listenerArray = ArrayOf::handle(listener);
    nelson_handle listenerHandle = getHandleScalarId(listenerArray);
    source->addListener(listenerHandle, bindToSource);
    retval << listenerArray;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefAddListenerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // addlistener: the source object keeps the listener alive.
    return createListener(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefListenerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // listener: lifetime follows the returned listener object only.
    return createListener(eval, nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefNotifyBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    nelson_handle sourceHandle = getHandleScalarId(argIn[0]);
    ClassdefHandleObject* source = getClassdefHandleObject(argIn[0]);
    std::string eventName = argIn[1].asUtf8String();
    if (!source->hasEvent(eventName)) {
        RaiseError(_(CLASSDEF_ERROR_NO_SUCH_EVENT) + eventName, "Nelson:handle:noSuchEvent");
    }
    if (!classdefCanNotifyEvent(source->category(), eventName,
            eval == nullptr ? std::string() : eval->getClassdefAccessContext())) {
        RaiseError(
            _(CLASSDEF_ERROR_CANNOT_NOTIFY_EVENT) + eventName, "Nelson:handle:cannotNotifyEvent");
    }
    ArrayOf eventData = argIn.size() == 3 ? argIn[2] : ArrayOf::empty();
    if (!eventData.isEmpty()
        && (!eventData.isClassType() || !classdefIsA(eventData.classType(), "event.EventData"))) {
        Error(
            "Nelson:handle:invalidNotifyArgument", "Invalid input argument for function 'notify'.");
    }
    source->notifyEvent(eval, sourceHandle, eventName, eventData);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefListenerDeleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    const auto* qp = static_cast<const HandleRef*>(argIn[0].dataPointer());
    indexType elementCount = argIn[0].elementCount();
    for (indexType k = 0; qp != nullptr && k < elementCount; ++k) {
        HandleRef ref = qp[k];
        auto* listener = ref ? dynamic_cast<ClassdefListenerObject*>(ref->object()) : nullptr;
        if (listener != nullptr) {
            ref->invalidate(nullptr);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefListenerIsValidBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << IsValidHandle(argIn[0]);
    return retval;
}
//=============================================================================
