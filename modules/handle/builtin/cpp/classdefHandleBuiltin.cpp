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
#include "HandleManager.hpp"
#include "IsValidHandle.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define CLASSDEF_ACCESSOR_GET "get"
#define CLASSDEF_ACCESSOR_SET "set"
#define CLASSDEF_ERROR_CANNOT_GET_PROPERTY "Cannot get property: "
#define CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY "Cannot modify constant property: "
#define CLASSDEF_ERROR_CANNOT_SET_PROPERTY "Cannot set property: "
#define CLASSDEF_ERROR_EXPECTED_VALID_HANDLE "Expected a valid handle."
#define CLASSDEF_ERROR_HANDLE_CLASS_NOT_FOUND "Handle class not found: "
#define CLASSDEF_ERROR_INVALID_CLASSDEF_HANDLE "Invalid classdef handle."
#define CLASSDEF_ERROR_INVALID_LISTENER_HANDLE "Invalid listener handle."
#define CLASSDEF_ERROR_LISTENER_CALLBACK_FUNCTION_HANDLE                                           \
    "Listener callback must be a function handle."
#define CLASSDEF_ERROR_NO_SUCH_EVENT "No such event: "
#define CLASSDEF_ERROR_NO_SUCH_PROPERTY "No such property: "
#define CLASSDEF_ERROR_NO_SUCH_PROPERTY_EVENT "No such property event: "
#define CLASSDEF_EVENT_POST_GET "PostGet"
#define CLASSDEF_EVENT_POST_SET "PostSet"
#define CLASSDEF_EVENT_PRE_GET "PreGet"
#define CLASSDEF_EVENT_PRE_SET "PreSet"
#define CLASSDEF_PROPERTY_EVENT_DISPLAY_SEPARATOR "."
#define CLASSDEF_PROPERTY_EVENT_KEY_SEPARATOR ":"
//=============================================================================
namespace {
//=============================================================================
ClassdefHandleObject*
getClassdefHandleObject(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    auto* object = dynamic_cast<ClassdefHandleObject*>(value.getContentAsHandleScalar());
    if (object == nullptr) {
        Error(_W(CLASSDEF_ERROR_INVALID_CLASSDEF_HANDLE));
    }
    return object;
}
//=============================================================================
nelson_handle
getHandleScalarId(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    const auto* qp = static_cast<const nelson_handle*>(value.getDataPointer());
    if (qp == nullptr || !HandleManager::getInstance()->isValid(qp[0])) {
        Error(_W(CLASSDEF_ERROR_EXPECTED_VALID_HANDLE));
    }
    return qp[0];
}
//=============================================================================
ClassdefListenerObject*
getClassdefListenerObject(nelson_handle listenerHandle)
{
    auto* object = dynamic_cast<ClassdefListenerObject*>(
        HandleManager::getInstance()->getPointer(listenerHandle));
    if (object == nullptr) {
        Error(_W(CLASSDEF_ERROR_INVALID_LISTENER_HANDLE));
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
ArrayOfVector
callDependentGetter(Evaluator* eval, const ArrayOf& object, const std::string& className,
    const std::string& propertyName)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        Error(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName);
    }
    std::string functionName;
    auto* manager = ClassdefDefinitionManager::getInstance();
    if (!manager->resolvePropertyAccessorFunction(
            className, propertyName, CLASSDEF_ACCESSOR_GET, functionName)) {
        Error(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName);
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        Error(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName);
    }
    ArrayOfVector inputs;
    inputs << object;
    return functionDef->evaluateFunction(eval, inputs, 1);
}
//=============================================================================
void
callDependentSetter(Evaluator* eval, const ArrayOf& object, const std::string& className,
    const std::string& propertyName, const ArrayOf& value)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        Error(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName);
    }
    std::string functionName;
    auto* manager = ClassdefDefinitionManager::getInstance();
    if (!manager->resolvePropertyAccessorFunction(
            className, propertyName, CLASSDEF_ACCESSOR_SET, functionName)) {
        Error(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName);
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        Error(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName);
    }
    ArrayOfVector inputs;
    inputs << object;
    inputs << value;
    int nLhs = functionDef->outputArgCount() == 0 ? 0 : 1;
    functionDef->evaluateFunction(eval, inputs, nLhs);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefHandleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    std::string className = argIn[0].getContentAsCString();
    auto* manager = ClassdefDefinitionManager::getInstance();
    if (!manager->loadClass(className) || !manager->isHandleClass(className)) {
        Error(_(CLASSDEF_ERROR_HANDLE_CLASS_NOT_FOUND) + className);
    }
    retval << ArrayOf::handleConstructor(new ClassdefHandleObject(className));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefHandleGetBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }
    std::string propertyName = argIn[1].getContentAsCString();
    ClassdefHandleObject* object = getClassdefHandleObject(argIn[0]);
    if (!object->hasProperty(propertyName)) {
        Error(_(CLASSDEF_ERROR_NO_SUCH_PROPERTY) + propertyName);
    }
    auto* manager = ClassdefDefinitionManager::getInstance();
    if (!manager->canGetProperty(object->getCategory(), propertyName,
            eval == nullptr ? std::string() : eval->getClassdefAccessContext())) {
        Error(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName);
    }
    nelson_handle sourceHandle = getHandleScalarId(argIn[0]);
    object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_PRE_GET);
    ArrayOfVector result;
    if (manager->hasDependentProperty(object->getCategory(), propertyName)) {
        result = callDependentGetter(eval, argIn[0], object->getCategory(), propertyName);
    } else {
        result << object->getProperty(propertyName);
    }
    object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_POST_GET);
    retval = result;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefHandleSetBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }
    std::string propertyName = argIn[1].getContentAsCString();
    ClassdefHandleObject* object = getClassdefHandleObject(argIn[0]);
    if (!object->hasProperty(propertyName)) {
        Error(_(CLASSDEF_ERROR_NO_SUCH_PROPERTY) + propertyName);
    }
    auto* manager = ClassdefDefinitionManager::getInstance();
    const bool isDependentProperty
        = manager->hasDependentProperty(object->getCategory(), propertyName);
    const bool propertyAlreadyExists
        = isDependentProperty ? true : object->hasStoredProperty(propertyName);
    if (manager->hasConstantProperty(object->getCategory(), propertyName)
        && propertyAlreadyExists) {
        Error(CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY + propertyName);
    }
    if (!manager->canSetProperty(object->getCategory(), propertyName,
            eval == nullptr ? std::string() : eval->getClassdefAccessContext(),
            propertyAlreadyExists)) {
        Error(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName);
    }
    nelson_handle sourceHandle = getHandleScalarId(argIn[0]);
    object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_PRE_SET);
    if (isDependentProperty) {
        callDependentSetter(eval, argIn[0], object->getCategory(), propertyName, argIn[2]);
        object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_POST_SET);
        return retval;
    }
    object->setProperty(propertyName, argIn[2]);
    object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_POST_SET);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefHandleIsValidBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool isValid = false;
    if (argIn[0].isHandle() && argIn[0].isScalar()) {
        const auto* qp = static_cast<const nelson_handle*>(argIn[0].getDataPointer());
        isValid = qp != nullptr && HandleManager::getInstance()->isValid(qp[0]);
    }
    retval << ArrayOf::logicalConstructor(isValid);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefHandleStructBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    retval << getClassdefHandleObject(argIn[0])->toStruct();
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefHandleDeleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    const auto* qp = static_cast<const nelson_handle*>(argIn[0].getDataPointer());
    indexType elementCount = argIn[0].getElementCount();
    for (indexType k = 0; qp != nullptr && k < elementCount; ++k) {
        nelson_handle handle = qp[k];
        auto* object
            = dynamic_cast<ClassdefHandleObject*>(HandleManager::getInstance()->getPointer(handle));
        if (object != nullptr) {
            object->deleteListeners();
            delete object;
        }
        HandleManager::getInstance()->removeHandle(handle);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefAddListenerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }

    nelson_handle sourceHandle = getHandleScalarId(argIn[0]);
    ClassdefHandleObject* source = getClassdefHandleObject(argIn[0]);
    std::string listenerEventName;
    ArrayOf callback;
    if (argIn.size() == 3) {
        callback = argIn[2];
        listenerEventName = argIn[1].getContentAsCString();
        if (!source->hasEvent(listenerEventName)) {
            Error(_(CLASSDEF_ERROR_NO_SUCH_EVENT) + listenerEventName);
        }
    } else {
        if (!argIn[2].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_3_TYPE_STRING_EXPECTED);
        }
        std::string propertyName = argIn[1].getContentAsCString();
        std::string eventName = argIn[2].getContentAsCString();
        if (!ClassdefDefinitionManager::getInstance()->isObservablePropertyEvent(
                source->getCategory(), propertyName, eventName)) {
            Error(_(CLASSDEF_ERROR_NO_SUCH_PROPERTY_EVENT) + propertyName
                + CLASSDEF_PROPERTY_EVENT_DISPLAY_SEPARATOR + eventName);
        }
        listenerEventName = propertyEventKey(propertyName, eventName);
        callback = argIn[3];
    }
    if (!callback.isFunctionHandle()) {
        Error(_W(CLASSDEF_ERROR_LISTENER_CALLBACK_FUNCTION_HANDLE));
    }

    auto* listener = new ClassdefListenerObject(sourceHandle, listenerEventName, callback);
    ArrayOf listenerArray = ArrayOf::handleConstructor(listener);
    nelson_handle listenerHandle = getHandleScalarId(listenerArray);
    source->addListener(listenerHandle);
    retval << listenerArray;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::classdefNotifyBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }
    nelson_handle sourceHandle = getHandleScalarId(argIn[0]);
    ClassdefHandleObject* source = getClassdefHandleObject(argIn[0]);
    std::string eventName = argIn[1].getContentAsCString();
    if (!source->hasEvent(eventName)) {
        Error(_(CLASSDEF_ERROR_NO_SUCH_EVENT) + eventName);
    }
    ArrayOf eventData = argIn.size() == 3 ? argIn[2] : ArrayOf::emptyConstructor();
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
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    const auto* qp = static_cast<const nelson_handle*>(argIn[0].getDataPointer());
    indexType elementCount = argIn[0].getElementCount();
    for (indexType k = 0; qp != nullptr && k < elementCount; ++k) {
        nelson_handle listenerHandle = qp[k];
        ClassdefListenerObject* listener = getClassdefListenerObject(listenerHandle);
        listener->detachFromSource();
        delete listener;
        HandleManager::getInstance()->removeHandle(listenerHandle);
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
