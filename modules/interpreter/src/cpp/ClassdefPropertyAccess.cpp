//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cctype>
#include <string>
#include "ClassdefPropertyAccess.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include "HandleManager.hpp"
#include "NelsonException.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
#define CLASSDEF_ACCESSOR_GET "get"
#define CLASSDEF_ACCESSOR_SET "set"
#define CLASSDEF_ATTR_ABORT_SET "AbortSet"
#define CLASSDEF_DYNAMICPROPS_STR "dynamicprops"
#define CLASSDEF_ERROR_CANNOT_GET_PROPERTY "Cannot get property: "
#define CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY "Cannot modify constant property: "
#define CLASSDEF_ERROR_CANNOT_SET_PROPERTY "Cannot set property: "
#define CLASSDEF_ERROR_DYNAMIC_PROPERTY_EXISTS "Dynamic property already exists: "
#define CLASSDEF_ERROR_EXPECTED_VALID_HANDLE "Expected a valid handle."
#define CLASSDEF_ERROR_INVALID_CLASSDEF_HANDLE "Invalid classdef handle."
#define CLASSDEF_ERROR_INVALID_DYNAMIC_PROPERTY_NAME "Invalid dynamic property name: "
#define CLASSDEF_ERROR_NOT_DYNAMIC_PROPERTY "Not a dynamic property: "
#define CLASSDEF_ERROR_NO_SUCH_PROPERTY "No such property: %s"
#define CLASSDEF_EVENT_POST_GET "PostGet"
#define CLASSDEF_EVENT_POST_SET "PostSet"
#define CLASSDEF_EVENT_PRE_GET "PreGet"
#define CLASSDEF_EVENT_PRE_SET "PreSet"
//=============================================================================
namespace Nelson {
//=============================================================================
static ClassdefHandleObject*
classdefHandleObjectArg(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    auto* object = dynamic_cast<ClassdefHandleObject*>(value.asHandleScalar());
    if (object == nullptr) {
        RaiseError(_W(CLASSDEF_ERROR_INVALID_CLASSDEF_HANDLE),
            L"Nelson:interpreter:invalidClassdefHandle");
    }
    return object;
}
//=============================================================================
static nelson_handle
handleScalarId(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    const auto* qp = static_cast<const HandleRef*>(value.dataPointer());
    if (qp == nullptr || !qp[0] || !qp[0]->isValid()) {
        RaiseError(
            _W(CLASSDEF_ERROR_EXPECTED_VALID_HANDLE), L"Nelson:interpreter:invalidClassdefHandle");
    }
    return qp[0]->id();
}
//=============================================================================
static bool
isIdentifierPart(char ch)
{
    return std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_';
}
//=============================================================================
static bool
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
static std::string
trimString(const std::string& value)
{
    size_t first = 0;
    while (first < value.size() && std::isspace(static_cast<unsigned char>(value[first])) != 0) {
        ++first;
    }
    size_t last = value.size();
    while (last > first && std::isspace(static_cast<unsigned char>(value[last - 1])) != 0) {
        --last;
    }
    return value.substr(first, last - first);
}
//=============================================================================
static std::string
lowerString(std::string value)
{
    for (char& ch : value) {
        ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
    }
    return value;
}
//=============================================================================
static bool
classdefAttributeListContains(const stringVector& attributes, const std::string& name)
{
    const std::string normalizedName = lowerString(name);
    for (std::string attribute : attributes) {
        attribute = trimString(attribute);
        size_t equals = attribute.find('=');
        if (equals != std::string::npos) {
            attribute = trimString(attribute.substr(0, equals));
        }
        if (lowerString(attribute) == normalizedName) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
classdefCanAccessDynamicProperty(const std::string& definingClassName, const std::string& access,
    const std::string& callerClassName)
{
    const std::string normalizedAccess = lowerString(trimString(access));
    if (normalizedAccess.empty() || normalizedAccess == "public") {
        return true;
    }
    if (callerClassName.empty()) {
        return false;
    }
    if (normalizedAccess == "private") {
        return lowerString(callerClassName) == lowerString(definingClassName);
    }
    if (normalizedAccess == "protected") {
        return lowerString(callerClassName) == lowerString(definingClassName)
            || classdefIsA(callerClassName, definingClassName);
    }
    return false;
}
//=============================================================================
static bool
classdefValuesAreEqual(Evaluator* eval, const ArrayOf& lhs, const ArrayOf& rhs)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        return false;
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction("isequal", functionDef) || functionDef == nullptr) {
        return false;
    }
    ArrayOfVector inputs;
    inputs << lhs;
    inputs << rhs;
    ArrayOfVector result = functionDef->evaluateFunction(eval, inputs, 1);
    return !result.empty() && result[0].isLogical() && result[0].asLogicalScalar();
}
//=============================================================================
static ArrayOfVector
callDynamicPropertyGetMethod(Evaluator* eval, const ArrayOf& object,
    const ArrayOf& functionHandleValue, const std::string& propertyName)
{
    if (eval == nullptr || !functionHandleValue.isFunctionHandle()) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    function_handle functionHandle = functionHandleValue.asFunctionHandle();
    if (functionHandle.anonymousHandle == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    FunctionDef* functionDef = reinterpret_cast<FunctionDef*>(functionHandle.anonymousHandle);
    ArrayOfVector inputs;
    inputs << object;
    return functionDef->evaluateFunction(eval, inputs, 1);
}
//=============================================================================
static void
callDynamicPropertySetMethod(Evaluator* eval, const ArrayOf& object,
    const ArrayOf& functionHandleValue, const std::string& propertyName, const ArrayOf& value)
{
    if (eval == nullptr || !functionHandleValue.isFunctionHandle()) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    function_handle functionHandle = functionHandleValue.asFunctionHandle();
    if (functionHandle.anonymousHandle == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    FunctionDef* functionDef = reinterpret_cast<FunctionDef*>(functionHandle.anonymousHandle);
    ArrayOfVector inputs;
    inputs << object;
    inputs << value;
    functionDef->evaluateFunction(eval, inputs, 0);
}
//=============================================================================
static ArrayOfVector
callDependentGetter(Evaluator* eval, const ArrayOf& object, const std::string& className,
    const std::string& propertyName)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    std::string functionName;
    if (!classdefResolvePropertyAccessorFunction(
            className, propertyName, CLASSDEF_ACCESSOR_GET, functionName)) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    ArrayOfVector inputs;
    inputs << object;
    return functionDef->evaluateFunction(eval, inputs, 1);
}
//=============================================================================
static void
callDependentSetter(Evaluator* eval, const ArrayOf& object, const std::string& className,
    const std::string& propertyName, const ArrayOf& value)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    std::string functionName;
    if (!classdefResolvePropertyAccessorFunction(
            className, propertyName, CLASSDEF_ACCESSOR_SET, functionName)) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    ArrayOfVector inputs;
    inputs << object;
    inputs << value;
    int nLhs = functionDef->outputArgCount() == 0 ? 0 : 1;
    functionDef->evaluateFunction(eval, inputs, nLhs);
}
//=============================================================================
static ArrayOfVector
callConstantGetter(Evaluator* eval, const std::string& className, const std::string& propertyName,
    const std::string& accessContext)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    std::string functionName;
    if (!classdefResolveConstantPropertyFunction(
            className, propertyName, functionName, accessContext)) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    return functionDef->evaluateFunction(eval, {}, 1);
}
//=============================================================================
ArrayOfVector
evaluateClassdefHandleGet(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || !argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    std::string propertyName = argIn[1].asUtf8String();
    ClassdefHandleObject* object = classdefHandleObjectArg(argIn[0]);
    if (!object->hasProperty(propertyName)) {
        RaiseError(formatI18nMessage(_(CLASSDEF_ERROR_NO_SUCH_PROPERTY), propertyName),
            "Nelson:interpreter:noSuchProperty");
    }
    const bool isDynamicProperty = object->hasDynamicProperty(propertyName);
    const std::string accessContext
        = eval == nullptr ? std::string() : eval->getClassdefAccessContext();
    if (isDynamicProperty) {
        const ClassdefDynamicPropertyAttributes dynamicAttributes
            = object->dynamicPropertyAttributes(propertyName);
        if (!classdefCanAccessDynamicProperty(
                object->category(), dynamicAttributes.getAccess, accessContext)) {
            RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
                "Nelson:interpreter:cannotGetProperty");
        }
    } else if (!classdefCanGetProperty(object->category(), propertyName, accessContext)) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    }
    nelson_handle sourceHandle = handleScalarId(argIn[0]);
    object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_PRE_GET);
    ArrayOfVector result;
    const ClassdefDynamicPropertyAttributes dynamicAttributes = isDynamicProperty
        ? object->dynamicPropertyAttributes(propertyName)
        : ClassdefDynamicPropertyAttributes();
    if (isDynamicProperty && dynamicAttributes.getMethod.isFunctionHandle()) {
        result = callDynamicPropertyGetMethod(
            eval, argIn[0], dynamicAttributes.getMethod, propertyName);
    } else if (isDynamicProperty && dynamicAttributes.dependent) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotGetProperty");
    } else if (!isDynamicProperty
        && classdefHasConstantProperty(object->category(), propertyName)) {
        result = callConstantGetter(eval, object->category(), propertyName, accessContext);
    } else if (classdefHasDependentProperty(object->category(), propertyName)) {
        result = callDependentGetter(eval, argIn[0], object->category(), propertyName);
    } else {
        result << object->getProperty(propertyName);
    }
    object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_POST_GET);
    retval = result;
    return retval;
}
//=============================================================================
ArrayOfVector
evaluateClassdefHandleSet(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 3 || !argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    std::string propertyName = argIn[1].asUtf8String();
    ClassdefHandleObject* object = classdefHandleObjectArg(argIn[0]);
    if (!object->hasProperty(propertyName)) {
        RaiseError(formatI18nMessage(_(CLASSDEF_ERROR_NO_SUCH_PROPERTY), propertyName),
            "Nelson:interpreter:noSuchProperty");
    }
    const bool isDynamicProperty = object->hasDynamicProperty(propertyName);
    const std::string accessContext
        = eval == nullptr ? std::string() : eval->getClassdefAccessContext();
    const bool isDependentProperty
        = !isDynamicProperty && classdefHasDependentProperty(object->category(), propertyName);
    ClassdefPropertyDefinition property;
    const bool hasPropertyDefinition
        = classdefPropertyDefinition(object->category(), propertyName, property);
    const ClassdefDynamicPropertyAttributes dynamicAttributes = isDynamicProperty
        ? object->dynamicPropertyAttributes(propertyName)
        : ClassdefDynamicPropertyAttributes();
    const bool isDynamicDependentProperty = isDynamicProperty && dynamicAttributes.dependent;
    const bool propertyAlreadyExists = (isDependentProperty || isDynamicDependentProperty)
        ? true
        : object->hasStoredProperty(propertyName);
    if (!isDynamicProperty && classdefHasConstantProperty(object->category(), propertyName)) {
        RaiseError(CLASSDEF_ERROR_CANNOT_MODIFY_CONSTANT_PROPERTY + propertyName,
            "Nelson:interpreter:cannotModifyConstantProperty");
    }
    if (isDynamicProperty) {
        if (!classdefCanAccessDynamicProperty(
                object->category(), dynamicAttributes.setAccess, accessContext)) {
            RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
                "Nelson:interpreter:cannotSetProperty");
        }
    } else if (!classdefCanSetProperty(
                   object->category(), propertyName, accessContext, propertyAlreadyExists, eval)) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    const bool abortSet = (isDynamicProperty && dynamicAttributes.abortSet)
        || (hasPropertyDefinition
            && classdefAttributeListContains(property.attributes, CLASSDEF_ATTR_ABORT_SET));
    if (abortSet && propertyAlreadyExists) {
        ArrayOfVector oldValues;
        if (isDynamicProperty && dynamicAttributes.getMethod.isFunctionHandle()) {
            oldValues = callDynamicPropertyGetMethod(
                eval, argIn[0], dynamicAttributes.getMethod, propertyName);
        } else if (isDynamicDependentProperty) {
            oldValues = {};
        } else if (isDependentProperty) {
            oldValues = callDependentGetter(eval, argIn[0], object->category(), propertyName);
        } else {
            oldValues << object->getProperty(propertyName);
        }
        if (!oldValues.empty() && classdefValuesAreEqual(eval, oldValues[0], argIn[2])) {
            return retval;
        }
    }
    ArrayOf assignedValue = argIn[2];
    if (hasPropertyDefinition) {
        assignedValue = classdefValidatePropertyValue(eval, property, assignedValue);
    } else if (isDynamicProperty && !dynamicAttributes.validationExpression.empty()) {
        ClassdefPropertyDefinition dynamicProperty;
        dynamicProperty.name = propertyName;
        dynamicProperty.definingClassName = object->category();
        dynamicProperty.validationExpression = dynamicAttributes.validationExpression;
        assignedValue = classdefValidatePropertyValue(eval, dynamicProperty, assignedValue);
    }
    nelson_handle sourceHandle = handleScalarId(argIn[0]);
    object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_PRE_SET);
    if (isDependentProperty) {
        callDependentSetter(eval, argIn[0], object->category(), propertyName, assignedValue);
        object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_POST_SET);
        return retval;
    }
    if (isDynamicProperty && dynamicAttributes.setMethod.isFunctionHandle()) {
        callDynamicPropertySetMethod(
            eval, argIn[0], dynamicAttributes.setMethod, propertyName, assignedValue);
        object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_POST_SET);
        return retval;
    }
    if (isDynamicDependentProperty) {
        RaiseError(_(CLASSDEF_ERROR_CANNOT_SET_PROPERTY) + propertyName,
            "Nelson:interpreter:cannotSetProperty");
    }
    object->setProperty(propertyName, assignedValue);
    object->notifyPropertyEvent(eval, sourceHandle, propertyName, CLASSDEF_EVENT_POST_SET);
    return retval;
}
//=============================================================================
ArrayOfVector
evaluateClassdefAddDynamicProperty(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || !argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    ClassdefHandleObject* object = classdefHandleObjectArg(argIn[0]);
    if (!classdefIsA(object->category(), CLASSDEF_DYNAMICPROPS_STR)) {
        Error(L"Nelson:interpreter:notDynamicProps", L"Object must inherit from dynamicprops.");
    }
    const std::string propertyName = argIn[1].asUtf8String();
    if (!isValidIdentifierName(propertyName)) {
        RaiseError(_(CLASSDEF_ERROR_INVALID_DYNAMIC_PROPERTY_NAME) + propertyName,
            "Nelson:interpreter:invalidDynamicPropertyName");
    }
    if (object->hasProperty(propertyName)) {
        RaiseError(_(CLASSDEF_ERROR_DYNAMIC_PROPERTY_EXISTS) + propertyName,
            "Nelson:interpreter:dynamicPropertyExists");
    }
    nelson_handle sourceHandle = handleScalarId(argIn[0]);
    object->addDynamicProperty(propertyName);
    object->notifyDynamicPropertyEvent(eval, sourceHandle, propertyName, "PropertyAdded");
    retval << ArrayOf::handle(
        new ClassdefDynamicPropertyObject(sourceHandle, object->category(), propertyName));
    return retval;
}
//=============================================================================
ArrayOfVector
evaluateClassdefRemoveDynamicProperty(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || !argIn[1].isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    ClassdefHandleObject* object = classdefHandleObjectArg(argIn[0]);
    if (!classdefIsA(object->category(), CLASSDEF_DYNAMICPROPS_STR)) {
        Error(L"Nelson:interpreter:notDynamicProps", L"Object must inherit from dynamicprops.");
    }
    const std::string propertyName = argIn[1].asUtf8String();
    nelson_handle sourceHandle = handleScalarId(argIn[0]);
    if (!object->hasDynamicProperty(propertyName)) {
        RaiseError(_(CLASSDEF_ERROR_NOT_DYNAMIC_PROPERTY) + propertyName,
            "Nelson:interpreter:notDynamicProperty");
    }
    object->notifyDynamicPropertyEvent(eval, sourceHandle, propertyName, "PropertyRemoved");
    if (!object->removeDynamicProperty(propertyName)) {
        RaiseError(_(CLASSDEF_ERROR_NOT_DYNAMIC_PROPERTY) + propertyName,
            "Nelson:interpreter:notDynamicProperty");
    }
    retval << argIn[0];
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
