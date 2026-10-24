//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "metaclassBuiltin.hpp"
#include <algorithm>
#include <cctype>
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefMixinNames.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define METACLASS_ACCESS_PUBLIC "public"
#define METACLASS_ATTR_ABSTRACT "Abstract"
#define METACLASS_ATTR_ABORT_SET "AbortSet"
#define METACLASS_ATTR_ACCESS "Access"
#define METACLASS_ATTR_CONSTANT "Constant"
#define METACLASS_ATTR_CONSTRUCT_ON_LOAD "ConstructOnLoad"
#define METACLASS_ATTR_DEPENDENT "Dependent"
#define METACLASS_ATTR_GET_ACCESS "GetAccess"
#define METACLASS_ATTR_GET_OBSERVABLE "GetObservable"
#define METACLASS_ATTR_HIDDEN "Hidden"
#define METACLASS_ATTR_INFERIOR_CLASSES "InferiorClasses"
#define METACLASS_ATTR_LISTEN_ACCESS "ListenAccess"
#define METACLASS_ATTR_NONTUNABLE "Nontunable"
#define METACLASS_ATTR_NON_COPYABLE "NonCopyable"
#define METACLASS_ATTR_NOTIFY_ACCESS "NotifyAccess"
#define METACLASS_ATTR_SEALED "Sealed"
#define METACLASS_ATTR_SET_ACCESS "SetAccess"
#define METACLASS_ATTR_SET_OBSERVABLE "SetObservable"
#define METACLASS_ATTR_STATIC "Static"
#define METACLASS_ATTR_TRANSIENT "Transient"
#define METACLASS_FIELD_ABORT_SET "AbortSet"
#define METACLASS_FIELD_ABSTRACT "Abstract"
#define METACLASS_FIELD_ACCESS "Access"
#define METACLASS_FIELD_ATTRIBUTES "Attributes"
#define METACLASS_FIELD_CLASS_DETAILS "ClassDetails"
#define METACLASS_FIELD_CONSTANT "Constant"
#define METACLASS_FIELD_CONSTRUCT_ON_LOAD "ConstructOnLoad"
#define METACLASS_FIELD_CONSTRUCTOR_ARGUMENTS "ConstructorArguments"
#define METACLASS_FIELD_DEFAULT_VALUE_EXPRESSION "DefaultValueExpression"
#define METACLASS_FIELD_DEFINING_CLASS "DefiningClass"
#define METACLASS_FIELD_DEPENDENT "Dependent"
#define METACLASS_FIELD_DYNAMIC "Dynamic"
#define METACLASS_FIELD_ENUMERATION_DETAILS "EnumerationDetails"
#define METACLASS_FIELD_ENUMERATION_MEMBER_LIST "EnumerationMemberList"
#define METACLASS_FIELD_EVENT_DETAILS "EventDetails"
#define METACLASS_FIELD_EVENT_LIST "EventList"
#define METACLASS_FIELD_GET_ACCESS "GetAccess"
#define METACLASS_FIELD_GET_METHOD "GetMethod"
#define METACLASS_FIELD_GET_OBSERVABLE "GetObservable"
#define METACLASS_FIELD_HANDLE "Handle"
#define METACLASS_FIELD_HIDDEN "Hidden"
#define METACLASS_FIELD_INFERIOR_CLASSES "InferiorClasses"
#define METACLASS_FIELD_LISTEN_ACCESS "ListenAccess"
#define METACLASS_FIELD_METHOD_DETAILS "MethodDetails"
#define METACLASS_FIELD_METHOD_LIST "MethodList"
#define METACLASS_FIELD_NAME "Name"
#define METACLASS_FIELD_NONTUNABLE "Nontunable"
#define METACLASS_FIELD_NON_COPYABLE "NonCopyable"
#define METACLASS_FIELD_NOTIFY_ACCESS "NotifyAccess"
#define METACLASS_FIELD_PROPERTY_DETAILS "PropertyDetails"
#define METACLASS_FIELD_PROPERTY_LIST "PropertyList"
#define METACLASS_FIELD_SEALED "Sealed"
#define METACLASS_FIELD_SET_ACCESS "SetAccess"
#define METACLASS_FIELD_SET_METHOD "SetMethod"
#define METACLASS_FIELD_SET_OBSERVABLE "SetObservable"
#define METACLASS_FIELD_STATIC "Static"
#define METACLASS_FIELD_SUPERCLASS_LIST "SuperclassList"
#define METACLASS_FIELD_TRANSIENT "Transient"
#define METACLASS_FIELD_VALIDATION_EXPRESSION "ValidationExpression"
//=============================================================================
namespace {
//=============================================================================
std::string
trim(const std::string& input)
{
    size_t first = 0;
    while (first < input.size() && std::isspace(static_cast<unsigned char>(input[first])) != 0) {
        ++first;
    }
    size_t last = input.size();
    while (last > first && std::isspace(static_cast<unsigned char>(input[last - 1])) != 0) {
        --last;
    }
    return input.substr(first, last - first);
}
//=============================================================================
std::string
tolowerString(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return value;
}
//=============================================================================
bool
attributeValueMeansFalse(std::string value)
{
    value = trim(value);
    if (value.size() >= 2
        && ((value.front() == '\'' && value.back() == '\'')
            || (value.front() == '"' && value.back() == '"'))) {
        value = value.substr(1, value.size() - 2);
    }
    value = tolowerString(value);
    return value == "false" || value == "0";
}
//=============================================================================
bool
attributeListContains(const stringVector& attributes, const std::string& name)
{
    const std::string normalizedName = tolowerString(name);
    for (std::string attribute : attributes) {
        attribute = trim(attribute);
        size_t equals = attribute.find('=');
        if (equals != std::string::npos) {
            const std::string value = trim(attribute.substr(equals + 1));
            attribute = trim(attribute.substr(0, equals));
            if (tolowerString(attribute) == normalizedName) {
                return !attributeValueMeansFalse(value);
            }
        }
        if (tolowerString(attribute) == normalizedName) {
            return true;
        }
    }
    return false;
}
//=============================================================================
std::string
attributeValue(const stringVector& attributes, const std::string& name)
{
    const std::string normalizedName = tolowerString(name);
    for (std::string attribute : attributes) {
        attribute = trim(attribute);
        size_t equals = attribute.find('=');
        if (equals == std::string::npos) {
            continue;
        }
        std::string attributeName = trim(attribute.substr(0, equals));
        if (tolowerString(attributeName) == normalizedName) {
            return trim(attribute.substr(equals + 1));
        }
    }
    return {};
}
//=============================================================================
std::string
unquotedAccessValue(std::string value)
{
    value = trim(value);
    if (value.size() >= 2
        && ((value.front() == '\'' && value.back() == '\'')
            || (value.front() == '"' && value.back() == '"'))) {
        value = value.substr(1, value.size() - 2);
    }
    return trim(value);
}
//=============================================================================
std::string
accessAttributeValue(const stringVector& attributes)
{
    std::string access = unquotedAccessValue(attributeValue(attributes, METACLASS_ATTR_ACCESS));
    return access.empty() ? METACLASS_ACCESS_PUBLIC : access;
}
//=============================================================================
std::string
propertyAccessAttributeValue(const stringVector& attributes, const std::string& attributeName)
{
    std::string access = attributeValue(attributes, attributeName);
    if (trim(access).empty()) {
        access = attributeValue(attributes, METACLASS_ATTR_ACCESS);
    }
    access = unquotedAccessValue(access);
    return access.empty() ? METACLASS_ACCESS_PUBLIC : access;
}
//=============================================================================
ArrayOf
cellArrayOfLogicalColumnVector(const std::vector<bool>& values)
{
    ArrayOfMatrix matrix;
    matrix.reserve(values.size());
    for (bool value : values) {
        ArrayOfVector row;
        row << ArrayOf::logicalScalar(value);
        matrix.push_back(row);
    }
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
ArrayOf
cellArrayOfCellstrColumnVector(const std::vector<stringVector>& values)
{
    ArrayOfMatrix matrix;
    matrix.reserve(values.size());
    for (const auto& value : values) {
        ArrayOfVector row;
        row << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(value);
        matrix.push_back(row);
    }
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
ArrayOf
cellArrayOfArrayColumnVector(const ArrayOfVector& values)
{
    ArrayOfMatrix matrix;
    matrix.reserve(values.size());
    for (const auto& value : values) {
        ArrayOfVector row;
        row << value;
        matrix.push_back(row);
    }
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
ArrayOf
emptyStructArray(const stringVector& fieldnames)
{
    Dimensions dims(0, 1);
    return ArrayOf::emptyStruct(fieldnames, dims);
}
//=============================================================================
ArrayOf
classDetailsStruct(
    const std::string& className, const stringVector& attributes, bool isHandle, bool isAbstract)
{
    stringVector fieldnames
        = { METACLASS_FIELD_NAME, METACLASS_FIELD_ATTRIBUTES, METACLASS_FIELD_ABSTRACT,
              METACLASS_FIELD_SEALED, METACLASS_FIELD_HANDLE, METACLASS_FIELD_HIDDEN,
              METACLASS_FIELD_CONSTRUCT_ON_LOAD, METACLASS_FIELD_INFERIOR_CLASSES };
    ArrayOfVector values;
    values << ArrayOf::characterArray(className);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(attributes);
    values << ArrayOf::logicalScalar(isAbstract);
    values << ArrayOf::logicalScalar(attributeListContains(attributes, METACLASS_ATTR_SEALED));
    values << ArrayOf::logicalScalar(isHandle);
    values << ArrayOf::logicalScalar(attributeListContains(attributes, METACLASS_ATTR_HIDDEN));
    values << ArrayOf::logicalScalar(
        attributeListContains(attributes, METACLASS_ATTR_CONSTRUCT_ON_LOAD));
    values << ArrayOf::characterArray(attributeValue(attributes, METACLASS_ATTR_INFERIOR_CLASSES));
    return ArrayOf::structScalar(fieldnames, values);
}
//=============================================================================
ArrayOf
propertyDetailsStruct(const std::vector<ClassdefPropertyDefinition>& properties,
    const stringVector& dynamicProperties = {},
    const std::vector<ClassdefDynamicPropertyAttributes>& dynamicAttributes = {},
    const std::string& dynamicDefiningClass = {})
{
    stringVector fieldnames = { METACLASS_FIELD_NAME, METACLASS_FIELD_DEFINING_CLASS,
        METACLASS_FIELD_DEFAULT_VALUE_EXPRESSION, METACLASS_FIELD_ACCESS,
        METACLASS_FIELD_GET_ACCESS, METACLASS_FIELD_SET_ACCESS, METACLASS_FIELD_GET_METHOD,
        METACLASS_FIELD_SET_METHOD, METACLASS_FIELD_CONSTANT, METACLASS_FIELD_DEPENDENT,
        METACLASS_FIELD_ABSTRACT, METACLASS_FIELD_HIDDEN, METACLASS_FIELD_GET_OBSERVABLE,
        METACLASS_FIELD_SET_OBSERVABLE, METACLASS_FIELD_ABORT_SET, METACLASS_FIELD_TRANSIENT,
        METACLASS_FIELD_NON_COPYABLE, METACLASS_FIELD_NONTUNABLE, METACLASS_FIELD_DYNAMIC,
        METACLASS_FIELD_VALIDATION_EXPRESSION, METACLASS_FIELD_ATTRIBUTES };
    if (properties.empty() && dynamicProperties.empty()) {
        return emptyStructArray(fieldnames);
    }

    stringVector names;
    stringVector definingClasses;
    stringVector defaultExpressions;
    stringVector accessValues;
    stringVector getAccessValues;
    stringVector setAccessValues;
    ArrayOfVector getMethodValues;
    ArrayOfVector setMethodValues;
    std::vector<bool> constantValues;
    std::vector<bool> dependentValues;
    std::vector<bool> abstractValues;
    std::vector<bool> hiddenValues;
    std::vector<bool> getObservableValues;
    std::vector<bool> setObservableValues;
    std::vector<bool> abortSetValues;
    std::vector<bool> transientValues;
    std::vector<bool> nonCopyableValues;
    std::vector<bool> nontunableValues;
    std::vector<bool> dynamicValues;
    stringVector validationExpressions;
    std::vector<stringVector> attributeValues;

    for (const auto& property : properties) {
        names.push_back(property.name);
        definingClasses.push_back(property.definingClassName);
        defaultExpressions.push_back(property.defaultExpression);
        accessValues.push_back(accessAttributeValue(property.attributes));
        getAccessValues.push_back(
            propertyAccessAttributeValue(property.attributes, METACLASS_ATTR_GET_ACCESS));
        setAccessValues.push_back(
            propertyAccessAttributeValue(property.attributes, METACLASS_ATTR_SET_ACCESS));
        getMethodValues.push_back(ArrayOf::empty());
        setMethodValues.push_back(ArrayOf::empty());
        constantValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_CONSTANT));
        dependentValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_DEPENDENT));
        abstractValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_ABSTRACT));
        hiddenValues.push_back(attributeListContains(property.attributes, METACLASS_ATTR_HIDDEN));
        getObservableValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_GET_OBSERVABLE));
        setObservableValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_SET_OBSERVABLE));
        abortSetValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_ABORT_SET));
        transientValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_TRANSIENT));
        nonCopyableValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_NON_COPYABLE));
        nontunableValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_NONTUNABLE));
        dynamicValues.push_back(false);
        validationExpressions.push_back(property.validationExpression);
        attributeValues.push_back(property.attributes);
    }
    for (size_t k = 0; k < dynamicProperties.size(); ++k) {
        const std::string& propertyName = dynamicProperties[k];
        ClassdefDynamicPropertyAttributes attributes;
        if (k < dynamicAttributes.size()) {
            attributes = dynamicAttributes[k];
        }
        stringVector attributeNames;
        if (attributes.hidden) {
            attributeNames.push_back(METACLASS_ATTR_HIDDEN);
        }
        if (attributes.nonCopyable) {
            attributeNames.push_back(METACLASS_ATTR_NON_COPYABLE);
        }
        if (attributes.getObservable) {
            attributeNames.push_back(METACLASS_ATTR_GET_OBSERVABLE);
        }
        if (attributes.setObservable) {
            attributeNames.push_back(METACLASS_ATTR_SET_OBSERVABLE);
        }
        if (attributes.abortSet) {
            attributeNames.push_back(METACLASS_ATTR_ABORT_SET);
        }
        if (attributes.dependent) {
            attributeNames.push_back(METACLASS_ATTR_DEPENDENT);
        }
        if (attributes.transient) {
            attributeNames.push_back(METACLASS_ATTR_TRANSIENT);
        }
        names.push_back(propertyName);
        definingClasses.push_back(dynamicDefiningClass);
        defaultExpressions.push_back({});
        accessValues.push_back(METACLASS_ACCESS_PUBLIC);
        getAccessValues.push_back(attributes.getAccess);
        setAccessValues.push_back(attributes.setAccess);
        getMethodValues.push_back(attributes.getMethod);
        setMethodValues.push_back(attributes.setMethod);
        constantValues.push_back(false);
        dependentValues.push_back(attributes.dependent);
        abstractValues.push_back(false);
        hiddenValues.push_back(attributes.hidden);
        getObservableValues.push_back(attributes.getObservable);
        setObservableValues.push_back(attributes.setObservable);
        abortSetValues.push_back(attributes.abortSet);
        transientValues.push_back(attributes.transient);
        nonCopyableValues.push_back(attributes.nonCopyable);
        nontunableValues.push_back(false);
        dynamicValues.push_back(true);
        validationExpressions.push_back(attributes.validationExpression);
        attributeValues.push_back(attributeNames);
    }

    ArrayOfVector values;
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(names);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(definingClasses);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(defaultExpressions);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(accessValues);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(getAccessValues);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(setAccessValues);
    values << cellArrayOfArrayColumnVector(getMethodValues);
    values << cellArrayOfArrayColumnVector(setMethodValues);
    values << cellArrayOfLogicalColumnVector(constantValues);
    values << cellArrayOfLogicalColumnVector(dependentValues);
    values << cellArrayOfLogicalColumnVector(abstractValues);
    values << cellArrayOfLogicalColumnVector(hiddenValues);
    values << cellArrayOfLogicalColumnVector(getObservableValues);
    values << cellArrayOfLogicalColumnVector(setObservableValues);
    values << cellArrayOfLogicalColumnVector(abortSetValues);
    values << cellArrayOfLogicalColumnVector(transientValues);
    values << cellArrayOfLogicalColumnVector(nonCopyableValues);
    values << cellArrayOfLogicalColumnVector(nontunableValues);
    values << cellArrayOfLogicalColumnVector(dynamicValues);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(validationExpressions);
    values << cellArrayOfCellstrColumnVector(attributeValues);
    return ArrayOf::structArray(fieldnames, values);
}
//=============================================================================
ArrayOf
methodDetailsStruct(const std::vector<ClassdefMethodDefinition>& methods)
{
    stringVector fieldnames = { METACLASS_FIELD_NAME, METACLASS_FIELD_DEFINING_CLASS,
        METACLASS_FIELD_ACCESS, METACLASS_FIELD_STATIC, METACLASS_FIELD_HIDDEN,
        METACLASS_FIELD_SEALED, METACLASS_FIELD_ABSTRACT, METACLASS_FIELD_ATTRIBUTES };
    if (methods.empty()) {
        return emptyStructArray(fieldnames);
    }

    stringVector names;
    stringVector definingClasses;
    stringVector accessValues;
    std::vector<bool> staticValues;
    std::vector<bool> hiddenValues;
    std::vector<bool> sealedValues;
    std::vector<bool> abstractValues;
    std::vector<stringVector> attributeValues;

    for (const auto& method : methods) {
        names.push_back(method.name);
        definingClasses.push_back(method.definingClassName);
        accessValues.push_back(accessAttributeValue(method.attributes));
        staticValues.push_back(
            method.isStatic || attributeListContains(method.attributes, METACLASS_ATTR_STATIC));
        hiddenValues.push_back(attributeListContains(method.attributes, METACLASS_ATTR_HIDDEN));
        sealedValues.push_back(attributeListContains(method.attributes, METACLASS_ATTR_SEALED));
        abstractValues.push_back(attributeListContains(method.attributes, METACLASS_ATTR_ABSTRACT));
        attributeValues.push_back(method.attributes);
    }

    ArrayOfVector values;
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(names);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(definingClasses);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(accessValues);
    values << cellArrayOfLogicalColumnVector(staticValues);
    values << cellArrayOfLogicalColumnVector(hiddenValues);
    values << cellArrayOfLogicalColumnVector(sealedValues);
    values << cellArrayOfLogicalColumnVector(abstractValues);
    values << cellArrayOfCellstrColumnVector(attributeValues);
    return ArrayOf::structArray(fieldnames, values);
}
//=============================================================================
ArrayOf
eventDetailsStruct(const std::vector<ClassdefEventDefinition>& events)
{
    stringVector fieldnames
        = { METACLASS_FIELD_NAME, METACLASS_FIELD_DEFINING_CLASS, METACLASS_FIELD_LISTEN_ACCESS,
              METACLASS_FIELD_NOTIFY_ACCESS, METACLASS_FIELD_HIDDEN, METACLASS_FIELD_ATTRIBUTES };
    if (events.empty()) {
        return emptyStructArray(fieldnames);
    }

    stringVector names;
    stringVector definingClasses;
    stringVector listenAccessValues;
    stringVector notifyAccessValues;
    std::vector<bool> hiddenValues;
    std::vector<stringVector> attributeValues;
    for (const auto& event : events) {
        names.push_back(event.name);
        definingClasses.push_back(event.definingClassName);
        listenAccessValues.push_back(
            propertyAccessAttributeValue(event.attributes, METACLASS_ATTR_LISTEN_ACCESS));
        notifyAccessValues.push_back(
            propertyAccessAttributeValue(event.attributes, METACLASS_ATTR_NOTIFY_ACCESS));
        hiddenValues.push_back(attributeListContains(event.attributes, METACLASS_ATTR_HIDDEN));
        attributeValues.push_back(event.attributes);
    }

    ArrayOfVector values;
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(names);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(definingClasses);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(listenAccessValues);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(notifyAccessValues);
    values << cellArrayOfLogicalColumnVector(hiddenValues);
    values << cellArrayOfCellstrColumnVector(attributeValues);
    return ArrayOf::structArray(fieldnames, values);
}
//=============================================================================
ArrayOf
enumerationDetailsStruct(const std::vector<ClassdefEnumerationDefinition>& enumerations)
{
    stringVector fieldnames = { METACLASS_FIELD_NAME, METACLASS_FIELD_DEFINING_CLASS,
        METACLASS_FIELD_CONSTRUCTOR_ARGUMENTS };
    if (enumerations.empty()) {
        return emptyStructArray(fieldnames);
    }

    stringVector names;
    stringVector definingClasses;
    stringVector constructorArguments;
    for (const auto& member : enumerations) {
        names.push_back(member.name);
        definingClasses.push_back(member.definingClassName);
        constructorArguments.push_back(member.arguments);
    }

    ArrayOfVector values;
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(names);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(definingClasses);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(constructorArguments);
    return ArrayOf::structArray(fieldnames, values);
}
//=============================================================================
ArrayOf
structToMetaObjects(const ArrayOf& structValue, const std::string& metaClassName)
{
    stringVector fieldnames = structValue.fieldNames();
    Dimensions dims = structValue.dimensions();
    void* data
        = ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, dims.elementCount(), fieldnames, true);
    ArrayOf result(NLS_CLASSDEF_ARRAY, dims, data, false, fieldnames);
    result.setClassType(metaClassName);
    if (dims.elementCount() > 0) {
        ArrayOf source = structValue;
        source.copyElements(0, data, 0, dims.elementCount());
    }
    return result;
}
//=============================================================================
ArrayOf
buildMetaClassObject(const std::string& className, const stringVector& dynamicProperties,
    const std::vector<ClassdefDynamicPropertyAttributes>& dynamicAttributes, int depth);
//=============================================================================
ArrayOf
metaClassArrayFromNames(const stringVector& names, int depth)
{
    std::vector<ArrayOf> elements;
    elements.reserve(names.size());
    for (const auto& name : names) {
        elements.push_back(buildMetaClassObject(name, {}, {}, depth));
    }
    if (elements.empty()) {
        stringVector fieldnames;
        Dimensions dims(0, 0);
        void* data = ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, 0, fieldnames, true);
        ArrayOf result(NLS_CLASSDEF_ARRAY, dims, data, false, fieldnames);
        result.setClassType(NLS_METADATA_CLASS);
        return result;
    }
    stringVector fieldnames = elements[0].fieldNames();
    Dimensions dims(1, static_cast<indexType>(elements.size()));
    void* data
        = ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, dims.elementCount(), fieldnames, true);
    ArrayOf result(NLS_CLASSDEF_ARRAY, dims, data, false, fieldnames);
    result.setClassType(NLS_METADATA_CLASS);
    for (size_t k = 0; k < elements.size(); ++k) {
        elements[k].copyElements(0, data, static_cast<indexType>(k), 1);
    }
    return result;
}
//=============================================================================
ArrayOf
buildMetaClassObject(const std::string& className, const stringVector& dynamicProperties,
    const std::vector<ClassdefDynamicPropertyAttributes>& dynamicAttributes, int depth)
{
    const bool loaded = depth > 0 && classdefLoadClass(className);
    const stringVector classAttributes
        = loaded ? classdefClassAttributes(className) : stringVector();
    ArrayOf classDetails = classDetailsStruct(className, classAttributes,
        loaded && classdefIsHandleClass(className), loaded && classdefIsAbstractClass(className));
    ArrayOf propertyDetails = loaded
        ? propertyDetailsStruct(classdefPropertyDefinitions(className), dynamicProperties,
              dynamicAttributes, className)
        : propertyDetailsStruct(std::vector<ClassdefPropertyDefinition>(), stringVector(),
              std::vector<ClassdefDynamicPropertyAttributes>(), className);
    ArrayOf methodDetails = methodDetailsStruct(
        loaded ? classdefMethodDefinitions(className) : std::vector<ClassdefMethodDefinition>());
    std::vector<ClassdefEventDefinition> allEvents
        = loaded ? classdefEventDefinitions(className) : std::vector<ClassdefEventDefinition>();
    ArrayOf eventDetails = eventDetailsStruct(allEvents);
    ArrayOf enumerationDetails
        = enumerationDetailsStruct(loaded ? classdefEnumerationDefinitions(className)
                                          : std::vector<ClassdefEnumerationDefinition>());

    std::vector<ClassdefEventDefinition> visibleEvents;
    if (loaded) {
        const stringVector visibleNames = classdefVisibleEvents(className);
        for (const auto& event : allEvents) {
            if (std::find(visibleNames.begin(), visibleNames.end(), event.name)
                != visibleNames.end()) {
                visibleEvents.push_back(event);
            }
        }
    }

    ArrayOf superclassList = metaClassArrayFromNames(
        loaded ? classdefSuperclasses(className) : stringVector(), depth - 1);

    stringVector fieldnames = { METACLASS_FIELD_NAME, METACLASS_FIELD_SUPERCLASS_LIST,
        METACLASS_FIELD_PROPERTY_LIST, METACLASS_FIELD_METHOD_LIST, METACLASS_FIELD_EVENT_LIST,
        METACLASS_FIELD_ENUMERATION_MEMBER_LIST, METACLASS_FIELD_ABSTRACT, METACLASS_FIELD_SEALED,
        METACLASS_FIELD_HIDDEN, METACLASS_FIELD_CONSTRUCT_ON_LOAD, METACLASS_FIELD_HANDLE,
        METACLASS_FIELD_INFERIOR_CLASSES, METACLASS_FIELD_CLASS_DETAILS,
        METACLASS_FIELD_PROPERTY_DETAILS, METACLASS_FIELD_METHOD_DETAILS,
        METACLASS_FIELD_EVENT_DETAILS, METACLASS_FIELD_ENUMERATION_DETAILS };
    ArrayOfVector values;
    values << ArrayOf::characterArray(className);
    values << superclassList;
    values << structToMetaObjects(propertyDetails, NLS_METADATA_PROPERTY);
    values << structToMetaObjects(methodDetails, NLS_METADATA_METHOD);
    values << structToMetaObjects(eventDetailsStruct(visibleEvents), NLS_METADATA_EVENT);
    values << structToMetaObjects(enumerationDetails, NLS_METADATA_ENUMERATED_VALUE);
    values << classDetails.field(METACLASS_FIELD_ABSTRACT);
    values << classDetails.field(METACLASS_FIELD_SEALED);
    values << classDetails.field(METACLASS_FIELD_HIDDEN);
    values << classDetails.field(METACLASS_FIELD_CONSTRUCT_ON_LOAD);
    values << classDetails.field(METACLASS_FIELD_HANDLE);
    values << classDetails.field(METACLASS_FIELD_INFERIOR_CLASSES);
    values << classDetails;
    values << propertyDetails;
    values << methodDetails;
    values << eventDetails;
    values << enumerationDetails;
    return ArrayOf::classdefArray(NLS_METADATA_CLASS, fieldnames, values);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::metaclassBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    std::string className;
    stringVector dynamicProperties;
    std::vector<ClassdefDynamicPropertyAttributes> dynamicAttributes;
    if (argIn[0].isClassType()) {
        className = argIn[0].classType();
    } else if (argIn[0].isHandle()) {
        className = argIn[0].handleClassName();
        if (argIn[0].isScalar()) {
            auto* object = dynamic_cast<ClassdefHandleObject*>(argIn[0].asHandleScalar());
            if (object != nullptr) {
                dynamicProperties = object->dynamicProperties();
                dynamicAttributes.reserve(dynamicProperties.size());
                for (const auto& propertyName : dynamicProperties) {
                    dynamicAttributes.push_back(object->dynamicPropertyAttributes(propertyName));
                }
            }
        }
    } else if (argIn[0].isRowVectorCharacterArray()) {
        className = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }

    if (!classdefLoadClass(className)) {
        RaiseError(
            resolveMessageTemplate("Nelson:handle:classFound", "Class not found: ") + className,
            "Nelson:metaclass:classNotFound");
    }

    retval << buildMetaClassObject(className, dynamicProperties, dynamicAttributes, 32);
    return retval;
}
//=============================================================================
