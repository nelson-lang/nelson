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
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define METACLASS_ACCESS_PUBLIC "public"
#define METACLASS_ATTR_ABSTRACT "Abstract"
#define METACLASS_ATTR_ACCESS "Access"
#define METACLASS_ATTR_CONSTANT "Constant"
#define METACLASS_ATTR_DEPENDENT "Dependent"
#define METACLASS_ATTR_GET_ACCESS "GetAccess"
#define METACLASS_ATTR_GET_OBSERVABLE "GetObservable"
#define METACLASS_ATTR_HIDDEN "Hidden"
#define METACLASS_ATTR_SEALED "Sealed"
#define METACLASS_ATTR_SET_ACCESS "SetAccess"
#define METACLASS_ATTR_SET_OBSERVABLE "SetObservable"
#define METACLASS_ATTR_STATIC "Static"
#define METACLASS_FIELD_ABSTRACT "Abstract"
#define METACLASS_FIELD_ACCESS "Access"
#define METACLASS_FIELD_ATTRIBUTES "Attributes"
#define METACLASS_FIELD_CLASS_DETAILS "ClassDetails"
#define METACLASS_FIELD_CONSTANT "Constant"
#define METACLASS_FIELD_CONSTRUCTOR_ARGUMENTS "ConstructorArguments"
#define METACLASS_FIELD_DEFAULT_VALUE_EXPRESSION "DefaultValueExpression"
#define METACLASS_FIELD_DEFINING_CLASS "DefiningClass"
#define METACLASS_FIELD_DEPENDENT "Dependent"
#define METACLASS_FIELD_ENUMERATION_DETAILS "EnumerationDetails"
#define METACLASS_FIELD_ENUMERATION_MEMBER_LIST "EnumerationMemberList"
#define METACLASS_FIELD_EVENT_DETAILS "EventDetails"
#define METACLASS_FIELD_EVENT_LIST "EventList"
#define METACLASS_FIELD_GET_ACCESS "GetAccess"
#define METACLASS_FIELD_GET_OBSERVABLE "GetObservable"
#define METACLASS_FIELD_HANDLE "Handle"
#define METACLASS_FIELD_HIDDEN "Hidden"
#define METACLASS_FIELD_METHOD_DETAILS "MethodDetails"
#define METACLASS_FIELD_METHOD_LIST "MethodList"
#define METACLASS_FIELD_NAME "Name"
#define METACLASS_FIELD_PROPERTY_DETAILS "PropertyDetails"
#define METACLASS_FIELD_PROPERTY_LIST "PropertyList"
#define METACLASS_FIELD_SEALED "Sealed"
#define METACLASS_FIELD_SET_ACCESS "SetAccess"
#define METACLASS_FIELD_SET_OBSERVABLE "SetObservable"
#define METACLASS_FIELD_STATIC "Static"
#define METACLASS_FIELD_SUPERCLASS_LIST "SuperclassList"
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
attributeListContains(const stringVector& attributes, const std::string& name)
{
    const std::string normalizedName = tolowerString(name);
    for (std::string attribute : attributes) {
        attribute = trim(attribute);
        size_t equals = attribute.find('=');
        if (equals != std::string::npos) {
            attribute = trim(attribute.substr(0, equals));
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
        row << ArrayOf::logicalConstructor(value);
        matrix.push_back(row);
    }
    return ArrayOf::cellConstructor(matrix);
}
//=============================================================================
ArrayOf
cellArrayOfCellstrColumnVector(const std::vector<stringVector>& values)
{
    ArrayOfMatrix matrix;
    matrix.reserve(values.size());
    for (const auto& value : values) {
        ArrayOfVector row;
        row << ArrayOf::toCellArrayOfCharacterColumnVectors(value);
        matrix.push_back(row);
    }
    return ArrayOf::cellConstructor(matrix);
}
//=============================================================================
ArrayOf
emptyStructArray(const stringVector& fieldnames)
{
    Dimensions dims(0, 1);
    return ArrayOf::emptyStructConstructor(fieldnames, dims);
}
//=============================================================================
ArrayOf
classDetailsStruct(const std::string& className, const stringVector& attributes, bool isHandle)
{
    stringVector fieldnames = { METACLASS_FIELD_NAME, METACLASS_FIELD_ATTRIBUTES,
        METACLASS_FIELD_ABSTRACT, METACLASS_FIELD_SEALED, METACLASS_FIELD_HANDLE };
    ArrayOfVector values;
    values << ArrayOf::characterArrayConstructor(className);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(attributes);
    values << ArrayOf::logicalConstructor(
        attributeListContains(attributes, METACLASS_ATTR_ABSTRACT));
    values << ArrayOf::logicalConstructor(attributeListContains(attributes, METACLASS_ATTR_SEALED));
    values << ArrayOf::logicalConstructor(isHandle);
    return ArrayOf::structScalarConstructor(fieldnames, values);
}
//=============================================================================
ArrayOf
propertyDetailsStruct(const std::vector<ClassdefPropertyDefinition>& properties)
{
    stringVector fieldnames = { METACLASS_FIELD_NAME, METACLASS_FIELD_DEFINING_CLASS,
        METACLASS_FIELD_DEFAULT_VALUE_EXPRESSION, METACLASS_FIELD_ACCESS,
        METACLASS_FIELD_GET_ACCESS, METACLASS_FIELD_SET_ACCESS, METACLASS_FIELD_CONSTANT,
        METACLASS_FIELD_DEPENDENT, METACLASS_FIELD_HIDDEN, METACLASS_FIELD_GET_OBSERVABLE,
        METACLASS_FIELD_SET_OBSERVABLE, METACLASS_FIELD_ATTRIBUTES };
    if (properties.empty()) {
        return emptyStructArray(fieldnames);
    }

    stringVector names;
    stringVector definingClasses;
    stringVector defaultExpressions;
    stringVector accessValues;
    stringVector getAccessValues;
    stringVector setAccessValues;
    std::vector<bool> constantValues;
    std::vector<bool> dependentValues;
    std::vector<bool> hiddenValues;
    std::vector<bool> getObservableValues;
    std::vector<bool> setObservableValues;
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
        constantValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_CONSTANT));
        dependentValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_DEPENDENT));
        hiddenValues.push_back(attributeListContains(property.attributes, METACLASS_ATTR_HIDDEN));
        getObservableValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_GET_OBSERVABLE));
        setObservableValues.push_back(
            attributeListContains(property.attributes, METACLASS_ATTR_SET_OBSERVABLE));
        attributeValues.push_back(property.attributes);
    }

    ArrayOfVector values;
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(names);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(definingClasses);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(defaultExpressions);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(accessValues);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(getAccessValues);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(setAccessValues);
    values << cellArrayOfLogicalColumnVector(constantValues);
    values << cellArrayOfLogicalColumnVector(dependentValues);
    values << cellArrayOfLogicalColumnVector(hiddenValues);
    values << cellArrayOfLogicalColumnVector(getObservableValues);
    values << cellArrayOfLogicalColumnVector(setObservableValues);
    values << cellArrayOfCellstrColumnVector(attributeValues);
    return ArrayOf::structConstructor(fieldnames, values);
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
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(names);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(definingClasses);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(accessValues);
    values << cellArrayOfLogicalColumnVector(staticValues);
    values << cellArrayOfLogicalColumnVector(hiddenValues);
    values << cellArrayOfLogicalColumnVector(sealedValues);
    values << cellArrayOfLogicalColumnVector(abstractValues);
    values << cellArrayOfCellstrColumnVector(attributeValues);
    return ArrayOf::structConstructor(fieldnames, values);
}
//=============================================================================
ArrayOf
eventDetailsStruct(const std::vector<ClassdefEventDefinition>& events)
{
    stringVector fieldnames = { METACLASS_FIELD_NAME, METACLASS_FIELD_DEFINING_CLASS };
    if (events.empty()) {
        return emptyStructArray(fieldnames);
    }

    stringVector names;
    stringVector definingClasses;
    for (const auto& event : events) {
        names.push_back(event.name);
        definingClasses.push_back(event.definingClassName);
    }

    ArrayOfVector values;
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(names);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(definingClasses);
    return ArrayOf::structConstructor(fieldnames, values);
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
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(names);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(definingClasses);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(constructorArguments);
    return ArrayOf::structConstructor(fieldnames, values);
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
    if (argIn[0].isClassType()) {
        className = argIn[0].getClassType();
    } else if (argIn[0].isHandle()) {
        className = argIn[0].getHandleClassName();
    } else if (argIn[0].isRowVectorCharacterArray()) {
        className = argIn[0].getContentAsCString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }

    auto* manager = ClassdefDefinitionManager::getInstance();
    if (!manager->loadClass(className)) {
        Error(_("Class not found: ") + className);
    }

    const stringVector classAttributes = manager->classAttributes(className);
    stringVector fieldnames = { METACLASS_FIELD_NAME, METACLASS_FIELD_SUPERCLASS_LIST,
        METACLASS_FIELD_PROPERTY_LIST, METACLASS_FIELD_METHOD_LIST, METACLASS_FIELD_EVENT_LIST,
        METACLASS_FIELD_ENUMERATION_MEMBER_LIST, METACLASS_FIELD_CLASS_DETAILS,
        METACLASS_FIELD_PROPERTY_DETAILS, METACLASS_FIELD_METHOD_DETAILS,
        METACLASS_FIELD_EVENT_DETAILS, METACLASS_FIELD_ENUMERATION_DETAILS };
    ArrayOfVector values;
    values << ArrayOf::characterArrayConstructor(className);
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(manager->superclasses(className));
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(manager->properties(className));
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(manager->methods(className));
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(manager->events(className));
    values << ArrayOf::toCellArrayOfCharacterColumnVectors(manager->enumerations(className));
    values << classDetailsStruct(className, classAttributes, manager->isHandleClass(className));
    values << propertyDetailsStruct(manager->propertyDefinitions(className));
    values << methodDetailsStruct(manager->methodDefinitions(className));
    values << eventDetailsStruct(manager->eventDefinitions(className));
    values << enumerationDetailsStruct(manager->enumerationDefinitions(className));
    retval << ArrayOf::structScalarConstructor(fieldnames, values);
    return retval;
}
//=============================================================================
