//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserAttributes.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include <algorithm>
#include <cctype>
//=============================================================================
namespace Nelson::classdef_detail {
bool
parseAttributesFromHeader(const std::string& line, const std::string& keyword,
    stringVector& attributes, std::string& errorMessage, bool allowAttributes)
{
    attributes.clear();
    std::string value = trim(stripComment(line));
    if (!startsWithKeyword(value, keyword)) {
        return false;
    }
    value = trim(value.substr(keyword.size()));
    value = withoutTrailingStatementSeparator(value);
    if (value.empty()) {
        return true;
    }
    if (!allowAttributes || value[0] != '(') {
        errorMessage = CLASSDEF_ERROR_INVALID_BLOCK_HEADER + trim(stripComment(line));
        return false;
    }
    int depth = 0;
    size_t end = std::string::npos;
    for (size_t k = 0; k < value.size(); ++k) {
        if (value[k] == '(') {
            ++depth;
        } else if (value[k] == ')') {
            --depth;
            if (depth == 0) {
                end = k;
                break;
            }
        }
    }
    if (end == std::string::npos || end <= 1) {
        errorMessage = CLASSDEF_ERROR_UNTERMINATED_ATTRIBUTES;
        return false;
    }
    std::string trailing = withoutTrailingStatementSeparator(value.substr(end + 1));
    if (!trailing.empty()) {
        errorMessage = CLASSDEF_ERROR_INVALID_BLOCK_HEADER + trim(stripComment(line));
        return false;
    }
    attributes = splitCommaList(value.substr(1, end - 1));
    return true;
}
//=============================================================================
std::string
unquoteAttributeValue(std::string value)
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
bool
attributeValueMeansFalse(std::string value)
{
    value = tolowerString(unquoteAttributeValue(value));
    return value == "false" || value == "0";
}
//=============================================================================
bool
isBooleanAttributeValue(std::string value)
{
    const std::string unquoted = unquoteAttributeValue(value);
    const std::string lowered = tolowerString(unquoted);
    if (lowered == "true" || lowered == "false" || lowered == "1" || lowered == "0") {
        return true;
    }
    // Non-literal expressions are accepted (e.g. Sealed = logical(1)) when they
    // look like an expression, a call or an operator, so plain typos such as
    // "tru" are still rejected. The boolean value stays syntactic for now (see
    // attributeValueMeansFalse); evaluating the expression is deferred.
    return unquoted.find_first_of("()~<>=&|+-*/") != std::string::npos;
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
attributeName(std::string attribute)
{
    attribute = trim(attribute);
    size_t equals = attribute.find('=');
    if (equals != std::string::npos) {
        attribute = trim(attribute.substr(0, equals));
    }
    return attribute;
}
//=============================================================================
bool
isBooleanAttributeName(const std::string& name, bool allowSystemObjectAttributes)
{
    return equalsIgnoreCase(name, CLASSDEF_ATTR_ABSTRACT)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_ABORT_SET)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_CONSTANT)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_CONSTRUCT_ON_LOAD)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_DEPENDENT)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_GET_OBSERVABLE)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_HIDDEN)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_NON_COPYABLE)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_SEALED)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_SET_OBSERVABLE)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_STATIC)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_TRANSIENT)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_WEAK_HANDLE)
        || (allowSystemObjectAttributes && equalsIgnoreCase(name, CLASSDEF_ATTR_NONTUNABLE));
}
//=============================================================================
bool
isBooleanAttributeName(const std::string& name)
{
    return isBooleanAttributeName(name, false);
}
//=============================================================================
void
validateBooleanAttributeValues(const stringVector& attributes, bool allowSystemObjectAttributes)
{
    for (const auto& rawAttribute : attributes) {
        const std::string name = attributeName(rawAttribute);
        if (!isBooleanAttributeName(name, allowSystemObjectAttributes)) {
            continue;
        }
        const size_t equals = rawAttribute.find('=');
        if (equals == std::string::npos) {
            continue;
        }
        if (!isBooleanAttributeValue(rawAttribute.substr(equals + 1))) {
            RaiseError(CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute),
                "Nelson:class:invalidAttribute");
        }
    }
}
//=============================================================================
void
validateBooleanAttributeValues(const stringVector& attributes)
{
    validateBooleanAttributeValues(attributes, false);
}
//=============================================================================
void
validatePropertyBooleanAttributeValues(
    const stringVector& attributes, bool allowSystemObjectAttributes)
{
    validateBooleanAttributeValues(attributes, allowSystemObjectAttributes);
}
//=============================================================================
bool
validateBooleanAttributeValues(
    const stringVector& attributes, bool allowSystemObjectAttributes, std::string& errorMessage)
{
    for (const auto& rawAttribute : attributes) {
        const std::string name = attributeName(rawAttribute);
        if (!isBooleanAttributeName(name, allowSystemObjectAttributes)) {
            continue;
        }
        const size_t equals = rawAttribute.find('=');
        if (equals == std::string::npos) {
            continue;
        }
        if (!isBooleanAttributeValue(rawAttribute.substr(equals + 1))) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute);
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
validateBooleanAttributeValues(const stringVector& attributes, std::string& errorMessage)
{
    return validateBooleanAttributeValues(attributes, false, errorMessage);
}
//=============================================================================
bool
validatePropertyBooleanAttributeValues(
    const stringVector& attributes, bool allowSystemObjectAttributes, std::string& errorMessage)
{
    return validateBooleanAttributeValues(attributes, allowSystemObjectAttributes, errorMessage);
}
//=============================================================================
bool
isClassReference(std::string value)
{
    value = trim(value);
    if (value.size() < 2 || value.front() != '?') {
        return false;
    }
    value = trim(value.substr(1));
    return !value.empty() && parseLeadingQualifiedIdentifier(value) == value;
}
//=============================================================================
bool
isClassAccessReference(std::string value)
{
    return isClassReference(unquoteAttributeValue(value));
}
//=============================================================================
bool
isValidAccessAttributeValue(std::string value)
{
    value = trim(value);
    // Double-quoted values are not accepted in access attributes; only
    // single-quoted (or bare) identifiers and metaclass references are.
    if (value.size() >= 2 && value.front() == '"' && value.back() == '"') {
        return false;
    }
    value = unquoteAttributeValue(value);
    if (equalsIgnoreCase(value, CLASSDEF_ACCESS_PUBLIC)
        || equalsIgnoreCase(value, CLASSDEF_ACCESS_PRIVATE)
        || equalsIgnoreCase(value, CLASSDEF_ACCESS_PROTECTED)) {
        return true;
    }
    if (isClassAccessReference(value)) {
        return true;
    }
    if (value.size() >= 2 && value.front() == '{' && value.back() == '}') {
        const stringVector items = splitCommaList(value.substr(1, value.size() - 2));
        if (items.empty()) {
            return false;
        }
        for (const auto& item : items) {
            if (!isClassAccessReference(item)) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
bool
isValidInferiorClassesAttributeValue(std::string value)
{
    value = trim(value);
    if (isClassReference(value)) {
        return true;
    }
    if (value.size() >= 2 && value.front() == '{' && value.back() == '}') {
        const stringVector items = splitCommaList(value.substr(1, value.size() - 2));
        if (items.empty()) {
            return false;
        }
        for (const auto& item : items) {
            if (!isClassReference(item)) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
stringVector
classReferenceNamesFromAttributeValue(std::string value)
{
    value = trim(value);
    stringVector values;
    if (value.size() >= 2 && value.front() == '{' && value.back() == '}') {
        values = splitCommaList(value.substr(1, value.size() - 2));
    } else if (!value.empty()) {
        values.push_back(value);
    }
    stringVector classNames;
    for (std::string item : values) {
        item = trim(item);
        if (!item.empty() && item.front() == '?') {
            item = trim(item.substr(1));
        }
        if (!item.empty()) {
            classNames.push_back(item);
        }
    }
    return classNames;
}
//=============================================================================
void
validateInferiorClassesAttributeValue(const stringVector& attributes)
{
    for (const auto& rawAttribute : attributes) {
        const std::string name = attributeName(rawAttribute);
        if (!equalsIgnoreCase(name, CLASSDEF_ATTR_INFERIOR_CLASSES)) {
            continue;
        }
        const size_t equals = rawAttribute.find('=');
        if (equals == std::string::npos) {
            RaiseError(CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute),
                "Nelson:class:invalidAttribute");
        }
        const std::string value = trim(rawAttribute.substr(equals + 1));
        if (!isValidInferiorClassesAttributeValue(value)) {
            RaiseError(CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute),
                "Nelson:class:invalidAttribute");
        }
    }
}
//=============================================================================
bool
validateInferiorClassesAttributeValue(const stringVector& attributes, std::string& errorMessage)
{
    for (const auto& rawAttribute : attributes) {
        const std::string name = attributeName(rawAttribute);
        if (!equalsIgnoreCase(name, CLASSDEF_ATTR_INFERIOR_CLASSES)) {
            continue;
        }
        const size_t equals = rawAttribute.find('=');
        if (equals == std::string::npos) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute);
            return false;
        }
        const std::string value = trim(rawAttribute.substr(equals + 1));
        if (!isValidInferiorClassesAttributeValue(value)) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute);
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
isAccessAttributeName(const std::string& name)
{
    return equalsIgnoreCase(name, CLASSDEF_ATTR_ACCESS)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_GET_ACCESS)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_SET_ACCESS)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_LISTEN_ACCESS)
        || equalsIgnoreCase(name, CLASSDEF_ATTR_NOTIFY_ACCESS);
}
//=============================================================================
// 'immutable' is a valid value for SetAccess only (a property settable during
// construction, read-only afterwards). It is not a valid Access/GetAccess
// value.
bool
isImmutableSetAccess(const std::string& name, const std::string& value)
{
    return equalsIgnoreCase(name, CLASSDEF_ATTR_SET_ACCESS)
        && equalsIgnoreCase(unquoteAttributeValue(value), CLASSDEF_ACCESS_IMMUTABLE);
}
//=============================================================================
void
validateAccessAttributeValues(const stringVector& attributes)
{
    for (const auto& rawAttribute : attributes) {
        const std::string name = attributeName(rawAttribute);
        if (!isAccessAttributeName(name)) {
            continue;
        }
        const size_t equals = rawAttribute.find('=');
        if (equals == std::string::npos) {
            RaiseError(CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute),
                "Nelson:class:invalidAttribute");
        }
        const std::string value = trim(rawAttribute.substr(equals + 1));
        if (isImmutableSetAccess(name, value)) {
            continue;
        }
        if (!isValidAccessAttributeValue(value)) {
            RaiseError(CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute),
                "Nelson:class:invalidAttribute");
        }
    }
}
//=============================================================================
bool
validateAccessAttributeValues(const stringVector& attributes, std::string& errorMessage)
{
    for (const auto& rawAttribute : attributes) {
        const std::string name = attributeName(rawAttribute);
        if (!isAccessAttributeName(name)) {
            continue;
        }
        const size_t equals = rawAttribute.find('=');
        if (equals == std::string::npos) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute);
            return false;
        }
        const std::string value = trim(rawAttribute.substr(equals + 1));
        if (isImmutableSetAccess(name, value)) {
            continue;
        }
        if (!isValidAccessAttributeValue(value)) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_VALUE + trim(rawAttribute);
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
validateDefinitionBooleanAttributeValues(
    const ClassdefDefinition& definition, std::string& errorMessage)
{
    if (!validateBooleanAttributeValues(definition.attributes, errorMessage)) {
        return false;
    }
    for (const auto& property : definition.properties) {
        if (!validateBooleanAttributeValues(property.attributes, errorMessage)) {
            return false;
        }
    }
    for (const auto& method : definition.methods) {
        if (!validateBooleanAttributeValues(method.attributes, errorMessage)) {
            return false;
        }
    }
    for (const auto& event : definition.events) {
        if (!validateBooleanAttributeValues(event.attributes, errorMessage)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
validateDefinitionAccessAttributeValues(
    const ClassdefDefinition& definition, std::string& errorMessage)
{
    for (const auto& property : definition.properties) {
        if (!validateAccessAttributeValues(property.attributes, errorMessage)) {
            return false;
        }
    }
    for (const auto& method : definition.methods) {
        if (!validateAccessAttributeValues(method.attributes, errorMessage)) {
            return false;
        }
    }
    for (const auto& event : definition.events) {
        if (!validateAccessAttributeValues(event.attributes, errorMessage)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
const ClassdefPropertyDefinition*
findLocalPropertyDefinition(const ClassdefDefinition& definition, const std::string& propertyName)
{
    for (const auto& property : definition.properties) {
        if (equalsIgnoreCase(property.name, propertyName)) {
            return &property;
        }
    }
    return nullptr;
}
//=============================================================================
std::string
setAccessorPropertyName(const std::string& methodName)
{
    if (methodName.rfind(CLASSDEF_METHOD_SET_PREFIX, 0) != 0) {
        return {};
    }
    return methodName.substr(std::string(CLASSDEF_METHOD_SET_PREFIX).size());
}
//=============================================================================
bool
invalidAttributeCombination(const ClassdefDefinition& definition, std::string& errorMessage)
{
    if (attributeListContains(definition.attributes, CLASSDEF_ATTR_ABSTRACT)
        && attributeListContains(definition.attributes, CLASSDEF_ATTR_SEALED)) {
        errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_COMBINATION "class Abstract and Sealed";
        return true;
    }
    for (const auto& property : definition.properties) {
        if (attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
            && attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_COMBINATION
                "Constant and Dependent property: "
                + property.name;
            return true;
        }
        if (attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
            && !attributeValue(property.attributes, CLASSDEF_ATTR_SET_ACCESS).empty()) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_COMBINATION
                "SetAccess on Constant property: "
                + property.name;
            return true;
        }
    }
    for (const auto& method : definition.methods) {
        if (attributeListContains(method.attributes, CLASSDEF_ATTR_ABSTRACT)
            && attributeListContains(method.attributes, CLASSDEF_ATTR_SEALED)) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_COMBINATION
                "Abstract and Sealed method: "
                + method.name;
            return true;
        }
        if (attributeListContains(method.attributes, CLASSDEF_ATTR_ABSTRACT)
            && !method.source.empty()) {
            errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_COMBINATION "Abstract method body: "
                + method.name;
            return true;
        }
        const std::string propertyName = setAccessorPropertyName(method.name);
        if (!propertyName.empty()) {
            const ClassdefPropertyDefinition* property
                = findLocalPropertyDefinition(definition, propertyName);
            if (property != nullptr
                && attributeListContains(property->attributes, CLASSDEF_ATTR_CONSTANT)) {
                errorMessage = CLASSDEF_ERROR_INVALID_ATTRIBUTE_COMBINATION
                    "setter for Constant property: "
                    + propertyName;
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
void
validateAttributeCombinations(const ClassdefDefinition& definition)
{
    std::string errorMessage;
    if (invalidAttributeCombination(definition, errorMessage)) {
        Error("Nelson:interpreter:invalidAttributeCombination", errorMessage);
    }
}
//=============================================================================
bool
isKnownAttribute(const std::string& attribute, const char* const* names, size_t count)
{
    const std::string name = tolowerString(attributeName(attribute));
    if (name.empty()) {
        return false;
    }
    for (size_t k = 0; k < count; ++k) {
        if (name == tolowerString(names[k])) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
isEnumerationBaseType(const std::string& className)
{
    const std::string name = tolowerString(trim(className));
    // logical belongs here as much as the numeric types: an enumeration
    // deriving from it is how a two-state switch keeps `if obj` and
    // logical(obj) working. Without it such a class matches neither this
    // list nor builtinBaseTypeName (which bails out on enumerations), so it
    // would get no base machinery at all.
    return name == NLS_UINT8_STR || name == NLS_INT8_STR || name == NLS_UINT16_STR
        || name == NLS_INT16_STR || name == NLS_UINT32_STR || name == NLS_INT32_STR
        || name == NLS_UINT64_STR || name == NLS_INT64_STR || name == NLS_SINGLE_STR
        || name == NLS_DOUBLE_STR || name == NLS_LOGICAL_STR;
}
//=============================================================================
bool
isBuiltinBaseClassName(const std::string& className)
{
    const std::string name = tolowerString(trim(className));
    return isEnumerationBaseType(className) || name == NLS_LOGICAL_STR || name == NLS_CHAR_STR;
}
//=============================================================================
std::string
builtinBaseTypeName(const ClassdefDefinition& definition)
{
    if (!definition.enumerations.empty()) {
        return {};
    }
    for (const auto& superclass : definition.superclasses) {
        const std::string name = trim(superclass);
        if (isBuiltinBaseClassName(name)) {
            return tolowerString(name);
        }
    }
    return {};
}
//=============================================================================
std::string
enumerationBaseType(const ClassdefDefinition& definition)
{
    if (definition.enumerations.empty()) {
        return {};
    }
    for (const auto& superclass : definition.superclasses) {
        const std::string name = trim(superclass);
        if (isEnumerationBaseType(name)) {
            return name;
        }
    }
    return {};
}
//=============================================================================
void
validateAttributes(const stringVector& attributes, const char* const* names, size_t count)
{
    for (const auto& attribute : attributes) {
        if (!isKnownAttribute(attribute, names, count)) {
            RaiseError(CLASSDEF_ERROR_INVALID_ATTRIBUTE + trim(attribute),
                "Nelson:class:invalidAttribute");
        }
    }
}
//=============================================================================
bool
hasPublicAccess(const stringVector& attributes)
{
    std::string access = trim(attributeValue(attributes, CLASSDEF_ATTR_ACCESS));
    if (access.size() >= 2
        && ((access.front() == '\'' && access.back() == '\'')
            || (access.front() == '"' && access.back() == '"'))) {
        access = access.substr(1, access.size() - 2);
    }
    return access.empty() || equalsIgnoreCase(access, CLASSDEF_ACCESS_PUBLIC);
}
//=============================================================================
std::string
accessAttributeValue(const stringVector& attributes)
{
    std::string access = trim(attributeValue(attributes, CLASSDEF_ATTR_ACCESS));
    if (access.empty()) {
        return CLASSDEF_ACCESS_PUBLIC;
    }
    if (access.size() >= 2
        && ((access.front() == '\'' && access.back() == '\'')
            || (access.front() == '"' && access.back() == '"'))) {
        access = access.substr(1, access.size() - 2);
    }
    return trim(access);
}
//=============================================================================
std::string
propertyAccessAttributeValue(const stringVector& attributes, const std::string& attributeName)
{
    std::string access = trim(attributeValue(attributes, attributeName));
    if (access.empty()) {
        access = trim(attributeValue(attributes, CLASSDEF_ATTR_ACCESS));
    }
    if (access.empty()) {
        return CLASSDEF_ACCESS_PUBLIC;
    }
    if (access.size() >= 2
        && ((access.front() == '\'' && access.back() == '\'')
            || (access.front() == '"' && access.back() == '"'))) {
        access = access.substr(1, access.size() - 2);
    }
    return trim(access);
}
//=============================================================================
std::string
metaclassAccessName(std::string value)
{
    value = trim(value);
    if (value.size() >= 2
        && ((value.front() == '\'' && value.back() == '\'')
            || (value.front() == '"' && value.back() == '"'))) {
        value = value.substr(1, value.size() - 2);
    }
    value = trim(value);
    if (!value.empty() && value.front() == '?') {
        value = trim(value.substr(1));
    }
    return value;
}
//=============================================================================
bool
isAccessListAllowedFromClassdefContext(const std::string& definingClassName,
    const std::string& access, const std::string& callerClassName)
{
    if (equalsIgnoreCase(callerClassName, definingClassName)) {
        return true;
    }

    std::string list = trim(access);
    if (list.size() >= 2 && list.front() == '{' && list.back() == '}') {
        list = trim(list.substr(1, list.size() - 2));
    }
    if (list.empty()) {
        return false;
    }

    for (const std::string& token : splitCommaList(list)) {
        std::string allowedClassName = metaclassAccessName(token);
        if (allowedClassName.empty()) {
            continue;
        }
        if (equalsIgnoreCase(callerClassName, allowedClassName)
            || classdefIsA(callerClassName, allowedClassName)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
isAccessAllowedFromClassdefContext(const std::string& definingClassName, const std::string& access,
    const std::string& callerClassName)
{
    if (equalsIgnoreCase(access, CLASSDEF_ACCESS_PUBLIC)) {
        return true;
    }
    if (callerClassName.empty()) {
        return false;
    }
    if (equalsIgnoreCase(access, CLASSDEF_ACCESS_PRIVATE)) {
        return equalsIgnoreCase(callerClassName, definingClassName);
    }
    if (equalsIgnoreCase(access, CLASSDEF_ACCESS_PROTECTED)) {
        return equalsIgnoreCase(callerClassName, definingClassName)
            || classdefIsA(callerClassName, definingClassName);
    }
    return isAccessListAllowedFromClassdefContext(definingClassName, access, callerClassName);
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
