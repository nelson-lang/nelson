//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserValidation.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserRegistry.hpp"
#include "ClassdefParserDefinitionQuery.hpp"
#include "ClassdefParserAttributes.hpp"
#include "ClassdefParserScanner.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FunctionsInMemory.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "ClassName.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
static void
validateWeakHandleProperties(const ClassdefDefinition& definition);
//=============================================================================
bool
isCallableFromClassdefContext(const std::string& definingClassName, const stringVector& attributes,
    const std::string& callerClassName)
{
    return isAccessAllowedFromClassdefContext(
        definingClassName, accessAttributeValue(attributes), callerClassName);
}
//=============================================================================
bool
isVisibleMember(const stringVector& attributes)
{
    return hasPublicAccess(attributes) && !attributeListContains(attributes, CLASSDEF_ATTR_HIDDEN);
}
//=============================================================================
bool
isVisibleProperty(const stringVector& attributes)
{
    return equalsIgnoreCase(propertyAccessAttributeValue(attributes, CLASSDEF_ATTR_GET_ACCESS),
               CLASSDEF_ACCESS_PUBLIC)
        && !attributeListContains(attributes, CLASSDEF_ATTR_HIDDEN);
}
//=============================================================================
bool
isVisibleEvent(const stringVector& attributes)
{
    return equalsIgnoreCase(propertyAccessAttributeValue(attributes, CLASSDEF_ATTR_LISTEN_ACCESS),
               CLASSDEF_ACCESS_PUBLIC)
        && !attributeListContains(attributes, CLASSDEF_ATTR_HIDDEN);
}
//=============================================================================
void
appendUnique(stringVector& values, const std::string& value)
{
    if (std::find(values.begin(), values.end(), value) == values.end()) {
        values.push_back(value);
    }
}
//=============================================================================
bool
functionExistsInMemoryOnly(const std::string& functionName)
{
    FunctionDefPtr functionDef = nullptr;
    if (FunctionsInMemory::getInstance()->find(
            functionName, functionDef, FunctionsInMemory::FIND_FUNCTION_TYPE::MACRO)) {
        return true;
    }
    return BuiltInFunctionDefManager::getInstance()->find(functionName, functionDef);
}
//=============================================================================
bool
functionExistsInMemoryOrPath(const std::string& functionName)
{
    if (functionExistsInMemoryOnly(functionName)) {
        return true;
    }
    std::wstring filename;
    return PathFunctionIndexerManager::getInstance()->find(functionName, filename);
}
//=============================================================================
std::string
inheritedMemberOwner(const std::string& fallbackClassName, const std::string& definingClassName)
{
    return definingClassName.empty() ? trim(fallbackClassName) : trim(definingClassName);
}
//=============================================================================
bool
isInheritedMethodConflictCandidate(
    const ClassdefMethodDefinition& method, const std::string& ownerClassName)
{
    return !equalsClassNameOrLeaf(method.name, ownerClassName)
        && method.name.rfind(CLASSDEF_METHOD_GET_PREFIX, 0) != 0
        && method.name.rfind(CLASSDEF_METHOD_SET_PREFIX, 0) != 0
        && isVisibleMember(method.attributes);
}
//=============================================================================
void
validateDuplicateProperties(const ClassdefDefinition& definition)
{
    std::unordered_set<std::string> propertyNames;
    for (const auto& property : definition.properties) {
        // Names are case-sensitive: members that differ only in letter case
        // (a private 'foo' and a dependent 'Foo') are not duplicates.
        if (!propertyNames.insert(property.name).second) {
            RaiseError(CLASSDEF_ERROR_DUPLICATE_PROPERTY + property.name,
                "Nelson:class:duplicateProperty");
        }
    }
}
//=============================================================================
void
validateDuplicateMethods(const ClassdefDefinition& definition)
{
    std::unordered_set<std::string> methodNames;
    for (const auto& method : definition.methods) {
        // Method names are case-sensitive: names differing only in case are
        // not duplicates.
        if (!methodNames.insert(method.name).second) {
            RaiseError(
                CLASSDEF_ERROR_DUPLICATE_METHOD + method.name, "Nelson:class:duplicateMethod");
        }
    }
}
//=============================================================================
void
validateDuplicateEnumerations(const ClassdefDefinition& definition)
{
    std::unordered_set<std::string> enumerationNames;
    for (const auto& member : definition.enumerations) {
        // Enumeration member names are case-sensitive: names differing only
        // in case are not duplicates.
        if (!enumerationNames.insert(member).second) {
            RaiseError(
                CLASSDEF_ERROR_DUPLICATE_ENUMERATION + member, "Nelson:class:duplicateEnumeration");
        }
    }
}
//=============================================================================
void
enumerationMemberConflictName(const ClassdefDefinition& definition, std::string& conflictName)
{
    conflictName.clear();
    std::unordered_set<std::string> memberNames;
    for (const auto& member : definition.enumerations) {
        memberNames.insert(tolowerString(member));
    }
    if (memberNames.empty()) {
        return;
    }
    for (const auto& property : definition.properties) {
        if (memberNames.find(tolowerString(property.name)) != memberNames.end()) {
            conflictName = property.name;
            return;
        }
    }
    for (const auto& method : definition.methods) {
        if (equalsIgnoreCase(method.name, definition.name)
            || method.name.rfind(CLASSDEF_METHOD_GET_PREFIX, 0) == 0
            || method.name.rfind(CLASSDEF_METHOD_SET_PREFIX, 0) == 0) {
            continue;
        }
        if (memberNames.find(tolowerString(method.name)) != memberNames.end()) {
            conflictName = method.name;
            return;
        }
    }
}
//=============================================================================
void
validateEnumerationMemberConflicts(const ClassdefDefinition& definition)
{
    std::string conflictName;
    enumerationMemberConflictName(definition, conflictName);
    if (!conflictName.empty()) {
        RaiseError(CLASSDEF_ERROR_ENUMERATION_MEMBER_CONFLICT + conflictName,
            "Nelson:class:enumerationConflict");
    }
    std::unordered_set<std::string> memberNames;
    for (const auto& member : definition.enumerations) {
        memberNames.insert(tolowerString(member));
    }
    if (memberNames.empty()) {
        return;
    }
    for (const auto& superclass : definition.superclasses) {
        const std::string superclassName = trim(superclass);
        if (superclassName.empty() || equalsIgnoreCase(superclassName, NLS_HANDLE_STR)) {
            continue;
        }
        for (const auto& property : classdefPropertyDefinitions(superclassName)) {
            if (!isVisibleProperty(property.attributes)) {
                continue;
            }
            if (memberNames.find(tolowerString(property.name)) != memberNames.end()) {
                RaiseError(CLASSDEF_ERROR_ENUMERATION_MEMBER_CONFLICT + property.name,
                    "Nelson:class:enumerationConflict");
            }
        }
        for (const auto& method : classdefMethodDefinitions(superclassName)) {
            if (!isInheritedMethodConflictCandidate(method, superclassName)) {
                continue;
            }
            if (memberNames.find(tolowerString(method.name)) != memberNames.end()) {
                RaiseError(CLASSDEF_ERROR_ENUMERATION_MEMBER_CONFLICT + method.name,
                    "Nelson:class:enumerationConflict");
            }
        }
    }
}
//=============================================================================
void
validateInheritedPropertyConflicts(const ClassdefDefinition& definition)
{
    struct InheritedPropertyOwner
    {
        std::string owner;
        bool isAbstract;
    };
    std::unordered_map<std::string, InheritedPropertyOwner> ownersByName;
    for (const auto& superclass : definition.superclasses) {
        const std::string superclassName = trim(superclass);
        if (superclassName.empty() || equalsIgnoreCase(superclassName, NLS_HANDLE_STR)) {
            continue;
        }
        for (const auto& property : classdefPropertyDefinitions(superclassName)) {
            if (!isVisibleProperty(property.attributes)) {
                continue;
            }
            const std::string key = tolowerString(property.name);
            const std::string owner
                = inheritedMemberOwner(superclassName, property.definingClassName);
            const bool isAbstract
                = attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT);
            const auto inserted
                = ownersByName.emplace(key, InheritedPropertyOwner { owner, isAbstract });
            if (!inserted.second && !equalsIgnoreCase(inserted.first->second.owner, owner)
                && !inserted.first->second.isAbstract && !isAbstract) {
                RaiseError(CLASSDEF_ERROR_INHERITANCE_PROPERTY_CONFLICT + property.name,
                    "Nelson:class:inheritanceConflict");
            }
            if (!inserted.second && inserted.first->second.isAbstract && !isAbstract) {
                inserted.first->second = InheritedPropertyOwner { owner, isAbstract };
            }
        }
    }
}
//=============================================================================
void
validateInheritedMethodConflicts(const ClassdefDefinition& definition)
{
    std::unordered_set<std::string> localMethodNames;
    for (const auto& method : definition.methods) {
        if (isInheritedMethodConflictCandidate(method, definition.name)) {
            localMethodNames.insert(tolowerString(method.name));
        }
    }

    std::unordered_map<std::string, std::string> ownersByName;
    for (const auto& superclass : definition.superclasses) {
        const std::string superclassName = trim(superclass);
        if (superclassName.empty() || equalsIgnoreCase(superclassName, NLS_HANDLE_STR)) {
            continue;
        }
        for (const auto& method : classdefMethodDefinitions(superclassName)) {
            if (!isInheritedMethodConflictCandidate(method, superclassName)) {
                continue;
            }
            const std::string key = tolowerString(method.name);
            if (localMethodNames.find(key) != localMethodNames.end()) {
                continue;
            }
            const std::string owner
                = inheritedMemberOwner(superclassName, method.definingClassName);
            const auto inserted = ownersByName.emplace(key, owner);
            if (!inserted.second && !equalsIgnoreCase(inserted.first->second, owner)) {
                RaiseError(CLASSDEF_ERROR_INHERITANCE_METHOD_CONFLICT + method.name,
                    "Nelson:class:inheritanceConflict");
            }
        }
    }
}
//=============================================================================
void
validateClassAttributes(const ClassdefDefinition& definition)
{
    static const char* classAttributes[]
        = { CLASSDEF_ATTR_ABSTRACT, CLASSDEF_ATTR_CONSTRUCT_ON_LOAD, CLASSDEF_ATTR_HIDDEN,
              CLASSDEF_ATTR_INFERIOR_CLASSES, CLASSDEF_ATTR_SEALED };
    static const char* propertyAttributes[] = { CLASSDEF_ATTR_ABORT_SET, CLASSDEF_ATTR_ABSTRACT,
        CLASSDEF_ATTR_ACCESS, CLASSDEF_ATTR_CONSTANT, CLASSDEF_ATTR_DEPENDENT,
        CLASSDEF_ATTR_GET_ACCESS, CLASSDEF_ATTR_GET_OBSERVABLE, CLASSDEF_ATTR_HIDDEN,
        CLASSDEF_ATTR_NON_COPYABLE, CLASSDEF_ATTR_SET_ACCESS, CLASSDEF_ATTR_SET_OBSERVABLE,
        CLASSDEF_ATTR_TRANSIENT, CLASSDEF_ATTR_WEAK_HANDLE };
    static const char* systemObjectPropertyAttributes[]
        = { CLASSDEF_ATTR_ABORT_SET, CLASSDEF_ATTR_ABSTRACT, CLASSDEF_ATTR_ACCESS,
              CLASSDEF_ATTR_CONSTANT, CLASSDEF_ATTR_DEPENDENT, CLASSDEF_ATTR_GET_ACCESS,
              CLASSDEF_ATTR_GET_OBSERVABLE, CLASSDEF_ATTR_HIDDEN, CLASSDEF_ATTR_NON_COPYABLE,
              CLASSDEF_ATTR_NONTUNABLE, CLASSDEF_ATTR_SET_ACCESS, CLASSDEF_ATTR_SET_OBSERVABLE,
              CLASSDEF_ATTR_TRANSIENT, CLASSDEF_ATTR_WEAK_HANDLE };
    static const char* methodAttributes[] = { CLASSDEF_ATTR_ABSTRACT, CLASSDEF_ATTR_ACCESS,
        CLASSDEF_ATTR_HIDDEN, CLASSDEF_ATTR_SEALED, CLASSDEF_ATTR_STATIC };
    static const char* eventAttributes[]
        = { CLASSDEF_ATTR_HIDDEN, CLASSDEF_ATTR_LISTEN_ACCESS, CLASSDEF_ATTR_NOTIFY_ACCESS };
    const bool allowSystemObjectAttributes
        = definitionIsA(definition, CLASSDEF_SYSTEM_OBJECT_CLASS);

    validateAttributes(definition.attributes, classAttributes,
        sizeof(classAttributes) / sizeof(classAttributes[0]));
    validateBooleanAttributeValues(definition.attributes);
    validateInferiorClassesAttributeValue(definition.attributes);
    for (const auto& property : definition.properties) {
        const char* const* validPropertyAttributes
            = allowSystemObjectAttributes ? systemObjectPropertyAttributes : propertyAttributes;
        const size_t validPropertyAttributeCount = allowSystemObjectAttributes
            ? sizeof(systemObjectPropertyAttributes) / sizeof(systemObjectPropertyAttributes[0])
            : sizeof(propertyAttributes) / sizeof(propertyAttributes[0]);
        validateAttributes(
            property.attributes, validPropertyAttributes, validPropertyAttributeCount);
        validatePropertyBooleanAttributeValues(property.attributes, allowSystemObjectAttributes);
        validateAccessAttributeValues(property.attributes);
    }
    for (const auto& method : definition.methods) {
        validateAttributes(method.attributes, methodAttributes,
            sizeof(methodAttributes) / sizeof(methodAttributes[0]));
        validateBooleanAttributeValues(method.attributes);
        validateAccessAttributeValues(method.attributes);
    }
    for (const auto& event : definition.events) {
        validateAttributes(event.attributes, eventAttributes,
            sizeof(eventAttributes) / sizeof(eventAttributes[0]));
        validateBooleanAttributeValues(event.attributes);
        validateAccessAttributeValues(event.attributes);
    }
    validateAttributeCombinations(definition);
    validateWeakHandleProperties(definition);
    validateDuplicateProperties(definition);
    validateDuplicateMethods(definition);
    validateDuplicateEnumerations(definition);
    validateEnumerationMemberConflicts(definition);
    if (!definition.events.empty() && !definitionIsHandleClass(definition)) {
        Error("Nelson:class:eventsRequireHandle", CLASSDEF_ERROR_EVENTS_REQUIRE_HANDLE_CLASS);
    }
    validateInheritedPropertyConflicts(definition);
    validateInheritedMethodConflicts(definition);
    for (const auto& superclass : definition.superclasses) {
        const std::string superclassName = trim(superclass);
        if (superclassName.empty() || equalsIgnoreCase(superclassName, NLS_HANDLE_STR)) {
            continue;
        }
        if (!isBuiltinBaseClassName(superclassName) && !classdefLoadClass(superclassName)) {
            RaiseError(
                CLASSDEF_ERROR_UNKNOWN_SUPERCLASS + superclassName, "Nelson:class:undefinedClass");
        }
        if (classdefIsSealedClass(superclassName)) {
            RaiseError(
                CLASSDEF_ERROR_CANNOT_INHERIT_SEALED + superclassName, "Nelson:class:sealed");
        }
    }
    for (const auto& method : definition.methods) {
        if (equalsIgnoreCase(method.name, definition.name)) {
            continue;
        }
        for (const auto& superclass : definition.superclasses) {
            const std::string superclassName = trim(superclass);
            if (!superclassName.empty() && classdefHasSealedMethod(superclassName, method.name)) {
                RaiseError(
                    CLASSDEF_ERROR_CANNOT_OVERRIDE_SEALED + method.name, "Nelson:class:sealed");
            }
        }
    }
}
//=============================================================================
void
validateWeakHandleProperties(const ClassdefDefinition& definition)
{
    for (const auto& property : definition.properties) {
        if (!attributeListContains(property.attributes, CLASSDEF_ATTR_WEAK_HANDLE)) {
            continue;
        }
        if (!definitionIsHandleClass(definition)) {
            RaiseError(CLASSDEF_ERROR_WEAK_HANDLE_REQUIRES_HANDLE_CLASS + property.name,
                "Nelson:class:weakHandleRequiresHandleClass");
        }
        if (attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
            || attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
            RaiseError(CLASSDEF_ERROR_INVALID_ATTRIBUTE_COMBINATION
                    "WeakHandle and Constant or Dependent property: "
                    + property.name,
                "Nelson:interpreter:invalidAttributeCombination");
        }
        stringVector dimensions;
        std::string typeName;
        stringVector validators;
        if (!parseValidationExpression(
                property.validationExpression, dimensions, typeName, validators)
            || typeName.empty()) {
            RaiseError(CLASSDEF_ERROR_WEAK_HANDLE_REQUIRES_CLASS_VALIDATION + property.name,
                "Nelson:class:weakHandleRequiresClassValidation");
        }
    }
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
