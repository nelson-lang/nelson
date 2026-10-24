//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <memory>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include "ClassdefParser.hpp"
#include "ClassdefMixinNames.hpp"
#include "AbstractSyntaxTree.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClassName.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "FunctionsInMemory.hpp"
#include "LexerContext.hpp"
#include "OverloadName.hpp"
#include "ParserInterface.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "StringHelpers.hpp"
#include "Types.hpp"
#include "characters_encoding.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserAttributes.hpp"
#include "ClassdefParserScanner.hpp"
#include "ClassdefParserRegistry.hpp"
#include "ClassdefParserDefinitionQuery.hpp"
#include "ClassdefParserValidation.hpp"
#include "ClassdefParserGenerator.hpp"
#include "ClassdefParserRegistration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace Nelson::classdef_detail;
//=============================================================================
stringVector
classdefMethods(const std::string& className)
{
    stringVector names;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return;
        }
        for (const auto& superclass : definition->superclasses) {
            collect(superclass);
        }
        for (const auto& method : definition->methods) {
            if ((!isBuiltinClassdef(trimmedClass) && equalsIgnoreCase(method.name, trimmedClass))
                || isPropertyAccessorMethodName(method.name)
                || !isVisibleMember(method.attributes)) {
                continue;
            }
            appendUnique(names, method.name);
        }
    };
    collect(className);

    // The class constructor (explicit and visible, or implicit) is listed,
    // superclass constructors are not.
    if (!isBuiltinClassdef(className) && classdefLoadClass(className)) {
        const ClassdefDefinition* definition = findClassdefDefinition(className);
        if (definition != nullptr
            && !attributeListContains(definition->attributes, CLASSDEF_ATTR_ABSTRACT)) {
            const ClassdefMethodDefinition* constructor = findConstructorMethod(*definition);
            if (constructor == nullptr || isVisibleMember(constructor->attributes)) {
                const std::string constructorName = classNameLeaf(definition->name);
                bool alreadyListed = false;
                for (const auto& name : names) {
                    if (equalsIgnoreCase(name, constructorName)) {
                        alreadyListed = true;
                        break;
                    }
                }
                if (!alreadyListed) {
                    names.insert(names.begin(), constructorName);
                }
            }
        }
    }

    const std::string overloadPrefix = "@" + className + "/";
    wstringVector knownMacros = PathFunctionIndexerManager::getInstance()->getMacrosList(true);
    for (const auto& macro : knownMacros) {
        std::string macroName = wstring_to_utf8(macro);
        if (macroName.rfind(overloadPrefix, 0) == 0) {
            const std::string methodName = macroName.substr(overloadPrefix.size());
            bool declared = false;
            bool visible = true;
            if (classdefLoadClass(className)) {
                const ClassdefDefinition* definition = findClassdefDefinition(className);
                if (definition != nullptr) {
                    for (const auto& method : definition->methods) {
                        if (equalsIgnoreCase(method.name, methodName)) {
                            declared = true;
                            visible = isVisibleMember(method.attributes);
                            break;
                        }
                    }
                }
            }
            if (!declared || visible) {
                appendUnique(names, methodName);
            }
        }
    }
    std::sort(names.begin(), names.end());
    names.erase(std::unique(names.begin(), names.end()), names.end());
    return names;
}
//=============================================================================
stringVector
classdefProperties(const std::string& className)
{
    stringVector names;
    for (const auto& property : classdefPropertyDefinitions(className)) {
        if (isVisibleProperty(property.attributes)) {
            appendUnique(names, property.name);
        }
    }
    return names;
}
//=============================================================================
stringVector
classdefEnumerations(const std::string& className)
{
    if (!classdefLoadClass(className)) {
        return {};
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    if (definition == nullptr) {
        return {};
    }
    return definition->enumerations;
}
//=============================================================================
stringVector
classdefEvents(const std::string& className)
{
    stringVector names;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return;
        }
        for (const auto& superclass : definition->superclasses) {
            collect(superclass);
        }
        for (const auto& event : definition->events) {
            appendUnique(names, event.name);
        }
    };
    collect(className);
    if (classdefIsHandleClass(className)) {
        appendUnique(names, CLASSDEF_EVENT_OBJECT_BEING_DESTROYED);
    }
    return names;
}
//=============================================================================
stringVector
classdefVisibleEvents(const std::string& className)
{
    stringVector names;
    for (const auto& event : classdefEventDefinitions(className)) {
        if (isVisibleEvent(event.attributes)) {
            appendUnique(names, event.name);
        }
    }
    if (classdefIsHandleClass(className)) {
        appendUnique(names, CLASSDEF_EVENT_OBJECT_BEING_DESTROYED);
    }
    return names;
}
//=============================================================================
bool
classdefCanListenEvent(
    const std::string& className, const std::string& eventName, const std::string& callerClassName)
{
    for (const auto& event : classdefEventDefinitions(className)) {
        if (!equalsIgnoreCase(event.name, eventName)) {
            continue;
        }
        std::string access
            = propertyAccessAttributeValue(event.attributes, CLASSDEF_ATTR_LISTEN_ACCESS);
        return isAccessAllowedFromClassdefContext(event.definingClassName, access, callerClassName);
    }
    return false;
}
//=============================================================================
bool
classdefCanNotifyEvent(
    const std::string& className, const std::string& eventName, const std::string& callerClassName)
{
    for (const auto& event : classdefEventDefinitions(className)) {
        if (!equalsIgnoreCase(event.name, eventName)) {
            continue;
        }
        std::string access
            = propertyAccessAttributeValue(event.attributes, CLASSDEF_ATTR_NOTIFY_ACCESS);
        return isAccessAllowedFromClassdefContext(event.definingClassName, access, callerClassName);
    }
    return false;
}
//=============================================================================
stringVector
classdefSuperclasses(const std::string& className)
{
    if (!classdefLoadClass(className)) {
        return {};
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    if (definition == nullptr) {
        return {};
    }
    stringVector names;
    for (const auto& superclass : definition->superclasses) {
        std::string name = trim(superclass);
        if (!name.empty()) {
            names.push_back(name);
        }
    }
    return names;
}
//=============================================================================
stringVector
classdefClassAttributes(const std::string& className)
{
    if (!classdefLoadClass(className)) {
        return {};
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    if (definition == nullptr) {
        return {};
    }
    return definition->attributes;
}
//=============================================================================
stringVector
classdefInferiorClasses(const std::string& className)
{
    if (!classdefLoadClass(className)) {
        return {};
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    if (definition == nullptr) {
        return {};
    }
    return classReferenceNamesFromAttributeValue(
        attributeValue(definition->attributes, CLASSDEF_ATTR_INFERIOR_CLASSES));
}
//=============================================================================
std::vector<ClassdefPropertyDefinition>
classdefPropertyDefinitions(const std::string& className)
{
    std::vector<ClassdefPropertyDefinition> properties;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return;
        }
        for (const auto& superclass : definition->superclasses) {
            collect(superclass);
        }
        for (const auto& property : definition->properties) {
            replaceOrAppendByName(properties, property);
        }
        if (!definition->enumerations.empty()) {
            ClassdefPropertyDefinition nameProperty;
            nameProperty.name = CLASSDEF_ENUM_PROPERTY_NAME;
            nameProperty.defaultExpression = CLASSDEF_ENUM_NAME_DEFAULT;
            nameProperty.definingClassName = trimmedClass;
            appendUniqueByName(properties, nameProperty);

            ClassdefPropertyDefinition ordinalProperty;
            ordinalProperty.name = CLASSDEF_ENUM_PROPERTY_ORDINAL;
            ordinalProperty.defaultExpression = CLASSDEF_ENUM_ORDINAL_DEFAULT;
            ordinalProperty.definingClassName = trimmedClass;
            appendUniqueByName(properties, ordinalProperty);
        }
    };
    collect(className);
    return properties;
}
//=============================================================================
std::vector<ClassdefMethodDefinition>
classdefMethodDefinitions(const std::string& className)
{
    std::vector<ClassdefMethodDefinition> methods;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return;
        }
        for (const auto& superclass : definition->superclasses) {
            collect(superclass);
        }
        for (auto method : definition->methods) {
            if (equalsClassNameOrLeaf(method.name, trimmedClass)
                || isPropertyAccessorMethodName(method.name)) {
                continue;
            }
            if (method.definingClassName.empty()) {
                method.definingClassName = trimmedClass;
            }
            replaceOrAppendByName(methods, method);
        }
    };
    collect(className);
    return methods;
}
//=============================================================================
std::vector<ClassdefEventDefinition>
classdefEventDefinitions(const std::string& className)
{
    std::vector<ClassdefEventDefinition> events;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return;
        }
        for (const auto& superclass : definition->superclasses) {
            collect(superclass);
        }
        for (const auto& eventDefinition : definition->events) {
            ClassdefEventDefinition event;
            event.name = eventDefinition.name;
            event.definingClassName = eventDefinition.definingClassName.empty()
                ? trimmedClass
                : eventDefinition.definingClassName;
            event.attributes = eventDefinition.attributes;
            appendUniqueByName(events, event);
        }
    };
    collect(className);
    if (classdefIsHandleClass(className)) {
        ClassdefEventDefinition destroyedEvent;
        destroyedEvent.name = CLASSDEF_EVENT_OBJECT_BEING_DESTROYED;
        destroyedEvent.definingClassName = NLS_HANDLE_STR;
        appendUniqueByName(events, destroyedEvent);
    }
    return events;
}
//=============================================================================
std::vector<ClassdefEnumerationDefinition>
classdefEnumerationDefinitions(const std::string& className)
{
    std::vector<ClassdefEnumerationDefinition> enumerations;
    if (!classdefLoadClass(className)) {
        return enumerations;
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    if (definition == nullptr) {
        return enumerations;
    }
    for (const auto& memberName : definition->enumerations) {
        ClassdefEnumerationDefinition member;
        member.name = memberName;
        member.definingClassName = className;
        const auto argumentsIt = definition->enumerationArguments.find(memberName);
        member.arguments = argumentsIt == definition->enumerationArguments.end()
            ? std::string()
            : argumentsIt->second;
        appendUniqueByName(enumerations, member);
    }
    return enumerations;
}
//=============================================================================
static ClassdefResolvedProperty
resolveClassdefProperty(const ClassdefPropertyDefinition& property)
{
    ClassdefResolvedProperty resolved;
    resolved.definition = property;
    resolved.isConstant = attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT);
    resolved.isDependent = attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT);
    resolved.isGetObservable
        = attributeListContains(property.attributes, CLASSDEF_ATTR_GET_OBSERVABLE);
    resolved.isSetObservable
        = attributeListContains(property.attributes, CLASSDEF_ATTR_SET_OBSERVABLE);
    resolved.getAccess
        = propertyAccessAttributeValue(property.attributes, CLASSDEF_ATTR_GET_ACCESS);
    resolved.setAccess
        = propertyAccessAttributeValue(property.attributes, CLASSDEF_ATTR_SET_ACCESS);
    return resolved;
}
//=============================================================================
const ClassdefResolvedProperty*
classdef_detail::findClassdefResolvedProperty(
    const std::string& className, const std::string& propertyName)
{
    // classdefPropertyDefinitions walks the inheritance chain and copies every
    // definition; a single property read or write asks for its definition
    // several times (existence, access, Dependent, Constant, observability).
    // The merged table only depends on the classdef registry, so it is built
    // once per loaded class and dropped when the registry generation moves.
    struct ClassProperties
    {
        std::vector<ClassdefResolvedProperty> properties;
        std::unordered_map<std::string, size_t> indexByName;
    };
    static std::unordered_map<std::string, ClassProperties> cache;
    static size_t cacheGeneration = 0;
    if (cacheGeneration != classdefGenerationCounter()) {
        cache.clear();
        cacheGeneration = classdefGenerationCounter();
    }
    auto classIt = cache.find(className);
    if (classIt == cache.end()) {
        // an unloadable class has no property; the miss is not memoized since
        // the class may appear on the path without the generation moving
        if (!classdefLoadClass(trim(className))) {
            return nullptr;
        }
        ClassProperties entry;
        for (const auto& property : classdefPropertyDefinitions(className)) {
            // Property names are case-sensitive, so members differing only in
            // case (for example a private 'foo' and a dependent 'Foo') resolve
            // to distinct entries. First exact-name match wins, preserving the
            // usual subclass-shadows-superclass behaviour.
            if (entry.indexByName.emplace(property.name, entry.properties.size()).second) {
                entry.properties.push_back(resolveClassdefProperty(property));
            }
        }
        if (cacheGeneration != classdefGenerationCounter()) {
            // building the table loaded classes: earlier entries are stale
            cache.clear();
            cacheGeneration = classdefGenerationCounter();
        }
        classIt = cache.emplace(className, std::move(entry)).first;
    }
    const ClassProperties& entry = classIt->second;
    auto it = entry.indexByName.find(propertyName);
    return it == entry.indexByName.end() ? nullptr : &entry.properties[it->second];
}
//=============================================================================
bool
classdefPropertyDefinition(const std::string& className, const std::string& propertyName,
    ClassdefPropertyDefinition& property)
{
    const ClassdefResolvedProperty* found = findClassdefResolvedProperty(className, propertyName);
    if (found == nullptr) {
        return false;
    }
    property = found->definition;
    return true;
}
//=============================================================================
bool
classdefPropertyHasAttribute(
    const ClassdefPropertyDefinition& property, const std::string& attributeName)
{
    return attributeListContains(property.attributes, attributeName);
}
//=============================================================================
bool
classdefIsWeakHandleProperty(const std::string& className, const std::string& propertyName)
{
    static std::unordered_map<std::string, std::unordered_set<std::string>> cache;
    static size_t cacheGeneration = 0;
    if (cacheGeneration != classdefGenerationCounter()) {
        cache.clear();
        cacheGeneration = classdefGenerationCounter();
    }
    auto it = cache.find(className);
    if (it == cache.end()) {
        std::unordered_set<std::string> names;
        for (const auto& property : classdefPropertyDefinitions(className)) {
            if (classdefPropertyHasAttribute(property, CLASSDEF_ATTR_WEAK_HANDLE)) {
                names.insert(property.name);
            }
        }
        it = cache.emplace(className, std::move(names)).first;
    }
    if (propertyName.empty()) {
        return !it->second.empty();
    }
    return it->second.find(propertyName) != it->second.end();
}
//=============================================================================
static bool
expressionReferencesIdentifier(const std::string& expression, const std::string& identifier)
{
    if (identifier.empty()) {
        return false;
    }
    size_t pos = expression.find(identifier);
    while (pos != std::string::npos) {
        const bool leftBoundary = pos == 0
            || (!isalnum(static_cast<unsigned char>(expression[pos - 1]))
                && expression[pos - 1] != '_' && expression[pos - 1] != '.');
        const size_t end = pos + identifier.size();
        const bool rightBoundary = end >= expression.size()
            || (!isalnum(static_cast<unsigned char>(expression[end])) && expression[end] != '_');
        if (leftBoundary && rightBoundary) {
            return true;
        }
        pos = expression.find(identifier, pos + 1);
    }
    return false;
}
//=============================================================================
ArrayOfVector
evaluateValidationArgumentExpressions(Evaluator* eval, const stringVector& argumentExpressions,
    const ClassdefPropertyDefinition& property, const ArrayOf& propertyValue)
{
    ArrayOfVector values;
    if (argumentExpressions.empty()) {
        return values;
    }
    // The property name is in scope inside validator argument expressions and
    // is bound to the value being validated: C {mustBeMember(C, {'a', 'b'})}.
    const bool bindPropertyName
        = !property.name.empty() && parseLeadingIdentifier(property.name) == property.name;
    std::string source = "function [";
    for (size_t k = 0; k < argumentExpressions.size(); ++k) {
        if (k != 0) {
            source += ", ";
        }
        source += "arg" + std::to_string(k + 1);
    }
    source += "] = __classdef_validation_arguments__(";
    if (bindPropertyName) {
        source += property.name;
    }
    source += ")\n";
    for (size_t k = 0; k < argumentExpressions.size(); ++k) {
        source += "arg" + std::to_string(k + 1) + " = " + argumentExpressions[k] + ";\n";
    }
    source += "end\n";
    std::unique_ptr<MacroFunctionDef> functionDef(parseGeneratedFunction(source, L""));
    if (functionDef == nullptr) {
        RaiseError(propertyValidationFailureMessage(property),
            "Nelson:interpreter:propertyValidationFailure");
    }
    ArrayOfVector inputs;
    if (bindPropertyName) {
        inputs << propertyValue;
    }
    return functionDef->evaluateFunction(
        eval, inputs, static_cast<int>(argumentExpressions.size()));
}
//=============================================================================
ArrayOf
classdefValidatePropertyValue(
    Evaluator* eval, const ClassdefPropertyDefinition& property, const ArrayOf& value)
{
    struct ParsedValidation
    {
        stringVector dimensions;
        std::string typeName;
        stringVector validators;
        bool valid = false;
    };
    // validation expressions are parsed on every property assignment; the parse
    // is a pure function of the expression text, so memoize it
    static std::unordered_map<std::string, ParsedValidation> parsedValidationCache;
    auto cached = parsedValidationCache.find(property.validationExpression);
    if (cached == parsedValidationCache.end()) {
        ParsedValidation parsed;
        parsed.valid = parseValidationExpression(
            property.validationExpression, parsed.dimensions, parsed.typeName, parsed.validators);
        cached
            = parsedValidationCache.emplace(property.validationExpression, std::move(parsed)).first;
    }
    ArrayOf convertedValue = value;
    if (!cached->second.valid) {
        return convertedValue;
    }
    const stringVector& dimensions = cached->second.dimensions;
    const std::string& typeName = cached->second.typeName;
    const stringVector& validators = cached->second.validators;
    if (!validationTypeMatches(typeName, convertedValue)) {
        RaiseError(propertyValidationFailureMessage(property),
            "Nelson:interpreter:propertyValidationFailure");
    }
    if (!validationDimensionsMatch(dimensions, convertedValue)
        && (!expandScalarToValidationDimensions(dimensions, convertedValue)
            || !validationDimensionsMatch(dimensions, convertedValue))) {
        RaiseError(propertyValidationFailureMessage(property),
            "Nelson:interpreter:propertyValidationFailure");
    }
    if (validators.empty()) {
        return convertedValue;
    }
    if (eval == nullptr || eval->getContext() == nullptr) {
        RaiseError(propertyValidationFailureMessage(property),
            "Nelson:interpreter:propertyValidationFailure");
    }
    for (const std::string& validator : validators) {
        std::string functionName;
        stringVector argumentExpressions;
        if (!parseValidatorCall(validator, functionName, argumentExpressions)) {
            RaiseError(propertyValidationFailureMessage(property),
                "Nelson:interpreter:propertyValidationFailure");
        }
        FunctionDefPtr functionDef = nullptr;
        if (!eval->getContext()->lookupFunction(functionName, functionDef)
            || functionDef == nullptr) {
            RaiseError(propertyValidationFailureMessage(property),
                "Nelson:interpreter:propertyValidationFailure");
        }
        // When the property name appears in the argument list, the validator is
        // called exactly as written with the name bound to the candidate value.
        // Otherwise the value is passed implicitly as first input.
        bool referencesProperty = false;
        for (const auto& expression : argumentExpressions) {
            if (expressionReferencesIdentifier(expression, property.name)) {
                referencesProperty = true;
                break;
            }
        }
        ArrayOfVector inputs;
        if (!referencesProperty) {
            inputs << convertedValue;
        }
        ArrayOfVector arguments = evaluateValidationArgumentExpressions(
            eval, argumentExpressions, property, convertedValue);
        for (const auto& argument : arguments) {
            inputs << argument;
        }
        functionDef->evaluateFunction(eval, inputs, 0);
    }
    return convertedValue;
}
//=============================================================================
bool
classdefIsValidPropertyValidationExpression(const std::string& validationExpression)
{
    stringVector dimensions;
    std::string typeName;
    stringVector validators;
    return parseValidationExpression(validationExpression, dimensions, typeName, validators);
}
//=============================================================================
std::string
classdefBuiltinBaseType(const std::string& className)
{
    if (!classdefLoadClass(className)) {
        return {};
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    return definition == nullptr ? std::string() : builtinBaseTypeName(*definition);
}
//=============================================================================
std::string
classdefBuiltinBasePropertyName()
{
    return CLASSDEF_BUILTIN_BASE_PROPERTY;
}
//=============================================================================
bool
classdefCanCallConstructor(const std::string& className, const std::string& callerClassName)
{
    if (!classdefLoadClass(className)) {
        return true;
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    if (definition == nullptr) {
        return true;
    }
    const ClassdefMethodDefinition* constructor = findConstructorMethod(*definition);
    if (constructor == nullptr) {
        return true;
    }
    return isCallableFromClassdefContext(
        definition->name, constructor->attributes, callerClassName);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
