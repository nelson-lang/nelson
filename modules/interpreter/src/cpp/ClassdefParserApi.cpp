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
ClassdefLayoutPtr
classdefLayout(const std::string& className)
{
    if (!classdefLoadClass(className)) {
        return nullptr;
    }
    const auto layoutIt = classdefLayouts().find(className);
    if (layoutIt != classdefLayouts().end()) {
        return layoutIt->second;
    }
    ClassdefLayoutPtr layout = buildClassdefLayout(className);
    classdefLayouts()[className] = layout;
    return layout;
}
//=============================================================================
bool
classdefIsBuiltinClass(const std::string& className)
{
    return isBuiltinClassdef(className);
}
//=============================================================================
static bool
classdefHasMethodUncached(const std::string& className, const std::string& methodName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return false;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return false;
        }
        for (const auto& method : definition->methods) {
            if (!method.isStatic && equalsIgnoreCase(method.name, methodName)) {
                return !isPropertyAccessorMethodName(method.name)
                    && isVisibleMember(method.attributes);
            }
        }
        if (functionExistsInMemoryOrPath(getOverloadFunctionName(trimmedClass, methodName))) {
            return true;
        }
        for (const auto& superclass : definition->superclasses) {
            if (visit(superclass)) {
                return true;
            }
        }
        return false;
    };
    return visit(className);
}
//=============================================================================
bool
classdefHasMethod(const std::string& className, const std::string& methodName)
{
    // Walks the inheritance chain and probes the function tables and the path
    // per step; hot dispatch paths ask this on every call. The answer only
    // depends on the classdef registry and the resolvable functions, so both
    // outcomes are memoized against the two generations (any path rehash bumps
    // the function-tables generation).
    static std::unordered_map<std::string, bool> cache;
    static size_t cacheClassdefGeneration = 0;
    static uint64 cacheFunctionsGeneration = 0;
    const size_t classdefGeneration = classdefGenerationCounter();
    const uint64 functionsGeneration = FunctionsInMemory::getInstance()->generation();
    if (cacheClassdefGeneration != classdefGeneration
        || cacheFunctionsGeneration != functionsGeneration) {
        cache.clear();
        cacheClassdefGeneration = classdefGeneration;
        cacheFunctionsGeneration = functionsGeneration;
    }
    std::string key;
    key.reserve(className.size() + methodName.size() + 1);
    key += className;
    key += '\x01';
    key += methodName;
    auto it = cache.find(key);
    if (it != cache.end()) {
        return it->second;
    }
    bool result = classdefHasMethodUncached(className, methodName);
    cache.emplace(std::move(key), result);
    return result;
}
//=============================================================================
static bool
classdefHasMethodDefinitionUncached(const std::string& className, const std::string& methodName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return false;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return false;
        }
        for (const auto& method : definition->methods) {
            if (!method.isStatic && equalsIgnoreCase(method.name, methodName)) {
                return !isPropertyAccessorMethodName(method.name);
            }
        }
        for (const auto& superclass : definition->superclasses) {
            if (visit(superclass)) {
                return true;
            }
        }
        return false;
    };
    return visit(className);
}
//=============================================================================
bool
classdefHasMethodDefinition(const std::string& className, const std::string& methodName)
{
    // registry-only walk: both outcomes are stable until the registry moves
    static std::unordered_map<std::string, bool> cache;
    static size_t cacheClassdefGeneration = 0;
    const size_t classdefGeneration = classdefGenerationCounter();
    if (cacheClassdefGeneration != classdefGeneration) {
        cache.clear();
        cacheClassdefGeneration = classdefGeneration;
    }
    std::string key;
    key.reserve(className.size() + methodName.size() + 1);
    key += className;
    key += '\x01';
    key += methodName;
    auto it = cache.find(key);
    if (it != cache.end()) {
        return it->second;
    }
    bool result = classdefHasMethodDefinitionUncached(className, methodName);
    cache.emplace(std::move(key), result);
    return result;
}
//=============================================================================
bool
classdefHasStaticMethod(const std::string& className, const std::string& methodName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return false;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return false;
        }
        for (const auto& method : definition->methods) {
            if (method.isStatic && equalsIgnoreCase(method.name, methodName)
                && isVisibleMember(method.attributes)) {
                return true;
            }
        }
        for (const auto& superclass : definition->superclasses) {
            if (visit(superclass)) {
                return true;
            }
        }
        return false;
    };
    return visit(className);
}
//=============================================================================
bool
classdefHasProperty(const std::string& className, const std::string& propertyName)
{
    return findClassdefResolvedProperty(className, propertyName) != nullptr;
}
//=============================================================================
bool
classdefHasExactProperty(const std::string& className, const std::string& propertyName)
{
    const ClassdefResolvedProperty* property
        = findClassdefResolvedProperty(className, propertyName);
    return property != nullptr && property->definition.name == propertyName;
}
//=============================================================================
bool
classdefHasConstantProperty(const std::string& className, const std::string& propertyName)
{
    const ClassdefResolvedProperty* property
        = findClassdefResolvedProperty(className, propertyName);
    return property != nullptr && property->isConstant;
}
//=============================================================================
bool
classdefHasDependentProperty(const std::string& className, const std::string& propertyName)
{
    const ClassdefResolvedProperty* property
        = findClassdefResolvedProperty(className, propertyName);
    return property != nullptr && property->isDependent;
}
//=============================================================================
bool
classdefIsObservablePropertyEvent(
    const std::string& className, const std::string& propertyName, const std::string& eventName)
{
    const ClassdefResolvedProperty* property
        = findClassdefResolvedProperty(className, propertyName);
    if (property == nullptr) {
        return false;
    }
    if (equalsIgnoreCase(eventName, CLASSDEF_EVENT_PRE_GET)
        || equalsIgnoreCase(eventName, CLASSDEF_EVENT_POST_GET)) {
        return property->isGetObservable;
    }
    if (equalsIgnoreCase(eventName, CLASSDEF_EVENT_PRE_SET)
        || equalsIgnoreCase(eventName, CLASSDEF_EVENT_POST_SET)) {
        return property->isSetObservable;
    }
    return false;
}
//=============================================================================
bool
classdefCanGetProperty(const std::string& className, const std::string& propertyName,
    const std::string& callerClassName)
{
    const ClassdefResolvedProperty* property
        = findClassdefResolvedProperty(className, propertyName);
    if (property == nullptr) {
        return false;
    }
    // copied: classdefIsA (protected / class-list access) may load classes
    // and invalidate the cache
    const std::string definingClassName = property->definition.definingClassName;
    const std::string getAccess = property->getAccess;
    return isAccessAllowedFromClassdefContext(definingClassName, getAccess, callerClassName);
}
//=============================================================================
bool
classdefCanSetProperty(const std::string& className, const std::string& propertyName,
    const std::string& callerClassName, bool propertyAlreadyExists, Evaluator* eval)
{
    const ClassdefResolvedProperty* resolved
        = findClassdefResolvedProperty(className, propertyName);
    if (resolved == nullptr) {
        return false;
    }
    if (!propertyAlreadyExists) {
        return true;
    }
    if (resolved->isConstant) {
        return false;
    }
    // copied: classdefIsA below may load classes and invalidate the cache
    const std::string definingClassName = resolved->definition.definingClassName;
    const std::string setAccess = resolved->setAccess;
    if (equalsIgnoreCase(setAccess, CLASSDEF_ACCESS_IMMUTABLE)) {
        // Immutable: settable only while the defining class is being
        // constructed (its constructor, or a subclass constructor through the
        // @Super chain), read-only afterwards.
        return eval != nullptr && eval->isConstructingClass(definingClassName);
    }
    return isAccessAllowedFromClassdefContext(definingClassName, setAccess, callerClassName);
}
//=============================================================================
bool
classdefIsA(const std::string& className, const std::string& expectedClassName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        if (equalsIgnoreCase(trimmedClass, expectedClassName)) {
            return true;
        }
        if (equalsIgnoreCase(trimmedClass, CLASSDEF_DYNAMICPROPS_STR)
            && equalsIgnoreCase(expectedClassName, NLS_HANDLE_STR)) {
            return true;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return false;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return false;
        }
        for (const auto& superclass : definition->superclasses) {
            if (visit(superclass)) {
                return true;
            }
        }
        return false;
    };
    return visit(className);
}
//=============================================================================
bool
classdefIsHandleClass(const std::string& className)
{
    return classdefIsA(className, NLS_HANDLE_STR);
}
//=============================================================================
bool
classdefIsAbstractClass(const std::string& className)
{
    if (!classdefLoadClass(className)) {
        return false;
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    return definition != nullptr
        && (attributeListContains(definition->attributes, CLASSDEF_ATTR_ABSTRACT)
            || hasUnimplementedAbstractMethods(*definition)
            || hasUnimplementedAbstractProperties(*definition));
}
//=============================================================================
bool
classdefIsSealedClass(const std::string& className)
{
    if (!classdefLoadClass(className)) {
        return false;
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    return definition != nullptr
        && attributeListContains(definition->attributes, CLASSDEF_ATTR_SEALED);
}
//=============================================================================
bool
classdefHasSealedMethod(const std::string& className, const std::string& methodName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return false;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return false;
        }
        for (const auto& method : definition->methods) {
            if (equalsIgnoreCase(method.name, methodName)
                && attributeListContains(method.attributes, CLASSDEF_ATTR_SEALED)) {
                return true;
            }
        }
        for (const auto& superclass : definition->superclasses) {
            if (visit(superclass)) {
                return true;
            }
        }
        return false;
    };
    return visit(className);
}
//=============================================================================
static bool
classdefResolveMethodFunctionUncached(const std::string& className, const std::string& methodName,
    std::string& functionName, const std::string& callerClassName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);

        std::string candidate = getOverloadFunctionName(trimmedClass, methodName);
        if (!classdefLoadClass(trimmedClass)) {
            if (functionExistsInMemoryOrPath(candidate)) {
                functionName = candidate;
                return true;
            }
            return false;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return false;
        }
        for (const auto& method : definition->methods) {
            if (!method.isStatic && equalsIgnoreCase(method.name, methodName)) {
                if (isCallableFromClassdefContext(trimmedClass, method.attributes, callerClassName)
                    && functionExistsInMemoryOrPath(candidate)) {
                    functionName = candidate;
                    return true;
                }
                return false;
            }
        }
        // get.X/set.X accessors exist only as classdef methods; when the definition
        // does not declare one, probing the path would stat the filesystem per call
        bool undeclaredAccessor = isPropertyAccessorMethodName(methodName);
        if (undeclaredAccessor ? functionExistsInMemoryOnly(candidate)
                               : functionExistsInMemoryOrPath(candidate)) {
            functionName = candidate;
            return true;
        }
        for (const auto& superclass : definition->superclasses) {
            if (visit(superclass)) {
                return true;
            }
        }
        return false;
    };
    functionName.clear();
    return visit(className);
}
//=============================================================================
bool
classdefResolveMethodFunction(const std::string& className, const std::string& methodName,
    std::string& functionName, const std::string& callerClassName)
{
    // The inheritance walk is costly and method dispatch repeats it on every
    // call. Results depend only on the classdef registry and the function
    // tables, so they are memoized against both generations. Misses are cached
    // too (empty name): they are common for operators probing an inherited
    // overload, and path index changes bump the function tables generation.
    static std::unordered_map<std::string, std::string> cache;
    static size_t cacheClassdefGeneration = 0;
    static uint64 cacheFunctionsGeneration = 0;
    const size_t classdefGeneration = classdefGenerationCounter();
    const uint64 functionsGeneration = FunctionsInMemory::getInstance()->generation();
    if (cacheClassdefGeneration != classdefGeneration
        || cacheFunctionsGeneration != functionsGeneration) {
        cache.clear();
        cacheClassdefGeneration = classdefGeneration;
        cacheFunctionsGeneration = functionsGeneration;
    }
    std::string key;
    key.reserve(className.size() + methodName.size() + callerClassName.size() + 2);
    key += className;
    key += '\x01';
    key += methodName;
    key += '\x01';
    key += callerClassName;
    auto it = cache.find(key);
    if (it != cache.end()) {
        functionName = it->second;
        return !functionName.empty();
    }
    const bool found = classdefResolveMethodFunctionUncached(
        className, methodName, functionName, callerClassName);
    cache.emplace(std::move(key), found ? functionName : std::string());
    return found;
}
//=============================================================================
static bool
classdefResolveStaticMethodFunctionUncached(const std::string& className,
    const std::string& methodName, std::string& functionName, const std::string& callerClassName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return false;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return false;
        }
        for (const auto& method : definition->methods) {
            if (method.isStatic && equalsIgnoreCase(method.name, methodName)
                && isCallableFromClassdefContext(
                    trimmedClass, method.attributes, callerClassName)) {
                std::string candidate = trimmedClass + "." + method.name;
                if (functionExistsInMemoryOrPath(candidate)) {
                    functionName = candidate;
                    return true;
                }
                candidate = getOverloadFunctionName(trimmedClass, method.name);
                if (functionExistsInMemoryOrPath(candidate)) {
                    functionName = candidate;
                    return true;
                }
            }
        }
        if (equalsIgnoreCase(methodName, CLASSDEF_FUNCTION_EMPTY)) {
            const std::string candidate = trimmedClass + "." + CLASSDEF_FUNCTION_EMPTY;
            if (functionExistsInMemoryOrPath(candidate)) {
                functionName = candidate;
                return true;
            }
        }
        for (const auto& superclass : definition->superclasses) {
            if (visit(superclass)) {
                return true;
            }
        }
        return false;
    };
    functionName.clear();
    return visit(className);
}
//=============================================================================
bool
classdefResolveStaticMethodFunction(const std::string& className, const std::string& methodName,
    std::string& functionName, const std::string& callerClassName)
{
    // Same memoization as classdefResolveMethodFunction: static calls with an
    // object argument re-resolve at run time on every call.
    static std::unordered_map<std::string, std::string> cache;
    static size_t cacheClassdefGeneration = 0;
    static uint64 cacheFunctionsGeneration = 0;
    const size_t classdefGeneration = classdefGenerationCounter();
    const uint64 functionsGeneration = FunctionsInMemory::getInstance()->generation();
    if (cacheClassdefGeneration != classdefGeneration
        || cacheFunctionsGeneration != functionsGeneration) {
        cache.clear();
        cacheClassdefGeneration = classdefGeneration;
        cacheFunctionsGeneration = functionsGeneration;
    }
    std::string key;
    key.reserve(className.size() + methodName.size() + callerClassName.size() + 2);
    key += className;
    key += '\x01';
    key += methodName;
    key += '\x01';
    key += callerClassName;
    auto it = cache.find(key);
    if (it != cache.end()) {
        functionName = it->second;
        return true;
    }
    if (classdefResolveStaticMethodFunctionUncached(
            className, methodName, functionName, callerClassName)) {
        cache.emplace(std::move(key), functionName);
        return true;
    }
    return false;
}
//=============================================================================
bool
classdefResolveConstantPropertyFunction(const std::string& className,
    const std::string& propertyName, std::string& functionName, const std::string& callerClassName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!classdefLoadClass(trimmedClass)) {
            return false;
        }
        const ClassdefDefinition* definition = findClassdefDefinition(trimmedClass);
        if (definition == nullptr) {
            return false;
        }
        for (const auto& property : definition->properties) {
            if (equalsIgnoreCase(property.name, propertyName)
                && attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
                && isAccessAllowedFromClassdefContext(trimmedClass,
                    propertyAccessAttributeValue(property.attributes, CLASSDEF_ATTR_GET_ACCESS),
                    callerClassName)) {
                std::string candidate = trimmedClass + "." + property.name;
                if (functionExistsInMemoryOrPath(candidate)) {
                    functionName = candidate;
                    return true;
                }
            }
        }
        for (const auto& superclass : definition->superclasses) {
            if (visit(superclass)) {
                return true;
            }
        }
        return false;
    };
    functionName.clear();
    return visit(className);
}
//=============================================================================
bool
classdefResolvePropertyAccessorFunction(const std::string& className,
    const std::string& propertyName, const std::string& accessorPrefix, std::string& functionName)
{
    ClassdefPropertyDefinition property;
    if (!classdefPropertyDefinition(className, propertyName, property)) {
        functionName.clear();
        return false;
    }
    return classdefResolveMethodFunction(className, accessorPrefix + "." + property.name,
        functionName, property.definingClassName.empty() ? className : property.definingClassName);
}
//=============================================================================
bool
classdefResolveEnumerationMemberFunction(
    const std::string& className, const std::string& memberName, std::string& functionName)
{
    if (!classdefLoadClass(className)) {
        functionName.clear();
        return false;
    }
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    if (definition == nullptr) {
        functionName.clear();
        return false;
    }
    for (const auto& member : definition->enumerations) {
        if (equalsIgnoreCase(member, memberName)) {
            std::string candidate = className + "." + member;
            if (functionExistsInMemoryOrPath(candidate)) {
                functionName = candidate;
                return true;
            }
        }
    }
    functionName.clear();
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
