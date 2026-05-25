//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <unordered_map>
#include <vector>
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
#define CLASSDEF_SERIALIZATION_HANDLE_FIELD "__nelson_classdef_handle__"
#define CLASSDEF_SERIALIZATION_NAME_FIELD "__nelson_classdef_name__"
//=============================================================================
namespace Nelson {
//=============================================================================
struct ClassdefPropertyDefinition
{
    std::string name;
    std::string defaultExpression;
    std::string definingClassName;
    stringVector attributes;
};
//=============================================================================
struct ClassdefMethodDefinition
{
    std::string name;
    std::string source;
    std::string definingClassName;
    stringVector attributes;
    size_t sourceStartLine = 0;
    bool isStatic = false;
};
//=============================================================================
struct ClassdefEventDefinition
{
    std::string name;
    std::string definingClassName;
};
//=============================================================================
struct ClassdefEnumerationDefinition
{
    std::string name;
    std::string definingClassName;
    std::string arguments;
};
//=============================================================================
struct ClassdefDefinition
{
    std::string name;
    stringVector attributes;
    stringVector superclasses;
    std::vector<ClassdefPropertyDefinition> properties;
    std::vector<ClassdefMethodDefinition> methods;
    stringVector events;
    stringVector enumerations;
    std::unordered_map<std::string, std::string> enumerationArguments;
};
//=============================================================================
NLSINTERPRETER_IMPEXP bool
classdefLooksLikeSource(const std::string& source);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
parseClassdefSource(
    const std::string& source, ClassdefDefinition& definition, std::string& errorMessage);
//=============================================================================
class NLSINTERPRETER_IMPEXP ClassdefDefinitionManager
{
private:
    std::unordered_map<std::string, ClassdefDefinition> _definitions;
    std::unordered_map<std::string, std::wstring> _filenames;

    ClassdefDefinitionManager() = default;
    ~ClassdefDefinitionManager() = default;

public:
    ClassdefDefinitionManager(const ClassdefDefinitionManager&) = delete;
    ClassdefDefinitionManager&
    operator=(const ClassdefDefinitionManager&)
        = delete;

    static ClassdefDefinitionManager*
    getInstance();

    void
    clear();

    bool
    isClassdefFile(const std::wstring& filename);

    FunctionDef*
    loadConstructor(const std::wstring& filename, const std::string& expectedClassName);

    bool
    loadClass(const std::string& className);

    bool
    hasMethod(const std::string& className, const std::string& methodName);

    bool
    hasStaticMethod(const std::string& className, const std::string& methodName);

    bool
    hasProperty(const std::string& className, const std::string& propertyName);

    bool
    hasConstantProperty(const std::string& className, const std::string& propertyName);

    bool
    hasDependentProperty(const std::string& className, const std::string& propertyName);

    bool
    isObservablePropertyEvent(const std::string& className, const std::string& propertyName,
        const std::string& eventName);

    bool
    canGetProperty(const std::string& className, const std::string& propertyName,
        const std::string& callerClassName);

    bool
    canSetProperty(const std::string& className, const std::string& propertyName,
        const std::string& callerClassName, bool propertyAlreadyExists);

    bool
    isA(const std::string& className, const std::string& expectedClassName);

    bool
    isHandleClass(const std::string& className);

    bool
    isAbstractClass(const std::string& className);

    bool
    isSealedClass(const std::string& className);

    bool
    hasSealedMethod(const std::string& className, const std::string& methodName);

    bool
    resolveMethodFunction(const std::string& className, const std::string& methodName,
        std::string& functionName, const std::string& callerClassName = std::string());

    bool
    resolveStaticMethodFunction(const std::string& className, const std::string& methodName,
        std::string& functionName, const std::string& callerClassName = std::string());

    bool
    resolveConstantPropertyFunction(const std::string& className, const std::string& propertyName,
        std::string& functionName, const std::string& callerClassName = std::string());

    bool
    resolvePropertyAccessorFunction(const std::string& className, const std::string& propertyName,
        const std::string& accessorPrefix, std::string& functionName);

    bool
    resolveEnumerationMemberFunction(
        const std::string& className, const std::string& memberName, std::string& functionName);

    stringVector
    methods(const std::string& className);

    stringVector
    properties(const std::string& className);

    stringVector
    enumerations(const std::string& className);

    stringVector
    events(const std::string& className);

    stringVector
    superclasses(const std::string& className);

    stringVector
    classAttributes(const std::string& className);

    std::vector<ClassdefPropertyDefinition>
    propertyDefinitions(const std::string& className);

    std::vector<ClassdefMethodDefinition>
    methodDefinitions(const std::string& className);

    std::vector<ClassdefEventDefinition>
    eventDefinitions(const std::string& className);

    std::vector<ClassdefEnumerationDefinition>
    enumerationDefinitions(const std::string& className);

    bool
    propertyDefinition(const std::string& className, const std::string& propertyName,
        ClassdefPropertyDefinition& property);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
