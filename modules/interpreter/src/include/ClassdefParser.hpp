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
#include "ClassdefLayout.hpp"
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
#define CLASSDEF_SERIALIZATION_HANDLE_FIELD "__nelson_classdef_handle__"
#define CLASSDEF_SERIALIZATION_NAME_FIELD "__nelson_classdef_name__"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
struct ClassdefPropertyDefinition
{
    std::string name;
    std::string defaultExpression;
    std::string validationExpression;
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
    stringVector attributes;
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
    std::vector<ClassdefEventDefinition> events;
    stringVector enumerations;
    std::unordered_map<std::string, std::string> enumerationArguments;
    std::string localFunctionsSource;
};
//=============================================================================
NLSINTERPRETER_IMPEXP bool
classdefLooksLikeSource(const std::string& source);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
parseClassdefSource(
    const std::string& source, ClassdefDefinition& definition, std::string& errorMessage);
//=============================================================================
// Deterministic structural rendering of the parsed classdef skeleton, used to
// compare the native front-end with the legacy parser.
NLSINTERPRETER_IMPEXP std::string
classdefCanonicalDefinitionDump(const std::string& source);
//=============================================================================
NLSINTERPRETER_IMPEXP std::string
classdefCanonicalDefinitionDumpFromDefinition(const ClassdefDefinition& definition);
//=============================================================================
NLSINTERPRETER_IMPEXP void
classdefClear();
NLSINTERPRETER_IMPEXP void
classdefRegisterBuiltinClass(const std::string& className, const stringVector& properties,
    const stringVector& methods, const stringVector& staticMethods, bool readOnlyProperties = false,
    const stringVector& events = {}, const stringVector& superclasses = {});
NLSINTERPRETER_IMPEXP bool
classdefIsClassdefFile(const std::wstring& filename);
NLSINTERPRETER_IMPEXP FunctionDef*
classdefLoadConstructor(const std::wstring& filename, const std::string& expectedClassName);
NLSINTERPRETER_IMPEXP bool
classdefLoadClass(const std::string& className);
// bumped whenever a classdef definition is registered, replaced or cleared;
// lets callers invalidate caches keyed on class definitions
NLSINTERPRETER_IMPEXP size_t
classdefRegistryGeneration();
// Property default expressions evaluate once per defining class; every
// instance then shares the stored value until that class definition is
// cleared or reloaded. Returns false while no value has been stored yet.
NLSINTERPRETER_IMPEXP bool
classdefStoredPropertyDefault(
    const std::string& className, const std::string& propertyName, ArrayOf& value);
NLSINTERPRETER_IMPEXP void
classdefStorePropertyDefault(
    const std::string& className, const std::string& propertyName, const ArrayOf& value);
// returns the cached definition of a stored property that direct read/write
// paths may access without dispatch: no attributes and no get.X (read) or
// set.X (write) accessor; nullptr otherwise. The pointer stays valid until a
// class definition changes - use immediately, do not retain.
NLSINTERPRETER_IMPEXP const ClassdefPropertyDefinition*
classdefPlainStoredProperty(
    const std::string& className, const std::string& propertyName, bool forWrite);
// same, and reports whether the class overloads subsref (read) or subsasgn
// (write): a dot access from outside its methods then belongs to the overload.
NLSINTERPRETER_IMPEXP const ClassdefPropertyDefinition*
classdefPlainStoredProperty(const std::string& className, const std::string& propertyName,
    bool forWrite, bool& indexingOverloaded);
NLSINTERPRETER_IMPEXP bool
classdefIsBuiltinClass(const std::string& className);
NLSINTERPRETER_IMPEXP ClassdefLayoutPtr
classdefLayout(const std::string& className);
NLSINTERPRETER_IMPEXP bool
classdefHasMethod(const std::string& className, const std::string& methodName);
NLSINTERPRETER_IMPEXP bool
classdefHasMethodDefinition(const std::string& className, const std::string& methodName);
NLSINTERPRETER_IMPEXP bool
classdefHasStaticMethod(const std::string& className, const std::string& methodName);
NLSINTERPRETER_IMPEXP bool
classdefHasProperty(const std::string& className, const std::string& propertyName);
// declared property whose name matches exactly (classdefHasProperty ignores case)
NLSINTERPRETER_IMPEXP bool
classdefHasExactProperty(const std::string& className, const std::string& propertyName);
NLSINTERPRETER_IMPEXP bool
classdefHasConstantProperty(const std::string& className, const std::string& propertyName);
NLSINTERPRETER_IMPEXP bool
classdefHasDependentProperty(const std::string& className, const std::string& propertyName);
NLSINTERPRETER_IMPEXP bool
classdefIsObservablePropertyEvent(
    const std::string& className, const std::string& propertyName, const std::string& eventName);
NLSINTERPRETER_IMPEXP bool
classdefCanGetProperty(const std::string& className, const std::string& propertyName,
    const std::string& callerClassName);
NLSINTERPRETER_IMPEXP bool
classdefCanSetProperty(const std::string& className, const std::string& propertyName,
    const std::string& callerClassName, bool propertyAlreadyExists, Evaluator* eval);
NLSINTERPRETER_IMPEXP bool
classdefIsA(const std::string& className, const std::string& expectedClassName);
NLSINTERPRETER_IMPEXP bool
classdefIsHandleClass(const std::string& className);
NLSINTERPRETER_IMPEXP bool
classdefIsAbstractClass(const std::string& className);
NLSINTERPRETER_IMPEXP bool
classdefIsSealedClass(const std::string& className);
NLSINTERPRETER_IMPEXP bool
classdefHasSealedMethod(const std::string& className, const std::string& methodName);
NLSINTERPRETER_IMPEXP bool
classdefResolveMethodFunction(const std::string& className, const std::string& methodName,
    std::string& functionName, const std::string& callerClassName = std::string());
NLSINTERPRETER_IMPEXP bool
classdefResolveStaticMethodFunction(const std::string& className, const std::string& methodName,
    std::string& functionName, const std::string& callerClassName = std::string());
NLSINTERPRETER_IMPEXP bool
classdefResolveConstantPropertyFunction(const std::string& className,
    const std::string& propertyName, std::string& functionName,
    const std::string& callerClassName = std::string());
NLSINTERPRETER_IMPEXP bool
classdefResolvePropertyAccessorFunction(const std::string& className,
    const std::string& propertyName, const std::string& accessorPrefix, std::string& functionName);
NLSINTERPRETER_IMPEXP bool
classdefResolveEnumerationMemberFunction(
    const std::string& className, const std::string& memberName, std::string& functionName);
NLSINTERPRETER_IMPEXP stringVector
classdefMethods(const std::string& className);
NLSINTERPRETER_IMPEXP stringVector
classdefProperties(const std::string& className);
NLSINTERPRETER_IMPEXP stringVector
classdefEnumerations(const std::string& className);
NLSINTERPRETER_IMPEXP stringVector
classdefEvents(const std::string& className);
NLSINTERPRETER_IMPEXP stringVector
classdefVisibleEvents(const std::string& className);
NLSINTERPRETER_IMPEXP bool
classdefCanListenEvent(
    const std::string& className, const std::string& eventName, const std::string& callerClassName);
NLSINTERPRETER_IMPEXP bool
classdefCanNotifyEvent(
    const std::string& className, const std::string& eventName, const std::string& callerClassName);
NLSINTERPRETER_IMPEXP stringVector
classdefSuperclasses(const std::string& className);
NLSINTERPRETER_IMPEXP stringVector
classdefClassAttributes(const std::string& className);
NLSINTERPRETER_IMPEXP stringVector
classdefInferiorClasses(const std::string& className);
NLSINTERPRETER_IMPEXP std::vector<ClassdefPropertyDefinition>
classdefPropertyDefinitions(const std::string& className);
NLSINTERPRETER_IMPEXP std::vector<ClassdefMethodDefinition>
classdefMethodDefinitions(const std::string& className);
NLSINTERPRETER_IMPEXP std::vector<ClassdefEventDefinition>
classdefEventDefinitions(const std::string& className);
NLSINTERPRETER_IMPEXP std::vector<ClassdefEnumerationDefinition>
classdefEnumerationDefinitions(const std::string& className);
NLSINTERPRETER_IMPEXP bool
classdefPropertyDefinition(const std::string& className, const std::string& propertyName,
    ClassdefPropertyDefinition& property);
NLSINTERPRETER_IMPEXP bool
classdefPropertyHasAttribute(
    const ClassdefPropertyDefinition& property, const std::string& attributeName);
// true when propertyName of className (or an inherited definition) has the
// WeakHandle attribute; an empty propertyName asks whether any property has it
NLSINTERPRETER_IMPEXP bool
classdefIsWeakHandleProperty(const std::string& className, const std::string& propertyName);
NLSINTERPRETER_IMPEXP ArrayOf
classdefValidatePropertyValue(
    Evaluator* eval, const ClassdefPropertyDefinition& property, const ArrayOf& value);
NLSINTERPRETER_IMPEXP bool
classdefIsValidPropertyValidationExpression(const std::string& validationExpression);
NLSINTERPRETER_IMPEXP bool
classdefCanCallConstructor(const std::string& className, const std::string& callerClassName);
NLSINTERPRETER_IMPEXP std::string
classdefBuiltinBaseType(const std::string& className);
NLSINTERPRETER_IMPEXP std::string
classdefBuiltinBasePropertyName();
//=============================================================================
} // namespace Nelson
//=============================================================================
