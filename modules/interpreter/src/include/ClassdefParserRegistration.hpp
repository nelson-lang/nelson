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
#include <vector>
#include "ClassdefParser.hpp"
#include "ClassdefLayout.hpp"
#include "MacroFunctionDef.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
void
registerGeneratedFunction(const std::string& key, MacroFunctionDef* macro);
void
unregisterGeneratedFunction(const std::string& key);
void
unregisterClassdefGeneratedFunctions(const ClassdefDefinition& definition);
void
registerConstantProperties(const ClassdefDefinition& definition, const std::wstring& filename);
void
registerEmptyStaticMethod(const ClassdefDefinition& definition, const std::wstring& filename);
void
registerEnumerationMembers(const ClassdefDefinition& definition, const std::wstring& filename);
void
registerEnumerationBaseConversion(
    const ClassdefDefinition& definition, const std::wstring& filename);
void
registerEnumerationBaseSupport(const ClassdefDefinition& definition, const std::wstring& filename);
void
registerEnumerationOperators(const ClassdefDefinition& definition, const std::wstring& filename);
void
registerBuiltinBaseSupport(const ClassdefDefinition& definition, const std::wstring& filename);
void
registerHandleSupport(const ClassdefDefinition& definition, const std::wstring& filename);
bool
hasInstanceMethod(const ClassdefDefinition& definition, const std::string& methodName);
void
registerOwnedGeneratedFunction(const ClassdefDefinition& definition, const std::wstring& filename,
    const std::string& key, const std::string& source);
void
registerMixinSupport(const ClassdefDefinition& definition, const std::wstring& filename);
void
registerRedefinesIndexingDerivedMethods(
    const ClassdefDefinition& definition, const std::wstring& filename);
const ClassdefMethodDefinition*
findConstructorMethod(const ClassdefDefinition& definition);
NLSINTERPRETER_IMPEXP bool
isAbstractMethod(const ClassdefMethodDefinition& method);
bool
isAbstractProperty(const ClassdefPropertyDefinition& property);
bool
hasUnimplementedAbstractMethods(const ClassdefDefinition& definition);
bool
hasUnimplementedAbstractProperties(const ClassdefDefinition& definition);
bool
hasExplicitDefaultValue(const ClassdefPropertyDefinition& property);
std::string
abstractPropertyDefaultValueName(const ClassdefDefinition& definition);
std::string
privateAbstractMethodName(const ClassdefDefinition& definition);
std::string
abstractClassConstructorSource(const ClassdefDefinition& definition);
void
registerSuperclassArray(const ClassdefDefinition& definition, const std::wstring& filename);
void
registerMethods(const ClassdefDefinition& definition, const std::wstring& filename);
FunctionDef*
buildConstructor(const ClassdefDefinition& definition, const std::wstring& filename);
bool
loadDefinitionFromFile(
    const std::wstring& filename, const std::string& expectedName, ClassdefDefinition& definition);
std::string
joinAttributeFlags(const stringVector& attributes);
void
collectInheritanceOrder(
    const std::string& className, stringVector& inheritanceOrder, stringVector& visited);
ClassdefLayoutPtr
buildClassdefLayout(const std::string& className);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
