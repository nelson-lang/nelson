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
#include "MacroFunctionDef.hpp"
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
struct GeneratedClassdefSource
{
    std::string source;
    std::vector<size_t> sourceLineMap;
};
//=============================================================================
bool
keywordTokenAt(const std::string& line, size_t pos, const std::string& keyword);
size_t
findFunctionHeaderBodySeparator(const std::string& line);
bool
stripTrailingEndKeyword(std::string& value);
std::string
normalizedInlineFunctionSource(const std::string& source);
bool
isInlineFunctionDefinition(const std::string& line);
std::string
methodNameFromFunctionLine(const std::string& line);
std::string
methodNameFromPrototypeLine(const std::string& line);
std::string
constructorReturnNameFromFunctionLine(const std::string& line);
size_t
parseMethodsBlock(const std::vector<std::string>& lines, size_t first,
    const stringVector& blockAttributes, ClassdefDefinition& definition);
size_t
findSimpleBlockEnd(const std::vector<std::string>& lines, size_t first);
NLSINTERPRETER_IMPEXP MacroFunctionDef*
parseGeneratedFunction(const std::string& source, const std::wstring& filename,
    size_t sourceLineOffset = 0, const std::vector<size_t>& sourceLineMap = {});
// Build the constructor function of an enumeration member natively (no generated
// source, no re-parse): assembles the AST for "obj = __classdef_value__('Class');
// obj.Name = 'member'; obj.Ordinal = N;". Covers the typed-enumeration and
// no-argument cases; the untyped case with arguments stays on the generated path.
MacroFunctionDef*
buildEnumerationMemberFunction(const ClassdefDefinition& definition, const std::string& member,
    size_t ordinal, const std::wstring& filename);
// Build the getter of a Constant property whose default is evaluation-invariant
// natively: "function value = Class_Prop()\n value = <expr>;\n end", parsing the
// user default expression. Returns nullptr if the expression does not parse (the
// caller then falls back to the generated path); only call for invariant defaults.
MacroFunctionDef*
buildInvariantConstantPropertyGetter(const ClassdefDefinition& definition,
    const ClassdefPropertyDefinition& property, const std::wstring& filename);
// Build the enumeration char operator ("function r = char(obj)\n r = obj.Name;\n
// end") and ne operator ("function r = ne(a, b)\n r = ~eq(a, b);\n end") natively.
MacroFunctionDef*
buildEnumerationCharOperator(const ClassdefDefinition& definition, const std::wstring& filename);
MacroFunctionDef*
buildEnumerationNeOperator(const ClassdefDefinition& definition, const std::wstring& filename);
// Build the enumeration eq operator natively (member-name comparison, text
// comparison, or underlying-value comparison for a base-typed enumeration).
MacroFunctionDef*
buildEnumerationEqOperator(const ClassdefDefinition& definition, const std::wstring& filename);
// Build the enumeration disp (scalar or array member listing) and display
// (optional "name =" header then disp) operators natively.
MacroFunctionDef*
buildEnumerationDispOperator(const ClassdefDefinition& definition, const std::wstring& filename);
MacroFunctionDef*
buildEnumerationDisplayOperator(const ClassdefDefinition& definition, const std::wstring& filename);
// Build a one-line forwarding function "function [ret =] name(args)\n
// [ret =] callee(args);\n end" natively (ret empty => a call statement with no
// output). Used for the simple handle-support helpers.
MacroFunctionDef*
buildForwardingFunction(const std::string& returnValue, const std::string& functionName,
    const std::vector<std::string>& argumentNames, const std::string& callee,
    const std::wstring& filename);
// Build the handle destructor natively: delete(obj) forwards to
// __classdef_handle_delete__(obj), which runs the destructor protocol of each
// valid element (ObjectBeingDestroyed, user delete chain, invalidation).
MacroFunctionDef*
buildHandleDeleteFunction(const std::wstring& filename);
// Build the default constructor ("function obj = Class()") natively when the
// class has no user constructor, no class-local functions, and every stored
// property has an evaluation-invariant default (so no stored-default registry is
// needed). Returns nullptr otherwise, so the caller falls back to generation.
MacroFunctionDef*
buildDefaultConstructor(const ClassdefDefinition& definition, const std::wstring& filename);
bool
canUseStoredDefaultAssignment(
    const ClassdefDefinition& definition, const ClassdefPropertyDefinition& property);
bool
isInvariantDefaultIdentifier(const std::string& name);
bool
isEvaluationInvariantDefaultExpression(const std::string& expression);
std::string
classInitializationSource(const ClassdefDefinition& definition, const std::string& objectName);
// Nearest user-class ancestors declaring a constructor, except through the
// superclasses in explicitlyCalled: the constructor must call each of them
// with no argument when the class code does not.
stringVector
implicitSuperclassConstructors(
    const ClassdefDefinition& definition, const stringVector& explicitlyCalled = {});
std::string
generatedDefaultConstructorSource(const ClassdefDefinition& definition);
std::string
generatedTypedEnumerationConstructorSource(const ClassdefDefinition& definition);
GeneratedClassdefSource
injectConstructorInitialization(
    const ClassdefDefinition& definition, const ClassdefMethodDefinition& constructor);
bool
isPropertyAccessorMethodName(const std::string& methodName);
std::string
sanitizeGeneratedFunctionName(std::string value);
std::string
generatedAccessorFunctionName(
    const ClassdefDefinition& definition, const ClassdefMethodDefinition& method);
std::string
generatedConstructorFunctionName(const ClassdefDefinition& definition);
void
renameGeneratedFunctionLine(
    GeneratedClassdefSource& generated, const std::string& oldName, const std::string& newName);
size_t
findTopLevelChar(const std::string& value, char searched, size_t first = 0);
bool
isQualifiedIdentifier(const std::string& value);
bool
hasMisplacedSuperclassCall(
    const ClassdefMethodDefinition& method, const std::string& className, std::string& badLine);
NLSINTERPRETER_IMPEXP std::string
generatedMethodSource(const ClassdefDefinition& definition, const ClassdefMethodDefinition& method);
NLSINTERPRETER_IMPEXP std::string
appendClassdefLocalFunctions(const ClassdefDefinition& definition, std::string source);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
