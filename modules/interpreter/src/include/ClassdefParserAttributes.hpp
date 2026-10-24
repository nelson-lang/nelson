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
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
bool
parseAttributesFromHeader(const std::string& line, const std::string& keyword,
    stringVector& attributes, std::string& errorMessage, bool allowAttributes = true);
std::string
unquoteAttributeValue(std::string value);
bool
attributeValueMeansFalse(std::string value);
bool
isBooleanAttributeValue(std::string value);
bool
attributeListContains(const stringVector& attributes, const std::string& name);
std::string
attributeValue(const stringVector& attributes, const std::string& name);
std::string
attributeName(std::string attribute);
bool
isBooleanAttributeName(const std::string& name);
void
validateBooleanAttributeValues(const stringVector& attributes);
void
validatePropertyBooleanAttributeValues(
    const stringVector& attributes, bool allowSystemObjectAttributes);
bool
validateBooleanAttributeValues(const stringVector& attributes, std::string& errorMessage);
bool
validatePropertyBooleanAttributeValues(
    const stringVector& attributes, bool allowSystemObjectAttributes, std::string& errorMessage);
bool
isClassReference(std::string value);
bool
isClassAccessReference(std::string value);
bool
isValidAccessAttributeValue(std::string value);
bool
isValidInferiorClassesAttributeValue(std::string value);
stringVector
classReferenceNamesFromAttributeValue(std::string value);
void
validateInferiorClassesAttributeValue(const stringVector& attributes);
bool
validateInferiorClassesAttributeValue(const stringVector& attributes, std::string& errorMessage);
bool
isAccessAttributeName(const std::string& name);
void
validateAccessAttributeValues(const stringVector& attributes);
bool
validateAccessAttributeValues(const stringVector& attributes, std::string& errorMessage);
bool
validateDefinitionBooleanAttributeValues(
    const ClassdefDefinition& definition, std::string& errorMessage);
bool
validateDefinitionAccessAttributeValues(
    const ClassdefDefinition& definition, std::string& errorMessage);
const ClassdefPropertyDefinition*
findLocalPropertyDefinition(const ClassdefDefinition& definition, const std::string& propertyName);
std::string
setAccessorPropertyName(const std::string& methodName);
bool
invalidAttributeCombination(const ClassdefDefinition& definition, std::string& errorMessage);
void
validateAttributeCombinations(const ClassdefDefinition& definition);
bool
isKnownAttribute(const std::string& attribute, const char* const* names, size_t count);
bool
isEnumerationBaseType(const std::string& className);
bool
isBuiltinBaseClassName(const std::string& className);
std::string
builtinBaseTypeName(const ClassdefDefinition& definition);
std::string
enumerationBaseType(const ClassdefDefinition& definition);
void
validateAttributes(const stringVector& attributes, const char* const* names, size_t count);
bool
hasPublicAccess(const stringVector& attributes);
std::string
accessAttributeValue(const stringVector& attributes);
std::string
propertyAccessAttributeValue(const stringVector& attributes, const std::string& attributeName);
std::string
metaclassAccessName(std::string value);
bool
isAccessListAllowedFromClassdefContext(const std::string& definingClassName,
    const std::string& access, const std::string& callerClassName);
bool
isAccessAllowedFromClassdefContext(const std::string& definingClassName, const std::string& access,
    const std::string& callerClassName);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
