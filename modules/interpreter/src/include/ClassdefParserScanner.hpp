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
readTextFile(const std::wstring& filename, std::string& source);
bool
parseClassHeader(
    const std::string& line, ClassdefDefinition& definition, std::string& errorMessage);
void
parsePropertiesBlock(const std::vector<std::string>& lines, size_t first, size_t last,
    const stringVector& blockAttributes, ClassdefDefinition& definition);
std::string
parseEventOrEnumerationName(const std::string& line);
std::string
propertyValidationFailureMessage(const ClassdefPropertyDefinition& property);
std::string
propertyDefaultExpression(const ClassdefPropertyDefinition& property);
bool
extractLeadingParenthesesContent(const std::string& value, std::string& content, std::string& rest);
bool
extractLeadingBraceContent(const std::string& value, std::string& content, std::string& rest);
NLSINTERPRETER_IMPEXP bool
parseValidatorCall(
    const std::string& validator, std::string& functionName, stringVector& argumentExpressions);
NLSINTERPRETER_IMPEXP bool
parseValidationExpression(const std::string& expression, stringVector& dimensions,
    std::string& typeName, stringVector& validators);
bool
validationDimensionsMatch(const stringVector& expectedDimensions, const ArrayOf& value);
bool
validationTardimensions(const stringVector& expectedDimensions, Dimensions& target);
bool
expandScalarToValidationDimensions(const stringVector& expectedDimensions, ArrayOf& value);
bool
validationNumericTypeName(const std::string& typeName, NelsonType& targetType);
bool
validationTypeMatches(const std::string& expectedType, ArrayOf& value);
bool
parseEnumerationDeclaration(const std::string& line, std::string& name, std::string& arguments);
bool
lineStartsFunctionBlock(const std::string& line);
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
