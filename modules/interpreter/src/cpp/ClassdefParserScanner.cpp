//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS /* _wfopen */
//=============================================================================
#include "ClassdefParserScanner.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserAttributes.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include <algorithm>
#include <cctype>
#include <cstdio>
//=============================================================================
namespace Nelson::classdef_detail {
bool
readTextFile(const std::wstring& filename, std::string& source)
{
    source.clear();
    FILE* fp = nullptr;
#ifdef _MSC_VER
    fp = _wfopen(filename.c_str(), L"rt");
#else
    fp = fopen(wstring_to_utf8(filename).c_str(), "rt");
#endif
    if (fp == nullptr) {
        return false;
    }
    char buffer[4096];
    while (fgets(buffer, sizeof(buffer), fp) != nullptr) {
        source += buffer;
    }
    fclose(fp);
    return true;
}
//=============================================================================
bool
parseClassHeader(const std::string& line, ClassdefDefinition& definition, std::string& errorMessage)
{
    std::string rest = trim(stripComment(line));
    if (!startsWithKeyword(rest, CLASSDEF_KEYWORD_CLASSDEF)) {
        return false;
    }
    rest = trim(rest.substr(std::string(CLASSDEF_KEYWORD_CLASSDEF).size()));
    while (!rest.empty() && rest.front() == '(') {
        int depth = 0;
        size_t end = std::string::npos;
        for (size_t k = 0; k < rest.size(); ++k) {
            if (rest[k] == '(') {
                ++depth;
            } else if (rest[k] == ')') {
                --depth;
                if (depth == 0) {
                    end = k;
                    break;
                }
            }
        }
        if (end == std::string::npos) {
            errorMessage = CLASSDEF_ERROR_UNTERMINATED_ATTRIBUTES;
            return false;
        }
        stringVector attributes = splitCommaList(rest.substr(1, end - 1));
        definition.attributes.insert(
            definition.attributes.end(), attributes.begin(), attributes.end());
        rest = trim(rest.substr(end + 1));
    }
    size_t nameEnd = 0;
    while (nameEnd < rest.size() && (isIdentifierPart(rest[nameEnd]) || rest[nameEnd] == '.')) {
        ++nameEnd;
    }
    definition.name = rest.substr(0, nameEnd);
    if (definition.name.empty()) {
        errorMessage = CLASSDEF_ERROR_CLASS_NAME_EXPECTED;
        return false;
    }
    rest = trim(rest.substr(nameEnd));
    if (!rest.empty() && rest.front() == '<') {
        definition.superclasses = splitCommaList(withoutTrailingStatementSeparator(rest.substr(1)));
        if (definition.superclasses.empty()) {
            errorMessage = CLASSDEF_ERROR_INVALID_SUPERCLASS_DECLARATION + trim(rest);
            return false;
        }
        for (const auto& superclass : definition.superclasses) {
            const std::string trimmedSuperclass = trim(superclass);
            if (parseLeadingQualifiedIdentifier(trimmedSuperclass) != trimmedSuperclass) {
                errorMessage = CLASSDEF_ERROR_INVALID_SUPERCLASS_DECLARATION + trimmedSuperclass;
                return false;
            }
        }
    } else if (!withoutTrailingStatementSeparator(rest).empty()) {
        errorMessage = CLASSDEF_ERROR_INVALID_BLOCK_HEADER + line;
        return false;
    }
    return true;
}
//=============================================================================
void
parsePropertiesBlock(const std::vector<std::string>& lines, size_t first, size_t last,
    const stringVector& blockAttributes, ClassdefDefinition& definition)
{
    for (size_t k = first; k < last && k < lines.size(); ++k) {
        size_t logicalLast = k;
        std::string line = withoutTrailingStatementSeparator(
            joinedContinuationLine(lines, k, last, logicalLast));
        k = logicalLast;
        if (line.empty()) {
            continue;
        }
        ClassdefPropertyDefinition property;
        property.attributes = blockAttributes;
        property.definingClassName = definition.name;
        size_t equals = line.find('=');
        std::string declaration = equals == std::string::npos ? line : line.substr(0, equals);
        property.defaultExpression = equals == std::string::npos
            ? CLASSDEF_DEFAULT_PROPERTY_VALUE
            : withoutTrailingStatementSeparator(line.substr(equals + 1));
        declaration = trim(declaration);
        property.name = parseLeadingIdentifier(declaration);
        if (!property.name.empty()) {
            property.validationExpression = trim(declaration.substr(property.name.size()));
        }
        if (!property.name.empty()) {
            definition.properties.push_back(property);
        }
    }
}
//=============================================================================
std::string
parseEventOrEnumerationName(const std::string& line)
{
    return parseLeadingIdentifier(withoutTrailingStatementSeparator(stripComment(line)));
}
//=============================================================================
std::string
propertyValidationFailureMessage(const ClassdefPropertyDefinition& property)
{
    return "Property validation failed: " + property.name;
}
//=============================================================================
std::string
propertyDefaultExpression(const ClassdefPropertyDefinition& property)
{
    const std::string fallback = property.defaultExpression.empty()
        ? std::string(CLASSDEF_DEFAULT_PROPERTY_VALUE)
        : property.defaultExpression;
    if (trim(fallback) != CLASSDEF_DEFAULT_PROPERTY_VALUE
        || !attributeListContains(property.attributes, CLASSDEF_ATTR_WEAK_HANDLE)) {
        return fallback;
    }
    stringVector dimensions;
    std::string typeName;
    stringVector validators;
    if (!parseValidationExpression(property.validationExpression, dimensions, typeName, validators)
        || typeName.empty()) {
        return fallback;
    }
    // a constructed default object would be released at once by the weak
    // reference: the default is an array of deleted handles of the class
    std::string expression = "__classdef_invalid_handle__('" + typeName + "'";
    if (dimensions.empty()) {
        expression += ", 1, 1";
    }
    for (const std::string& dimension : dimensions) {
        expression += ", " + (trim(dimension) == ":" ? std::string("0") : trim(dimension));
    }
    return expression + ")";
}
//=============================================================================
bool
extractLeadingParenthesesContent(const std::string& value, std::string& content, std::string& rest)
{
    content.clear();
    rest.clear();
    std::string trimmed = trim(value);
    if (trimmed.empty() || trimmed.front() != '(') {
        return false;
    }
    int depth = 0;
    bool inQuote = false;
    for (size_t k = 0; k < trimmed.size(); ++k) {
        const char ch = trimmed[k];
        if (ch == '\'') {
            inQuote = !inQuote;
        } else if (!inQuote && ch == '(') {
            ++depth;
        } else if (!inQuote && ch == ')') {
            --depth;
            if (depth == 0) {
                content = trimmed.substr(1, k - 1);
                rest = trim(trimmed.substr(k + 1));
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
extractLeadingBraceContent(const std::string& value, std::string& content, std::string& rest)
{
    content.clear();
    rest.clear();
    std::string trimmed = trim(value);
    if (trimmed.empty() || trimmed.front() != '{') {
        return false;
    }
    int depth = 0;
    bool inQuote = false;
    for (size_t k = 0; k < trimmed.size(); ++k) {
        const char ch = trimmed[k];
        if (ch == '\'') {
            inQuote = !inQuote;
        } else if (!inQuote && ch == '{') {
            ++depth;
        } else if (!inQuote && ch == '}') {
            --depth;
            if (depth == 0) {
                content = trimmed.substr(1, k - 1);
                rest = trim(trimmed.substr(k + 1));
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
parseValidatorCall(
    const std::string& validator, std::string& functionName, stringVector& argumentExpressions)
{
    functionName.clear();
    argumentExpressions.clear();
    const std::string value = trim(validator);
    functionName = parseLeadingQualifiedIdentifier(value);
    if (functionName.empty()) {
        return false;
    }
    std::string rest = trim(value.substr(functionName.size()));
    if (rest.empty()) {
        return true;
    }
    if (rest.front() != '(') {
        return false;
    }
    std::string argumentList;
    std::string trailing;
    if (!extractLeadingParenthesesContent(rest, argumentList, trailing) || !trailing.empty()) {
        return false;
    }
    argumentExpressions = splitCommaList(argumentList);
    return true;
}
//=============================================================================
bool
parseValidationExpression(const std::string& expression, stringVector& dimensions,
    std::string& typeName, stringVector& validators)
{
    dimensions.clear();
    typeName.clear();
    validators.clear();
    std::string value = trim(expression);
    if (value.empty()) {
        return false;
    }
    if (value.front() == '(') {
        std::string dimensionList;
        std::string rest;
        if (!extractLeadingParenthesesContent(value, dimensionList, rest)) {
            return false;
        }
        dimensions = splitCommaList(dimensionList);
        value = rest;
    }
    typeName = parseLeadingQualifiedIdentifier(value);
    if (!typeName.empty()) {
        value = trim(value.substr(typeName.size()));
    }
    std::string validatorList;
    std::string rest;
    if (extractLeadingBraceContent(value, validatorList, rest)) {
        if (trim(validatorList).empty()) {
            return false;
        }
        validators = splitCommaList(validatorList);
        if (validators.empty()) {
            return false;
        }
        for (const auto& validator : validators) {
            std::string functionName;
            stringVector argumentExpressions;
            if (!parseValidatorCall(validator, functionName, argumentExpressions)) {
                return false;
            }
        }
        value = rest;
    }
    return value.empty() && (!dimensions.empty() || !typeName.empty() || !validators.empty());
}
//=============================================================================
bool
validationDimensionsMatch(const stringVector& expectedDimensions, const ArrayOf& value)
{
    if (expectedDimensions.empty()) {
        return true;
    }
    for (size_t k = 0; k < expectedDimensions.size(); ++k) {
        const std::string expected = trim(expectedDimensions[k]);
        if (expected.empty() || expected == ":") {
            continue;
        }
        char* end = nullptr;
        const long requested = std::strtol(expected.c_str(), &end, 10);
        if (end == expected.c_str() || *end != '\0' || requested < 0) {
            continue;
        }
        if (value.dimensions().dimensionLength(static_cast<indexType>(k))
            != static_cast<indexType>(requested)) {
            return false;
        }
    }
    const Dimensions valueDimensions = value.dimensions();
    for (indexType k = static_cast<indexType>(expectedDimensions.size());
        k < valueDimensions.length(); ++k) {
        if (valueDimensions.dimensionLength(k) != 1) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
validationTardimensions(const stringVector& expectedDimensions, Dimensions& target)
{
    if (expectedDimensions.empty()) {
        return false;
    }
    target = Dimensions(static_cast<indexType>(expectedDimensions.size()));
    for (size_t k = 0; k < expectedDimensions.size(); ++k) {
        const std::string expected = trim(expectedDimensions[k]);
        if (expected.empty() || expected == ":") {
            return false;
        }
        char* end = nullptr;
        const long requested = std::strtol(expected.c_str(), &end, 10);
        if (end == expected.c_str() || *end != '\0' || requested < 0) {
            return false;
        }
        target.setDimensionLength(static_cast<indexType>(k), static_cast<indexType>(requested));
    }
    return true;
}
//=============================================================================
bool
expandScalarToValidationDimensions(const stringVector& expectedDimensions, ArrayOf& value)
{
    if (!value.isScalar() || value.isSparse() || value.isFunctionHandle()) {
        return false;
    }
    Dimensions target;
    if (!validationTardimensions(expectedDimensions, target) || value.dimensions().equals(target)
        || target.elementCount() == 0) {
        return false;
    }
    const stringVector fieldNames = value.fieldNames();
    void* elements
        = ArrayOf::allocateArrayOf(value.dataClass(), target.elementCount(), fieldNames, false);
    for (indexType k = 0; k < target.elementCount(); ++k) {
        value.copyElements(0, elements, k, 1);
    }
    ArrayOf expanded(value.dataClass(), target, elements, false, fieldNames);
    if (value.isClassType()) {
        expanded.setClassType(value.classType());
    }
    if (value.isClassdefType()) {
        expanded.setClassdefLayout(value.classdefLayout());
    }
    value = expanded;
    return true;
}
//=============================================================================
bool
validationNumericTypeName(const std::string& typeName, NelsonType& targetType)
{
    const std::string name = tolowerString(trim(typeName));
    if (name == NLS_DOUBLE_STR) {
        targetType = NLS_DOUBLE;
        return true;
    }
    if (name == NLS_SINGLE_STR) {
        targetType = NLS_SINGLE;
        return true;
    }
    if (name == NLS_UINT8_STR) {
        targetType = NLS_UINT8;
        return true;
    }
    if (name == NLS_INT8_STR) {
        targetType = NLS_INT8;
        return true;
    }
    if (name == NLS_UINT16_STR) {
        targetType = NLS_UINT16;
        return true;
    }
    if (name == NLS_INT16_STR) {
        targetType = NLS_INT16;
        return true;
    }
    if (name == NLS_UINT32_STR) {
        targetType = NLS_UINT32;
        return true;
    }
    if (name == NLS_INT32_STR) {
        targetType = NLS_INT32;
        return true;
    }
    if (name == NLS_UINT64_STR) {
        targetType = NLS_UINT64;
        return true;
    }
    if (name == NLS_INT64_STR) {
        targetType = NLS_INT64;
        return true;
    }
    if (name == NLS_LOGICAL_STR) {
        targetType = NLS_LOGICAL;
        return true;
    }
    return false;
}
//=============================================================================
bool
validationTypeMatches(const std::string& expectedType, ArrayOf& value)
{
    if (expectedType.empty()) {
        return true;
    }
    const std::string actualType = ClassName(value);
    if (equalsIgnoreCase(expectedType, NLS_CHAR_STR)) {
        if (value.isStringArray()) {
            value = ArrayOf::characterArrayFromStringArray(value);
            return true;
        }
        if (value.isNumeric() && value.canBePromotedTo(NLS_CHAR)) {
            value.promoteType(NLS_CHAR);
            return true;
        }
    }
    if (equalsIgnoreCase(expectedType, "string") && !value.isStringArray()) {
        bool needToOverload = false;
        ArrayOf convertedValue = ArrayOf::toStringArray(value, needToOverload);
        if (!needToOverload) {
            value = convertedValue;
            return true;
        }
    }
    if (equalsIgnoreCase(actualType, expectedType)
        || (equalsIgnoreCase(expectedType, "string") && equalsIgnoreCase(actualType, "char"))) {
        return true;
    }
    NelsonType targetType = NLS_UNKNOWN;
    if (validationNumericTypeName(expectedType, targetType) && value.isNumeric()
        && value.canBePromotedTo(targetType)) {
        value.promoteType(targetType);
        return true;
    }
    return classdefIsA(actualType, expectedType);
}
//=============================================================================
bool
parseEnumerationDeclaration(const std::string& line, std::string& name, std::string& arguments)
{
    std::string value = withoutTrailingStatementSeparator(stripComment(line));
    name = parseLeadingIdentifier(value);
    arguments.clear();
    if (name.empty()) {
        return false;
    }

    std::string rest = trim(value.substr(name.size()));
    if (rest.empty()) {
        return true;
    }
    if (rest.front() != '(') {
        return false;
    }

    std::string trailing;
    if (!extractLeadingParenthesesContent(rest, arguments, trailing)) {
        return false;
    }
    arguments = trim(arguments);
    return trailing.empty();
}
//=============================================================================
bool
lineStartsFunctionBlock(const std::string& line)
{
    static const char* openers[]
        = { CLASSDEF_KEYWORD_FUNCTION, "if", "for", "while", "switch", "try", "arguments" };
    for (const char* opener : openers) {
        if (startsWithKeyword(stripComment(line), opener)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
