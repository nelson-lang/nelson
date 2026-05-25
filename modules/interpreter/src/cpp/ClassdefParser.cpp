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
#include <functional>
#include <sstream>
#include <unordered_set>
#include "ClassdefParser.hpp"
#include "AbstractSyntaxTree.hpp"
#include "Error.hpp"
#include "FunctionsInMemory.hpp"
#include "LexerContext.hpp"
#include "OverloadName.hpp"
#include "ParserInterface.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "StringHelpers.hpp"
#include "Types.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define CLASSDEF_ACCESS_PRIVATE "private"
#define CLASSDEF_ACCESS_PROTECTED "protected"
#define CLASSDEF_ACCESS_PUBLIC "public"
#define CLASSDEF_ATTR_ABSTRACT "Abstract"
#define CLASSDEF_ATTR_ACCESS "Access"
#define CLASSDEF_ATTR_CONSTANT "Constant"
#define CLASSDEF_ATTR_DEPENDENT "Dependent"
#define CLASSDEF_ATTR_GET_ACCESS "GetAccess"
#define CLASSDEF_ATTR_GET_OBSERVABLE "GetObservable"
#define CLASSDEF_ATTR_HIDDEN "Hidden"
#define CLASSDEF_ATTR_SEALED "Sealed"
#define CLASSDEF_ATTR_SET_ACCESS "SetAccess"
#define CLASSDEF_ATTR_SET_OBSERVABLE "SetObservable"
#define CLASSDEF_ATTR_STATIC "Static"
#define CLASSDEF_DEFAULT_PROPERTY_VALUE "[]"
#define CLASSDEF_ENUM_NAME_DEFAULT "''"
#define CLASSDEF_ENUM_ORDINAL_DEFAULT "0"
#define CLASSDEF_ENUM_PROPERTY_NAME "Name"
#define CLASSDEF_ENUM_PROPERTY_ORDINAL "Ordinal"
#define CLASSDEF_EVENT_OBJECT_BEING_DESTROYED "ObjectBeingDestroyed"
#define CLASSDEF_EVENT_POST_GET "PostGet"
#define CLASSDEF_EVENT_POST_SET "PostSet"
#define CLASSDEF_EVENT_PRE_GET "PreGet"
#define CLASSDEF_EVENT_PRE_SET "PreSet"
#define CLASSDEF_FUNCTION_CHAR "char"
#define CLASSDEF_FUNCTION_DELETE "delete"
#define CLASSDEF_FUNCTION_EQ "eq"
#define CLASSDEF_FUNCTION_FIELDNAMES "fieldnames"
#define CLASSDEF_FUNCTION_GET "get"
#define CLASSDEF_FUNCTION_ISVALID "isvalid"
#define CLASSDEF_FUNCTION_NE "ne"
#define CLASSDEF_FUNCTION_SET "set"
#define CLASSDEF_FUNCTION_STRUCT "struct"
#define CLASSDEF_GENERATED_FUNCTION_PREFIX "__classdef_"
#define CLASSDEF_GENERATED_FUNCTION_SUFFIX "__"
#define CLASSDEF_HANDLE_DELETE_BUILTIN "__classdef_handle_delete__"
#define CLASSDEF_HANDLE_GET_BUILTIN "__classdef_handle_get__"
#define CLASSDEF_HANDLE_ISVALID_BUILTIN "__classdef_handle_isvalid__"
#define CLASSDEF_HANDLE_STRUCT_BUILTIN "__classdef_handle_struct__"
#define CLASSDEF_HANDLE_SET_BUILTIN "__classdef_handle_set__"
#define CLASSDEF_HANDLE_BUILTIN "__classdef_handle__"
#define CLASSDEF_KEYWORD_CLASSDEF "classdef"
#define CLASSDEF_KEYWORD_END "end"
#define CLASSDEF_KEYWORD_ENUMERATION "enumeration"
#define CLASSDEF_KEYWORD_EVENTS "events"
#define CLASSDEF_KEYWORD_FUNCTION "function"
#define CLASSDEF_KEYWORD_METHODS "methods"
#define CLASSDEF_KEYWORD_PROPERTIES "properties"
#define CLASSDEF_METHOD_GET_PREFIX "get."
#define CLASSDEF_METHOD_SET_PREFIX "set."
#define CLASSDEF_ERROR_CLASSDEF_HEADER_EXPECTED "classdef header expected."
#define CLASSDEF_ERROR_CLASS_NAME_EXPECTED "Class name expected after classdef."
#define CLASSDEF_ERROR_CANNOT_INHERIT_SEALED "Cannot inherit from sealed class: "
#define CLASSDEF_ERROR_CANNOT_OVERRIDE_SEALED "Cannot override sealed method: "
#define CLASSDEF_ERROR_MISSING_END_CLASSDEF "Missing end for classdef."
#define CLASSDEF_ERROR_MISSING_END_ENUMERATION "Missing end for enumeration block."
#define CLASSDEF_ERROR_MISSING_END_EVENTS "Missing end for events block."
#define CLASSDEF_ERROR_MISSING_END_METHODS "Missing end for methods block."
#define CLASSDEF_ERROR_MISSING_END_PROPERTIES "Missing end for properties block."
#define CLASSDEF_ERROR_UNTERMINATED_ATTRIBUTES "Unterminated classdef attribute list."
//=============================================================================
namespace {
    //=============================================================================
    std::string
    trim(const std::string& input)
    {
        size_t first = 0;
        while (
            first < input.size() && std::isspace(static_cast<unsigned char>(input[first])) != 0) {
            ++first;
        }
        size_t last = input.size();
        while (last > first && std::isspace(static_cast<unsigned char>(input[last - 1])) != 0) {
            --last;
        }
        return input.substr(first, last - first);
    }
    //=============================================================================
    std::string
    tolowerString(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
        return value;
    }
    //=============================================================================
    bool
    equalsIgnoreCase(const std::string& left, const std::string& right)
    {
        return tolowerString(left) == tolowerString(right);
    }
    //=============================================================================
    std::string
    classNameLeaf(const std::string& className)
    {
        size_t pos = className.find_last_of('.');
        if (pos == std::string::npos || pos + 1 >= className.size()) {
            return className;
        }
        return className.substr(pos + 1);
    }
    //=============================================================================
    bool
    equalsClassNameOrLeaf(const std::string& name, const std::string& className)
    {
        return equalsIgnoreCase(name, className)
            || equalsIgnoreCase(name, classNameLeaf(className));
    }
    //=============================================================================
    bool
    startsWithKeyword(const std::string& line, const std::string& keyword)
    {
        const std::string value = trim(line);
        if (value.size() < keyword.size() || value.compare(0, keyword.size(), keyword) != 0) {
            return false;
        }
        if (value.size() == keyword.size()) {
            return true;
        }
        const unsigned char next = static_cast<unsigned char>(value[keyword.size()]);
        return std::isspace(next) != 0 || next == '(' || next == ';' || next == ',';
    }
    //=============================================================================
    std::string
    stripComment(const std::string& line)
    {
        bool inQuote = false;
        for (size_t k = 0; k < line.size(); ++k) {
            if (line[k] == '\'') {
                inQuote = !inQuote;
            }
            if (!inQuote && line[k] == '%') {
                return line.substr(0, k);
            }
        }
        return line;
    }
    //=============================================================================
    std::string
    withoutTrailingStatementSeparator(std::string value)
    {
        value = trim(value);
        while (!value.empty() && (value.back() == ';' || value.back() == ',')) {
            value.pop_back();
            value = trim(value);
        }
        return value;
    }
    //=============================================================================
    std::vector<std::string>
    splitLines(const std::string& source)
    {
        std::vector<std::string> lines;
        std::stringstream stream(source);
        std::string line;
        while (std::getline(stream, line)) {
            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }
            lines.push_back(line);
        }
        if (source.empty() || source.back() == '\n') {
            return lines;
        }
        return lines;
    }
    //=============================================================================
    std::string
    joinLines(const std::vector<std::string>& lines, size_t first, size_t last)
    {
        std::string result;
        for (size_t k = first; k <= last && k < lines.size(); ++k) {
            result += lines[k];
            result += '\n';
        }
        return result;
    }
    //=============================================================================
    stringVector
    splitCommaList(const std::string& value)
    {
        stringVector result;
        std::string current;
        int depth = 0;
        bool inQuote = false;
        for (char ch : value) {
            if (ch == '\'') {
                inQuote = !inQuote;
            } else if (!inQuote && (ch == '(' || ch == '[' || ch == '{')) {
                ++depth;
            } else if (!inQuote && (ch == ')' || ch == ']' || ch == '}') && depth > 0) {
                --depth;
            }
            if (!inQuote && depth == 0 && (ch == ',' || ch == '&')) {
                std::string item = trim(current);
                if (!item.empty()) {
                    result.push_back(item);
                }
                current.clear();
            } else {
                current += ch;
            }
        }
        std::string item = trim(current);
        if (!item.empty()) {
            result.push_back(item);
        }
        return result;
    }
    //=============================================================================
    stringVector
    parseAttributesFromHeader(const std::string& line, const std::string& keyword)
    {
        std::string value = trim(stripComment(line));
        if (!startsWithKeyword(value, keyword)) {
            return {};
        }
        value = trim(value.substr(keyword.size()));
        if (value.empty() || value[0] != '(') {
            return {};
        }
        int depth = 0;
        size_t end = std::string::npos;
        for (size_t k = 0; k < value.size(); ++k) {
            if (value[k] == '(') {
                ++depth;
            } else if (value[k] == ')') {
                --depth;
                if (depth == 0) {
                    end = k;
                    break;
                }
            }
        }
        if (end == std::string::npos || end <= 1) {
            return {};
        }
        return splitCommaList(value.substr(1, end - 1));
    }
    //=============================================================================
    bool
    attributeListContains(const stringVector& attributes, const std::string& name)
    {
        const std::string normalizedName = tolowerString(name);
        for (std::string attribute : attributes) {
            attribute = trim(attribute);
            size_t equals = attribute.find('=');
            if (equals != std::string::npos) {
                attribute = trim(attribute.substr(0, equals));
            }
            if (tolowerString(attribute) == normalizedName) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    std::string
    attributeValue(const stringVector& attributes, const std::string& name)
    {
        const std::string normalizedName = tolowerString(name);
        for (std::string attribute : attributes) {
            attribute = trim(attribute);
            size_t equals = attribute.find('=');
            if (equals == std::string::npos) {
                continue;
            }
            std::string attributeName = trim(attribute.substr(0, equals));
            if (tolowerString(attributeName) == normalizedName) {
                return trim(attribute.substr(equals + 1));
            }
        }
        return {};
    }
    //=============================================================================
    bool
    hasPublicAccess(const stringVector& attributes)
    {
        std::string access = trim(attributeValue(attributes, CLASSDEF_ATTR_ACCESS));
        if (access.size() >= 2
            && ((access.front() == '\'' && access.back() == '\'')
                || (access.front() == '"' && access.back() == '"'))) {
            access = access.substr(1, access.size() - 2);
        }
        return access.empty() || equalsIgnoreCase(access, CLASSDEF_ACCESS_PUBLIC);
    }
    //=============================================================================
    std::string
    accessAttributeValue(const stringVector& attributes)
    {
        std::string access = trim(attributeValue(attributes, CLASSDEF_ATTR_ACCESS));
        if (access.empty()) {
            return CLASSDEF_ACCESS_PUBLIC;
        }
        if (access.size() >= 2
            && ((access.front() == '\'' && access.back() == '\'')
                || (access.front() == '"' && access.back() == '"'))) {
            access = access.substr(1, access.size() - 2);
        }
        return trim(access);
    }
    //=============================================================================
    std::string
    propertyAccessAttributeValue(const stringVector& attributes, const std::string& attributeName)
    {
        std::string access = trim(attributeValue(attributes, attributeName));
        if (access.empty()) {
            access = trim(attributeValue(attributes, CLASSDEF_ATTR_ACCESS));
        }
        if (access.empty()) {
            return CLASSDEF_ACCESS_PUBLIC;
        }
        if (access.size() >= 2
            && ((access.front() == '\'' && access.back() == '\'')
                || (access.front() == '"' && access.back() == '"'))) {
            access = access.substr(1, access.size() - 2);
        }
        return trim(access);
    }
    //=============================================================================
    std::string
    metaclassAccessName(std::string value)
    {
        value = trim(value);
        if (value.size() >= 2
            && ((value.front() == '\'' && value.back() == '\'')
                || (value.front() == '"' && value.back() == '"'))) {
            value = value.substr(1, value.size() - 2);
        }
        value = trim(value);
        if (!value.empty() && value.front() == '?') {
            value = trim(value.substr(1));
        }
        return value;
    }
    //=============================================================================
    bool
    isAccessListAllowedFromClassdefContext(const std::string& definingClassName,
        const std::string& access, const std::string& callerClassName)
    {
        if (equalsIgnoreCase(callerClassName, definingClassName)) {
            return true;
        }

        std::string list = trim(access);
        if (list.size() >= 2 && list.front() == '{' && list.back() == '}') {
            list = trim(list.substr(1, list.size() - 2));
        }
        if (list.empty()) {
            return false;
        }

        for (const std::string& token : splitCommaList(list)) {
            std::string allowedClassName = metaclassAccessName(token);
            if (allowedClassName.empty()) {
                continue;
            }
            if (equalsIgnoreCase(callerClassName, allowedClassName)
                || ClassdefDefinitionManager::getInstance()->isA(
                    callerClassName, allowedClassName)) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    isAccessAllowedFromClassdefContext(const std::string& definingClassName,
        const std::string& access, const std::string& callerClassName)
    {
        if (equalsIgnoreCase(access, CLASSDEF_ACCESS_PUBLIC)) {
            return true;
        }
        if (callerClassName.empty()) {
            return false;
        }
        if (equalsIgnoreCase(access, CLASSDEF_ACCESS_PRIVATE)) {
            return equalsIgnoreCase(callerClassName, definingClassName);
        }
        if (equalsIgnoreCase(access, CLASSDEF_ACCESS_PROTECTED)) {
            return equalsIgnoreCase(callerClassName, definingClassName)
                || ClassdefDefinitionManager::getInstance()->isA(
                    callerClassName, definingClassName);
        }
        return isAccessListAllowedFromClassdefContext(definingClassName, access, callerClassName);
    }
    //=============================================================================
    bool
    isCallableFromClassdefContext(const std::string& definingClassName,
        const stringVector& attributes, const std::string& callerClassName)
    {
        return isAccessAllowedFromClassdefContext(
            definingClassName, accessAttributeValue(attributes), callerClassName);
    }
    //=============================================================================
    bool
    isVisibleMember(const stringVector& attributes)
    {
        return hasPublicAccess(attributes)
            && !attributeListContains(attributes, CLASSDEF_ATTR_HIDDEN);
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
    void
    appendUnique(stringVector& values, const std::string& value)
    {
        if (std::find(values.begin(), values.end(), value) == values.end()) {
            values.push_back(value);
        }
    }
    //=============================================================================
    template <typename T>
    void
    appendUniqueByName(std::vector<T>& values, const T& value)
    {
        auto found = std::find_if(values.begin(), values.end(),
            [&value](const T& current) { return equalsIgnoreCase(current.name, value.name); });
        if (found == values.end()) {
            values.push_back(value);
        }
    }
    //=============================================================================
    template <typename T>
    void
    replaceOrAppendByName(std::vector<T>& values, const T& value)
    {
        auto found = std::find_if(values.begin(), values.end(),
            [&value](const T& current) { return equalsIgnoreCase(current.name, value.name); });
        if (found == values.end()) {
            values.push_back(value);
        } else {
            *found = value;
        }
    }
    //=============================================================================
    bool
    functionExistsInMemoryOrPath(const std::string& functionName)
    {
        FunctionDefPtr functionDef = nullptr;
        if (FunctionsInMemory::getInstance()->find(
                functionName, functionDef, FunctionsInMemory::FIND_FUNCTION_TYPE::MACRO)) {
            return true;
        }
        std::wstring filename;
        return PathFunctionIndexerManager::getInstance()->find(functionName, filename);
    }
    //=============================================================================
    void
    validateClassAttributes(const ClassdefDefinition& definition)
    {
        auto* manager = ClassdefDefinitionManager::getInstance();
        for (const auto& superclass : definition.superclasses) {
            const std::string superclassName = trim(superclass);
            if (superclassName.empty() || equalsIgnoreCase(superclassName, NLS_HANDLE_STR)) {
                continue;
            }
            if (manager->isSealedClass(superclassName)) {
                Error(CLASSDEF_ERROR_CANNOT_INHERIT_SEALED + superclassName);
            }
        }
        for (const auto& method : definition.methods) {
            if (equalsIgnoreCase(method.name, definition.name)) {
                continue;
            }
            for (const auto& superclass : definition.superclasses) {
                const std::string superclassName = trim(superclass);
                if (!superclassName.empty()
                    && manager->hasSealedMethod(superclassName, method.name)) {
                    Error(CLASSDEF_ERROR_CANNOT_OVERRIDE_SEALED + method.name);
                }
            }
        }
    }
    //=============================================================================
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
    isIdentifierStart(char ch)
    {
        return std::isalpha(static_cast<unsigned char>(ch)) != 0 || ch == '_';
    }
    //=============================================================================
    bool
    isIdentifierPart(char ch)
    {
        return std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_';
    }
    //=============================================================================
    std::string
    parseLeadingIdentifier(const std::string& value)
    {
        std::string trimmed = trim(value);
        if (trimmed.empty() || !isIdentifierStart(trimmed[0])) {
            return {};
        }
        size_t pos = 1;
        while (pos < trimmed.size() && isIdentifierPart(trimmed[pos])) {
            ++pos;
        }
        return trimmed.substr(0, pos);
    }
    //=============================================================================
    std::string
    parseLeadingQualifiedIdentifier(const std::string& value)
    {
        std::string trimmed = trim(value);
        std::string result = parseLeadingIdentifier(trimmed);
        if (result.empty()) {
            return {};
        }
        size_t pos = result.size();
        while (pos < trimmed.size() && trimmed[pos] == '.') {
            size_t partStart = pos + 1;
            if (partStart >= trimmed.size() || !isIdentifierStart(trimmed[partStart])) {
                break;
            }
            size_t partEnd = partStart + 1;
            while (partEnd < trimmed.size() && isIdentifierPart(trimmed[partEnd])) {
                ++partEnd;
            }
            result += trimmed.substr(pos, partEnd - pos);
            pos = partEnd;
        }
        return result;
    }
    //=============================================================================
    bool
    parseClassHeader(
        const std::string& line, ClassdefDefinition& definition, std::string& errorMessage)
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
            definition.superclasses = splitCommaList(rest.substr(1));
        }
        return true;
    }
    //=============================================================================
    void
    parsePropertiesBlock(const std::vector<std::string>& lines, size_t first, size_t last,
        const stringVector& blockAttributes, ClassdefDefinition& definition)
    {
        for (size_t k = first; k < last && k < lines.size(); ++k) {
            std::string line = withoutTrailingStatementSeparator(stripComment(lines[k]));
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
        if (rest.empty() || rest.front() != '(') {
            return true;
        }

        int depth = 0;
        bool inQuote = false;
        for (size_t k = 0; k < rest.size(); ++k) {
            const char ch = rest[k];
            if (ch == '\'') {
                inQuote = !inQuote;
            } else if (!inQuote && ch == '(') {
                ++depth;
            } else if (!inQuote && ch == ')') {
                --depth;
                if (depth == 0) {
                    arguments = trim(rest.substr(1, k - 1));
                    return true;
                }
            }
        }
        return true;
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
    std::string
    methodNameFromFunctionLine(const std::string& line)
    {
        std::string value = trim(stripComment(line));
        if (!startsWithKeyword(value, CLASSDEF_KEYWORD_FUNCTION)) {
            return {};
        }
        value = trim(value.substr(std::string(CLASSDEF_KEYWORD_FUNCTION).size()));
        size_t equals = value.find('=');
        if (equals != std::string::npos) {
            value = trim(value.substr(equals + 1));
        }
        if (!value.empty() && value.front() == '[') {
            size_t close = value.find(']');
            if (close != std::string::npos) {
                value = trim(value.substr(close + 1));
                if (!value.empty() && value.front() == '=') {
                    value = trim(value.substr(1));
                }
            }
        }
        return parseLeadingQualifiedIdentifier(value);
    }
    //=============================================================================
    std::string
    methodNameFromPrototypeLine(const std::string& line)
    {
        std::string value = withoutTrailingStatementSeparator(stripComment(line));
        if (value.empty() || startsWithKeyword(value, CLASSDEF_KEYWORD_FUNCTION)
            || startsWithKeyword(value, CLASSDEF_KEYWORD_END)) {
            return {};
        }
        size_t equals = value.find('=');
        if (equals != std::string::npos) {
            value = trim(value.substr(equals + 1));
        }
        if (!value.empty() && value.front() == '[') {
            size_t close = value.find(']');
            if (close != std::string::npos) {
                value = trim(value.substr(close + 1));
                if (!value.empty() && value.front() == '=') {
                    value = trim(value.substr(1));
                }
            }
        }
        return parseLeadingQualifiedIdentifier(value);
    }
    //=============================================================================
    std::string
    constructorReturnNameFromFunctionLine(const std::string& line)
    {
        std::string value = trim(stripComment(line));
        if (!startsWithKeyword(value, CLASSDEF_KEYWORD_FUNCTION)) {
            return {};
        }
        value = trim(value.substr(std::string(CLASSDEF_KEYWORD_FUNCTION).size()));
        size_t equals = value.find('=');
        if (equals == std::string::npos) {
            return {};
        }
        std::string left = trim(value.substr(0, equals));
        if (!left.empty() && left.front() == '[') {
            size_t close = left.find(']');
            if (close != std::string::npos) {
                left = trim(left.substr(1, close - 1));
                size_t comma = left.find(',');
                if (comma != std::string::npos) {
                    left = left.substr(0, comma);
                }
            }
        }
        return parseLeadingIdentifier(left);
    }
    //=============================================================================
    size_t
    parseMethodsBlock(const std::vector<std::string>& lines, size_t first,
        const stringVector& blockAttributes, ClassdefDefinition& definition)
    {
        size_t k = first;
        while (k < lines.size()) {
            std::string line = trim(stripComment(lines[k]));
            if (line.empty()) {
                ++k;
                continue;
            }
            if (startsWithKeyword(line, CLASSDEF_KEYWORD_END)) {
                return k;
            }
            if (!startsWithKeyword(line, CLASSDEF_KEYWORD_FUNCTION)) {
                ClassdefMethodDefinition method;
                method.name = methodNameFromPrototypeLine(lines[k]);
                method.sourceStartLine = k + 1;
                method.definingClassName = definition.name;
                method.attributes = blockAttributes;
                method.isStatic = attributeListContains(blockAttributes, CLASSDEF_ATTR_STATIC);
                if (!method.name.empty()) {
                    definition.methods.push_back(method);
                }
                ++k;
                continue;
            }

            int depth = 0;
            size_t last = k;
            for (; last < lines.size(); ++last) {
                std::string current = trim(stripComment(lines[last]));
                if (current.empty()) {
                    continue;
                }
                if (lineStartsFunctionBlock(current)) {
                    ++depth;
                } else if (startsWithKeyword(current, CLASSDEF_KEYWORD_END)) {
                    --depth;
                    if (depth == 0) {
                        break;
                    }
                }
            }
            if (last >= lines.size()) {
                return lines.size();
            }
            ClassdefMethodDefinition method;
            method.name = methodNameFromFunctionLine(lines[k]);
            method.sourceStartLine = k + 1;
            method.source = joinLines(lines, k, last);
            method.definingClassName = definition.name;
            method.attributes = blockAttributes;
            method.isStatic = attributeListContains(blockAttributes, CLASSDEF_ATTR_STATIC);
            if (!method.name.empty()) {
                definition.methods.push_back(method);
            }
            k = last + 1;
        }
        return lines.size();
    }
    //=============================================================================
    size_t
    findSimpleBlockEnd(const std::vector<std::string>& lines, size_t first)
    {
        for (size_t k = first; k < lines.size(); ++k) {
            if (startsWithKeyword(stripComment(lines[k]), CLASSDEF_KEYWORD_END)) {
                return k;
            }
        }
        return lines.size();
    }
    //=============================================================================
    MacroFunctionDef*
    parseGeneratedFunction(const std::string& source, const std::wstring& filename,
        size_t sourceLineOffset = 0, const std::vector<size_t>& sourceLineMap = {})
    {
        AbstractSyntaxTree::clearReferences();
        LexerContext lexerContext;
        ParserContext parsed = parseStringResult(lexerContext, source);
        if (parsed.state() != FuncDef || parsed.macroFunctionDef == nullptr) {
            AbstractSyntaxTreePtrVector refs = AbstractSyntaxTree::getReferences();
            AbstractSyntaxTree::deleteReferences(refs);
            AbstractSyntaxTree::clearReferences();
            return nullptr;
        }
        MacroFunctionDef* macro = parsed.macroFunctionDef;
        macro->setFilename(filename);
        macro->ptrAstCodeAsVector = AbstractSyntaxTree::getReferences();
        if (!sourceLineMap.empty()) {
            for (auto* node : macro->ptrAstCodeAsVector) {
                if (node == nullptr) {
                    continue;
                }
                int context = node->getContext();
                int line = context & 0x0000FFFF;
                if (line > 0 && static_cast<size_t>(line) <= sourceLineMap.size()) {
                    int column = context & 0xFFFF0000;
                    node->m_context = column
                        | (static_cast<int>(sourceLineMap[static_cast<size_t>(line) - 1]) & 0xFFFF);
                }
            }
        } else if (sourceLineOffset != 0) {
            for (auto* node : macro->ptrAstCodeAsVector) {
                if (node == nullptr) {
                    continue;
                }
                int context = node->getContext();
                int line = context & 0x0000FFFF;
                if (line > 0) {
                    int column = context & 0xFFFF0000;
                    node->m_context
                        = column | ((line + static_cast<int>(sourceLineOffset)) & 0xFFFF);
                }
            }
        }
        macro->overloadAutoMode = NLS_OVERLOAD_AUTO_OFF;
        AbstractSyntaxTree::clearReferences();
        return macro;
    }
    //=============================================================================
    struct GeneratedClassdefSource
    {
        std::string source;
        std::vector<size_t> sourceLineMap;
    };
    //=============================================================================
    std::string
    classInitializationSource(const ClassdefDefinition& definition, const std::string& objectName)
    {
        std::string source;
        if (ClassdefDefinitionManager::getInstance()->isHandleClass(definition.name)) {
            source += objectName + " = " + std::string(CLASSDEF_HANDLE_BUILTIN) + "('"
                + definition.name + "');\n";
        } else {
            source += objectName + " = class(struct(), '" + definition.name + "');\n";
        }
        std::vector<ClassdefPropertyDefinition> properties
            = ClassdefDefinitionManager::getInstance()->propertyDefinitions(definition.name);
        for (const auto& property : properties) {
            if (attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
                continue;
            }
            std::string defaultExpression = property.defaultExpression.empty()
                ? std::string(CLASSDEF_DEFAULT_PROPERTY_VALUE)
                : property.defaultExpression;
            source += objectName + "." + property.name + " = " + defaultExpression + ";\n";
        }
        return source;
    }
    //=============================================================================
    std::string
    generatedDefaultConstructorSource(const ClassdefDefinition& definition)
    {
        std::string source;
        source += "function obj = " + classNameLeaf(definition.name) + "()\n";
        source += classInitializationSource(definition, "obj");
        source += "end\n";
        return source;
    }
    //=============================================================================
    GeneratedClassdefSource
    injectConstructorInitialization(
        const ClassdefDefinition& definition, const ClassdefMethodDefinition& constructor)
    {
        GeneratedClassdefSource generated;
        std::vector<std::string> lines = splitLines(constructor.source);
        if (lines.empty()) {
            return generated;
        }
        std::string objectName = constructorReturnNameFromFunctionLine(lines.front());
        if (objectName.empty()) {
            return generated;
        }
        auto appendLine = [&](const std::string& line, size_t sourceLine) {
            generated.source += line;
            generated.source += "\n";
            generated.sourceLineMap.push_back(sourceLine);
        };
        appendLine(lines.front(), constructor.sourceStartLine);
        std::vector<std::string> initializationLines
            = splitLines(classInitializationSource(definition, objectName));
        for (const auto& line : initializationLines) {
            appendLine(line, constructor.sourceStartLine);
        }
        for (size_t k = 1; k < lines.size(); ++k) {
            appendLine(lines[k], constructor.sourceStartLine + k);
        }
        return generated;
    }
    //=============================================================================
    bool
    isPropertyAccessorMethodName(const std::string& methodName)
    {
        return methodName.rfind(CLASSDEF_METHOD_GET_PREFIX, 0) == 0
            || methodName.rfind(CLASSDEF_METHOD_SET_PREFIX, 0) == 0;
    }
    //=============================================================================
    std::string
    sanitizeGeneratedFunctionName(std::string value)
    {
        for (char& ch : value) {
            if (!isIdentifierPart(ch)) {
                ch = '_';
            }
        }
        return value;
    }
    //=============================================================================
    std::string
    generatedAccessorFunctionName(
        const ClassdefDefinition& definition, const ClassdefMethodDefinition& method)
    {
        return CLASSDEF_GENERATED_FUNCTION_PREFIX + sanitizeGeneratedFunctionName(definition.name)
            + "_" + sanitizeGeneratedFunctionName(method.name) + CLASSDEF_GENERATED_FUNCTION_SUFFIX;
    }
    //=============================================================================
    std::string
    generatedMethodSource(
        const ClassdefDefinition& definition, const ClassdefMethodDefinition& method)
    {
        if (!isPropertyAccessorMethodName(method.name)) {
            return method.source;
        }
        std::vector<std::string> lines = splitLines(method.source);
        if (lines.empty()) {
            return method.source;
        }
        const std::string generatedName = generatedAccessorFunctionName(definition, method);
        size_t pos = lines[0].find(method.name);
        if (pos != std::string::npos) {
            lines[0].replace(pos, method.name.size(), generatedName);
        }
        std::string source;
        for (const auto& line : lines) {
            source += line;
            source += "\n";
        }
        return source;
    }
    //=============================================================================
    void
    registerGeneratedFunction(const std::string& key, MacroFunctionDef* macro)
    {
        if (macro == nullptr) {
            return;
        }
        FunctionsInMemory::getInstance()->deleteMFunction(key);
        FunctionsInMemory::getInstance()->add(key, macro);
    }
    //=============================================================================
    void
    unregisterGeneratedFunction(const std::string& key)
    {
        FunctionsInMemory::getInstance()->deleteMFunction(key);
    }
    //=============================================================================
    void
    unregisterClassdefGeneratedFunctions(const ClassdefDefinition& definition)
    {
        unregisterGeneratedFunction(definition.name);
        for (const auto& property : definition.properties) {
            if (attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)) {
                unregisterGeneratedFunction(definition.name + "." + property.name);
            }
        }
        for (const auto& member : definition.enumerations) {
            unregisterGeneratedFunction(definition.name + "." + member);
        }
        if (!definition.enumerations.empty()) {
            unregisterGeneratedFunction(
                getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_EQ));
            unregisterGeneratedFunction(
                getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_NE));
            unregisterGeneratedFunction(
                getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_CHAR));
        }
        for (const auto& method : definition.methods) {
            if (equalsClassNameOrLeaf(method.name, definition.name) || method.source.empty()) {
                continue;
            }
            if (method.isStatic) {
                unregisterGeneratedFunction(definition.name + "." + method.name);
            } else if (equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DELETE)) {
                unregisterGeneratedFunction(CLASSDEF_GENERATED_FUNCTION_PREFIX + definition.name
                    + "_" + CLASSDEF_FUNCTION_DELETE + CLASSDEF_GENERATED_FUNCTION_SUFFIX);
                unregisterGeneratedFunction(
                    getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DELETE));
            } else {
                unregisterGeneratedFunction(getOverloadFunctionName(definition.name, method.name));
            }
        }
        unregisterGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_GET));
        unregisterGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_SET));
        unregisterGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_ISVALID));
        unregisterGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_FIELDNAMES));
        unregisterGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_STRUCT));
        unregisterGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DELETE));
    }
    //=============================================================================
    void
    registerConstantProperties(const ClassdefDefinition& definition, const std::wstring& filename)
    {
        for (const auto& property : definition.properties) {
            if (!attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)) {
                continue;
            }
            std::string source = "function value = " + classNameLeaf(definition.name) + "_"
                + property.name + "()\n";
            source += "value = ";
            source += property.defaultExpression.empty() ? CLASSDEF_DEFAULT_PROPERTY_VALUE
                                                         : property.defaultExpression;
            source += ";\nend\n";
            MacroFunctionDef* macro = parseGeneratedFunction(source, filename);
            registerGeneratedFunction(definition.name + "." + property.name, macro);
        }
    }
    //=============================================================================
    void
    registerEnumerationMembers(const ClassdefDefinition& definition, const std::wstring& filename)
    {
        size_t ordinal = 1;
        for (const auto& member : definition.enumerations) {
            std::string source
                = "function obj = " + classNameLeaf(definition.name) + "_" + member + "()\n";
            auto argumentsIt = definition.enumerationArguments.find(member);
            const std::string arguments = argumentsIt == definition.enumerationArguments.end()
                ? std::string()
                : trim(argumentsIt->second);
            if (!arguments.empty()) {
                source += "tmp = " + classNameLeaf(definition.name) + "(" + arguments + ");\n";
            }
            source += "obj = class(struct(), '" + definition.name + "');\n";
            if (!arguments.empty()) {
                std::vector<ClassdefPropertyDefinition> properties
                    = ClassdefDefinitionManager::getInstance()->propertyDefinitions(
                        definition.name);
                for (const auto& property : properties) {
                    if (attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
                        || attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
                        continue;
                    }
                    source += "obj." + property.name + " = tmp." + property.name + ";\n";
                }
            }
            source += "obj." + std::string(CLASSDEF_ENUM_PROPERTY_NAME) + " = '" + member + "';\n";
            source += "obj." + std::string(CLASSDEF_ENUM_PROPERTY_ORDINAL) + " = "
                + std::to_string(ordinal) + ";\n";
            source += "end\n";
            MacroFunctionDef* macro = parseGeneratedFunction(source, filename);
            registerGeneratedFunction(definition.name + "." + member, macro);
            ++ordinal;
        }
    }
    //=============================================================================
    void
    registerEnumerationOperators(const ClassdefDefinition& definition, const std::wstring& filename)
    {
        if (definition.enumerations.empty()) {
            return;
        }

        std::string eqSource = "function r = eq(a, b)\n";
        eqSource += "r = strcmp(a." + std::string(CLASSDEF_ENUM_PROPERTY_NAME) + ", b."
            + CLASSDEF_ENUM_PROPERTY_NAME + ");\n";
        eqSource += "end\n";
        registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_EQ),
            parseGeneratedFunction(eqSource, filename));

        std::string neSource = "function r = ne(a, b)\n";
        neSource += "r = ~eq(a, b);\n";
        neSource += "end\n";
        registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_NE),
            parseGeneratedFunction(neSource, filename));

        std::string charSource = "function r = char(obj)\n";
        charSource += "r = obj." + std::string(CLASSDEF_ENUM_PROPERTY_NAME) + ";\n";
        charSource += "end\n";
        registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_CHAR),
            parseGeneratedFunction(charSource, filename));
    }
    //=============================================================================
    void
    registerHandleSupport(const ClassdefDefinition& definition, const std::wstring& filename)
    {
        if (!ClassdefDefinitionManager::getInstance()->isHandleClass(definition.name)) {
            return;
        }

        std::string getSource = "function value = get(obj, name)\n";
        getSource += "value = " + std::string(CLASSDEF_HANDLE_GET_BUILTIN) + "(obj, name);\n";
        getSource += "end\n";
        registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_GET),
            parseGeneratedFunction(getSource, filename));

        std::string setSource = "function set(obj, name, value)\n";
        setSource += std::string(CLASSDEF_HANDLE_SET_BUILTIN) + "(obj, name, value);\n";
        setSource += "end\n";
        registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_SET),
            parseGeneratedFunction(setSource, filename));

        std::string isvalidSource = "function r = isvalid(obj)\n";
        isvalidSource += "r = " + std::string(CLASSDEF_HANDLE_ISVALID_BUILTIN) + "(obj);\n";
        isvalidSource += "end\n";
        registerGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_ISVALID),
            parseGeneratedFunction(isvalidSource, filename));

        std::string fieldnamesSource = "function names = fieldnames(obj)\n";
        fieldnamesSource += "names = properties(obj);\n";
        fieldnamesSource += "end\n";
        registerGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_FIELDNAMES),
            parseGeneratedFunction(fieldnamesSource, filename));

        std::string structSource = "function value = struct(obj)\n";
        structSource += "value = " + std::string(CLASSDEF_HANDLE_STRUCT_BUILTIN) + "(obj);\n";
        structSource += "end\n";
        registerGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_STRUCT),
            parseGeneratedFunction(structSource, filename));

        bool hasUserDelete = false;
        for (const auto& method : definition.methods) {
            if (equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DELETE)) {
                hasUserDelete = true;
                break;
            }
        }
        if (!hasUserDelete) {
            std::string deleteSource = "function delete(obj)\n";
            deleteSource += "for k = 1:numel(obj)\n";
            deleteSource += "notify(obj(k), '" + std::string(CLASSDEF_EVENT_OBJECT_BEING_DESTROYED)
                + "');\n";
            deleteSource += "end\n";
            deleteSource += std::string(CLASSDEF_HANDLE_DELETE_BUILTIN) + "(obj);\n";
            deleteSource += "end\n";
            registerGeneratedFunction(
                getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DELETE),
                parseGeneratedFunction(deleteSource, filename));
        }
    }
    //=============================================================================
    void
    registerMethods(const ClassdefDefinition& definition, const std::wstring& filename)
    {
        for (const auto& method : definition.methods) {
            if (equalsClassNameOrLeaf(method.name, definition.name) || method.source.empty()) {
                continue;
            }
            MacroFunctionDef* macro
                = parseGeneratedFunction(generatedMethodSource(definition, method), filename,
                    method.sourceStartLine == 0 ? 0 : method.sourceStartLine - 1);
            if (macro != nullptr) {
                macro->classdefOwnerClassName = definition.name;
            }
            if (method.isStatic) {
                registerGeneratedFunction(definition.name + "." + method.name, macro);
            } else if (equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DELETE)
                && ClassdefDefinitionManager::getInstance()->isHandleClass(definition.name)) {
                const std::string userDeleteName = CLASSDEF_GENERATED_FUNCTION_PREFIX
                    + definition.name + "_" + CLASSDEF_FUNCTION_DELETE
                    + CLASSDEF_GENERATED_FUNCTION_SUFFIX;
                registerGeneratedFunction(userDeleteName, macro);
                std::string deleteSource = "function delete(obj)\n";
                deleteSource += "for k = 1:numel(obj)\n";
                deleteSource += "notify(obj(k), '"
                    + std::string(CLASSDEF_EVENT_OBJECT_BEING_DESTROYED) + "');\n";
                deleteSource += userDeleteName + "(obj(k));\n";
                deleteSource += "end\n";
                deleteSource += std::string(CLASSDEF_HANDLE_DELETE_BUILTIN) + "(obj);\n";
                deleteSource += "end\n";
                MacroFunctionDef* deleteMacro = parseGeneratedFunction(deleteSource, filename);
                if (deleteMacro != nullptr) {
                    deleteMacro->classdefOwnerClassName = definition.name;
                }
                registerGeneratedFunction(
                    getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DELETE),
                    deleteMacro);
            } else {
                registerGeneratedFunction(
                    getOverloadFunctionName(definition.name, method.name), macro);
            }
        }
    }
    //=============================================================================
    FunctionDef*
    buildConstructor(const ClassdefDefinition& definition, const std::wstring& filename)
    {
        const ClassdefMethodDefinition* constructor = nullptr;
        for (const auto& method : definition.methods) {
            if (equalsClassNameOrLeaf(method.name, definition.name)) {
                constructor = &method;
                break;
            }
        }

        GeneratedClassdefSource generated;
        if (attributeListContains(definition.attributes, CLASSDEF_ATTR_ABSTRACT)) {
            generated.source = "function obj = " + classNameLeaf(definition.name) + "()\n";
            generated.source
                += "error('Cannot instantiate abstract class: " + definition.name + "');\n";
            generated.source += "end\n";
        } else {
            if (constructor == nullptr) {
                generated.source = generatedDefaultConstructorSource(definition);
            } else {
                generated = injectConstructorInitialization(definition, *constructor);
            }
        }
        if (generated.source.empty()) {
            return nullptr;
        }
        MacroFunctionDef* macro = parseGeneratedFunction(generated.source, filename,
            constructor == nullptr || constructor->sourceStartLine == 0
                ? 0
                : constructor->sourceStartLine - 1,
            generated.sourceLineMap);
        if (macro != nullptr) {
            macro->classdefOwnerClassName = definition.name;
        }
        registerGeneratedFunction(definition.name, macro);
        return macro;
    }
    //=============================================================================
    bool
    loadDefinitionFromFile(const std::wstring& filename, const std::string& expectedName,
        ClassdefDefinition& definition)
    {
        std::string source;
        if (!readTextFile(filename, source)) {
            return false;
        }
        std::string errorMessage;
        if (!parseClassdefSource(source, definition, errorMessage)) {
            return false;
        }
        if (!expectedName.empty() && !equalsIgnoreCase(expectedName, definition.name)) {
            if (!equalsIgnoreCase(classNameLeaf(expectedName), definition.name)) {
                return false;
            }
            definition.name = expectedName;
            for (auto& property : definition.properties) {
                property.definingClassName = definition.name;
            }
        }
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
classdefLooksLikeSource(const std::string& source)
{
    const std::vector<std::string> lines = splitLines(source);
    for (const auto& rawLine : lines) {
        const std::string line = trim(stripComment(rawLine));
        if (line.empty()) {
            continue;
        }
        return startsWithKeyword(line, CLASSDEF_KEYWORD_CLASSDEF);
    }
    return false;
}
//=============================================================================
bool
parseClassdefSource(
    const std::string& source, ClassdefDefinition& definition, std::string& errorMessage)
{
    definition = ClassdefDefinition {};
    const std::vector<std::string> lines = splitLines(source);
    size_t headerLine = lines.size();
    for (size_t k = 0; k < lines.size(); ++k) {
        std::string line = trim(stripComment(lines[k]));
        if (line.empty()) {
            continue;
        }
        if (!parseClassHeader(line, definition, errorMessage)) {
            return false;
        }
        headerLine = k;
        break;
    }
    if (definition.name.empty()) {
        errorMessage = CLASSDEF_ERROR_CLASSDEF_HEADER_EXPECTED;
        return false;
    }

    for (size_t k = headerLine + 1; k < lines.size(); ++k) {
        std::string line = trim(stripComment(lines[k]));
        if (line.empty()) {
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_PROPERTIES)) {
            stringVector attributes = parseAttributesFromHeader(line, CLASSDEF_KEYWORD_PROPERTIES);
            size_t end = findSimpleBlockEnd(lines, k + 1);
            if (end >= lines.size()) {
                errorMessage = CLASSDEF_ERROR_MISSING_END_PROPERTIES;
                return false;
            }
            parsePropertiesBlock(lines, k + 1, end, attributes, definition);
            k = end;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_METHODS)) {
            stringVector attributes = parseAttributesFromHeader(line, CLASSDEF_KEYWORD_METHODS);
            size_t end = parseMethodsBlock(lines, k + 1, attributes, definition);
            if (end >= lines.size()) {
                errorMessage = CLASSDEF_ERROR_MISSING_END_METHODS;
                return false;
            }
            k = end;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_EVENTS)) {
            size_t end = findSimpleBlockEnd(lines, k + 1);
            if (end >= lines.size()) {
                errorMessage = CLASSDEF_ERROR_MISSING_END_EVENTS;
                return false;
            }
            for (size_t j = k + 1; j < end; ++j) {
                std::string name = parseEventOrEnumerationName(lines[j]);
                if (!name.empty()) {
                    definition.events.push_back(name);
                }
            }
            k = end;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_ENUMERATION)) {
            size_t end = findSimpleBlockEnd(lines, k + 1);
            if (end >= lines.size()) {
                errorMessage = CLASSDEF_ERROR_MISSING_END_ENUMERATION;
                return false;
            }
            for (size_t j = k + 1; j < end; ++j) {
                std::string name;
                std::string arguments;
                if (parseEnumerationDeclaration(lines[j], name, arguments) && !name.empty()) {
                    definition.enumerations.push_back(name);
                    definition.enumerationArguments[name] = arguments;
                }
            }
            k = end;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_END)) {
            return true;
        }
    }
    errorMessage = CLASSDEF_ERROR_MISSING_END_CLASSDEF;
    return false;
}
//=============================================================================
ClassdefDefinitionManager*
ClassdefDefinitionManager::getInstance()
{
    static ClassdefDefinitionManager manager;
    return &manager;
}
//=============================================================================
void
ClassdefDefinitionManager::clear()
{
    for (const auto& entry : _definitions) {
        unregisterClassdefGeneratedFunctions(entry.second);
    }
    _definitions.clear();
    _filenames.clear();
}
//=============================================================================
bool
ClassdefDefinitionManager::isClassdefFile(const std::wstring& filename)
{
    std::string source;
    return readTextFile(filename, source) && classdefLooksLikeSource(source);
}
//=============================================================================
FunctionDef*
ClassdefDefinitionManager::loadConstructor(
    const std::wstring& filename, const std::string& expectedClassName)
{
    ClassdefDefinition definition;
    if (!loadDefinitionFromFile(filename, expectedClassName, definition)) {
        return nullptr;
    }
    _definitions[definition.name] = definition;
    _filenames[definition.name] = filename;
    validateClassAttributes(definition);
    registerMethods(definition, filename);
    registerConstantProperties(definition, filename);
    registerEnumerationMembers(definition, filename);
    registerEnumerationOperators(definition, filename);
    registerHandleSupport(definition, filename);
    return buildConstructor(definition, filename);
}
//=============================================================================
bool
ClassdefDefinitionManager::loadClass(const std::string& className)
{
    if (_definitions.find(className) != _definitions.end()) {
        return true;
    }
    std::wstring filename;
    if (!PathFunctionIndexerManager::getInstance()->find(className, filename)) {
        return false;
    }
    return loadConstructor(filename, className) != nullptr;
}
//=============================================================================
bool
ClassdefDefinitionManager::hasMethod(const std::string& className, const std::string& methodName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return false;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return false;
        }
        for (const auto& method : it->second.methods) {
            if (!method.isStatic && equalsIgnoreCase(method.name, methodName)) {
                return !isPropertyAccessorMethodName(method.name)
                    && isVisibleMember(method.attributes);
            }
        }
        if (functionExistsInMemoryOrPath(getOverloadFunctionName(trimmedClass, methodName))) {
            return true;
        }
        for (const auto& superclass : it->second.superclasses) {
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
ClassdefDefinitionManager::hasStaticMethod(
    const std::string& className, const std::string& methodName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return false;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return false;
        }
        for (const auto& method : it->second.methods) {
            if (method.isStatic && equalsIgnoreCase(method.name, methodName)
                && isVisibleMember(method.attributes)) {
                return true;
            }
        }
        for (const auto& superclass : it->second.superclasses) {
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
ClassdefDefinitionManager::hasProperty(
    const std::string& className, const std::string& propertyName)
{
    ClassdefPropertyDefinition property;
    return propertyDefinition(className, propertyName, property);
}
//=============================================================================
bool
ClassdefDefinitionManager::hasConstantProperty(
    const std::string& className, const std::string& propertyName)
{
    ClassdefPropertyDefinition property;
    return propertyDefinition(className, propertyName, property)
        && attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT);
}
//=============================================================================
bool
ClassdefDefinitionManager::hasDependentProperty(
    const std::string& className, const std::string& propertyName)
{
    ClassdefPropertyDefinition property;
    return propertyDefinition(className, propertyName, property)
        && attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT);
}
//=============================================================================
bool
ClassdefDefinitionManager::isObservablePropertyEvent(
    const std::string& className, const std::string& propertyName, const std::string& eventName)
{
    ClassdefPropertyDefinition property;
    if (!propertyDefinition(className, propertyName, property)) {
        return false;
    }
    if (equalsIgnoreCase(eventName, CLASSDEF_EVENT_PRE_GET)
        || equalsIgnoreCase(eventName, CLASSDEF_EVENT_POST_GET)) {
        return attributeListContains(property.attributes, CLASSDEF_ATTR_GET_OBSERVABLE);
    }
    if (equalsIgnoreCase(eventName, CLASSDEF_EVENT_PRE_SET)
        || equalsIgnoreCase(eventName, CLASSDEF_EVENT_POST_SET)) {
        return attributeListContains(property.attributes, CLASSDEF_ATTR_SET_OBSERVABLE);
    }
    return false;
}
//=============================================================================
bool
ClassdefDefinitionManager::canGetProperty(const std::string& className,
    const std::string& propertyName, const std::string& callerClassName)
{
    ClassdefPropertyDefinition property;
    if (!propertyDefinition(className, propertyName, property)) {
        return false;
    }
    return isAccessAllowedFromClassdefContext(property.definingClassName,
        propertyAccessAttributeValue(property.attributes, CLASSDEF_ATTR_GET_ACCESS),
        callerClassName);
}
//=============================================================================
bool
ClassdefDefinitionManager::canSetProperty(const std::string& className,
    const std::string& propertyName, const std::string& callerClassName, bool propertyAlreadyExists)
{
    ClassdefPropertyDefinition property;
    if (!propertyDefinition(className, propertyName, property)) {
        return false;
    }
    if (!propertyAlreadyExists) {
        return true;
    }
    if (attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)) {
        return false;
    }
    return isAccessAllowedFromClassdefContext(property.definingClassName,
        propertyAccessAttributeValue(property.attributes, CLASSDEF_ATTR_SET_ACCESS),
        callerClassName);
}
//=============================================================================
bool
ClassdefDefinitionManager::isA(const std::string& className, const std::string& expectedClassName)
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
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return false;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return false;
        }
        for (const auto& superclass : it->second.superclasses) {
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
ClassdefDefinitionManager::isHandleClass(const std::string& className)
{
    return isA(className, NLS_HANDLE_STR);
}
//=============================================================================
bool
ClassdefDefinitionManager::isAbstractClass(const std::string& className)
{
    if (!loadClass(className)) {
        return false;
    }
    const auto it = _definitions.find(className);
    return it != _definitions.end()
        && attributeListContains(it->second.attributes, CLASSDEF_ATTR_ABSTRACT);
}
//=============================================================================
bool
ClassdefDefinitionManager::isSealedClass(const std::string& className)
{
    if (!loadClass(className)) {
        return false;
    }
    const auto it = _definitions.find(className);
    return it != _definitions.end()
        && attributeListContains(it->second.attributes, CLASSDEF_ATTR_SEALED);
}
//=============================================================================
bool
ClassdefDefinitionManager::hasSealedMethod(
    const std::string& className, const std::string& methodName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return false;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return false;
        }
        for (const auto& method : it->second.methods) {
            if (equalsIgnoreCase(method.name, methodName)
                && attributeListContains(method.attributes, CLASSDEF_ATTR_SEALED)) {
                return true;
            }
        }
        for (const auto& superclass : it->second.superclasses) {
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
ClassdefDefinitionManager::resolveMethodFunction(const std::string& className,
    const std::string& methodName, std::string& functionName, const std::string& callerClassName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);

        std::string candidate = getOverloadFunctionName(trimmedClass, methodName);
        if (!loadClass(trimmedClass)) {
            if (functionExistsInMemoryOrPath(candidate)) {
                functionName = candidate;
                return true;
            }
            return false;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return false;
        }
        for (const auto& method : it->second.methods) {
            if (!method.isStatic && equalsIgnoreCase(method.name, methodName)) {
                if (isCallableFromClassdefContext(trimmedClass, method.attributes, callerClassName)
                    && functionExistsInMemoryOrPath(candidate)) {
                    functionName = candidate;
                    return true;
                }
                return false;
            }
        }
        if (functionExistsInMemoryOrPath(candidate)) {
            functionName = candidate;
            return true;
        }
        for (const auto& superclass : it->second.superclasses) {
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
ClassdefDefinitionManager::resolveStaticMethodFunction(const std::string& className,
    const std::string& methodName, std::string& functionName, const std::string& callerClassName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return false;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return false;
        }
        for (const auto& method : it->second.methods) {
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
        for (const auto& superclass : it->second.superclasses) {
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
ClassdefDefinitionManager::resolveConstantPropertyFunction(const std::string& className,
    const std::string& propertyName, std::string& functionName, const std::string& callerClassName)
{
    std::unordered_set<std::string> visited;
    std::function<bool(const std::string&)> visit = [&](const std::string& currentClass) -> bool {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return false;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return false;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return false;
        }
        for (const auto& property : it->second.properties) {
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
        for (const auto& superclass : it->second.superclasses) {
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
ClassdefDefinitionManager::resolvePropertyAccessorFunction(const std::string& className,
    const std::string& propertyName, const std::string& accessorPrefix, std::string& functionName)
{
    ClassdefPropertyDefinition property;
    if (!propertyDefinition(className, propertyName, property)) {
        functionName.clear();
        return false;
    }
    return resolveMethodFunction(className, accessorPrefix + "." + property.name, functionName,
        property.definingClassName.empty() ? className : property.definingClassName);
}
//=============================================================================
bool
ClassdefDefinitionManager::resolveEnumerationMemberFunction(
    const std::string& className, const std::string& memberName, std::string& functionName)
{
    if (!loadClass(className)) {
        functionName.clear();
        return false;
    }
    const auto it = _definitions.find(className);
    if (it == _definitions.end()) {
        functionName.clear();
        return false;
    }
    for (const auto& member : it->second.enumerations) {
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
stringVector
ClassdefDefinitionManager::methods(const std::string& className)
{
    stringVector names;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return;
        }
        for (const auto& superclass : it->second.superclasses) {
            collect(superclass);
        }
        for (const auto& method : it->second.methods) {
            if (!equalsIgnoreCase(method.name, trimmedClass)
                && !isPropertyAccessorMethodName(method.name)
                && isVisibleMember(method.attributes)) {
                appendUnique(names, method.name);
            }
        }
    };
    collect(className);

    const std::string overloadPrefix = "@" + className + "/";
    wstringVector knownMacros = PathFunctionIndexerManager::getInstance()->getMacrosList(true);
    for (const auto& macro : knownMacros) {
        std::string macroName = wstring_to_utf8(macro);
        if (macroName.rfind(overloadPrefix, 0) == 0) {
            const std::string methodName = macroName.substr(overloadPrefix.size());
            bool declared = false;
            bool visible = true;
            if (loadClass(className)) {
                const auto it = _definitions.find(className);
                if (it != _definitions.end()) {
                    for (const auto& method : it->second.methods) {
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
ClassdefDefinitionManager::properties(const std::string& className)
{
    stringVector names;
    for (const auto& property : propertyDefinitions(className)) {
        if (isVisibleProperty(property.attributes)) {
            appendUnique(names, property.name);
        }
    }
    return names;
}
//=============================================================================
stringVector
ClassdefDefinitionManager::enumerations(const std::string& className)
{
    if (!loadClass(className)) {
        return {};
    }
    const auto it = _definitions.find(className);
    if (it == _definitions.end()) {
        return {};
    }
    return it->second.enumerations;
}
//=============================================================================
stringVector
ClassdefDefinitionManager::events(const std::string& className)
{
    stringVector names;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return;
        }
        for (const auto& superclass : it->second.superclasses) {
            collect(superclass);
        }
        for (const auto& eventName : it->second.events) {
            appendUnique(names, eventName);
        }
    };
    collect(className);
    if (isHandleClass(className)) {
        appendUnique(names, CLASSDEF_EVENT_OBJECT_BEING_DESTROYED);
    }
    return names;
}
//=============================================================================
stringVector
ClassdefDefinitionManager::superclasses(const std::string& className)
{
    if (!loadClass(className)) {
        return {};
    }
    const auto it = _definitions.find(className);
    if (it == _definitions.end()) {
        return {};
    }
    stringVector names;
    for (const auto& superclass : it->second.superclasses) {
        std::string name = trim(superclass);
        if (!name.empty()) {
            names.push_back(name);
        }
    }
    return names;
}
//=============================================================================
stringVector
ClassdefDefinitionManager::classAttributes(const std::string& className)
{
    if (!loadClass(className)) {
        return {};
    }
    const auto it = _definitions.find(className);
    if (it == _definitions.end()) {
        return {};
    }
    return it->second.attributes;
}
//=============================================================================
std::vector<ClassdefPropertyDefinition>
ClassdefDefinitionManager::propertyDefinitions(const std::string& className)
{
    std::vector<ClassdefPropertyDefinition> properties;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return;
        }
        for (const auto& superclass : it->second.superclasses) {
            collect(superclass);
        }
        for (const auto& property : it->second.properties) {
            replaceOrAppendByName(properties, property);
        }
        if (!it->second.enumerations.empty()) {
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
ClassdefDefinitionManager::methodDefinitions(const std::string& className)
{
    std::vector<ClassdefMethodDefinition> methods;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return;
        }
        for (const auto& superclass : it->second.superclasses) {
            collect(superclass);
        }
        for (auto method : it->second.methods) {
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
ClassdefDefinitionManager::eventDefinitions(const std::string& className)
{
    std::vector<ClassdefEventDefinition> events;
    std::unordered_set<std::string> visited;
    std::function<void(const std::string&)> collect = [&](const std::string& currentClass) {
        const std::string trimmedClass = trim(currentClass);
        if (trimmedClass.empty() || visited.find(trimmedClass) != visited.end()) {
            return;
        }
        visited.insert(trimmedClass);
        if (!loadClass(trimmedClass)) {
            return;
        }
        const auto it = _definitions.find(trimmedClass);
        if (it == _definitions.end()) {
            return;
        }
        for (const auto& superclass : it->second.superclasses) {
            collect(superclass);
        }
        for (const auto& eventName : it->second.events) {
            ClassdefEventDefinition event;
            event.name = eventName;
            event.definingClassName = trimmedClass;
            appendUniqueByName(events, event);
        }
    };
    collect(className);
    if (isHandleClass(className)) {
        ClassdefEventDefinition destroyedEvent;
        destroyedEvent.name = CLASSDEF_EVENT_OBJECT_BEING_DESTROYED;
        destroyedEvent.definingClassName = NLS_HANDLE_STR;
        appendUniqueByName(events, destroyedEvent);
    }
    return events;
}
//=============================================================================
std::vector<ClassdefEnumerationDefinition>
ClassdefDefinitionManager::enumerationDefinitions(const std::string& className)
{
    std::vector<ClassdefEnumerationDefinition> enumerations;
    if (!loadClass(className)) {
        return enumerations;
    }
    const auto it = _definitions.find(className);
    if (it == _definitions.end()) {
        return enumerations;
    }
    for (const auto& memberName : it->second.enumerations) {
        ClassdefEnumerationDefinition member;
        member.name = memberName;
        member.definingClassName = className;
        const auto argumentsIt = it->second.enumerationArguments.find(memberName);
        member.arguments = argumentsIt == it->second.enumerationArguments.end()
            ? std::string()
            : argumentsIt->second;
        appendUniqueByName(enumerations, member);
    }
    return enumerations;
}
//=============================================================================
bool
ClassdefDefinitionManager::propertyDefinition(const std::string& className,
    const std::string& propertyName, ClassdefPropertyDefinition& property)
{
    for (const auto& currentProperty : propertyDefinitions(className)) {
        if (equalsIgnoreCase(currentProperty.name, propertyName)) {
            property = currentProperty;
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
