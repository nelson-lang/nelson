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
#include "Operators.hpp"
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
static bool
isKnownNonClassdefFile(const std::string& className);
static void
rememberNonClassdefFile(const std::string& className);
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
    const std::vector<std::string> lines
        = expandInlineBlocks(stripBlockComments(splitLines(source)));
    size_t headerLine = lines.size();
    for (size_t k = 0; k < lines.size(); ++k) {
        size_t logicalLast = k;
        std::string line = trim(joinedContinuationLine(lines, k, lines.size(), logicalLast));
        if (line.empty()) {
            continue;
        }
        if (!parseClassHeader(line, definition, errorMessage)) {
            return false;
        }
        headerLine = logicalLast;
        break;
    }
    if (definition.name.empty()) {
        errorMessage = CLASSDEF_ERROR_CLASSDEF_HEADER_EXPECTED;
        return false;
    }

    for (size_t k = headerLine + 1; k < lines.size(); ++k) {
        size_t logicalLast = k;
        std::string line = trim(joinedContinuationLine(lines, k, lines.size(), logicalLast));
        if (line.empty()) {
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_PROPERTIES)) {
            stringVector attributes;
            if (!parseAttributesFromHeader(
                    line, CLASSDEF_KEYWORD_PROPERTIES, attributes, errorMessage)) {
                return false;
            }
            size_t end = findSimpleBlockEnd(lines, logicalLast + 1);
            if (end >= lines.size()) {
                errorMessage = CLASSDEF_ERROR_MISSING_END_PROPERTIES;
                return false;
            }
            parsePropertiesBlock(lines, logicalLast + 1, end, attributes, definition);
            k = end;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_METHODS)) {
            stringVector attributes;
            if (!parseAttributesFromHeader(
                    line, CLASSDEF_KEYWORD_METHODS, attributes, errorMessage)) {
                return false;
            }
            size_t end = parseMethodsBlock(lines, logicalLast + 1, attributes, definition);
            if (end >= lines.size()) {
                errorMessage = CLASSDEF_ERROR_MISSING_END_METHODS;
                return false;
            }
            k = end;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_EVENTS)) {
            stringVector attributes;
            if (!parseAttributesFromHeader(
                    line, CLASSDEF_KEYWORD_EVENTS, attributes, errorMessage)) {
                return false;
            }
            size_t end = findSimpleBlockEnd(lines, logicalLast + 1);
            if (end >= lines.size()) {
                errorMessage = CLASSDEF_ERROR_MISSING_END_EVENTS;
                return false;
            }
            for (size_t j = logicalLast + 1; j < end; ++j) {
                std::string name = parseEventOrEnumerationName(lines[j]);
                if (!name.empty()) {
                    ClassdefEventDefinition event;
                    event.name = name;
                    event.definingClassName = definition.name;
                    event.attributes = attributes;
                    definition.events.push_back(event);
                }
            }
            k = end;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_ENUMERATION)) {
            stringVector attributes;
            if (!parseAttributesFromHeader(
                    line, CLASSDEF_KEYWORD_ENUMERATION, attributes, errorMessage, false)) {
                return false;
            }
            size_t end = findSimpleBlockEnd(lines, logicalLast + 1);
            if (end >= lines.size()) {
                errorMessage = CLASSDEF_ERROR_MISSING_END_ENUMERATION;
                return false;
            }
            for (size_t j = logicalLast + 1; j < end; ++j) {
                size_t logicalLastEnum = j;
                const std::string folded = joinedContinuationLine(lines, j, end, logicalLastEnum);
                std::string name;
                std::string arguments;
                const std::string declaration = withoutTrailingStatementSeparator(folded);
                if (trim(declaration).empty()) {
                    j = logicalLastEnum;
                    continue;
                }
                if (!parseEnumerationDeclaration(folded, name, arguments) || name.empty()) {
                    errorMessage = CLASSDEF_ERROR_INVALID_ENUMERATION + trim(declaration);
                    return false;
                }
                // Enumeration member names are case-sensitive; members that
                // differ only in case are distinct, not duplicates.
                for (const auto& existing : definition.enumerations) {
                    if (existing == name) {
                        errorMessage = CLASSDEF_ERROR_DUPLICATE_ENUMERATION + name;
                        return false;
                    }
                }
                definition.enumerations.push_back(name);
                definition.enumerationArguments[name] = arguments;
                j = logicalLastEnum;
            }
            k = end;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_END)) {
            if (k + 1 < lines.size()) {
                definition.localFunctionsSource = joinLines(lines, k + 1, lines.size() - 1);
            }
            std::string enumerationConflictName;
            enumerationMemberConflictName(definition, enumerationConflictName);
            if (!enumerationConflictName.empty()) {
                errorMessage = CLASSDEF_ERROR_ENUMERATION_MEMBER_CONFLICT + enumerationConflictName;
                return false;
            }
            if (!validateDefinitionBooleanAttributeValues(definition, errorMessage)) {
                return false;
            }
            if (!validateInferiorClassesAttributeValue(definition.attributes, errorMessage)) {
                return false;
            }
            if (!validateDefinitionAccessAttributeValues(definition, errorMessage)) {
                return false;
            }
            if (invalidAttributeCombination(definition, errorMessage)) {
                return false;
            }
            for (const auto& property : definition.properties) {
                if (property.validationExpression.empty()) {
                    continue;
                }
                stringVector dimensions;
                std::string typeName;
                stringVector validators;
                if (!parseValidationExpression(
                        property.validationExpression, dimensions, typeName, validators)) {
                    errorMessage = CLASSDEF_ERROR_INVALID_PROPERTY_VALIDATION + property.name;
                    return false;
                }
            }
            return true;
        }
    }
    errorMessage = CLASSDEF_ERROR_MISSING_END_CLASSDEF;
    return false;
}
//=============================================================================
std::string
classdefCanonicalDefinitionDumpFromDefinition(const ClassdefDefinition& definition)
{
    std::string dump;
    dump += "class=" + definition.name + "\n";
    dump += "attributes=" + joinAttributeFlags(definition.attributes) + "\n";
    std::string superclasses;
    for (const auto& superclass : definition.superclasses) {
        if (!superclasses.empty()) {
            superclasses += ",";
        }
        superclasses += superclass;
    }
    dump += "superclasses=" + superclasses + "\n";
    for (const auto& property : definition.properties) {
        dump += "property=" + property.name + ";attrs=" + joinAttributeFlags(property.attributes)
            + ";default=" + property.defaultExpression + ";validation="
            + property.validationExpression + ";defining=" + property.definingClassName + "\n";
    }
    for (const auto& method : definition.methods) {
        dump += "method=" + method.name + ";static=" + (method.isStatic ? "1" : "0")
            + ";attrs=" + joinAttributeFlags(method.attributes) + ";line="
            + std::to_string(static_cast<unsigned long long>(method.sourceStartLine)) + ";srclen="
            + std::to_string(static_cast<unsigned long long>(method.source.size())) + "\n";
    }
    for (const auto& event : definition.events) {
        dump += "event=" + event.name + ";attrs=" + joinAttributeFlags(event.attributes)
            + ";defining=" + event.definingClassName + "\n";
    }
    for (const auto& enumeration : definition.enumerations) {
        std::string arguments;
        const auto it = definition.enumerationArguments.find(enumeration);
        if (it != definition.enumerationArguments.end()) {
            arguments = it->second;
        }
        dump += "enumeration=" + enumeration + ";args=" + arguments + "\n";
    }
    dump += "localfns="
        + std::to_string(static_cast<unsigned long long>(definition.localFunctionsSource.size()))
        + "\n";
    return dump;
}
//=============================================================================
std::string
classdefCanonicalDefinitionDump(const std::string& source)
{
    // Deterministic, structural rendering of the parsed classdef definition, used
    // to compare parser outputs on test fixtures. Method bodies and local-function
    // sources are reduced to their length to keep the dump compact.
    ClassdefDefinition definition;
    std::string errorMessage;
    if (!parseClassdefSource(source, definition, errorMessage)) {
        return std::string("error=") + errorMessage;
    }
    return classdefCanonicalDefinitionDumpFromDefinition(definition);
}
//=============================================================================
void
classdefClear()
{
    for (const auto& entry : classdefDefinitions()) {
        unregisterClassdefGeneratedFunctions(entry.second);
    }
    classdefDefinitions().clear();
    classdefFilenames().clear();
    classdefLayouts().clear();
    classdefPropertyDefaultValues().clear();
    ++classdefGenerationCounter();
}
//=============================================================================
size_t
classdefRegistryGeneration()
{
    return classdefGenerationCounter();
}
//=============================================================================
bool
classdefStoredPropertyDefault(
    const std::string& className, const std::string& propertyName, ArrayOf& value)
{
    const auto& values = classdefPropertyDefaultValues();
    const auto classIt = values.find(className);
    if (classIt == values.end()) {
        return false;
    }
    const auto propertyIt = classIt->second.find(propertyName);
    if (propertyIt == classIt->second.end()) {
        return false;
    }
    value = propertyIt->second;
    return true;
}
//=============================================================================
void
classdefStorePropertyDefault(
    const std::string& className, const std::string& propertyName, const ArrayOf& value)
{
    classdefPropertyDefaultValues()[className][propertyName] = value;
}
//=============================================================================
const ClassdefPropertyDefinition*
classdefPlainStoredProperty(
    const std::string& className, const std::string& propertyName, bool forWrite)
{
    bool indexingOverloaded = false;
    return classdefPlainStoredProperty(className, propertyName, forWrite, indexingOverloaded);
}
//=============================================================================
const ClassdefPropertyDefinition*
classdefPlainStoredProperty(const std::string& className, const std::string& propertyName,
    bool forWrite, bool& indexingOverloaded)
{
    struct PropertyFastEntry
    {
        ClassdefPropertyDefinition property;
        bool plainRead = false;
        bool plainWrite = false;
        // subsref/subsasgn may come from path files: refreshed with the
        // function tables generation
        bool subsrefOverloaded = false;
        bool subsasgnOverloaded = false;
        bool overloadsResolved = false;
        uint64 functionsGeneration = 0;
    };
    static std::unordered_map<std::string, std::unordered_map<std::string, PropertyFastEntry>>
        cache;
    static size_t cacheGeneration = 0;
    if (cacheGeneration != classdefGenerationCounter()) {
        cache.clear();
        cacheGeneration = classdefGenerationCounter();
    }
    auto& classEntries = cache[className];
    auto it = classEntries.find(propertyName);
    if (it == classEntries.end()) {
        PropertyFastEntry entry;
        // the definition lookup ignores case: a direct access needs the exact name
        if (classdefPropertyDefinition(className, propertyName, entry.property)
            && entry.property.attributes.empty() && entry.property.name == propertyName) {
            std::string accessorName;
            entry.plainRead = !classdefResolvePropertyAccessorFunction(
                className, propertyName, "get", accessorName);
            entry.plainWrite = !classdefResolvePropertyAccessorFunction(
                className, propertyName, "set", accessorName);
        }
        it = classEntries.emplace(propertyName, std::move(entry)).first;
    }
    PropertyFastEntry& entry = it->second;
    if (!(forWrite ? entry.plainWrite : entry.plainRead)) {
        return nullptr;
    }
    const uint64 functionsGeneration = FunctionsInMemory::getInstance()->generation();
    if (!entry.overloadsResolved || entry.functionsGeneration != functionsGeneration) {
        entry.subsrefOverloaded = classdefHasMethod(className, SUBSREF_OPERATOR_STR);
        entry.subsasgnOverloaded = classdefHasMethod(className, SUBSASGN_OPERATOR_STR);
        entry.functionsGeneration = functionsGeneration;
        entry.overloadsResolved = true;
    }
    indexingOverloaded = forWrite ? entry.subsasgnOverloaded : entry.subsrefOverloaded;
    return &entry.property;
}
//=============================================================================
void
classdefRegisterBuiltinClass(const std::string& className, const stringVector& properties,
    const stringVector& methods, const stringVector& staticMethods, bool readOnlyProperties,
    const stringVector& events, const stringVector& superclasses)
{
    ClassdefDefinition definition;
    definition.name = className;
    definition.superclasses = superclasses;
    for (const std::string& propertyName : properties) {
        ClassdefPropertyDefinition property;
        property.name = propertyName;
        property.definingClassName = className;
        if (readOnlyProperties) {
            property.attributes.push_back(std::string(CLASSDEF_ATTR_SET_ACCESS) + "=private");
        }
        definition.properties.push_back(property);
    }
    for (const std::string& methodName : methods) {
        ClassdefMethodDefinition method;
        method.name = methodName;
        method.definingClassName = className;
        method.isStatic = std::find(staticMethods.begin(), staticMethods.end(), methodName)
            != staticMethods.end();
        definition.methods.push_back(method);
    }
    for (const std::string& eventName : events) {
        ClassdefEventDefinition event;
        event.name = eventName;
        event.definingClassName = className;
        definition.events.push_back(event);
    }
    builtinClassdefDefinitions()[className] = definition;
    classdefLayouts().erase(className);
    classdefPropertyDefaultValues().erase(className);
    ++classdefGenerationCounter();
}
//=============================================================================
bool
classdefIsClassdefFile(const std::wstring& filename)
{
    std::string source;
    return readTextFile(filename, source) && classdefLooksLikeSource(source);
}
//=============================================================================
FunctionDef*
classdefLoadConstructor(const std::wstring& filename, const std::string& expectedClassName)
{
    ClassdefDefinition definition;
    if (!loadDefinitionFromFile(filename, expectedClassName, definition)) {
        return nullptr;
    }
    const std::string builtinBase = builtinBaseTypeName(definition);
    if (!builtinBase.empty()) {
        // The built-in base data is stored in a hidden property.
        ClassdefPropertyDefinition baseProperty;
        baseProperty.name = CLASSDEF_BUILTIN_BASE_PROPERTY;
        baseProperty.definingClassName = definition.name;
        baseProperty.attributes.push_back(CLASSDEF_ATTR_HIDDEN);
        baseProperty.defaultExpression = builtinBase + "([])";
        definition.properties.push_back(baseProperty);
        // Simple value indexing dispatches only on declared indexing methods.
        const stringVector injectedMethods
            = { CLASSDEF_FUNCTION_PAREN_REFERENCE, CLASSDEF_FUNCTION_PAREN_ASSIGN };
        for (const auto& methodName : injectedMethods) {
            bool declared = false;
            for (const auto& method : definition.methods) {
                if (equalsIgnoreCase(method.name, methodName)) {
                    declared = true;
                    break;
                }
            }
            if (!declared) {
                ClassdefMethodDefinition method;
                method.name = methodName;
                method.definingClassName = definition.name;
                definition.methods.push_back(method);
            }
        }
    }
    validateClassAttributes(definition);
    for (const auto& method : definition.methods) {
        std::string badLine;
        if (hasMisplacedSuperclassCall(method, definition.name, badLine)) {
            RaiseError(std::string(CLASSDEF_ERROR_INVALID_SUPERCLASS_DECLARATION) + badLine,
                "Nelson:interpreter:invalidSuperclassDeclaration");
        }
    }
    classdefDefinitions()[definition.name] = definition;
    classdefFilenames()[definition.name] = filename;
    // reloading a class invalidates its stored defaults so the (possibly
    // edited) expressions are evaluated again on the next instantiation
    classdefPropertyDefaultValues().erase(definition.name);
    ++classdefGenerationCounter();
    registerSuperclassArray(definition, filename);
    registerMethods(definition, filename);
    registerMixinSupport(definition, filename);
    registerConstantProperties(definition, filename);
    registerEmptyStaticMethod(definition, filename);
    registerEnumerationMembers(definition, filename);
    registerEnumerationBaseConversion(definition, filename);
    registerEnumerationBaseSupport(definition, filename);
    registerEnumerationOperators(definition, filename);
    registerBuiltinBaseSupport(definition, filename);
    registerHandleSupport(definition, filename);
    classdefLayouts()[definition.name] = buildClassdefLayout(definition.name);
    return buildConstructor(definition, filename);
}
//=============================================================================
bool
classdefLoadClass(const std::string& className)
{
    if (classdefDefinitions().find(className) != classdefDefinitions().end()
        || isBuiltinClassdef(className)) {
        return true;
    }
    if (isKnownNonClassdefFile(className)) {
        return false;
    }
    std::wstring filename;
    if (!PathFunctionIndexerManager::getInstance()->find(className, filename)) {
        return false;
    }
    if (classdefLoadConstructor(filename, className) != nullptr) {
        return true;
    }
    rememberNonClassdefFile(className);
    return false;
}
//=============================================================================
// Names whose file is not a classdef: a function called with an object is
// probed as a class conversion on every call, which re-read and re-parsed its
// file each time. Forgotten whenever the function tables change (path or
// watched file changes, the same event that reloads the function itself) or
// a class definition is registered or cleared.
static std::unordered_set<std::string>&
nonClassdefFiles()
{
    static std::unordered_set<std::string> names;
    static uint64 functionsGeneration = 0;
    static size_t classesGeneration = 0;
    const uint64 currentFunctionsGeneration = FunctionsInMemory::getInstance()->generation();
    const size_t currentClassesGeneration = classdefRegistryGeneration();
    if (functionsGeneration != currentFunctionsGeneration
        || classesGeneration != currentClassesGeneration) {
        names.clear();
        functionsGeneration = currentFunctionsGeneration;
        classesGeneration = currentClassesGeneration;
    }
    return names;
}
//=============================================================================
static bool
isKnownNonClassdefFile(const std::string& className)
{
    return nonClassdefFiles().count(className) != 0;
}
//=============================================================================
static void
rememberNonClassdefFile(const std::string& className)
{
    nonClassdefFiles().insert(className);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
