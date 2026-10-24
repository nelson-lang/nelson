//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserGenerator.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserAttributes.hpp"
#include "ClassdefParserScanner.hpp"
#include "ClassdefParserRegistry.hpp"
#include "ClassdefParserRegistration.hpp"
#include "ClassdefParserDefinitionQuery.hpp"
#include "AbstractSyntaxTree.hpp"
#include "Error.hpp"
#include "Evaluator.hpp"
#include "ParserInterface.hpp"
#include "LexerContext.hpp"
#include "Keywords.hpp"
#include "ClassName.hpp"
#include "ErrorID.hpp"
#include <algorithm>
#include <cctype>
#include <functional>
#include <unordered_set>
//=============================================================================
// Escape a resolved catalog message for a single-quoted generated literal.
static std::string
escapeForGeneratedLiteral(const std::string& text)
{
    std::string escaped;
    for (char c : text) {
        if (c == '\'') {
            escaped += "''";
        } else {
            escaped += c;
        }
    }
    return escaped;
}
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
static bool
parseSuperclassCallLine(const std::string& rawLine, std::string& superclass, std::string& atOperand,
    std::string& lhs, bool& assigned);
//=============================================================================
std::string
classInitializationSource(const ClassdefDefinition& definition, const std::string& objectName)
{
    std::string source;
    if (definitionIsHandleClass(definition)) {
        source += objectName + " = " + std::string(CLASSDEF_HANDLE_BUILTIN) + "('" + definition.name
            + "');\n";
    } else {
        source += objectName + " = " + std::string(CLASSDEF_VALUE_BUILTIN) + "('" + definition.name
            + "');\n";
    }
    std::vector<ClassdefPropertyDefinition> properties
        = classdefPropertyDefinitions(definition.name);
    for (const auto& property : properties) {
        if (attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT)
            || attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
            || attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
            continue;
        }
        std::string defaultExpression = propertyDefaultExpression(property);
        std::string valueExpression = defaultExpression;
        if (!isEvaluationInvariantDefaultExpression(defaultExpression)) {
            // The expression runs once per defining class (inherited
            // properties reuse the superclass value); every instance is
            // assigned the stored result.
            const std::string owningClass
                = property.definingClassName.empty() ? definition.name : property.definingClassName;
            const std::string keyArguments = "('" + owningClass + "', '" + property.name + "'";
            source += "[" CLASSDEF_PROPERTY_DEFAULT_FOUND_VARIABLE
                      ", " CLASSDEF_PROPERTY_DEFAULT_VALUE_VARIABLE
                      "] = " CLASSDEF_PROPERTY_DEFAULT_BUILTIN
                + keyArguments + ");\n";
            source += "if ~" CLASSDEF_PROPERTY_DEFAULT_FOUND_VARIABLE "\n";
            // A failing default is reported against the property and class
            // ("Invalid default value for property 'p' in class 'C': ...").
            source += "try\n";
            source += std::string(CLASSDEF_PROPERTY_DEFAULT_VALUE_VARIABLE) + " = "
                + defaultExpression + ";\n";
            source += "catch " CLASSDEF_PROPERTY_DEFAULT_ERROR_VARIABLE "\n";
            source += "error(['Invalid default value for property ''" + property.name
                + "'' in class ''" + owningClass
                + "'':', char(10), " CLASSDEF_PROPERTY_DEFAULT_ERROR_VARIABLE ".message]);\n";
            source += "end\n";
            source += std::string(CLASSDEF_PROPERTY_DEFAULT_BUILTIN) + keyArguments
                + ", " CLASSDEF_PROPERTY_DEFAULT_VALUE_VARIABLE ");\n";
            source += "end\n";
            valueExpression = CLASSDEF_PROPERTY_DEFAULT_VALUE_VARIABLE;
        }
        if (canUseStoredDefaultAssignment(definition, property)) {
            if (definitionIsHandleClass(definition)) {
                // Handle object: mutated in place, no reassignment needed.
                source += std::string(CLASSDEF_HANDLE_SET_STORED_BUILTIN) + "(" + objectName + ", '"
                    + property.name + "', " + valueExpression + ");\n";
            } else {
                // Value object: the store returns the updated copy.
                source += objectName + " = " + std::string(CLASSDEF_VALUE_SET_STORED_BUILTIN) + "("
                    + objectName + ", '" + property.name + "', " + valueExpression + ");\n";
            }
        } else {
            source += objectName + "." + property.name + " = " + valueExpression + ";\n";
        }
    }
    return source;
}
//=============================================================================
stringVector
implicitSuperclassConstructors(
    const ClassdefDefinition& definition, const stringVector& explicitlyCalled)
{
    // An ancestor without a constructor is skipped (its property defaults are
    // already part of classInitializationSource) and its own superclasses are
    // searched instead, so no intermediate default constructor runs.
    stringVector ancestors;
    std::unordered_set<std::string> visited(explicitlyCalled.begin(), explicitlyCalled.end());
    std::function<void(const ClassdefDefinition&)> visitSuperclasses
        = [&](const ClassdefDefinition& current) {
              for (const auto& superclass : current.superclasses) {
                  const std::string className = trim(superclass);
                  if (className.empty() || !visited.insert(className).second
                      || isBuiltinClassdef(className) || !classdefLoadClass(className)) {
                      continue;
                  }
                  const ClassdefDefinition* superDefinition = findClassdefDefinition(className);
                  if (superDefinition == nullptr) {
                      continue;
                  }
                  if (findConstructorMethod(*superDefinition) != nullptr) {
                      ancestors.push_back(superDefinition->name);
                  } else {
                      visitSuperclasses(*superDefinition);
                  }
              }
          };
    visitSuperclasses(definition);
    return ancestors;
}
//=============================================================================
std::string
generatedDefaultConstructorSource(const ClassdefDefinition& definition)
{
    std::string source;
    source += "function obj = " + classNameLeaf(definition.name) + "()\n";
    source += classInitializationSource(definition, "obj");
    for (const auto& ancestor : implicitSuperclassConstructors(definition)) {
        source += "obj = obj@" + ancestor + "();\n";
    }
    source += "end\n";
    return source;
}
//=============================================================================
std::string
generatedTypedEnumerationConstructorSource(const ClassdefDefinition& definition)
{
    const std::string baseType = enumerationBaseType(definition);
    if (baseType.empty() || definition.enumerations.empty()) {
        return {};
    }

    const std::string className = definition.name;
    std::string source = "function obj = " + classNameLeaf(className) + "(value)\n";
    source += "if nargin < 1\n";
    source += "error('Nelson:notEnoughInputs', '"
        + escapeForGeneratedLiteral(
            Nelson::resolveMessageTemplate("Nelson:notEnoughInputs", "Not enough input arguments."))
        + "');\n";
    source += "end\n";
    source += "if isa(value, '" + className + "')\n";
    source += "obj = value;\n";
    source += "return\n";
    source += "end\n";
    source += "if ischar(value) || isstring(value)\n";
    for (const auto& member : definition.enumerations) {
        source += "if strcmpi(char(value), '" + member + "')\n";
        source += "obj = " + className + "." + member + ";\n";
        source += "return\n";
        source += "end\n";
    }
    source += "error(_('Input does not correspond to an enumeration member.'));\n";
    source += "end\n";
    source += "if iscell(value)\n";
    source += "obj = [];\n";
    source += "for k = 1:numel(value)\n";
    source += "obj = [obj, " + className + "(value{k})];\n";
    source += "end\n";
    source += "return\n";
    source += "end\n";
    source += "obj = [];\n";
    source += "for k = 1:numel(value)\n";
    source += "matched = false;\n";
    size_t ordinal = 1;
    for (const auto& member : definition.enumerations) {
        auto argumentsIt = definition.enumerationArguments.find(member);
        std::string value = argumentsIt == definition.enumerationArguments.end()
            ? std::string()
            : trim(argumentsIt->second);
        stringVector values = splitCommaList(value);
        value = values.empty() ? std::to_string(ordinal) : values.front();
        source += "if ~matched && " + baseType + "(value(k)) == " + baseType + "(" + value + ")\n";
        source += "obj = [obj, " + className + "." + member + "];\n";
        source += "matched = true;\n";
        source += "end\n";
        ++ordinal;
    }
    source += "if ~matched\n";
    source += "error(_('Input does not correspond to an enumeration member.'));\n";
    source += "end\n";
    source += "end\n";
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
    // Superclasses the body does not construct explicitly are constructed
    // with no argument before the body runs.
    stringVector explicitlyCalled;
    for (size_t k = 1; k < lines.size(); ++k) {
        std::string superclass;
        std::string atOperand;
        std::string lhs;
        bool assigned = false;
        if (parseSuperclassCallLine(lines[k], superclass, atOperand, lhs, assigned)
            && atOperand == objectName) {
            explicitlyCalled.push_back(superclass);
        }
    }
    for (const auto& ancestor : implicitSuperclassConstructors(definition, explicitlyCalled)) {
        appendLine(
            objectName + " = " + objectName + "@" + ancestor + "();", constructor.sourceStartLine);
    }
    for (size_t k = 1; k < lines.size(); ++k) {
        // obj@Super(...) is parsed natively (OP_SUPERCLASS_CALL).
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
    return CLASSDEF_GENERATED_FUNCTION_PREFIX + sanitizeGeneratedFunctionName(definition.name) + "_"
        + sanitizeGeneratedFunctionName(method.name) + CLASSDEF_GENERATED_FUNCTION_SUFFIX;
}
//=============================================================================
std::string
generatedConstructorFunctionName(const ClassdefDefinition& definition)
{
    return CLASSDEF_GENERATED_FUNCTION_PREFIX + sanitizeGeneratedFunctionName(definition.name)
        + "_constructor" + CLASSDEF_GENERATED_FUNCTION_SUFFIX;
}
//=============================================================================
void
renameGeneratedFunctionLine(
    GeneratedClassdefSource& generated, const std::string& oldName, const std::string& newName)
{
    std::vector<std::string> lines = splitLines(generated.source);
    if (lines.empty()) {
        return;
    }
    size_t pos = lines[0].find(oldName);
    if (pos == std::string::npos) {
        return;
    }
    lines[0].replace(pos, oldName.size(), newName);
    generated.source.clear();
    for (const auto& line : lines) {
        generated.source += line;
        generated.source += "\n";
    }
}
//=============================================================================
size_t
findTopLevelChar(const std::string& value, char searched, size_t first)
{
    int depth = 0;
    bool inSingleQuote = false;
    bool inDoubleQuote = false;
    for (size_t k = first; k < value.size(); ++k) {
        char ch = value[k];
        if (!inSingleQuote && !inDoubleQuote && depth == 0 && ch == searched) {
            return k;
        }
        if (ch == '\'' && !inDoubleQuote) {
            inSingleQuote = !inSingleQuote;
        } else if (ch == '"' && !inSingleQuote) {
            inDoubleQuote = !inDoubleQuote;
        } else if (!inSingleQuote && !inDoubleQuote && (ch == '(' || ch == '[' || ch == '{')) {
            ++depth;
        } else if (!inSingleQuote && !inDoubleQuote && (ch == ')' || ch == ']' || ch == '}')) {
            if (depth > 0) {
                --depth;
            }
        }
    }
    return std::string::npos;
}
//=============================================================================
bool
isQualifiedIdentifier(const std::string& value)
{
    return !value.empty() && parseLeadingQualifiedIdentifier(value) == value;
}
//=============================================================================
// A superclass call x@Super(...) is only well-placed when the left operand of
// '@' names the enclosing method: in a constructor it is obj@Super(...) (the
// returned object), in a method M it must be M@Super(...) (calling the
// overridden method). Any other form makes the loader reject the whole class.
bool
hasMisplacedSuperclassCall(
    const ClassdefMethodDefinition& method, const std::string& className, std::string& badLine)
{
    const bool isConstructor = equalsClassNameOrLeaf(method.name, className);
    const std::vector<std::string> lines = splitLines(method.source);
    for (const auto& rawLine : lines) {
        std::string superclass;
        std::string atOperand;
        std::string lhs;
        bool assigned = false;
        if (!parseSuperclassCallLine(rawLine, superclass, atOperand, lhs, assigned)) {
            continue;
        }
        if (isConstructor) {
            // obj@Super(...) or obj = obj@Super(...): a plain identifier, and
            // if assigned, the same name on both sides.
            if (parseLeadingIdentifier(atOperand) == atOperand && (!assigned || lhs == atOperand)) {
                continue;
            }
        } else if (atOperand == method.name) {
            continue;
        }
        badLine = trim(rawLine);
        return true;
    }
    return false;
}
//=============================================================================
std::string
generatedMethodSource(const ClassdefDefinition& definition, const ClassdefMethodDefinition& method)
{
    std::vector<std::string> lines = splitLines(method.source);
    if (lines.empty()) {
        return method.source;
    }
    if (isPropertyAccessorMethodName(method.name)) {
        const std::string generatedName = generatedAccessorFunctionName(definition, method);
        size_t pos = lines[0].find(method.name);
        if (pos != std::string::npos) {
            lines[0].replace(pos, method.name.size(), generatedName);
        }
    }
    // method@Super(...) is parsed natively (OP_SUPERCLASS_CALL).
    std::string source;
    for (const auto& line : lines) {
        source += line;
        source += "\n";
    }
    return source;
}
//=============================================================================
std::string
appendClassdefLocalFunctions(const ClassdefDefinition& definition, std::string source)
{
    if (definition.localFunctionsSource.empty()) {
        return source;
    }
    if (!source.empty() && source.back() != '\n') {
        source += '\n';
    }
    source += definition.localFunctionsSource;
    return source;
}
//=============================================================================
// Splits a line holding x@Super(...) (or y = x@Super(...)) into its parts;
// false when the line holds no superclass call.
static bool
parseSuperclassCallLine(const std::string& rawLine, std::string& superclass, std::string& atOperand,
    std::string& lhs, bool& assigned)
{
    std::string body = trim(stripComment(rawLine));
    while (!body.empty() && (body.back() == ';' || body.back() == ',')) {
        body.pop_back();
        body = trim(body);
    }
    const size_t at = findTopLevelChar(body, '@');
    if (at == std::string::npos) {
        return false;
    }
    const size_t open = findTopLevelChar(body, '(', at + 1);
    if (open == std::string::npos) {
        return false;
    }
    superclass = trim(body.substr(at + 1, open - at - 1));
    if (!isQualifiedIdentifier(superclass)) {
        // '@' not followed by a class name: an anonymous handle, not a
        // superclass call.
        return false;
    }
    const std::string left = trim(body.substr(0, at));
    const size_t assign = findTopLevelChar(left, '=');
    assigned = assign != std::string::npos;
    atOperand = assigned ? trim(left.substr(assign + 1)) : left;
    lhs = assigned ? trim(left.substr(0, assign)) : std::string();
    return true;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
