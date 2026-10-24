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
#include "ClassdefParserDefinitionQuery.hpp"
#include "AbstractSyntaxTree.hpp"
#include "Error.hpp"
#include "Evaluator.hpp"
#include "ParserInterface.hpp"
#include "ClassName.hpp"
#include <algorithm>
#include <cctype>
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
bool
keywordTokenAt(const std::string& line, size_t pos, const std::string& keyword)
{
    if (pos + keyword.size() > line.size() || line.compare(pos, keyword.size(), keyword) != 0) {
        return false;
    }
    if (pos != 0) {
        const unsigned char previous = static_cast<unsigned char>(line[pos - 1]);
        if (isIdentifierPart(previous)
            || (std::isspace(previous) == 0 && previous != ';' && previous != ','
                && previous != '(')) {
            return false;
        }
    }
    if (pos + keyword.size() == line.size()) {
        return true;
    }
    const unsigned char next = static_cast<unsigned char>(line[pos + keyword.size()]);
    return std::isspace(next) != 0 || next == '(' || next == ';' || next == ',';
}
//=============================================================================
size_t
findFunctionHeaderBodySeparator(const std::string& line)
{
    bool inQuote = false;
    int parenDepth = 0;
    int bracketDepth = 0;
    int braceDepth = 0;
    bool inDoubleQuote = false;
    for (size_t pos = std::string(CLASSDEF_KEYWORD_FUNCTION).size(); pos < line.size(); ++pos) {
        const char ch = line[pos];
        if (!inDoubleQuote && ch == '\'') {
            inQuote = !inQuote;
            continue;
        }
        if (!inQuote && ch == '"') {
            inDoubleQuote = !inDoubleQuote;
            continue;
        }
        if (inQuote || inDoubleQuote) {
            continue;
        }
        if (ch == '(') {
            ++parenDepth;
        } else if (ch == ')' && parenDepth > 0) {
            --parenDepth;
        } else if (ch == '[') {
            ++bracketDepth;
        } else if (ch == ']' && bracketDepth > 0) {
            --bracketDepth;
        } else if (ch == '{') {
            ++braceDepth;
        } else if (ch == '}' && braceDepth > 0) {
            --braceDepth;
        } else if ((ch == ',' || ch == ';') && parenDepth == 0 && bracketDepth == 0
            && braceDepth == 0) {
            return pos;
        }
    }
    return std::string::npos;
}
//=============================================================================
bool
stripTrailingEndKeyword(std::string& value)
{
    value = trim(withoutTrailingStatementSeparator(value));
    if (value.size() < std::string(CLASSDEF_KEYWORD_END).size()) {
        return false;
    }
    size_t pos = value.size() - std::string(CLASSDEF_KEYWORD_END).size();
    if (!keywordTokenAt(value, pos, CLASSDEF_KEYWORD_END)) {
        return false;
    }
    value = trim(withoutTrailingStatementSeparator(value.substr(0, pos)));
    return true;
}
//=============================================================================
std::string
normalizedInlineFunctionSource(const std::string& source)
{
    const stringVector lines = splitLines(source);
    if (lines.size() != 1) {
        return source;
    }
    const std::string line = trim(lines.front());
    if (!startsWithKeyword(line, CLASSDEF_KEYWORD_FUNCTION)) {
        return source;
    }
    const size_t separator = findFunctionHeaderBodySeparator(line);
    if (separator == std::string::npos) {
        return source;
    }
    std::string body = line.substr(separator + 1);
    // Strip the trailing "end" of the inline method but keep the last statement's
    // own separator (';' or ','). It drives display suppression: removing it
    // would turn "r = x;" into a displayed assignment once the method is
    // expanded to several lines.
    std::string withoutTrailingEnd = trim(withoutTrailingStatementSeparator(body));
    if (withoutTrailingEnd.size() < std::string(CLASSDEF_KEYWORD_END).size()) {
        return source;
    }
    const size_t endPos = withoutTrailingEnd.size() - std::string(CLASSDEF_KEYWORD_END).size();
    if (!keywordTokenAt(withoutTrailingEnd, endPos, CLASSDEF_KEYWORD_END)) {
        return source;
    }
    std::string bodyContent = withoutTrailingEnd.substr(0, endPos);
    while (
        !bodyContent.empty() && std::isspace(static_cast<unsigned char>(bodyContent.back())) != 0) {
        bodyContent.pop_back();
    }
    std::string header = trim(line.substr(0, separator));
    return header + "\n" + bodyContent + "\nend";
}
//=============================================================================
bool
isInlineFunctionDefinition(const std::string& line)
{
    const std::string value = trim(line);
    if (!startsWithKeyword(value, CLASSDEF_KEYWORD_FUNCTION)) {
        return false;
    }
    const size_t separator = findFunctionHeaderBodySeparator(value);
    if (separator == std::string::npos) {
        return false;
    }
    std::string body = value.substr(separator + 1);
    return stripTrailingEndKeyword(body);
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
        size_t logicalLast = k;
        std::string line = trim(joinedContinuationLine(lines, k, lines.size(), logicalLast));
        if (line.empty()) {
            ++k;
            continue;
        }
        if (startsWithKeyword(line, CLASSDEF_KEYWORD_END)) {
            return k;
        }
        if (!startsWithKeyword(line, CLASSDEF_KEYWORD_FUNCTION)) {
            ClassdefMethodDefinition method;
            method.name = methodNameFromPrototypeLine(line);
            method.sourceStartLine = k + 1;
            method.definingClassName = definition.name;
            method.attributes = blockAttributes;
            method.isStatic = attributeListContains(blockAttributes, CLASSDEF_ATTR_STATIC);
            if (!method.name.empty()) {
                definition.methods.push_back(method);
            }
            k = logicalLast + 1;
            continue;
        }

        size_t last = logicalLast;
        if (!isInlineFunctionDefinition(line)) {
            int depth = 0;
            for (last = k; last < lines.size(); ++last) {
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
        }
        if (last >= lines.size()) {
            return lines.size();
        }
        std::string methodSource;
        if (logicalLast > k) {
            methodSource = line + "\n";
            if (logicalLast + 1 <= last) {
                methodSource += joinLines(lines, logicalLast + 1, last);
            }
        } else {
            methodSource = joinLines(lines, k, last);
        }
        ClassdefMethodDefinition method;
        method.name = methodNameFromFunctionLine(line);
        method.sourceStartLine = k + 1;
        method.source = normalizedInlineFunctionSource(methodSource);
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
    size_t sourceLineOffset, const std::vector<size_t>& sourceLineMap)
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
    for (MacroFunctionDef* cp = macro; cp != nullptr; cp = cp->nextFunction) {
        cp->setFilename(filename);
    }
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
                node->m_context = column | ((line + static_cast<int>(sourceLineOffset)) & 0xFFFF);
            }
        }
    }
    macro->overloadAutoMode = NLS_OVERLOAD_AUTO_OFF;
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
bool
canUseStoredDefaultAssignment(
    const ClassdefDefinition& definition, const ClassdefPropertyDefinition& property)
{
    // Property defaults are stored directly, bypassing any set method.
    // Validated properties keep the dotted assignment so validation still runs.
    if (!property.validationExpression.empty()) {
        return false;
    }
    // The handle raw store writes only the class's own direct storage, so a
    // handle subclass must dotted-assign inherited properties. The value raw
    // store addresses the full storage layout, so a value class can store
    // inherited properties directly too.
    if (definitionIsHandleClass(definition) && property.definingClassName != definition.name) {
        return false;
    }
    return !attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT)
        && !attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
        && !attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)
        && !attributeListContains(property.attributes, CLASSDEF_ATTR_ABORT_SET)
        && !attributeListContains(property.attributes, CLASSDEF_ATTR_GET_OBSERVABLE)
        && !attributeListContains(property.attributes, CLASSDEF_ATTR_SET_OBSERVABLE);
}
//=============================================================================
bool
isInvariantDefaultIdentifier(const std::string& name)
{
    // Pure, deterministic builders only: re-running them cannot be observed.
    static const std::unordered_set<std::string> names = { "true", "false", "pi", "eps", "Inf",
        "inf", "NaN", "nan", "double", "single", "logical", "char", "string", "int8", "int16",
        "int32", "int64", "uint8", "uint16", "uint32", "uint64", "zeros", "ones", "eye" };
    return names.find(name) != names.end();
}
//=============================================================================
// A default expression is evaluation-invariant when re-running it per
// instance is indistinguishable from evaluating it once: literals, literal
// aggregates, arithmetic on them and a short list of pure builders. Those
// keep the inline assignment (no per-instance overhead added); anything
// else - any other function call or class reference - must evaluate once
// per defining class with the result shared by every instance. Unknown
// constructs fall through to "not invariant", which is always correct.
bool
isEvaluationInvariantDefaultExpression(const std::string& expression)
{
    const size_t size = expression.size();
    size_t k = 0;
    // tracks whether a quote right after the previous token is a transpose
    bool previousIsValue = false;
    while (k < size) {
        const char ch = expression[k];
        if (std::isspace(static_cast<unsigned char>(ch)) != 0) {
            ++k;
            continue;
        }
        if (ch == '\'' || ch == '"') {
            if (ch == '\'' && previousIsValue) {
                ++k;
                continue;
            }
            ++k;
            bool closed = false;
            while (k < size) {
                if (expression[k] == ch) {
                    if (k + 1 < size && expression[k + 1] == ch) {
                        k += 2;
                        continue;
                    }
                    closed = true;
                    ++k;
                    break;
                }
                ++k;
            }
            if (!closed) {
                return false;
            }
            previousIsValue = true;
            continue;
        }
        if (std::isdigit(static_cast<unsigned char>(ch)) != 0
            || (ch == '.' && k + 1 < size
                && std::isdigit(static_cast<unsigned char>(expression[k + 1])) != 0)) {
            while (k < size && std::isdigit(static_cast<unsigned char>(expression[k])) != 0) {
                ++k;
            }
            if (k + 1 < size && expression[k] == '.'
                && std::isdigit(static_cast<unsigned char>(expression[k + 1])) != 0) {
                ++k;
                while (k < size && std::isdigit(static_cast<unsigned char>(expression[k])) != 0) {
                    ++k;
                }
            }
            if (k < size && (expression[k] == 'e' || expression[k] == 'E')) {
                size_t exponent = k + 1;
                if (exponent < size
                    && (expression[exponent] == '+' || expression[exponent] == '-')) {
                    ++exponent;
                }
                if (exponent < size
                    && std::isdigit(static_cast<unsigned char>(expression[exponent])) != 0) {
                    k = exponent;
                    while (
                        k < size && std::isdigit(static_cast<unsigned char>(expression[k])) != 0) {
                        ++k;
                    }
                }
            }
            if (k < size
                && (expression[k] == 'i' || expression[k] == 'j' || expression[k] == 'I'
                    || expression[k] == 'J')
                && (k + 1 >= size || !isIdentifierPart(expression[k + 1]))) {
                ++k;
            }
            if (k < size && isIdentifierPart(expression[k])) {
                return false;
            }
            previousIsValue = true;
            continue;
        }
        if (std::isalpha(static_cast<unsigned char>(ch)) != 0 || ch == '_') {
            const size_t start = k;
            while (k < size && isIdentifierPart(expression[k])) {
                ++k;
            }
            if (!isInvariantDefaultIdentifier(expression.substr(start, k - start))) {
                return false;
            }
            previousIsValue = true;
            continue;
        }
        switch (ch) {
        case ')':
        case ']':
        case '}': {
            previousIsValue = true;
            ++k;
            continue;
        }
        case '(':
        case '[':
        case '{':
        case ',':
        case ';':
        case ':':
        case '+':
        case '-':
        case '*':
        case '/':
        case '\\':
        case '^':
        case '<':
        case '>':
        case '=':
        case '&':
        case '|':
        case '~': {
            previousIsValue = false;
            ++k;
            continue;
        }
        case '.': {
            if (k + 1 < size) {
                const char next = expression[k + 1];
                if (next == '*' || next == '/' || next == '\\' || next == '^') {
                    previousIsValue = false;
                    k += 2;
                    continue;
                }
                if (next == '\'') {
                    previousIsValue = true;
                    k += 2;
                    continue;
                }
            }
            return false;
        }
        default: {
            return false;
        }
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
