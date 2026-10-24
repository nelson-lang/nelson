//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cctype>
#include "NelsonFormatSemanticCheck.hpp"
#include "AbstractSyntaxTree.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserNative.hpp"
#include "LexerContext.hpp"
#include "MacroFunctionDef.hpp"
#include "NelsonFormattingTokens.hpp"
#include "ParserInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static void
    appendText(std::string& out, const std::string& text)
    {
        out += std::to_string(text.size());
        out += ':';
        out += text;
    }
    //=============================================================================
    static std::string
    withoutWhitespace(const std::string& text)
    {
        std::string result;
        for (char ch : text) {
            if (std::isspace(static_cast<unsigned char>(ch)) == 0) {
                result += ch;
            }
        }
        return result;
    }
    //=============================================================================
    // Names and attribute lists ("Access=private" and "Access = private" alike).
    static void
    appendStrings(std::string& out, const stringVector& values)
    {
        out += '[';
        for (const std::string& value : values) {
            appendText(out, withoutWhitespace(value));
        }
        out += ']';
    }
    //=============================================================================
    // Node kind, operator, token and text of every node; source positions
    // (m_context) are left out since formatting moves code around on a line.
    // An arguments block keeps its source text opaque: compared without spaces.
    static void
    appendTree(std::string& out, const AbstractSyntaxTree* node, bool opaqueText = false)
    {
        for (; node != nullptr; node = node->right) {
            out += '(';
            out += std::to_string(static_cast<int>(node->type));
            out += ',';
            out += std::to_string(static_cast<int>(node->opNum));
            out += ',';
            out += std::to_string(node->tokenNumber);
            out += ',';
            appendText(out, opaqueText ? withoutWhitespace(node->text) : node->text);
            appendTree(out, node->down, opaqueText || node->opNum == OP_ARGUMENTS);
            out += ')';
        }
    }
    //=============================================================================
    static std::string
    parsedCodeFingerprint(const std::string& source)
    {
        std::string parseSource = maskFormattingBlockComments(source);
        if (parseSource.empty() || (parseSource.back() != '\n' && parseSource.back() != '\r')) {
            parseSource += '\n';
        }
        LexerContext lexerContext;
        ParserContext parserContext = parseStringResult(lexerContext, parseSource);
        std::string out = "state=" + std::to_string(static_cast<int>(parserContext.state()));
        if (parserContext.state() == ParseError) {
            // Nothing structural to compare: fall back to the text itself.
            appendText(out, withoutWhitespace(source));
            return out;
        }
        appendTree(out, parserContext.ast);
        for (MacroFunctionDef* functionDef = parserContext.macroFunctionDef; functionDef != nullptr;
            functionDef = functionDef->nextFunction) {
            out += "function=";
            appendText(out, functionDef->getName());
            appendStrings(out, functionDef->arguments);
            appendStrings(out, functionDef->returnVals);
            appendTree(out, functionDef->code);
        }
        return out;
    }
    //=============================================================================
    static std::string
    expressionFingerprint(const std::string& expression)
    {
        return expression.empty() ? std::string() : parsedCodeFingerprint("x = " + expression);
    }
    //=============================================================================
    static std::string
    classdefFingerprint(const std::string& source)
    {
        ClassdefDefinition definition;
        std::string errorMessage;
        if (!classdef_detail::parseClassdefSourceNativeFull(source, definition, errorMessage)) {
            return "classdef-error:" + withoutWhitespace(source);
        }
        std::string out = "classdef=";
        appendText(out, definition.name);
        appendStrings(out, definition.attributes);
        appendStrings(out, definition.superclasses);
        for (const auto& property : definition.properties) {
            out += "property=";
            appendText(out, property.name);
            appendStrings(out, property.attributes);
            appendText(out, expressionFingerprint(property.defaultExpression));
            appendText(out, withoutWhitespace(property.validationExpression));
        }
        for (const auto& method : definition.methods) {
            out += method.isStatic ? "static-method=" : "method=";
            appendText(out, method.name);
            appendStrings(out, method.attributes);
            appendText(out, parsedCodeFingerprint(method.source));
        }
        for (const auto& event : definition.events) {
            out += "event=";
            appendText(out, event.name);
            appendStrings(out, event.attributes);
        }
        for (const std::string& enumeration : definition.enumerations) {
            out += "enumeration=";
            appendText(out, enumeration);
            const auto it = definition.enumerationArguments.find(enumeration);
            if (it != definition.enumerationArguments.end()) {
                appendText(out, withoutWhitespace(it->second));
            }
        }
        out += "local-functions=";
        appendText(out, parsedCodeFingerprint(definition.localFunctionsSource));
        return out;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::string
formattingSemanticFingerprint(const std::string& source)
{
    const std::string maskedSource = maskFormattingBlockComments(source);
    return classdefLooksLikeSource(maskedSource) ? classdefFingerprint(maskedSource)
                                                 : parsedCodeFingerprint(source);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
