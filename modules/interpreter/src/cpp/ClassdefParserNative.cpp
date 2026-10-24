//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserNative.hpp"
#include "ClassdefParserTokenizer.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserScanner.hpp"
#include "ClassdefParserAttributes.hpp"
#include "ClassdefParserRegistration.hpp"
#include "ClassdefParserGenerator.hpp"
#include "ClassdefParserValidation.hpp"
#include "ClassdefParser.hpp"
#include <unordered_set>
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
// Read a dotted identifier (a, a.b, a.b.c) at token index i and advance i past
// it. Returns the joined name, or empty when the token is not an identifier.
static std::string
readDottedName(const std::vector<ClassdefToken>& tokens, size_t& i)
{
    if (i >= tokens.size() || tokens[i].type != ClassdefTokenType::Identifier) {
        return {};
    }
    std::string name = tokens[i].text;
    ++i;
    while (i + 1 < tokens.size() && tokens[i].type == ClassdefTokenType::Dot
        && tokens[i + 1].type == ClassdefTokenType::Identifier) {
        name += ".";
        name += tokens[i + 1].text;
        i += 2;
    }
    return name;
}
//=============================================================================
bool
parseClassdefHeaderNative(const std::string& source, std::string& name, stringVector& superclasses,
    std::string& errorMessage)
{
    name.clear();
    superclasses.clear();
    errorMessage.clear();

    std::vector<ClassdefToken> tokens;
    if (!tokenizeClassdefSource(source, tokens, errorMessage)) {
        return false;
    }

    size_t i = 0;
    if (i >= tokens.size() || tokens[i].type != ClassdefTokenType::Identifier
        || tokens[i].text != "classdef") {
        errorMessage = "classdef header expected.";
        return false;
    }
    ++i;

    if (i < tokens.size() && tokens[i].type == ClassdefTokenType::LeftParen) {
        int depth = 1;
        ++i;
        while (i < tokens.size() && depth > 0) {
            if (tokens[i].type == ClassdefTokenType::LeftParen) {
                ++depth;
            } else if (tokens[i].type == ClassdefTokenType::RightParen) {
                --depth;
            } else if (tokens[i].type == ClassdefTokenType::EndOfInput) {
                errorMessage = "Unterminated classdef attribute list.";
                return false;
            }
            ++i;
        }
    }

    name = readDottedName(tokens, i);
    if (name.empty()) {
        errorMessage = "Class name expected after classdef.";
        return false;
    }

    // superclass list: < A & B.C & ...
    if (i < tokens.size() && tokens[i].type == ClassdefTokenType::Less) {
        ++i;
        while (i < tokens.size() && tokens[i].type != ClassdefTokenType::Newline
            && tokens[i].type != ClassdefTokenType::EndOfInput
            && tokens[i].type != ClassdefTokenType::Semicolon) {
            const std::string superclass = readDottedName(tokens, i);
            if (superclass.empty()) {
                errorMessage = "Invalid superclass declaration.";
                return false;
            }
            superclasses.push_back(superclass);
            if (i < tokens.size() && tokens[i].type == ClassdefTokenType::Ampersand) {
                ++i;
                continue;
            }
            break;
        }
        if (superclasses.empty()) {
            errorMessage = "Invalid superclass declaration.";
            return false;
        }
    }
    return true;
}
//=============================================================================
// Parse the header from an already-tokenized stream, leaving i just past the
// header. Separate from the string entry point to avoid re-tokenizing in the
// skeleton parser.
static bool
parseHeaderFromTokens(const std::vector<ClassdefToken>& tokens, size_t& i, std::string& name,
    stringVector& superclasses, std::string& errorMessage)
{
    if (i >= tokens.size() || tokens[i].type != ClassdefTokenType::Identifier
        || tokens[i].text != "classdef") {
        errorMessage = "classdef header expected.";
        return false;
    }
    ++i;
    if (i < tokens.size() && tokens[i].type == ClassdefTokenType::LeftParen) {
        int depth = 1;
        ++i;
        while (i < tokens.size() && depth > 0) {
            if (tokens[i].type == ClassdefTokenType::LeftParen) {
                ++depth;
            } else if (tokens[i].type == ClassdefTokenType::RightParen) {
                --depth;
            } else if (tokens[i].type == ClassdefTokenType::EndOfInput) {
                errorMessage = "Unterminated classdef attribute list.";
                return false;
            }
            ++i;
        }
    }
    name = readDottedName(tokens, i);
    if (name.empty()) {
        errorMessage = "Class name expected after classdef.";
        return false;
    }
    if (i < tokens.size() && tokens[i].type == ClassdefTokenType::Less) {
        ++i;
        while (i < tokens.size() && tokens[i].type != ClassdefTokenType::Newline
            && tokens[i].type != ClassdefTokenType::EndOfInput
            && tokens[i].type != ClassdefTokenType::Semicolon) {
            const std::string superclass = readDottedName(tokens, i);
            if (superclass.empty()) {
                errorMessage = "Invalid superclass declaration.";
                return false;
            }
            superclasses.push_back(superclass);
            if (i < tokens.size() && tokens[i].type == ClassdefTokenType::Ampersand) {
                ++i;
                continue;
            }
            break;
        }
    }
    return true;
}
//=============================================================================
static bool
isControlBlockOpener(const std::string& text)
{
    return text == "if" || text == "for" || text == "while" || text == "switch" || text == "try"
        || text == "parfor" || text == "spmd" || text == "function" || text == "arguments";
}
//=============================================================================
// True when the token at index i begins a statement (first token, or the previous
// token is a newline, semicolon or comma). Only used at positions reached after
// skipToStatementEnd, where any such separator was top-level.
static bool
atStatementStart(const std::vector<ClassdefToken>& tokens, size_t i)
{
    if (i == 0) {
        return true;
    }
    const ClassdefTokenType previous = tokens[i - 1].type;
    return previous == ClassdefTokenType::Newline || previous == ClassdefTokenType::Semicolon
        || previous == ClassdefTokenType::Comma;
}
//=============================================================================
// Method name from a signature statement: the dotted identifier before the
// argument list "(", or the last identifier when the method takes no arguments.
// Scanning starts at index start: past 'function' for concrete methods, at the
// statement start for abstract declarations, which omit 'function'.
static std::string
methodNameBeforeParen(const std::vector<ClassdefToken>& tokens, size_t start)
{
    std::string lastName;
    size_t k = start;
    while (k < tokens.size() && tokens[k].type != ClassdefTokenType::Newline
        && tokens[k].type != ClassdefTokenType::Semicolon
        && tokens[k].type != ClassdefTokenType::EndOfInput) {
        if (tokens[k].type == ClassdefTokenType::LeftParen) {
            break;
        }
        if (tokens[k].type == ClassdefTokenType::Identifier) {
            size_t j = k;
            const std::string dotted = readDottedName(tokens, j);
            lastName = dotted;
            k = j;
            continue;
        }
        ++k;
    }
    return lastName;
}
//=============================================================================
// Advance past the current statement, including its terminating top-level
// separator (newline, or a semicolon/comma outside brackets). A newline is
// always top-level: the tokenizer never emits one inside open brackets.
static void
skipToStatementEnd(const std::vector<ClassdefToken>& tokens, size_t& i)
{
    int depth = 0;
    while (i < tokens.size()) {
        const ClassdefTokenType type = tokens[i].type;
        if (type == ClassdefTokenType::EndOfInput) {
            return;
        }
        if (type == ClassdefTokenType::LeftParen || type == ClassdefTokenType::LeftBracket
            || type == ClassdefTokenType::LeftBrace) {
            ++depth;
        } else if (type == ClassdefTokenType::RightParen || type == ClassdefTokenType::RightBracket
            || type == ClassdefTokenType::RightBrace) {
            --depth;
        } else if (type == ClassdefTokenType::Newline) {
            ++i;
            return;
        } else if (depth <= 0
            && (type == ClassdefTokenType::Semicolon || type == ClassdefTokenType::Comma)) {
            ++i;
            return;
        }
        ++i;
    }
}
//=============================================================================
bool
parseClassdefSkeletonNative(
    const std::string& source, ClassdefSkeletonNative& skeleton, std::string& errorMessage)
{
    skeleton = ClassdefSkeletonNative {};
    errorMessage.clear();

    std::vector<ClassdefToken> tokens;
    if (!tokenizeClassdefSource(source, tokens, errorMessage)) {
        return false;
    }
    const std::vector<std::string> sourceLines = splitLines(source);
    size_t i = 0;
    if (!parseHeaderFromTokens(tokens, i, skeleton.name, skeleton.superclasses, errorMessage)) {
        return false;
    }
    // Reuse the scanner's parseClassHeader on the folded header line so the
    // attributes are captured identically and trailing text ("classdef X
    // trailing") is rejected with the same message. The token parse stops at the
    // first unexpected token, so the span extends to the end of the header line.
    if (!tokens.empty()) {
        const size_t headerStart = tokens.front().offset;
        size_t headerEndIndex = i;
        while (headerEndIndex < tokens.size()
            && tokens[headerEndIndex].type != ClassdefTokenType::Newline
            && tokens[headerEndIndex].type != ClassdefTokenType::Semicolon
            && tokens[headerEndIndex].type != ClassdefTokenType::EndOfInput) {
            ++headerEndIndex;
        }
        const size_t headerEnd
            = (headerEndIndex < tokens.size()) ? tokens[headerEndIndex].offset : source.size();
        const std::string headerSource = source.substr(headerStart, headerEnd - headerStart);
        const std::vector<std::string> headerLines = splitLines(headerSource);
        size_t headerLast = 0;
        const std::string headerLogical
            = joinedContinuationLine(headerLines, 0, headerLines.size(), headerLast);
        ClassdefDefinition headerDefinition;
        std::string headerError;
        if (!parseClassHeader(headerLogical, headerDefinition, headerError)) {
            errorMessage = headerError;
            return false;
        }
        skeleton.classAttributes = headerDefinition.attributes;
    }
    while (i < tokens.size()) {
        const ClassdefToken& token = tokens[i];
        if (token.type == ClassdefTokenType::Newline || token.type == ClassdefTokenType::Semicolon
            || token.type == ClassdefTokenType::Comma) {
            ++i;
            continue;
        }
        if (token.type == ClassdefTokenType::EndOfInput) {
            errorMessage = "Missing end for classdef.";
            return false;
        }
        if (token.type == ClassdefTokenType::Identifier && token.text == "end") {
            // Local functions are the source lines after the closing 'end'.
            if (token.line >= 1 && token.line < sourceLines.size()) {
                skeleton.localFunctionsSource
                    = joinLines(sourceLines, token.line, sourceLines.size() - 1);
            }
            return true;
        }
        if (token.type != ClassdefTokenType::Identifier
            || (token.text != "properties" && token.text != "methods" && token.text != "events"
                && token.text != "enumeration")) {
            errorMessage = "Invalid classdef block header: " + token.text;
            return false;
        }
        const std::string blockKind = token.text;
        const size_t blockHeaderOffset = token.offset;
        ++i;
        if (i < tokens.size() && tokens[i].type == ClassdefTokenType::LeftParen) {
            int depth = 1;
            ++i;
            while (i < tokens.size() && depth > 0) {
                if (tokens[i].type == ClassdefTokenType::LeftParen) {
                    ++depth;
                } else if (tokens[i].type == ClassdefTokenType::RightParen) {
                    --depth;
                } else if (tokens[i].type == ClassdefTokenType::EndOfInput) {
                    errorMessage = "Unterminated attribute list.";
                    return false;
                }
                ++i;
            }
        }
        // Reuse the scanner's parseAttributesFromHeader on the folded header.
        size_t headerEndIndex = i;
        while (headerEndIndex < tokens.size()
            && tokens[headerEndIndex].type != ClassdefTokenType::Newline
            && tokens[headerEndIndex].type != ClassdefTokenType::Semicolon
            && tokens[headerEndIndex].type != ClassdefTokenType::EndOfInput) {
            ++headerEndIndex;
        }
        const size_t headerEndOffset
            = (headerEndIndex < tokens.size()) ? tokens[headerEndIndex].offset : source.size();
        const std::string blockHeaderSource
            = source.substr(blockHeaderOffset, headerEndOffset - blockHeaderOffset);
        const std::vector<std::string> blockHeaderLines = splitLines(blockHeaderSource);
        size_t blockHeaderLast = 0;
        const std::string blockHeaderLogical
            = joinedContinuationLine(blockHeaderLines, 0, blockHeaderLines.size(), blockHeaderLast);
        stringVector blockAttributes;
        std::string blockAttributeError;
        parseAttributesFromHeader(blockHeaderLogical, blockKind, blockAttributes,
            blockAttributeError, blockKind != "enumeration");
        // A block header is "keyword [ (attributes) ]" only: the scanner's
        // validation rejects trailing text ("properties foo") identically.
        if (!blockAttributeError.empty()) {
            errorMessage = blockAttributeError;
            return false;
        }
        const bool isAbstractBlock = attributeListContains(blockAttributes, CLASSDEF_ATTR_ABSTRACT);
        const std::string blockAttributesJoined = joinAttributeFlags(blockAttributes);
        const std::string methodStaticFlag
            = attributeListContains(blockAttributes, CLASSDEF_ATTR_STATIC) ? "1" : "0";
        // Control structures inside method bodies nest their own 'end'.
        int nesting = 0;
        bool blockClosed = false;
        bool pendingMethod = false;
        size_t pendingMethodStartLine = 0;
        while (i < tokens.size()) {
            const ClassdefToken& bodyToken = tokens[i];
            if (bodyToken.type == ClassdefTokenType::EndOfInput) {
                break;
            }
            if (bodyToken.type == ClassdefTokenType::Newline
                || bodyToken.type == ClassdefTokenType::Semicolon
                || bodyToken.type == ClassdefTokenType::Comma) {
                ++i;
                continue;
            }
            const bool statementStart = atStatementStart(tokens, i);
            if (bodyToken.type == ClassdefTokenType::Identifier && bodyToken.text == "end"
                && statementStart) {
                if (nesting == 0) {
                    ++i;
                    blockClosed = true;
                    break;
                }
                if (nesting == 1 && pendingMethod && !skeleton.methodSources.empty()
                    && pendingMethodStartLine >= 1 && bodyToken.line >= pendingMethodStartLine
                    && bodyToken.line - 1 < sourceLines.size()) {
                    // This 'end' closes the pending method. A continued signature
                    // is folded to one line (as parseMethodsBlock does) so the
                    // generated function parses.
                    const size_t functionLineIndex = pendingMethodStartLine - 1;
                    const size_t endLineIndex = bodyToken.line - 1;
                    size_t signatureLastIndex = functionLineIndex;
                    const std::string foldedSignature = trim(joinedContinuationLine(
                        sourceLines, functionLineIndex, sourceLines.size(), signatureLastIndex));
                    std::string methodSource;
                    if (signatureLastIndex > functionLineIndex) {
                        methodSource = foldedSignature + "\n";
                        if (signatureLastIndex + 1 <= endLineIndex) {
                            methodSource
                                += joinLines(sourceLines, signatureLastIndex + 1, endLineIndex);
                        }
                    } else {
                        methodSource = joinLines(sourceLines, functionLineIndex, endLineIndex);
                    }
                    skeleton.methodSources.back() = normalizedInlineFunctionSource(methodSource);
                }
                if (nesting == 1) {
                    pendingMethod = false;
                }
                --nesting;
                skipToStatementEnd(tokens, i);
                continue;
            }
            if (statementStart && bodyToken.type == ClassdefTokenType::Identifier
                && isControlBlockOpener(bodyToken.text)) {
                if (bodyToken.text == "function" && nesting == 0 && blockKind == "methods") {
                    const std::string methodName = methodNameBeforeParen(tokens, i + 1);
                    if (!methodName.empty()) {
                        skeleton.methods.push_back(methodName);
                        skeleton.methodAttributes.push_back(blockAttributesJoined);
                        skeleton.methodStatic.push_back(methodStaticFlag);
                        skeleton.methodSources.push_back(std::string());
                        skeleton.methodSourceStartLines.push_back(bodyToken.line);
                        pendingMethod = true;
                        pendingMethodStartLine = bodyToken.line;
                    }
                }
                ++nesting;
                skipToStatementEnd(tokens, i);
                continue;
            }
            // Method prototype "[out =] name(args)" with no 'function' and no
            // 'end': an abstract or external method (body in another file), both
            // recorded as bodiless. A multi-output prototype starts with '['.
            if (statementStart && nesting == 0 && blockKind == "methods"
                && (bodyToken.type == ClassdefTokenType::Identifier
                    || bodyToken.type == ClassdefTokenType::LeftBracket)) {
                const std::string methodName = methodNameBeforeParen(tokens, i);
                if (!methodName.empty()) {
                    skeleton.methods.push_back(methodName);
                    skeleton.methodAttributes.push_back(blockAttributesJoined);
                    skeleton.methodStatic.push_back(methodStaticFlag);
                    skeleton.methodSources.push_back(std::string()); // abstract: no body
                    skeleton.methodSourceStartLines.push_back(bodyToken.line);
                }
                skipToStatementEnd(tokens, i);
                continue;
            }
            if (statementStart && nesting == 0 && bodyToken.type == ClassdefTokenType::Identifier
                && blockKind != "methods") {
                const size_t statementStartIndex = i;
                skipToStatementEnd(tokens, i);
                // Same folding helpers as the text scanner, so the parsing below
                // matches it.
                const size_t startOffset = tokens[statementStartIndex].offset;
                const size_t separatorIndex = i - 1;
                size_t endOffset = source.size();
                if (separatorIndex < tokens.size()
                    && (tokens[separatorIndex].type == ClassdefTokenType::Newline
                        || tokens[separatorIndex].type == ClassdefTokenType::Semicolon
                        || tokens[separatorIndex].type == ClassdefTokenType::Comma)) {
                    endOffset = tokens[separatorIndex].offset;
                }
                const std::string span = source.substr(startOffset, endOffset - startOffset);
                const std::vector<std::string> spanLines = splitLines(span);
                size_t foldedLast = 0;
                const std::string logicalLine = withoutTrailingStatementSeparator(
                    joinedContinuationLine(spanLines, 0, spanLines.size(), foldedLast));
                if (blockKind == "properties") {
                    const size_t equals = logicalLine.find('=');
                    std::string declaration = (equals == std::string::npos)
                        ? logicalLine
                        : logicalLine.substr(0, equals);
                    const std::string defaultExpression = (equals == std::string::npos)
                        ? CLASSDEF_DEFAULT_PROPERTY_VALUE
                        : withoutTrailingStatementSeparator(logicalLine.substr(equals + 1));
                    declaration = trim(declaration);
                    const std::string propertyName = parseLeadingIdentifier(declaration);
                    const std::string validation = propertyName.empty()
                        ? std::string()
                        : trim(declaration.substr(propertyName.size()));
                    if (!propertyName.empty()) {
                        skeleton.properties.push_back(propertyName);
                        skeleton.propertyDefaults.push_back(defaultExpression);
                        skeleton.propertyValidations.push_back(validation);
                        skeleton.propertyAttributes.push_back(blockAttributesJoined);
                    }
                } else if (blockKind == "events") {
                    const std::string eventName = parseLeadingIdentifier(logicalLine);
                    if (!eventName.empty()) {
                        skeleton.events.push_back(eventName);
                        skeleton.eventAttributes.push_back(blockAttributesJoined);
                    }
                } else if (blockKind == "enumeration") {
                    // A member must be a bare "Name" or "Name(args)"; an assignment
                    // ("Red = 1") or trailing text is rejected, as in the scanner.
                    if (!trim(logicalLine).empty()) {
                        std::string enumerationName;
                        std::string enumerationArgs;
                        if (!parseEnumerationDeclaration(
                                logicalLine, enumerationName, enumerationArgs)
                            || enumerationName.empty()) {
                            errorMessage = CLASSDEF_ERROR_INVALID_ENUMERATION + trim(logicalLine);
                            return false;
                        }
                        skeleton.enumerations.push_back(enumerationName);
                        skeleton.enumerationArguments.push_back(enumerationArgs);
                    }
                }
                continue;
            }
            skipToStatementEnd(tokens, i);
        }
        if (!blockClosed) {
            errorMessage = "Missing end for " + blockKind + " block.";
            return false;
        }
    }
    errorMessage = "Missing end for classdef.";
    return false;
}
//=============================================================================
static stringVector
splitJoinedAttributes(const std::string& joined)
{
    stringVector attributes;
    if (joined.empty()) {
        return attributes;
    }
    size_t start = 0;
    while (true) {
        const size_t position = joined.find(';', start);
        if (position == std::string::npos) {
            attributes.push_back(joined.substr(start));
            break;
        }
        attributes.push_back(joined.substr(start, position - start));
        start = position + 1;
    }
    return attributes;
}
//=============================================================================
bool
parseClassdefSourceNativeFull(
    const std::string& source, ClassdefDefinition& definition, std::string& errorMessage)
{
    ClassdefSkeletonNative skeleton;
    if (!parseClassdefSkeletonNative(source, skeleton, errorMessage)) {
        return false;
    }
    definition = ClassdefDefinition {};
    definition.name = skeleton.name;
    definition.attributes = skeleton.classAttributes;
    definition.superclasses = skeleton.superclasses;
    for (size_t p = 0; p < skeleton.properties.size(); ++p) {
        ClassdefPropertyDefinition property;
        property.name = skeleton.properties[p];
        property.defaultExpression
            = p < skeleton.propertyDefaults.size() ? skeleton.propertyDefaults[p] : std::string();
        property.validationExpression = p < skeleton.propertyValidations.size()
            ? skeleton.propertyValidations[p]
            : std::string();
        property.definingClassName = skeleton.name;
        if (p < skeleton.propertyAttributes.size()) {
            property.attributes = splitJoinedAttributes(skeleton.propertyAttributes[p]);
        }
        definition.properties.push_back(property);
    }
    for (size_t m = 0; m < skeleton.methods.size(); ++m) {
        ClassdefMethodDefinition method;
        method.name = skeleton.methods[m];
        method.source
            = m < skeleton.methodSources.size() ? skeleton.methodSources[m] : std::string();
        method.definingClassName = skeleton.name;
        method.isStatic = m < skeleton.methodStatic.size() && skeleton.methodStatic[m] == "1";
        method.sourceStartLine
            = m < skeleton.methodSourceStartLines.size() ? skeleton.methodSourceStartLines[m] : 0;
        if (m < skeleton.methodAttributes.size()) {
            method.attributes = splitJoinedAttributes(skeleton.methodAttributes[m]);
        }
        definition.methods.push_back(method);
    }
    for (size_t e = 0; e < skeleton.events.size(); ++e) {
        ClassdefEventDefinition event;
        event.name = skeleton.events[e];
        event.definingClassName = skeleton.name;
        if (e < skeleton.eventAttributes.size()) {
            event.attributes = splitJoinedAttributes(skeleton.eventAttributes[e]);
        }
        definition.events.push_back(event);
    }
    for (size_t en = 0; en < skeleton.enumerations.size(); ++en) {
        definition.enumerations.push_back(skeleton.enumerations[en]);
        definition.enumerationArguments[skeleton.enumerations[en]]
            = en < skeleton.enumerationArguments.size() ? skeleton.enumerationArguments[en]
                                                        : std::string();
    }
    definition.localFunctionsSource = skeleton.localFunctionsSource;

    // Same end-of-parse validation as parseClassdefSource. Duplicate
    // property/method and inheritance checks run later in classdefLoadClass.
    // Enumeration member names are case-sensitive.
    std::unordered_set<std::string> seenEnumerations;
    for (const auto& enumeration : definition.enumerations) {
        if (!seenEnumerations.insert(enumeration).second) {
            errorMessage = CLASSDEF_ERROR_DUPLICATE_ENUMERATION + enumeration;
            return false;
        }
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
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
