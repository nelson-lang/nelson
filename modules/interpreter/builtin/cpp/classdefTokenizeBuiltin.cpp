//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "classdefTokenizeBuiltin.hpp"
#include "classdefParseDumpBuiltin.hpp"
#include "ClassdefParserTokenizer.hpp"
#include "ClassdefParserNative.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Single diagnostic entry point: dispatches by view name to the internal
// renderers below.
ArrayOfVector
Nelson::InterpreterGateway::classdefIntrospectBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    if (!(argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray())) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    const std::string mode = argIn[0].asUtf8String();
    ArrayOfVector forwarded;
    forwarded << argIn[1];
    if (mode == "parse_dump") {
        return classdefParseDumpBuiltin(eval, nLhs, forwarded);
    }
    if (mode == "tokenize") {
        return classdefTokenizeBuiltin(eval, nLhs, forwarded);
    }
    if (mode == "native_header") {
        return classdefNativeHeaderBuiltin(eval, nLhs, forwarded);
    }
    if (mode == "native_skeleton") {
        return classdefNativeSkeletonBuiltin(eval, nLhs, forwarded);
    }
    if (mode == "native_full_dump") {
        return classdefNativeFullDumpBuiltin(eval, nLhs, forwarded);
    }
    if (mode == "scanner_skeleton") {
        return classdefScannerSkeletonBuiltin(eval, nLhs, forwarded);
    }
    Error("Nelson:interpreter:unknownIntrospectionMode", "Unknown classdef introspection mode: %s",
        mode);
    return {};
}
//=============================================================================
// Internal diagnostic: returns the classdef token stream as a compact string.
ArrayOfVector
Nelson::InterpreterGateway::classdefTokenizeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string source;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        source = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    std::vector<classdef_detail::ClassdefToken> tokens;
    std::string errorMessage;
    if (!classdef_detail::tokenizeClassdefSource(source, tokens, errorMessage)) {
        retval << ArrayOf::characterArray(std::string("error=") + errorMessage);
        return retval;
    }
    retval << ArrayOf::characterArray(classdef_detail::classdefTokensToString(tokens));
    return retval;
}
//=============================================================================
// Internal diagnostic: parses the class header with the native token-driven
// parser and renders "name=...\nsuperclasses=a,b" for comparison against the
// text scanner (__classdef_parse_dump__).
ArrayOfVector
Nelson::InterpreterGateway::classdefNativeHeaderBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string source;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        source = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    std::string name;
    stringVector superclasses;
    std::string errorMessage;
    if (!classdef_detail::parseClassdefHeaderNative(source, name, superclasses, errorMessage)) {
        retval << ArrayOf::characterArray(std::string("error=") + errorMessage);
        return retval;
    }
    std::string joined;
    for (const auto& superclass : superclasses) {
        if (!joined.empty()) {
            joined += ",";
        }
        joined += superclass;
    }
    retval << ArrayOf::characterArray("name=" + name + "\nsuperclasses=" + joined + "\n");
    return retval;
}
//=============================================================================
// Internal diagnostic: renders the native structural skeleton (class name,
// superclasses and ordered member names) for comparison with the text scanner.
ArrayOfVector
Nelson::InterpreterGateway::classdefNativeSkeletonBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string source;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        source = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    classdef_detail::ClassdefSkeletonNative skeleton;
    std::string errorMessage;
    if (!classdef_detail::parseClassdefSkeletonNative(source, skeleton, errorMessage)) {
        retval << ArrayOf::characterArray(std::string("error=") + errorMessage);
        return retval;
    }
    std::string dump = "class=" + skeleton.name + "\n";
    std::string attributes;
    for (const auto& attribute : skeleton.classAttributes) {
        if (!attributes.empty()) {
            attributes += ";";
        }
        attributes += attribute;
    }
    dump += "attributes=" + attributes + "\n";
    std::string joined;
    for (const auto& superclass : skeleton.superclasses) {
        if (!joined.empty()) {
            joined += ",";
        }
        joined += superclass;
    }
    dump += "superclasses=" + joined + "\n";
    for (size_t p = 0; p < skeleton.properties.size(); ++p) {
        const std::string propertyDefault
            = p < skeleton.propertyDefaults.size() ? skeleton.propertyDefaults[p] : std::string();
        const std::string propertyValidation = p < skeleton.propertyValidations.size()
            ? skeleton.propertyValidations[p]
            : std::string();
        const std::string propertyAttribute = p < skeleton.propertyAttributes.size()
            ? skeleton.propertyAttributes[p]
            : std::string();
        dump += "property=" + skeleton.properties[p] + ";attrs=" + propertyAttribute
            + ";default=" + propertyDefault + ";validation=" + propertyValidation + "\n";
    }
    for (size_t mi = 0; mi < skeleton.methods.size(); ++mi) {
        const std::string methodStatic
            = mi < skeleton.methodStatic.size() ? skeleton.methodStatic[mi] : std::string("0");
        const std::string methodAttribute
            = mi < skeleton.methodAttributes.size() ? skeleton.methodAttributes[mi] : std::string();
        dump += "method=" + skeleton.methods[mi] + ";static=" + methodStatic
            + ";attrs=" + methodAttribute + "\n";
    }
    for (size_t ei = 0; ei < skeleton.events.size(); ++ei) {
        const std::string eventAttribute
            = ei < skeleton.eventAttributes.size() ? skeleton.eventAttributes[ei] : std::string();
        dump += "event=" + skeleton.events[ei] + ";attrs=" + eventAttribute + "\n";
    }
    for (size_t e = 0; e < skeleton.enumerations.size(); ++e) {
        const std::string enumerationArgs = e < skeleton.enumerationArguments.size()
            ? skeleton.enumerationArguments[e]
            : std::string();
        dump += "enumeration=" + skeleton.enumerations[e] + ";args=" + enumerationArgs + "\n";
    }
    retval << ArrayOf::characterArray(dump);
    return retval;
}
//=============================================================================
// Full canonical dump of the native token-driven parse, rendered with the same
// renderer as the scanner (classdefCanonicalDefinitionDump), so the two can be
// compared field by field.
ArrayOfVector
Nelson::InterpreterGateway::classdefNativeFullDumpBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string source;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        source = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    ClassdefDefinition definition;
    std::string errorMessage;
    if (!classdef_detail::parseClassdefSourceNativeFull(source, definition, errorMessage)) {
        retval << ArrayOf::characterArray(std::string("error=") + errorMessage);
        return retval;
    }
    retval << ArrayOf::characterArray(classdefCanonicalDefinitionDumpFromDefinition(definition));
    return retval;
}
//=============================================================================
// The same reduced skeleton view, produced from the text scanner's
// ClassdefDefinition, so both skeletons compare as plain strings (newlines in
// multi-line defaults do not break the comparison).
ArrayOfVector
Nelson::InterpreterGateway::classdefScannerSkeletonBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string source;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        source = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    ClassdefDefinition definition;
    std::string errorMessage;
    if (!parseClassdefSource(source, definition, errorMessage)) {
        retval << ArrayOf::characterArray(std::string("error=") + errorMessage);
        return retval;
    }
    std::string dump = "class=" + definition.name + "\n";
    std::string attributes;
    for (const auto& attribute : definition.attributes) {
        if (!attributes.empty()) {
            attributes += ";";
        }
        attributes += attribute;
    }
    dump += "attributes=" + attributes + "\n";
    std::string joined;
    for (const auto& superclass : definition.superclasses) {
        if (!joined.empty()) {
            joined += ",";
        }
        joined += superclass;
    }
    dump += "superclasses=" + joined + "\n";
    for (const auto& property : definition.properties) {
        std::string propertyAttribute;
        for (const auto& attribute : property.attributes) {
            if (!propertyAttribute.empty()) {
                propertyAttribute += ";";
            }
            propertyAttribute += attribute;
        }
        dump += "property=" + property.name + ";attrs=" + propertyAttribute + ";default="
            + property.defaultExpression + ";validation=" + property.validationExpression + "\n";
    }
    for (const auto& method : definition.methods) {
        std::string methodAttribute;
        for (const auto& attribute : method.attributes) {
            if (!methodAttribute.empty()) {
                methodAttribute += ";";
            }
            methodAttribute += attribute;
        }
        dump += "method=" + method.name + ";static=" + (method.isStatic ? "1" : "0")
            + ";attrs=" + methodAttribute + "\n";
    }
    for (const auto& event : definition.events) {
        std::string eventAttribute;
        for (const auto& attribute : event.attributes) {
            if (!eventAttribute.empty()) {
                eventAttribute += ";";
            }
            eventAttribute += attribute;
        }
        dump += "event=" + event.name + ";attrs=" + eventAttribute + "\n";
    }
    for (const auto& enumeration : definition.enumerations) {
        const auto it = definition.enumerationArguments.find(enumeration);
        const std::string enumerationArgs
            = it != definition.enumerationArguments.end() ? it->second : std::string();
        dump += "enumeration=" + enumeration + ";args=" + enumerationArgs + "\n";
    }
    retval << ArrayOf::characterArray(dump);
    return retval;
}
//=============================================================================
