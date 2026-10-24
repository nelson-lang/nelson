//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerFunctionModel.hpp"
#include "CodeAnalyzerAstUtilities.hpp"
#include <algorithm>
#include <cctype>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    using namespace CodeAnalyzerAst;
    //=============================================================================
    CodeAnalyzerFunction
    parseFunctionSignatureLine(const stringVector& lines, size_t lineIndex)
    {
        CodeAnalyzerFunction signature;
        std::string code = stripComment(lines[lineIndex]);
        size_t functionStart = skipSpaces(code, 0);
        if (!startsWithWord(toLowerAscii(code.substr(functionStart)), "function")) {
            return signature;
        }
        signature.line = static_cast<int>(lineIndex + 1);
        size_t cursor = skipSpaces(code, functionStart + 8);
        size_t equals = codeAnalyzerAssignmentOperator(code.substr(cursor));
        if (equals != std::string::npos) {
            equals += cursor;
        }
        size_t nameStart = cursor;
        std::string namePart = trim(code.substr(cursor));
        if (equals != std::string::npos) {
            std::string outputs = trim(code.substr(cursor, equals - cursor));
            if (outputs.size() >= 2 && outputs.front() == '[' && outputs.back() == ']') {
                outputs = outputs.substr(1, outputs.size() - 2);
            }
            signature.outputs = splitNames(outputs);
            nameStart = skipSpaces(code, equals + 1);
            namePart = trim(code.substr(nameStart));
        }
        size_t paren = namePart.find('(');
        signature.name = trim(paren == std::string::npos ? namePart : namePart.substr(0, paren));
        size_t nameOffset = namePart.find(signature.name);
        if (!signature.name.empty() && nameOffset != std::string::npos) {
            signature.column = static_cast<int>(nameStart + nameOffset + 1);
        }
        size_t close = namePart.find(')', paren == std::string::npos ? 0 : paren);
        if (paren != std::string::npos && close != std::string::npos && close > paren) {
            signature.inputs = splitNames(namePart.substr(paren + 1, close - paren - 1));
        }
        return signature;
    }
    //=============================================================================
    size_t
    skipBalancedParentheses(const std::string& value, size_t cursor)
    {
        if (cursor >= value.size() || value[cursor] != '(') {
            return cursor;
        }
        int depth = 0;
        for (size_t k = cursor; k < value.size(); ++k) {
            if (value[k] == '(') {
                ++depth;
            } else if (value[k] == ')') {
                --depth;
                if (depth == 0) {
                    return k + 1;
                }
            }
        }
        return cursor;
    }
    //=============================================================================
    size_t
    matchingParenthesis(const std::string& value, size_t cursor)
    {
        if (cursor >= value.size() || value[cursor] != '(') {
            return std::string::npos;
        }
        int depth = 0;
        for (size_t k = cursor; k < value.size(); ++k) {
            if (value[k] == '(') {
                ++depth;
            } else if (value[k] == ')') {
                --depth;
                if (depth == 0) {
                    return k;
                }
            }
        }
        return std::string::npos;
    }
    //=============================================================================
    bool
    hasContinuationMarker(const std::string& line)
    {
        std::string code = trim(codeWithoutStringsAndComments(line));
        return code.size() >= 3 && code.compare(code.size() - 3, 3, "...") == 0;
    }
    //=============================================================================
    std::string
    withoutContinuationMarker(const std::string& line)
    {
        std::string code = codeWithoutStringsAndComments(line);
        size_t end = code.size();
        while (end > 0 && std::isspace(static_cast<unsigned char>(code[end - 1]))) {
            --end;
        }
        if (end >= 3 && code.compare(end - 3, 3, "...") == 0) {
            end -= 3;
        }
        return trim(code.substr(0, end));
    }
    //=============================================================================
    std::string
    logicalHeaderLine(const stringVector& lines, size_t lineIndex)
    {
        std::string logical = withoutContinuationMarker(lines[lineIndex]);
        while (lineIndex + 1 < lines.size() && hasContinuationMarker(lines[lineIndex])) {
            ++lineIndex;
            const std::string continuation = withoutContinuationMarker(lines[lineIndex]);
            if (!continuation.empty()) {
                if (!logical.empty()) {
                    logical.push_back(' ');
                }
                logical += continuation;
            }
        }
        return logical;
    }
    //=============================================================================
    bool
    parseAttributeList(
        const std::string& code, size_t cursor, std::vector<CodeAnalyzerClassAttribute>& attributes)
    {
        cursor = skipSpaces(code, cursor);
        size_t close = matchingParenthesis(code, cursor);
        if (close == std::string::npos) {
            return false;
        }
        ++cursor;
        while (cursor < close) {
            while (cursor < close
                && (std::isspace(static_cast<unsigned char>(code[cursor]))
                    || code[cursor] == ',')) {
                ++cursor;
            }
            if (cursor >= close) {
                break;
            }
            if (!isIdentifierStart(code[cursor])) {
                while (cursor < close && code[cursor] != ',') {
                    ++cursor;
                }
                continue;
            }
            const size_t first = cursor++;
            while (cursor < close && isIdentifierChar(code[cursor])) {
                ++cursor;
            }
            CodeAnalyzerClassAttribute attribute;
            attribute.name = code.substr(first, cursor - first);
            attribute.column = static_cast<int>(first + 1);
            attributes.push_back(attribute);
            int depth = 0;
            while (cursor < close) {
                if (code[cursor] == '(' || code[cursor] == '[' || code[cursor] == '{') {
                    ++depth;
                } else if (code[cursor] == ')' || code[cursor] == ']' || code[cursor] == '}') {
                    depth = std::max(0, depth - 1);
                } else if (code[cursor] == ',' && depth == 0) {
                    break;
                }
                ++cursor;
            }
        }
        return !attributes.empty();
    }
    //=============================================================================
    bool
    parseSuperclassList(const std::string& code, size_t cursor,
        std::vector<CodeAnalyzerClassSuperclass>& superclasses)
    {
        cursor = skipSpaces(code, cursor);
        if (cursor >= code.size() || code[cursor] != '<') {
            return false;
        }
        ++cursor;
        while (cursor < code.size()) {
            while (cursor < code.size()
                && (std::isspace(static_cast<unsigned char>(code[cursor])) || code[cursor] == ','
                    || code[cursor] == '&')) {
                ++cursor;
            }
            if (cursor >= code.size() || !isIdentifierStart(code[cursor])) {
                break;
            }
            const size_t first = cursor;
            ++cursor;
            while (
                cursor < code.size() && (isIdentifierChar(code[cursor]) || code[cursor] == '.')) {
                ++cursor;
            }
            CodeAnalyzerClassSuperclass superclass;
            superclass.name = code.substr(first, cursor - first);
            superclass.column = static_cast<int>(first + 1);
            superclasses.push_back(superclass);
        }
        return !superclasses.empty();
    }
    //=============================================================================
    bool
    parseClassdefHeader(const std::string& line, std::string& name, int& column,
        std::vector<CodeAnalyzerClassSuperclass>* superclasses = nullptr,
        std::vector<CodeAnalyzerClassAttribute>* attributes = nullptr)
    {
        name.clear();
        column = 0;
        if (superclasses != nullptr) {
            superclasses->clear();
        }
        if (attributes != nullptr) {
            attributes->clear();
        }
        std::string code = codeWithoutStringsAndComments(line);
        size_t cursor = skipSpaces(code, 0);
        if (!startsWithWord(toLowerAscii(code.substr(cursor)), "classdef")) {
            return false;
        }
        cursor = skipSpaces(code, cursor + 8);
        if (cursor < code.size() && code[cursor] == '(') {
            if (attributes != nullptr) {
                parseAttributeList(code, cursor, *attributes);
            }
            cursor = skipSpaces(code, skipBalancedParentheses(code, cursor));
        }
        if (cursor >= code.size() || !isIdentifierStart(code[cursor])) {
            return false;
        }
        size_t end = cursor + 1;
        while (end < code.size() && isIdentifierChar(code[end])) {
            ++end;
        }
        name = code.substr(cursor, end - cursor);
        column = static_cast<int>(cursor + 1);
        if (superclasses != nullptr) {
            parseSuperclassList(code, end, *superclasses);
        }
        return true;
    }
    //=============================================================================
    bool
    parseClassPropertyLine(const std::string& line, CodeAnalyzerClassProperty& property)
    {
        property = {};
        std::string code = codeWithoutStringsAndComments(line);
        size_t cursor = skipSpaces(code, 0);
        if (cursor >= code.size() || !isIdentifierStart(code[cursor])) {
            return false;
        }
        const size_t first = cursor;
        ++cursor;
        while (cursor < code.size() && isIdentifierChar(code[cursor])) {
            ++cursor;
        }
        const std::string name = code.substr(first, cursor - first);
        if (reservedWords().find(toLowerAscii(name)) != reservedWords().end()) {
            return false;
        }
        property.name = name;
        property.column = static_cast<int>(first + 1);
        return true;
    }
    //=============================================================================
    bool
    parseClassEventLine(const std::string& line, CodeAnalyzerClassEvent& event)
    {
        CodeAnalyzerClassProperty property;
        if (!parseClassPropertyLine(line, property)) {
            return false;
        }
        event.name = property.name;
        event.line = property.line;
        event.column = property.column;
        return true;
    }
    //=============================================================================
    bool
    parseClassExternalMethodLine(const std::string& line, CodeAnalyzerClassMethod& method)
    {
        method = {};
        std::string code = codeWithoutStringsAndComments(line);
        size_t cursor = skipSpaces(code, 0);
        if (cursor >= code.size() || startsWithWord(toLowerAscii(code.substr(cursor)), "end")) {
            return false;
        }
        size_t equals = codeAnalyzerAssignmentOperator(code.substr(cursor));
        if (equals != std::string::npos) {
            cursor = skipSpaces(code, cursor + equals + 1);
        }
        if (cursor >= code.size() || !isIdentifierStart(code[cursor])) {
            return false;
        }
        const size_t first = cursor;
        ++cursor;
        while (cursor < code.size() && isIdentifierChar(code[cursor])) {
            ++cursor;
        }
        const std::string name = code.substr(first, cursor - first);
        if (reservedWords().find(toLowerAscii(name)) != reservedWords().end()) {
            return false;
        }
        cursor = skipSpaces(code, cursor);
        if (cursor < code.size() && code[cursor] != '(' && code[cursor] != ';') {
            return false;
        }
        method.name = name;
        method.column = static_cast<int>(first + 1);
        return true;
    }
    //=============================================================================
    std::vector<CodeAnalyzerFunction>
    parseFunctionSignatures(const stringVector& lines, MacroFunctionDef* parsedFunction)
    {
        struct Block
        {
            bool function = false;
            size_t functionIndex = 0;
            bool classdef = false;
            bool methods = false;
            std::string className;
        };
        auto nearestClassName = [](const std::vector<Block>& blocks) {
            auto found = std::find_if(blocks.rbegin(), blocks.rend(),
                [](const Block& block) { return block.classdef || block.methods; });
            return found == blocks.rend() ? std::string() : found->className;
        };
        auto opensRegularNonFunctionBlock = [](const std::string& line) {
            return startsWithWord(line, "if") || startsWithWord(line, "for")
                || startsWithWord(line, "while") || startsWithWord(line, "switch")
                || startsWithWord(line, "try") || startsWithWord(line, "arguments")
                || startsWithWord(line, "properties") || startsWithWord(line, "events")
                || startsWithWord(line, "enumeration");
        };
        std::vector<CodeAnalyzerFunction> signatures;
        std::vector<Block> blocks;
        for (size_t k = 0; k < lines.size(); ++k) {
            std::string line = toLowerAscii(trim(stripComment(lines[k])));
            CodeAnalyzerFunction signature = parseFunctionSignatureLine(lines, k);
            if (signature.line > 0) {
                auto parent = std::find_if(blocks.rbegin(), blocks.rend(),
                    [](const Block& block) { return block.function; });
                signature.parentLine
                    = parent == blocks.rend() ? 0 : signatures[parent->functionIndex].line;
                signature.depth = static_cast<int>(std::count_if(blocks.begin(), blocks.end(),
                    [](const Block& block) { return block.function; }));
                signature.classdefMethod = std::any_of(
                    blocks.begin(), blocks.end(), [](const Block& block) { return block.methods; });
                if (signature.classdefMethod) {
                    signature.classdefOwnerName = nearestClassName(blocks);
                }
                signatures.push_back(signature);
                blocks.push_back({ true, signatures.size() - 1, false, false, {} });
                continue;
            }
            std::string className;
            int classNameColumn = 0;
            if (parseClassdefHeader(logicalHeaderLine(lines, k), className, classNameColumn)) {
                blocks.push_back({ false, 0, true, false, className });
                continue;
            }
            if (startsWithWord(line, "methods")) {
                blocks.push_back({ false, 0, false, true, nearestClassName(blocks) });
                continue;
            }
            if (opensRegularNonFunctionBlock(line)) {
                blocks.push_back({ false, 0, false, false, {} });
                continue;
            }
            if (startsWithWord(line, "end") && !blocks.empty()) {
                if (blocks.back().function) {
                    signatures[blocks.back().functionIndex].endLine = static_cast<int>(k + 1);
                }
                blocks.pop_back();
            }
        }
        while (!blocks.empty()) {
            if (blocks.back().function) {
                signatures[blocks.back().functionIndex].endLine = static_cast<int>(lines.size());
            }
            blocks.pop_back();
        }
        MacroFunctionDef* current = parsedFunction;
        for (size_t k = 0; k < signatures.size() && current != nullptr; ++k) {
            if (signatures[k].name.empty() || signatures[k].name == current->getName()) {
                signatures[k].name = current->getName();
                signatures[k].inputs = current->arguments;
                signatures[k].outputs = current->returnVals;
                signatures[k].local = current->localFunction;
                signatures[k].nested = current->nestedFunction;
                current = current->nextFunction;
            }
        }
        return signatures;
    }
    //=============================================================================
    std::vector<CodeAnalyzerClassdef>
    parseClassdefs(const stringVector& lines)
    {
        struct Block
        {
            bool classdef = false;
            size_t classIndex = 0;
            bool properties = false;
            bool events = false;
            bool methods = false;
        };
        auto activePropertiesClass = [](const std::vector<Block>& blocks) -> const size_t* {
            if (blocks.empty() || !blocks.back().properties) {
                return nullptr;
            }
            return &blocks.back().classIndex;
        };
        auto activeEventsClass = [](const std::vector<Block>& blocks) -> const size_t* {
            if (blocks.empty() || !blocks.back().events) {
                return nullptr;
            }
            return &blocks.back().classIndex;
        };
        auto activeMethodsClass = [](const std::vector<Block>& blocks) -> const size_t* {
            if (blocks.empty() || !blocks.back().methods) {
                return nullptr;
            }
            return &blocks.back().classIndex;
        };
        auto nearestClassIndex = [](const std::vector<Block>& blocks) -> size_t {
            auto classBlock = std::find_if(
                blocks.rbegin(), blocks.rend(), [](const Block& block) { return block.classdef; });
            return classBlock == blocks.rend() ? static_cast<size_t>(-1) : classBlock->classIndex;
        };
        auto opensNonPropertyBlock = [](const std::string& line) {
            return startsWithWord(line, "events") || startsWithWord(line, "enumeration")
                || startsWithWord(line, "function") || startsWithWord(line, "if")
                || startsWithWord(line, "for") || startsWithWord(line, "while")
                || startsWithWord(line, "switch") || startsWithWord(line, "try")
                || startsWithWord(line, "arguments");
        };
        std::vector<CodeAnalyzerClassdef> classdefs;
        std::vector<Block> blocks;
        for (size_t k = 0; k < lines.size(); ++k) {
            std::string line = trim(toLowerAscii(codeWithoutStringsAndComments(lines[k])));
            std::string name;
            int column = 0;
            std::vector<CodeAnalyzerClassSuperclass> superclasses;
            std::vector<CodeAnalyzerClassAttribute> attributes;
            if (parseClassdefHeader(
                    logicalHeaderLine(lines, k), name, column, &superclasses, &attributes)) {
                CodeAnalyzerClassdef classdef;
                classdef.name = name;
                classdef.line = static_cast<int>(k + 1);
                classdef.column = column;
                for (CodeAnalyzerClassAttribute& attribute : attributes) {
                    attribute.line = classdef.line;
                }
                for (CodeAnalyzerClassSuperclass& superclass : superclasses) {
                    superclass.line = classdef.line;
                }
                classdef.attributes = std::move(attributes);
                classdef.superclasses = std::move(superclasses);
                classdefs.push_back(classdef);
                blocks.push_back({ true, classdefs.size() - 1, false, false, false });
                continue;
            }
            if (startsWithWord(line, "properties")) {
                blocks.push_back({ false, nearestClassIndex(blocks), true, false, false });
                continue;
            }
            if (startsWithWord(line, "events")) {
                blocks.push_back({ false, nearestClassIndex(blocks), false, true, false });
                continue;
            }
            if (startsWithWord(line, "methods")) {
                blocks.push_back({ false, nearestClassIndex(blocks), false, false, true });
                continue;
            }
            if (opensNonPropertyBlock(line)) {
                blocks.push_back({ false, 0, false, false, false });
                continue;
            }
            if (startsWithWord(line, "end") && !blocks.empty()) {
                if (blocks.back().classdef) {
                    classdefs[blocks.back().classIndex].endLine = static_cast<int>(k + 1);
                }
                blocks.pop_back();
                continue;
            }
            const size_t* propertyClassIndex = activePropertiesClass(blocks);
            if (propertyClassIndex != nullptr && *propertyClassIndex < classdefs.size()) {
                CodeAnalyzerClassProperty property;
                if (parseClassPropertyLine(lines[k], property)) {
                    property.line = static_cast<int>(k + 1);
                    classdefs[*propertyClassIndex].properties.push_back(property);
                }
            }
            const size_t* eventClassIndex = activeEventsClass(blocks);
            if (eventClassIndex != nullptr && *eventClassIndex < classdefs.size()) {
                CodeAnalyzerClassEvent event;
                if (parseClassEventLine(lines[k], event)) {
                    event.line = static_cast<int>(k + 1);
                    classdefs[*eventClassIndex].events.push_back(event);
                }
            }
            const size_t* methodClassIndex = activeMethodsClass(blocks);
            if (methodClassIndex != nullptr && *methodClassIndex < classdefs.size()) {
                CodeAnalyzerClassMethod method;
                if (parseClassExternalMethodLine(lines[k], method)) {
                    method.line = static_cast<int>(k + 1);
                    classdefs[*methodClassIndex].externalMethods.push_back(method);
                }
            }
        }
        while (!blocks.empty()) {
            if (blocks.back().classdef) {
                classdefs[blocks.back().classIndex].endLine = static_cast<int>(lines.size());
            }
            blocks.pop_back();
        }
        return classdefs;
    }
    //=============================================================================
    bool
    isConditionOrCaseLine(const std::string& line)
    {
        return startsWithWord(line, "if") || startsWithWord(line, "elseif")
            || startsWithWord(line, "for") || startsWithWord(line, "while")
            || startsWithWord(line, "switch") || startsWithWord(line, "try")
            || startsWithWord(line, "catch") || startsWithWord(line, "case");
    }
    //=============================================================================
    bool
    opensCognitiveNesting(const std::string& line)
    {
        return startsWithWord(line, "if") || startsWithWord(line, "for")
            || startsWithWord(line, "while") || startsWithWord(line, "switch")
            || startsWithWord(line, "try");
    }
    //=============================================================================
    void
    updateCyclomaticComplexity(const stringVector& lines, CodeAnalyzerFunction& function)
    {
        int standard = 1;
        int modified = 1;
        for (int lineNumber = function.line + 1; lineNumber <= function.endLine; ++lineNumber) {
            std::string line
                = trim(toLowerAscii(codeWithoutStringsAndComments(lines[lineNumber - 1])));
            if (line.empty()) {
                continue;
            }
            const bool conditionLine = startsWithWord(line, "if") || startsWithWord(line, "elseif")
                || startsWithWord(line, "while");
            const bool branchLine
                = conditionLine || startsWithWord(line, "for") || startsWithWord(line, "try");
            if (branchLine) {
                ++standard;
                ++modified;
            }
            if (startsWithWord(line, "case")) {
                ++standard;
            }
            if (startsWithWord(line, "switch")) {
                ++modified;
            }
            int shortCircuit = countOccurrences(line, "&&") + countOccurrences(line, "||");
            standard += shortCircuit;
            modified += shortCircuit;
            if (conditionLine) {
                standard += countOccurrences(line, "&") - countOccurrences(line, "&&") * 2;
                standard += countOccurrences(line, "|") - countOccurrences(line, "||") * 2;
                modified += countOccurrences(line, "&") - countOccurrences(line, "&&") * 2;
                modified += countOccurrences(line, "|") - countOccurrences(line, "||") * 2;
            }
        }
        function.cyclomaticComplexity = standard;
        function.modifiedCyclomaticComplexity = modified;
    }
    //=============================================================================
    int
    cognitiveComplexityFor(const stringVector& lines, const CodeAnalyzerFunction& function)
    {
        int complexity = 1;
        int nesting = 0;
        for (int lineNumber = function.line + 1; lineNumber <= function.endLine; ++lineNumber) {
            std::string line
                = toLowerAscii(trim(codeWithoutStringsAndComments(lines[lineNumber - 1])));
            if (line.empty()) {
                continue;
            }
            if (startsWithWord(line, "end")) {
                nesting = std::max(0, nesting - 1);
                continue;
            }
            if (isConditionOrCaseLine(line)) {
                complexity += 1 + nesting;
            }
            complexity += countOccurrences(line, "&&") + countOccurrences(line, "||");
            if (opensCognitiveNesting(line)) {
                ++nesting;
            }
        }
        return complexity;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::vector<CodeAnalyzerFunction>
buildCodeAnalyzerFunctionModel(const stringVector& lines, MacroFunctionDef* parsedFunction)
{
    std::vector<CodeAnalyzerFunction> functions = parseFunctionSignatures(lines, parsedFunction);
    for (CodeAnalyzerFunction& function : functions) {
        updateCyclomaticComplexity(lines, function);
        function.cognitiveComplexity = cognitiveComplexityFor(lines, function);
    }
    return functions;
}
//=============================================================================
std::vector<CodeAnalyzerClassdef>
buildCodeAnalyzerClassdefModel(const stringVector& lines)
{
    return parseClassdefs(lines);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
