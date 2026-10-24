//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerDataflowClassdef.hpp"
#include "CodeAnalyzerAstUtilities.hpp"
#include "CodeAnalyzerDataflowFixes.hpp"
#include <algorithm>
#include <cctype>
#include <map>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    CodeAnalyzerDiagnostic
    makeDiagnostic(const std::filesystem::path& path, const std::string& id,
        const std::string& severity, const std::string& category, const std::string& message,
        int line, int column = 1)
    {
        CodeAnalyzerDiagnostic diagnostic;
        diagnostic.file = path.wstring();
        diagnostic.checkId = id;
        diagnostic.severity = severity;
        diagnostic.category = category;
        diagnostic.fixability = "none";
        diagnostic.message = message;
        diagnostic.lineStart = line;
        diagnostic.lineEnd = line;
        diagnostic.columnStart = column;
        diagnostic.columnEnd = column;
        return diagnostic;
    }
    //=============================================================================
    std::string
    quotedNameMessage(const std::string& prefix, const std::string& name, const std::string& suffix)
    {
        return prefix + " '" + name + "' " + suffix;
    }
    //=============================================================================
    void
    addDuplicateSuperclassFix(CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines,
        const CodeAnalyzerClassSuperclass& superclass)
    {
        if (superclass.line <= 0 || superclass.line > static_cast<int>(lines.size())
            || superclass.column <= 0) {
            return;
        }
        const std::string& line = lines[static_cast<size_t>(superclass.line - 1)];
        size_t nameStart = static_cast<size_t>(superclass.column - 1);
        if (nameStart >= line.size()) {
            return;
        }
        size_t nameEnd = nameStart;
        while (nameEnd < line.size()
            && (CodeAnalyzerAst::isIdentifierChar(line[nameEnd]) || line[nameEnd] == '.')) {
            ++nameEnd;
        }
        if (nameEnd <= nameStart) {
            return;
        }
        size_t amp = line.rfind('&', nameStart);
        size_t comma = line.rfind(',', nameStart);
        size_t editStart
            = std::max(amp == std::string::npos ? 0 : amp, comma == std::string::npos ? 0 : comma);
        if (amp == std::string::npos && comma == std::string::npos) {
            editStart = nameStart;
        }
        while (editStart > 0 && std::isspace(static_cast<unsigned char>(line[editStart - 1]))) {
            --editStart;
        }
        size_t editEnd = nameEnd;
        while (editEnd < line.size() && std::isspace(static_cast<unsigned char>(line[editEnd]))) {
            ++editEnd;
        }
        if (editEnd <= editStart) {
            return;
        }
        CodeAnalyzerTextEdit edit;
        edit.lineStart = superclass.line;
        edit.lineEnd = superclass.line;
        edit.columnStart = static_cast<int>(editStart + 1);
        edit.columnEnd = static_cast<int>(editEnd + 1);
        CodeAnalyzerFix fix;
        fix.title = "Remove duplicate superclass";
        fix.edits.push_back(edit);
        diagnostic.fixability = "safe";
        diagnostic.fixes.push_back(fix);
    }
    //=============================================================================
    void
    addDuplicateAttributeFix(CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines,
        const CodeAnalyzerClassAttribute& attribute)
    {
        if (attribute.line <= 0 || attribute.line > static_cast<int>(lines.size())
            || attribute.column <= 0) {
            return;
        }
        const std::string& line = lines[static_cast<size_t>(attribute.line - 1)];
        size_t nameStart = static_cast<size_t>(attribute.column - 1);
        if (nameStart >= line.size()) {
            return;
        }
        size_t nameEnd = nameStart;
        while (nameEnd < line.size() && CodeAnalyzerAst::isIdentifierChar(line[nameEnd])) {
            ++nameEnd;
        }
        if (nameEnd <= nameStart) {
            return;
        }
        size_t editStart = line.rfind(',', nameStart);
        size_t editEnd = nameEnd;
        while (editEnd < line.size() && std::isspace(static_cast<unsigned char>(line[editEnd]))) {
            ++editEnd;
        }
        if (editStart == std::string::npos) {
            editStart = nameStart;
            if (editEnd < line.size() && line[editEnd] == ',') {
                ++editEnd;
                while (editEnd < line.size()
                    && std::isspace(static_cast<unsigned char>(line[editEnd]))) {
                    ++editEnd;
                }
            }
        }
        if (editEnd <= editStart) {
            return;
        }
        CodeAnalyzerTextEdit edit;
        edit.lineStart = attribute.line;
        edit.lineEnd = attribute.line;
        edit.columnStart = static_cast<int>(editStart + 1);
        edit.columnEnd = static_cast<int>(editEnd + 1);
        CodeAnalyzerFix fix;
        fix.title = "Remove duplicate class attribute";
        fix.edits.push_back(edit);
        diagnostic.fixability = "safe";
        diagnostic.fixes.push_back(fix);
    }
    //=============================================================================
    bool
    isSimpleDeclarationLine(const stringVector& lines, int lineNumber, const std::string& name)
    {
        if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size()) || name.empty()) {
            return false;
        }
        const std::string& line = lines[static_cast<size_t>(lineNumber - 1)];
        if (CodeAnalyzerAst::trim(CodeAnalyzerAst::stripComment(line))
            != CodeAnalyzerAst::trim(line)) {
            return false;
        }
        return CodeAnalyzerAst::trim(CodeAnalyzerAst::codeWithoutStringsAndComments(line)) == name;
    }
    //=============================================================================
    void
    addSimpleDeclarationRemovalFix(CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines,
        int lineNumber, const std::string& name, const std::string& title)
    {
        if (!isSimpleDeclarationLine(lines, lineNumber, name)) {
            return;
        }
        CodeAnalyzerTextEdit edit;
        edit.lineStart = lineNumber;
        edit.lineEnd = lineNumber + 1;
        edit.columnStart = 1;
        edit.columnEnd = 1;
        CodeAnalyzerFix fix;
        fix.title = title;
        fix.edits.push_back(edit);
        diagnostic.fixability = "safe";
        diagnostic.fixes.push_back(fix);
    }
    //=============================================================================
    void
    applyClassdefNamingRules(const std::filesystem::path& path,
        const std::vector<CodeAnalyzerClassdef>& classdefs,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        const std::string expected = path.stem().generic_string();
        if (expected.empty()) {
            return;
        }
        for (const CodeAnalyzerClassdef& classdef : classdefs) {
            if (classdef.name.empty() || classdef.name == expected) {
                continue;
            }
            CodeAnalyzerDiagnostic diagnostic = makeDiagnostic(path, "NLS0002", "warning", "Naming",
                "Class name differs from file name.", classdef.line, std::max(classdef.column, 1));
            diagnostic.columnEnd = diagnostic.columnStart + static_cast<int>(classdef.name.size());
            addClassdefNameMismatchFix(diagnostic, classdef, expected);
            appendDiagnostic(std::move(diagnostic));
        }
    }
    //=============================================================================
    void
    applyClassdefPropertyRules(const std::filesystem::path& path, const stringVector& lines,
        const std::vector<CodeAnalyzerClassdef>& classdefs,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        for (const CodeAnalyzerClassdef& classdef : classdefs) {
            std::map<std::string, CodeAnalyzerClassProperty> firstProperties;
            for (const CodeAnalyzerClassProperty& property : classdef.properties) {
                auto inserted = firstProperties.emplace(property.name, property);
                if (inserted.second) {
                    continue;
                }
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0031", "warning", "Classdef",
                        quotedNameMessage("Property", property.name,
                            "is declared more than once in class '" + classdef.name + "'."),
                        property.line, std::max(property.column, 1));
                addSimpleDeclarationRemovalFix(diagnostic, lines, property.line, property.name,
                    "Remove duplicate property declaration");
                appendDiagnostic(std::move(diagnostic));
            }
        }
    }
    //=============================================================================
    void
    applyClassdefMethodRules(const std::filesystem::path& path, const stringVector& lines,
        const std::vector<CodeAnalyzerFunction>& functions,
        const std::vector<CodeAnalyzerClassdef>& classdefs,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        struct MethodLocation
        {
            int line = 0;
            int column = 1;
        };
        std::map<std::string, std::map<std::string, MethodLocation>> firstMethodsByClass;
        auto addMethod = [&](const std::string& className, const std::string& methodName,
                             MethodLocation location) {
            auto& firstMethods = firstMethodsByClass[className];
            auto inserted = firstMethods.emplace(methodName, location);
            if (inserted.second) {
                return;
            }
            appendDiagnostic(makeDiagnostic(path, "NLS0032", "warning", "Classdef",
                quotedNameMessage("Method", methodName,
                    "is declared more than once in class '" + className + "'."),
                location.line, location.column));
        };
        for (const CodeAnalyzerFunction& function : functions) {
            if (!function.classdefMethod || function.parentLine != 0 || function.name.empty()
                || function.classdefOwnerName.empty()) {
                continue;
            }
            addMethod(function.classdefOwnerName, function.name,
                { function.line,
                    codeAnalyzerWordColumnInLine(lines[function.line - 1], function.name) });
        }
        for (const CodeAnalyzerClassdef& classdef : classdefs) {
            for (const CodeAnalyzerClassMethod& method : classdef.externalMethods) {
                if (method.name.empty()) {
                    continue;
                }
                addMethod(classdef.name, method.name, { method.line, std::max(method.column, 1) });
            }
        }
    }
    //=============================================================================
    void
    applyClassdefEventRules(const std::filesystem::path& path, const stringVector& lines,
        const std::vector<CodeAnalyzerClassdef>& classdefs,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        for (const CodeAnalyzerClassdef& classdef : classdefs) {
            std::map<std::string, CodeAnalyzerClassEvent> firstEvents;
            for (const CodeAnalyzerClassEvent& event : classdef.events) {
                auto inserted = firstEvents.emplace(event.name, event);
                if (inserted.second) {
                    continue;
                }
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0033", "warning", "Classdef",
                        quotedNameMessage("Event", event.name,
                            "is declared more than once in class '" + classdef.name + "'."),
                        event.line, std::max(event.column, 1));
                addSimpleDeclarationRemovalFix(diagnostic, lines, event.line, event.name,
                    "Remove duplicate event declaration");
                appendDiagnostic(std::move(diagnostic));
            }
        }
    }
    //=============================================================================
    void
    applyClassdefAttributeRules(const std::filesystem::path& path, const stringVector& lines,
        const std::vector<CodeAnalyzerClassdef>& classdefs,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        for (const CodeAnalyzerClassdef& classdef : classdefs) {
            std::map<std::string, CodeAnalyzerClassAttribute> firstAttributes;
            for (const CodeAnalyzerClassAttribute& attribute : classdef.attributes) {
                auto inserted = firstAttributes.emplace(
                    CodeAnalyzerAst::toLowerAscii(attribute.name), attribute);
                if (inserted.second) {
                    continue;
                }
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0038", "warning", "Classdef",
                        quotedNameMessage("Attribute", attribute.name,
                            "is listed more than once in class '" + classdef.name + "'."),
                        attribute.line, std::max(attribute.column, 1));
                addDuplicateAttributeFix(diagnostic, lines, attribute);
                appendDiagnostic(std::move(diagnostic));
            }
        }
    }
    //=============================================================================
    void
    applyClassdefSuperclassRules(const std::filesystem::path& path, const stringVector& lines,
        const std::vector<CodeAnalyzerClassdef>& classdefs,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        for (const CodeAnalyzerClassdef& classdef : classdefs) {
            std::map<std::string, CodeAnalyzerClassSuperclass> firstSuperclasses;
            for (const CodeAnalyzerClassSuperclass& superclass : classdef.superclasses) {
                auto inserted = firstSuperclasses.emplace(superclass.name, superclass);
                if (inserted.second) {
                    continue;
                }
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0037", "warning", "Classdef",
                        quotedNameMessage("Superclass", superclass.name,
                            "is listed more than once in class '" + classdef.name + "'."),
                        superclass.line, std::max(superclass.column, 1));
                addDuplicateSuperclassFix(diagnostic, lines, superclass);
                appendDiagnostic(std::move(diagnostic));
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
applyCodeAnalyzerClassdefRules(const std::filesystem::path& path, const stringVector& lines,
    const std::vector<CodeAnalyzerFunction>& functions,
    const std::vector<CodeAnalyzerClassdef>& classdefs,
    const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
{
    applyClassdefNamingRules(path, classdefs, appendDiagnostic);
    applyClassdefPropertyRules(path, lines, classdefs, appendDiagnostic);
    applyClassdefMethodRules(path, lines, functions, classdefs, appendDiagnostic);
    applyClassdefEventRules(path, lines, classdefs, appendDiagnostic);
    applyClassdefSuperclassRules(path, lines, classdefs, appendDiagnostic);
    applyClassdefAttributeRules(path, lines, classdefs, appendDiagnostic);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
