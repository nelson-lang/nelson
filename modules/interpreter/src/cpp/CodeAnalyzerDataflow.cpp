//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerDataflow.hpp"
#include "CodeAnalyzerAst.hpp"
#include "CodeAnalyzerDataflowClassdef.hpp"
#include "CodeAnalyzerDataflowFixes.hpp"
#include "CodeAnalyzerDataflowImports.hpp"
#include "CodeAnalyzerSemanticShadows.hpp"
#include <algorithm>
#include <cctype>
#include <map>
#include <sstream>
#include <set>
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
    std::string
    trim(const std::string& value)
    {
        size_t first = 0;
        while (first < value.size() && std::isspace(static_cast<unsigned char>(value[first]))) {
            ++first;
        }
        size_t last = value.size();
        while (last > first && std::isspace(static_cast<unsigned char>(value[last - 1]))) {
            --last;
        }
        return value.substr(first, last - first);
    }
    //=============================================================================
    bool
    startsWithComment(const std::string& line)
    {
        std::string trimmed = trim(line);
        return !trimmed.empty() && trimmed[0] == '%';
    }
    //=============================================================================
    bool
    isFunctionDeclarationLine(const std::vector<CodeAnalyzerFunction>& functions, int lineNumber)
    {
        return std::any_of(
            functions.begin(), functions.end(), [lineNumber](const CodeAnalyzerFunction& function) {
                return function.line == lineNumber;
            });
    }
    //=============================================================================
    const CodeAnalyzerLineUsage*
    lineUsageAt(const std::vector<CodeAnalyzerLineUsage>& lineUsages, int lineNumber)
    {
        if (lineNumber <= 0 || lineNumber > static_cast<int>(lineUsages.size())) {
            return nullptr;
        }
        return &lineUsages[static_cast<size_t>(lineNumber - 1)];
    }
    //=============================================================================
    bool
    lineCallsFunction(const std::vector<CodeAnalyzerLineUsage>& lineUsages, int lineNumber,
        const std::string& name)
    {
        const CodeAnalyzerLineUsage* usage = lineUsageAt(lineUsages, lineNumber);
        if (usage == nullptr) {
            return false;
        }
        return std::find(usage->callableUsed.begin(), usage->callableUsed.end(), name)
            != usage->callableUsed.end();
    }
    //=============================================================================
    bool
    hasCallOutsideFunctionBody(const std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const CodeAnalyzerFunction& function, const std::vector<CodeAnalyzerFunction>& functions)
    {
        for (int lineNumber = 1; lineNumber <= static_cast<int>(lineUsages.size()); ++lineNumber) {
            if (lineNumber >= function.line && lineNumber <= function.endLine) {
                continue;
            }
            if (isFunctionDeclarationLine(functions, lineNumber)) {
                continue;
            }
            if (lineCallsFunction(lineUsages, lineNumber, function.name)) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    std::set<std::string>
    localFunctionNamesFor(
        const CodeAnalyzerFunction& function, const std::vector<CodeAnalyzerFunction>& functions)
    {
        std::set<std::string> names;
        for (const CodeAnalyzerFunction& localFunction : functions) {
            if (localFunction.line != function.line && !localFunction.name.empty()
                && !localFunction.classdefMethod
                && (localFunction.parentLine == function.parentLine
                    || localFunction.parentLine == function.line)) {
                names.insert(localFunction.name);
            }
        }
        return names;
    }
    //=============================================================================
    const CodeAnalyzerFunction*
    childFunctionAtLine(const CodeAnalyzerFunction& function,
        const std::vector<CodeAnalyzerFunction>& functions, int lineNumber)
    {
        for (const CodeAnalyzerFunction& candidate : functions) {
            if (candidate.parentLine == function.line && candidate.line == lineNumber) {
                return &candidate;
            }
        }
        return nullptr;
    }
    //=============================================================================
    const CodeAnalyzerFunction*
    functionByLine(const std::vector<CodeAnalyzerFunction>& functions, int lineNumber)
    {
        for (const CodeAnalyzerFunction& function : functions) {
            if (function.line == lineNumber) {
                return &function;
            }
        }
        return nullptr;
    }
    //=============================================================================
    void
    appendFunctionSymbols(std::set<std::string>& symbols, const CodeAnalyzerFunction& function)
    {
        symbols.insert(function.inputs.begin(), function.inputs.end());
        symbols.insert(function.outputs.begin(), function.outputs.end());
    }
    //=============================================================================
    void
    appendAssignedSymbols(std::set<std::string>& symbols, const CodeAnalyzerFunction& function,
        const std::vector<CodeAnalyzerFunction>& functions,
        const std::vector<CodeAnalyzerLineUsage>& lineUsages)
    {
        bool inArgumentsBlock = false;
        for (int lineNumber = function.line + 1; lineNumber <= function.endLine; ++lineNumber) {
            const CodeAnalyzerLineUsage* usage = lineUsageAt(lineUsages, lineNumber);
            if (usage == nullptr) {
                continue;
            }
            if (usage->functionDeclaration) {
                const CodeAnalyzerFunction* child
                    = childFunctionAtLine(function, functions, lineNumber);
                if (child != nullptr) {
                    lineNumber = child->endLine;
                    continue;
                }
                break;
            }
            if (inArgumentsBlock) {
                if (usage->endsBlock) {
                    inArgumentsBlock = false;
                }
                continue;
            }
            if (usage->startsArgumentsBlock) {
                inArgumentsBlock = true;
                continue;
            }
            symbols.insert(usage->assigned.begin(), usage->assigned.end());
        }
    }
    //=============================================================================
    std::set<std::string>
    parentScopeSymbolsFor(const CodeAnalyzerFunction& function,
        const std::vector<CodeAnalyzerFunction>& functions,
        const std::vector<CodeAnalyzerLineUsage>& lineUsages)
    {
        std::set<std::string> symbols;
        const CodeAnalyzerFunction* parent = functionByLine(functions, function.parentLine);
        while (parent != nullptr) {
            appendFunctionSymbols(symbols, *parent);
            appendAssignedSymbols(symbols, *parent, functions, lineUsages);
            parent = functionByLine(functions, parent->parentLine);
        }
        return symbols;
    }
    //=============================================================================
    void
    appendParentShadowDiagnostic(const std::filesystem::path& path, const stringVector& lines,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic,
        const std::string& name, int lineNumber)
    {
        appendDiagnostic(makeDiagnostic(path, "NLS0028", "warning", "Dataflow",
            quotedNameMessage("Variable", name, "shadows a symbol from a parent scope."),
            lineNumber, codeAnalyzerWordColumnInLine(lines[lineNumber - 1], name)));
    }
    //=============================================================================
    void
    appendBuiltinShadowDiagnostic(const std::filesystem::path& path, const stringVector& lines,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic,
        const std::string& name, int lineNumber)
    {
        appendDiagnostic(makeDiagnostic(path, "NLS0034", "warning", "Dataflow",
            quotedNameMessage("Variable", name, "shadows a builtin function name."), lineNumber,
            codeAnalyzerWordColumnInLine(lines[lineNumber - 1], name)));
    }
    //=============================================================================
    void
    appendPropertyShadowDiagnostic(const std::filesystem::path& path, const stringVector& lines,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic,
        const std::string& name, int lineNumber)
    {
        appendDiagnostic(makeDiagnostic(path, "NLS0035", "warning", "Classdef",
            quotedNameMessage("Variable", name, "shadows a class property name."), lineNumber,
            codeAnalyzerWordColumnInLine(lines[lineNumber - 1], name)));
    }
    //=============================================================================
    void
    appendInputShadowDiagnostic(const std::filesystem::path& path, const stringVector& lines,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic,
        const std::string& name, int lineNumber)
    {
        appendDiagnostic(makeDiagnostic(path, "NLS0036", "info", "Dataflow",
            quotedNameMessage("Local assignment", name, "shadows an input argument."), lineNumber,
            codeAnalyzerWordColumnInLine(lines[lineNumber - 1], name)));
    }
    //=============================================================================
    bool
    isClassdefReceiverArgument(
        const CodeAnalyzerFunction& function, const std::string& input, size_t index)
    {
        if (!function.classdefMethod || index != 0) {
            return false;
        }
        return input == "obj" || input == "self" || input == "this";
    }
    //=============================================================================
    void
    applyForFunction(const std::filesystem::path& path, const stringVector& lines,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic,
        const CodeAnalyzerFunction& function, const std::vector<CodeAnalyzerFunction>& functions,
        const std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const std::vector<CodeAnalyzerClassdef>& classdefs)
    {
        std::map<std::string, int> assignedLine;
        std::map<std::string, int> assignedCount;
        std::map<std::string, int> useCount;
        std::map<std::string, int> firstUseLine;
        std::set<std::string> outputs(function.outputs.begin(), function.outputs.end());
        std::set<std::string> inputs(function.inputs.begin(), function.inputs.end());
        std::set<std::string> globalNames;
        std::set<std::string> persistentNames;
        std::set<std::string> importedNames;
        std::map<std::string, std::string> importedSpecs;
        std::map<std::string, int> importedLine;
        std::set<std::string> usedImports;
        std::set<std::string> localFunctionNames = localFunctionNamesFor(function, functions);
        std::set<std::string> parentScopeSymbols
            = parentScopeSymbolsFor(function, functions, lineUsages);
        std::set<std::string> reportedParentShadows;
        std::set<std::string> reportedBuiltinShadows;
        std::set<std::string> reportedPropertyShadows;
        std::set<std::string> reportedInputShadows;
        const std::set<std::string>& builtinNames = codeAnalyzerBuiltinFunctionNames();
        std::set<std::string> propertyNames
            = codeAnalyzerClassPropertyNamesFor(function, classdefs);
        bool dynamic = false;
        bool inArgumentsBlock = false;
        bool wildcardImport = false;
        for (const std::string& input : inputs) {
            assignedLine[input] = function.line;
            if (parentScopeSymbols.find(input) != parentScopeSymbols.end()) {
                appendParentShadowDiagnostic(path, lines, appendDiagnostic, input, function.line);
                reportedParentShadows.insert(input);
            }
            if (builtinNames.find(input) != builtinNames.end()) {
                appendBuiltinShadowDiagnostic(path, lines, appendDiagnostic, input, function.line);
                reportedBuiltinShadows.insert(input);
            }
            if (propertyNames.find(input) != propertyNames.end()) {
                appendPropertyShadowDiagnostic(path, lines, appendDiagnostic, input, function.line);
                reportedPropertyShadows.insert(input);
            }
        }
        for (const std::string& output : outputs) {
            if (parentScopeSymbols.find(output) != parentScopeSymbols.end()
                && reportedParentShadows.insert(output).second) {
                appendParentShadowDiagnostic(path, lines, appendDiagnostic, output, function.line);
            }
            if (builtinNames.find(output) != builtinNames.end()
                && reportedBuiltinShadows.insert(output).second) {
                appendBuiltinShadowDiagnostic(path, lines, appendDiagnostic, output, function.line);
            }
            if (propertyNames.find(output) != propertyNames.end()
                && reportedPropertyShadows.insert(output).second) {
                appendPropertyShadowDiagnostic(
                    path, lines, appendDiagnostic, output, function.line);
            }
        }
        for (int lineNumber = function.line + 1; lineNumber <= function.endLine; ++lineNumber) {
            const CodeAnalyzerLineUsage* usage = lineUsageAt(lineUsages, lineNumber);
            if (usage == nullptr) {
                continue;
            }
            if (usage->functionDeclaration) {
                const CodeAnalyzerFunction* child
                    = childFunctionAtLine(function, functions, lineNumber);
                if (child != nullptr) {
                    lineNumber = child->endLine;
                    continue;
                }
                break;
            }
            if (usage->importLine) {
                std::set<std::string> lineImportedNames(
                    usage->importedNames.begin(), usage->importedNames.end());
                std::map<std::string, std::string> lineImportedSpecs;
                for (const std::string& spec : usage->importedSpecs) {
                    const std::string name = codeAnalyzerImportedSymbolName(spec);
                    if (!name.empty() && name != "*") {
                        lineImportedSpecs.emplace(name, spec);
                    }
                }
                bool lineWildcardImport = usage->wildcardImport;
                if (!usage->importFromAst && lineImportedNames.empty() && !lineWildcardImport) {
                    for (const CodeAnalyzerImportSpec& importSpec :
                        collectCodeAnalyzerImportSpecs(lines, lineNumber)) {
                        if (importSpec.wildcard) {
                            lineWildcardImport = true;
                        } else {
                            lineImportedNames.insert(importSpec.name);
                            lineImportedSpecs.emplace(importSpec.name, importSpec.spec);
                        }
                    }
                }
                wildcardImport = wildcardImport || lineWildcardImport;
                for (const std::string& name : lineImportedNames) {
                    importedNames.insert(name);
                    if (assignedLine.find(name) == assignedLine.end()) {
                        assignedLine[name] = lineNumber;
                    }
                    if (importedLine.find(name) == importedLine.end()) {
                        importedLine[name] = lineNumber;
                    }
                    if (importedSpecs.find(name) == importedSpecs.end()) {
                        auto spec = lineImportedSpecs.find(name);
                        importedSpecs[name] = spec == lineImportedSpecs.end() ? name : spec->second;
                    }
                }
                continue;
            }
            if (inArgumentsBlock) {
                if (usage->endsBlock) {
                    inArgumentsBlock = false;
                }
                for (const std::string& token : usage->argumentValidationUsed) {
                    if (assignedLine.find(token) != assignedLine.end()
                        || inputs.find(token) != inputs.end()) {
                        useCount[token]++;
                        if (firstUseLine.find(token) == firstUseLine.end()) {
                            firstUseLine[token] = lineNumber;
                        }
                    }
                }
                continue;
            }
            if (usage->startsArgumentsBlock) {
                inArgumentsBlock = true;
                continue;
            }
            globalNames.insert(usage->globalDeclarations.begin(), usage->globalDeclarations.end());
            persistentNames.insert(
                usage->persistentDeclarations.begin(), usage->persistentDeclarations.end());
            dynamic = dynamic || usage->dynamicCode;
            if (usage->dynamicCode) {
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0022", "warning", "Security",
                        quotedNameMessage("Call", usage->dynamicCallName,
                            usage->dynamicCallName == "load"
                                ? "may mutate the current workspace."
                                : "uses dynamic code or workspace mutation."),
                        lineNumber, usage->dynamicCallColumn);
                if (usage->dynamicCallName == "load") {
                    addDynamicLoadCaptureFix(diagnostic, lines);
                }
                appendDiagnostic(std::move(diagnostic));
            }
            if (usage->suspiciousFloatComparison) {
                appendDiagnostic(makeDiagnostic(path, "NLS0025", "warning", "Numeric",
                    "Exact equality comparison with a floating-point literal.", lineNumber,
                    usage->suspiciousFloatComparisonColumn));
            }
            if (usage->displaySprintfCall) {
                CodeAnalyzerDiagnostic diagnostic = makeDiagnostic(path, "NLS0017", "info",
                    "Readability",
                    "Use formatted output directly instead of nesting sprintf in display output.",
                    lineNumber, usage->displaySprintfCallColumn);
                addDisplaySprintfFix(diagnostic, lines[lineNumber - 1]);
                appendDiagnostic(std::move(diagnostic));
            }
            if (usage->diagnosticSprintfCall) {
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0018", "info", "Readability",
                        "Pass formatted arguments directly instead of nesting sprintf in "
                        "diagnostic output.",
                        lineNumber, usage->diagnosticSprintfCallColumn);
                addDiagnosticSprintfFix(diagnostic, lines[lineNumber - 1]);
                appendDiagnostic(std::move(diagnostic));
            }
            for (const std::string& token : usage->used) {
                useCount[token]++;
                if (importedNames.find(token) != importedNames.end()) {
                    usedImports.insert(token);
                }
                if (firstUseLine.find(token) == firstUseLine.end()) {
                    firstUseLine[token] = lineNumber;
                }
            }
            for (const std::string& token : usage->callableUsed) {
                if (importedNames.find(token) != importedNames.end()) {
                    usedImports.insert(token);
                }
                if (assignedLine.find(token) != assignedLine.end()
                    || localFunctionNames.find(token) != localFunctionNames.end()) {
                    useCount[token]++;
                }
            }
            for (const std::string& token : usage->leftModified) {
                if (assignedLine.find(token) != assignedLine.end()) {
                    useCount[token]++;
                }
            }
            for (const std::string& name : usage->assigned) {
                if (assignedLine.find(name) == assignedLine.end()) {
                    assignedLine[name] = lineNumber;
                }
                if (localFunctionNames.find(name) != localFunctionNames.end()) {
                    appendDiagnostic(makeDiagnostic(path, "NLS0024", "warning", "Dataflow",
                        quotedNameMessage(
                            "Variable", name, "shadows a local function in this file."),
                        lineNumber, codeAnalyzerWordColumnInLine(lines[lineNumber - 1], name)));
                }
                if (importedNames.find(name) != importedNames.end()) {
                    appendDiagnostic(makeDiagnostic(path, "NLS0029", "warning", "Dataflow",
                        quotedNameMessage("Variable", name, "shadows an imported symbol."),
                        lineNumber, codeAnalyzerWordColumnInLine(lines[lineNumber - 1], name)));
                }
                if (parentScopeSymbols.find(name) != parentScopeSymbols.end()
                    && reportedParentShadows.insert(name).second) {
                    appendParentShadowDiagnostic(path, lines, appendDiagnostic, name, lineNumber);
                }
                if (builtinNames.find(name) != builtinNames.end()
                    && reportedBuiltinShadows.insert(name).second) {
                    appendBuiltinShadowDiagnostic(path, lines, appendDiagnostic, name, lineNumber);
                }
                if (propertyNames.find(name) != propertyNames.end()
                    && reportedPropertyShadows.insert(name).second) {
                    appendPropertyShadowDiagnostic(path, lines, appendDiagnostic, name, lineNumber);
                }
                if (inputs.find(name) != inputs.end() && outputs.find(name) == outputs.end()
                    && reportedInputShadows.insert(name).second) {
                    appendInputShadowDiagnostic(path, lines, appendDiagnostic, name, lineNumber);
                }
                assignedCount[name]++;
            }
        }
        for (const auto& imported : importedLine) {
            if (usedImports.find(imported.first) == usedImports.end()) {
                auto spec = importedSpecs.find(imported.first);
                const std::string diagnosticName
                    = spec == importedSpecs.end() ? imported.first : spec->second;
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0030", "info", "Dataflow",
                        quotedNameMessage("Imported symbol", diagnosticName, "is never used."),
                        imported.second,
                        codeAnalyzerWordColumnInLine(lines[imported.second - 1], imported.first));
                addUnusedImportFix(diagnostic, lines, imported.first);
                appendDiagnostic(std::move(diagnostic));
            }
        }
        if (!dynamic) {
            for (const auto& it : assignedLine) {
                bool declaredElsewhere = inputs.find(it.first) != inputs.end()
                    || outputs.find(it.first) != outputs.end()
                    || globalNames.find(it.first) != globalNames.end()
                    || persistentNames.find(it.first) != persistentNames.end()
                    || importedNames.find(it.first) != importedNames.end();
                if (!declaredElsewhere && useCount[it.first] == 0) {
                    CodeAnalyzerDiagnostic diagnostic = makeDiagnostic(path, "NLS0004", "info",
                        "Dataflow",
                        quotedNameMessage("Variable", it.first, "is assigned but never used."),
                        it.second, codeAnalyzerWordColumnInLine(lines[it.second - 1], it.first));
                    addUnusedAssignmentFix(diagnostic, lines, it.first);
                    appendDiagnostic(std::move(diagnostic));
                }
                auto firstUse = firstUseLine.find(it.first);
                if (firstUse != firstUseLine.end() && firstUse->second < it.second
                    && !declaredElsewhere && !wildcardImport) {
                    appendDiagnostic(makeDiagnostic(path, "NLS0005", "warning", "Dataflow",
                        quotedNameMessage(
                            "Variable", it.first, "is used before a local assignment."),
                        firstUse->second,
                        codeAnalyzerWordColumnInLine(lines[firstUse->second - 1], it.first)));
                }
            }
        }
        for (const std::string& output : function.outputs) {
            bool declaredElsewhere = globalNames.find(output) != globalNames.end()
                || persistentNames.find(output) != persistentNames.end();
            if (output != "varargout" && !declaredElsewhere
                && assignedLine.find(output) == assignedLine.end()) {
                appendDiagnostic(makeDiagnostic(path, "NLS0006", "warning", "Dataflow",
                    quotedNameMessage("Declared output", output, "is never assigned."),
                    function.line, codeAnalyzerWordColumnInLine(lines[function.line - 1], output)));
            }
        }
        for (size_t k = 0; k < function.inputs.size(); ++k) {
            const std::string& input = function.inputs[k];
            if (input != "varargin" && useCount[input] == 0) {
                if (isClassdefReceiverArgument(function, input, k)) {
                    continue;
                }
                appendDiagnostic(makeDiagnostic(path, "NLS0007", "info", "Dataflow",
                    quotedNameMessage("Input argument", input, "is never used."), function.line,
                    codeAnalyzerWordColumnInLine(lines[function.line - 1], input)));
            }
        }
    }
    //=============================================================================
    void
    applyEmptyBlockRules(const std::filesystem::path& path,
        const std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        int blockStart = 0;
        bool inArgumentsBlock = false;
        for (const CodeAnalyzerLineUsage& usage : lineUsages) {
            const int lineNumber = usage.line;
            if (!usage.codeLine) {
                continue;
            }
            if (inArgumentsBlock) {
                if (usage.endsBlock) {
                    inArgumentsBlock = false;
                }
                blockStart = 0;
                continue;
            }
            if (usage.startsArgumentsBlock) {
                inArgumentsBlock = true;
                blockStart = 0;
                continue;
            }
            if (usage.flowBlockStart) {
                blockStart = lineNumber;
            } else if (usage.flowBoundary && blockStart == lineNumber - 1) {
                appendDiagnostic(makeDiagnostic(
                    path, "NLS0008", "warning", "Flow", "Suspicious empty block.", blockStart, 1));
                blockStart = 0;
            } else if (!usage.flowBoundary) {
                blockStart = 0;
            }
        }
    }
    //=============================================================================
    void
    applyUnreachableCodeRules(const std::filesystem::path& path, const stringVector& lines,
        const std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        bool afterExit = false;
        for (const CodeAnalyzerLineUsage& usage : lineUsages) {
            const int lineNumber = usage.line;
            if (!usage.codeLine) {
                continue;
            }
            if (usage.functionDeclaration) {
                afterExit = false;
                continue;
            }
            if (afterExit && !usage.flowBoundary) {
                CodeAnalyzerDiagnostic diagnostic = makeDiagnostic(path, "NLS0003", "warning",
                    "Flow", "Code after an exit statement is unreachable.", lineNumber, 1);
                addUnreachableCodeFix(diagnostic, lines, usage);
                appendDiagnostic(std::move(diagnostic));
                afterExit = false;
            }
            afterExit = usage.exitStatement;
        }
    }
    //=============================================================================
    void
    applyConstantConditionRules(const std::filesystem::path& path,
        const std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        for (const CodeAnalyzerLineUsage& usage : lineUsages) {
            const int lineNumber = usage.line;
            if (usage.constantCondition) {
                appendDiagnostic(makeDiagnostic(path, "NLS0009", "warning", "Flow",
                    "Condition is a constant value.", lineNumber, usage.constantConditionColumn));
            }
        }
    }
    //=============================================================================
    void
    applyDocumentationRules(const std::filesystem::path& path, const stringVector& lines,
        const std::vector<CodeAnalyzerFunction>& functions,
        const std::vector<CodeAnalyzerClassdef>& classdefs,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        for (const CodeAnalyzerFunction& function : functions) {
            if (function.script) {
                continue;
            }
            if (function.parentLine != 0 || function.local || function.nested
                || function.classdefMethod) {
                continue;
            }
            int lineNumber = function.line + 1;
            while (lineNumber <= function.endLine && trim(lines[lineNumber - 1]).empty()) {
                ++lineNumber;
            }
            if (lineNumber > function.endLine || !startsWithComment(lines[lineNumber - 1])) {
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0027", "allow", "Documentation",
                        quotedNameMessage("Function", function.name, "has no help comment."),
                        function.line, 1);
                addMissingFunctionHelpFix(diagnostic, function, lines);
                appendDiagnostic(std::move(diagnostic));
            }
        }
        for (const CodeAnalyzerClassdef& classdef : classdefs) {
            int endLine = classdef.endLine > 0 ? classdef.endLine : static_cast<int>(lines.size());
            int lineNumber = classdef.line + 1;
            while (lineNumber <= endLine && trim(lines[lineNumber - 1]).empty()) {
                ++lineNumber;
            }
            if (lineNumber > endLine || !startsWithComment(lines[lineNumber - 1])) {
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0027", "allow", "Documentation",
                        quotedNameMessage("Class", classdef.name, "has no help comment."),
                        classdef.line, 1);
                addMissingClassdefHelpFix(diagnostic, classdef, lines);
                appendDiagnostic(std::move(diagnostic));
            }
        }
    }
    //=============================================================================
    void
    applyFunctionNamingRules(const std::filesystem::path& path,
        const std::vector<CodeAnalyzerFunction>& functions,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        const std::string expected = path.stem().generic_string();
        if (expected.empty() || functions.empty()) {
            return;
        }
        const CodeAnalyzerFunction& function = functions.front();
        if (function.script || function.name.empty() || function.name == expected
            || function.parentLine != 0 || function.local || function.nested
            || function.classdefMethod) {
            return;
        }
        CodeAnalyzerDiagnostic diagnostic = makeDiagnostic(path, "NLS0002", "warning", "Naming",
            "Main function name differs from file name.", function.line,
            std::max(function.column, 1));
        diagnostic.columnEnd = diagnostic.columnStart + static_cast<int>(function.name.size());
        addFunctionNameMismatchFix(diagnostic, function, expected);
        appendDiagnostic(std::move(diagnostic));
    }
    //=============================================================================
    void
    applyCyclomaticComplexityRules(const std::filesystem::path& path,
        const CodeAnalyzerOptions& options, const std::vector<CodeAnalyzerFunction>& functions,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        auto emitMetric = [&](const CodeAnalyzerFunction& function, const std::string& id,
                              int value, bool enabled, const std::string& label) {
            if (!enabled || value <= options.cyclomaticWarningThreshold) {
                return;
            }
            std::string severity = value > options.cyclomaticErrorThreshold ? "error" : "warning";
            std::ostringstream message;
            message << label << " is " << value << ".";
            appendDiagnostic(
                makeDiagnostic(path, id, severity, "Metrics", message.str(), function.line, 1));
        };
        for (const CodeAnalyzerFunction& function : functions) {
            if (function.script) {
                continue;
            }
            emitMetric(function, "NLS0014", function.cyclomaticComplexity,
                options.includeCyclomatic, "Cyclomatic complexity");
            emitMetric(function, "NLS0015", function.modifiedCyclomaticComplexity,
                options.includeModifiedCyclomatic, "Modified cyclomatic complexity");
        }
    }
    //=============================================================================
    void
    applyCognitiveComplexityRules(const std::filesystem::path& path,
        const CodeAnalyzerOptions& options, const std::vector<CodeAnalyzerFunction>& functions,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        if (!options.includeCyclomatic) {
            return;
        }
        for (const CodeAnalyzerFunction& function : functions) {
            if (function.script) {
                continue;
            }
            if (function.cognitiveComplexity <= options.cyclomaticWarningThreshold) {
                continue;
            }
            std::string severity = function.cognitiveComplexity > options.cyclomaticErrorThreshold
                ? "error"
                : "warning";
            std::ostringstream message;
            message << "Cognitive complexity is " << function.cognitiveComplexity << ".";
            appendDiagnostic(makeDiagnostic(
                path, "NLS0026", severity, "Metrics", message.str(), function.line, 1));
        }
    }
    //=============================================================================
    void
    applyFunctionSizeRules(const std::filesystem::path& path, const CodeAnalyzerOptions& options,
        const std::vector<CodeAnalyzerFunction>& functions,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        if (options.maxFunctionLines <= 0) {
            return;
        }
        for (const CodeAnalyzerFunction& function : functions) {
            if (function.script) {
                continue;
            }
            int functionLines = function.endLine - function.line + 1;
            if (functionLines <= options.maxFunctionLines) {
                continue;
            }
            appendDiagnostic(makeDiagnostic(path, "NLS0021", "warning", "Metrics",
                "Function is longer than the configured maximum number of lines.", function.line,
                1));
        }
    }
    //=============================================================================
    void
    applyLocalFunctionRules(const std::filesystem::path& path,
        const std::vector<CodeAnalyzerFunction>& functions,
        const std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        for (size_t k = 1; k < functions.size(); ++k) {
            const CodeAnalyzerFunction& function = functions[k];
            if (!function.name.empty() && !function.classdefMethod
                && !hasCallOutsideFunctionBody(lineUsages, function, functions)) {
                CodeAnalyzerDiagnostic diagnostic
                    = makeDiagnostic(path, "NLS0023", "info", "Dataflow",
                        quotedNameMessage("Local function", function.name, "is never used."),
                        function.line, 1);
                addUnusedLocalFunctionFix(diagnostic, function);
                appendDiagnostic(std::move(diagnostic));
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
applyCodeAnalyzerDataflowRules(const std::filesystem::path& path, const stringVector& lines,
    const ParserContext& parserContext, const CodeAnalyzerOptions& options,
    const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
{
    CodeAnalyzerAstModel model = buildCodeAnalyzerAstModel(lines, parserContext);
    applyEmptyBlockRules(path, model.lineUsages, appendDiagnostic);
    applyUnreachableCodeRules(path, lines, model.lineUsages, appendDiagnostic);
    applyConstantConditionRules(path, model.lineUsages, appendDiagnostic);
    for (const CodeAnalyzerFunction& function : model.functions) {
        applyForFunction(path, lines, appendDiagnostic, function, model.functions, model.lineUsages,
            model.classdefs);
    }
    applyLocalFunctionRules(path, model.functions, model.lineUsages, appendDiagnostic);
    applyFunctionSizeRules(path, options, model.functions, appendDiagnostic);
    applyCyclomaticComplexityRules(path, options, model.functions, appendDiagnostic);
    applyCognitiveComplexityRules(path, options, model.functions, appendDiagnostic);
    applyFunctionNamingRules(path, model.functions, appendDiagnostic);
    applyCodeAnalyzerClassdefRules(path, lines, model.functions, model.classdefs, appendDiagnostic);
    applyDocumentationRules(path, lines, model.functions, model.classdefs, appendDiagnostic);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
