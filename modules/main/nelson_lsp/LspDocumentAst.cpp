//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LspDocumentAst.hpp"
#include "CodeAnalyzerAst.hpp"
#include "LexerContext.hpp"
#include "ParserInterface.hpp"
#include <algorithm>
#include <sstream>
#include <vector>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
namespace {
    //=============================================================================
    void
    appendUnique(std::vector<LspModelSymbol>& symbols, LspModelSymbol symbol)
    {
        auto found
            = std::find_if(symbols.begin(), symbols.end(), [&symbol](const LspModelSymbol& item) {
                  return item.name == symbol.name && item.line == symbol.line;
              });
        if (found == symbols.end()) {
            symbols.push_back(std::move(symbol));
        }
    }
    //=============================================================================
    void
    appendVariable(LspDocumentModel& model, const std::string& name, const std::string& detail,
        int lineNumber, int column)
    {
        if (name.empty()) {
            return;
        }
        LspModelSymbol symbol;
        symbol.name = name;
        symbol.detail = detail;
        symbol.kind = 13;
        symbol.line = lineNumber;
        symbol.column = std::max(column, 1);
        symbol.endLine = lineNumber;
        symbol.endColumn = symbol.column + static_cast<int>(name.size());
        appendUnique(model.variables, symbol);
    }
    //=============================================================================
    void
    appendImport(LspDocumentModel& model, const std::string& name, const std::string& detail,
        int lineNumber, int column)
    {
        if (name.empty()) {
            return;
        }
        LspModelSymbol symbol;
        symbol.name = name;
        symbol.detail = detail;
        symbol.kind = 9;
        symbol.line = lineNumber;
        symbol.column = std::max(column, 1);
        symbol.endLine = lineNumber;
        symbol.endColumn
            = symbol.column + static_cast<int>(detail.empty() ? name.size() : detail.size());
        appendUnique(model.imports, symbol);
    }
    //=============================================================================
    void
    appendAstFunction(LspDocumentModel& model, const Nelson::CodeAnalyzerFunction& function)
    {
        if (function.name.empty()) {
            return;
        }
        LspModelSymbol symbol;
        symbol.name = function.name;
        symbol.parameters = function.inputs;
        std::ostringstream detail;
        detail << "function " << symbol.name << "(";
        for (size_t k = 0; k < symbol.parameters.size(); ++k) {
            if (k > 0) {
                detail << ", ";
            }
            detail << symbol.parameters[k];
        }
        detail << ")";
        symbol.detail = detail.str();
        symbol.kind = function.classdefMethod ? 6 : 12;
        symbol.line = std::max(function.line, 1);
        symbol.column = std::max(function.column, 1);
        symbol.endLine = std::max(function.endLine, symbol.line);
        symbol.endColumn = symbol.column + static_cast<int>(symbol.name.size());
        appendUnique(model.symbols, symbol);
        const std::string line
            = symbol.line > 0 && symbol.line <= static_cast<int>(model.lines.size())
            ? model.lines[static_cast<size_t>(symbol.line - 1)]
            : "";
        for (const std::string& input : function.inputs) {
            appendVariable(model, input, "argument", symbol.line,
                Nelson::codeAnalyzerWordColumnInLine(line, input));
        }
        for (const std::string& output : function.outputs) {
            appendVariable(model, output, "output", symbol.line,
                Nelson::codeAnalyzerWordColumnInLine(line, output));
        }
    }
    //=============================================================================
    void
    appendAstClassdef(LspDocumentModel& model, const Nelson::CodeAnalyzerClassdef& classdef)
    {
        if (classdef.name.empty()) {
            return;
        }
        LspModelSymbol symbol;
        symbol.name = classdef.name;
        symbol.detail = "class";
        symbol.kind = 5;
        symbol.line = std::max(classdef.line, 1);
        symbol.column = std::max(classdef.column, 1);
        symbol.endLine = std::max(classdef.endLine, symbol.line);
        symbol.endColumn = symbol.column + static_cast<int>(symbol.name.size());
        appendUnique(model.symbols, symbol);
        for (const auto& property : classdef.properties) {
            appendVariable(model, property.name, "property", property.line, property.column);
        }
    }
    //=============================================================================
    void
    appendAstLineUsage(LspDocumentModel& model, const Nelson::CodeAnalyzerLineUsage& usage)
    {
        if (usage.line <= 0 || usage.line > static_cast<int>(model.lines.size())) {
            return;
        }
        const std::string& line = model.lines[static_cast<size_t>(usage.line - 1)];
        for (const std::string& name : usage.assigned) {
            appendVariable(model, name, "assignment", usage.line,
                Nelson::codeAnalyzerWordColumnInLine(line, name));
        }
        for (const std::string& name : usage.globalDeclarations) {
            appendVariable(model, name, "global", usage.line,
                Nelson::codeAnalyzerWordColumnInLine(line, name));
        }
        for (const std::string& name : usage.persistentDeclarations) {
            appendVariable(model, name, "persistent", usage.line,
                Nelson::codeAnalyzerWordColumnInLine(line, name));
        }
        for (size_t k = 0; k < usage.importedNames.size(); ++k) {
            const std::string& name = usage.importedNames[k];
            const std::string detail
                = k < usage.importedSpecs.size() ? usage.importedSpecs[k] : usage.importedNames[k];
            appendImport(model, name, detail, usage.line,
                Nelson::codeAnalyzerWordColumnInLine(line, detail.empty() ? name : detail));
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
appendAstModel(LspDocumentModel& model)
{
    Nelson::LexerContext lexerContext;
    Nelson::ParserContext parserContext = Nelson::parseStringResult(lexerContext, model.text);
    if (parserContext.state() == Nelson::ParseError) {
        return false;
    }
    Nelson::CodeAnalyzerAstModel astModel
        = Nelson::buildCodeAnalyzerAstModel(model.lines, parserContext);
    for (const auto& classdef : astModel.classdefs) {
        appendAstClassdef(model, classdef);
    }
    for (const auto& function : astModel.functions) {
        appendAstFunction(model, function);
    }
    for (const auto& usage : astModel.lineUsages) {
        appendAstLineUsage(model, usage);
    }
    return !astModel.functions.empty() || !astModel.classdefs.empty()
        || !astModel.lineUsages.empty();
}
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
