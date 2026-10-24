//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LspDocumentModel.hpp"
#include "LspDocumentAst.hpp"
#include "LspUtilities.hpp"
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <regex>
#include <set>
#include <sstream>
#include <tuple>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
namespace {
    //=============================================================================
    bool
    isIdentifierStart(char c)
    {
        return std::isalpha(static_cast<unsigned char>(c)) || c == '_';
    }
    //=============================================================================
    bool
    isIdentifierChar(char c)
    {
        return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
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
    std::vector<std::string>
    splitParameters(const std::string& value)
    {
        std::vector<std::string> parameters;
        size_t start = 0;
        while (start <= value.size()) {
            size_t comma = value.find(',', start);
            if (comma == std::string::npos) {
                comma = value.size();
            }
            std::string parameter = trim(value.substr(start, comma - start));
            if (!parameter.empty()) {
                parameters.push_back(parameter);
            }
            if (comma == value.size()) {
                break;
            }
            start = comma + 1;
        }
        return parameters;
    }
    //=============================================================================
    std::vector<std::string>
    splitLines(const std::string& text)
    {
        std::vector<std::string> lines;
        size_t start = 0;
        while (start <= text.size()) {
            size_t end = text.find('\n', start);
            if (end == std::string::npos) {
                end = text.size();
            }
            std::string line = text.substr(start, end - start);
            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }
            lines.push_back(line);
            if (end == text.size()) {
                break;
            }
            start = end + 1;
        }
        return lines;
    }
    //=============================================================================
    std::string
    codeWithoutComment(const std::string& line)
    {
        bool inString = false;
        char delimiter = 0;
        for (size_t k = 0; k < line.size(); ++k) {
            char c = line[k];
            if (inString) {
                if (c == delimiter) {
                    if (k + 1 < line.size() && line[k + 1] == delimiter) {
                        ++k;
                    } else {
                        inString = false;
                    }
                }
                continue;
            }
            if (c == '\'' || c == '"') {
                inString = true;
                delimiter = c;
                continue;
            }
            if (c == '%') {
                return line.substr(0, k);
            }
        }
        return line;
    }
    //=============================================================================
    bool
    containsWord(const std::string& text, const std::string& word)
    {
        size_t pos = text.find(word);
        while (pos != std::string::npos) {
            bool left = pos == 0 || !isIdentifierChar(text[pos - 1]);
            size_t end = pos + word.size();
            bool right = end >= text.size() || !isIdentifierChar(text[end]);
            if (left && right) {
                return true;
            }
            pos = text.find(word, pos + 1);
        }
        return false;
    }
    //=============================================================================
    int
    wordColumn(const std::string& line, const std::string& word)
    {
        size_t pos = line.find(word);
        while (pos != std::string::npos) {
            bool left = pos == 0 || !isIdentifierChar(line[pos - 1]);
            size_t end = pos + word.size();
            bool right = end >= line.size() || !isIdentifierChar(line[end]);
            if (left && right) {
                return static_cast<int>(pos + 1);
            }
            pos = line.find(word, pos + 1);
        }
        return 1;
    }
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
    collectFunction(LspDocumentModel& model, const std::string& line, int lineNumber)
    {
        static const std::regex functionPattern(
            R"(^\s*function\s+(?:\[[^\]]*\]\s*=\s*|[A-Za-z_]\w*\s*=\s*)?([A-Za-z_]\w*)\s*(?:\(([^\)]*)\))?)",
            std::regex_constants::icase);
        std::smatch match;
        if (!std::regex_search(line, match, functionPattern)) {
            return;
        }
        LspModelSymbol symbol;
        symbol.name = match[1].str();
        if (match.size() > 2 && match[2].matched) {
            symbol.parameters = splitParameters(match[2].str());
        }
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
        symbol.kind = 12;
        symbol.line = lineNumber;
        symbol.column = wordColumn(line, symbol.name);
        symbol.endLine = lineNumber;
        symbol.endColumn = symbol.column + static_cast<int>(symbol.name.size());
        appendUnique(model.symbols, symbol);
        for (const auto& parameter : symbol.parameters) {
            appendVariable(model, parameter, "argument", lineNumber, wordColumn(line, parameter));
        }
    }
    //=============================================================================
    void
    collectClassdef(LspDocumentModel& model, const std::string& line, int lineNumber)
    {
        static const std::regex classPattern(
            R"(^\s*classdef(?:\s*\([^\)]*\))?\s+([A-Za-z_]\w*))", std::regex_constants::icase);
        std::smatch match;
        if (!std::regex_search(line, match, classPattern)) {
            return;
        }
        LspModelSymbol symbol;
        symbol.name = match[1].str();
        symbol.detail = "class";
        symbol.kind = 5;
        symbol.line = lineNumber;
        symbol.column = wordColumn(line, symbol.name);
        symbol.endLine = lineNumber;
        symbol.endColumn = symbol.column + static_cast<int>(symbol.name.size());
        appendUnique(model.symbols, symbol);
    }
    //=============================================================================
    void
    collectImport(LspDocumentModel& model, const std::string& line, int lineNumber)
    {
        static const std::regex importPattern(
            R"(^\s*import\s+([A-Za-z_][A-Za-z0-9_\.]*\*?))", std::regex_constants::icase);
        std::smatch match;
        if (!std::regex_search(line, match, importPattern)) {
            return;
        }
        std::string spec = match[1].str();
        size_t dot = spec.find_last_of('.');
        LspModelSymbol symbol;
        symbol.name = dot == std::string::npos ? spec : spec.substr(dot + 1);
        symbol.detail = spec;
        symbol.kind = 9;
        symbol.line = lineNumber;
        symbol.column = wordColumn(line, spec);
        symbol.endLine = lineNumber;
        symbol.endColumn = symbol.column + static_cast<int>(spec.size());
        appendUnique(model.imports, symbol);
    }
    //=============================================================================
    void
    collectAssignments(LspDocumentModel& model, const std::string& line, int lineNumber)
    {
        static const std::regex assignmentPattern(R"((?:^|[,;\s])([A-Za-z_]\w*)\s*=)");
        auto begin = std::sregex_iterator(line.begin(), line.end(), assignmentPattern);
        auto end = std::sregex_iterator();
        for (auto it = begin; it != end; ++it) {
            std::string name = (*it)[1].str();
            if (containsWord(line.substr(0, static_cast<size_t>((*it).position())), "function")) {
                continue;
            }
            appendVariable(model, name, "variable", lineNumber, wordColumn(line, name));
        }
    }
    //=============================================================================
    void
    collectDeclarations(LspDocumentModel& model, const std::string& line, int lineNumber)
    {
        static const std::regex declarationPattern(
            R"(^\s*(global|persistent)\s+(.+)$)", std::regex_constants::icase);
        std::smatch match;
        if (!std::regex_search(line, match, declarationPattern)) {
            return;
        }
        const std::string detail = match[1].str();
        for (const auto& name : splitParameters(match[2].str())) {
            appendVariable(model, name, detail, lineNumber, wordColumn(line, name));
        }
    }
    //=============================================================================
    bool
    matchesPrefix(const std::string& value, const std::string& prefix)
    {
        if (prefix.empty()) {
            return true;
        }
        return value.size() >= prefix.size()
            && std::equal(prefix.begin(), prefix.end(), value.begin(), [](char a, char b) {
                   return std::tolower(static_cast<unsigned char>(a))
                       == std::tolower(static_cast<unsigned char>(b));
               });
    }
    //=============================================================================
    nlohmann::json
    completionItem(const std::string& label, int kind, const std::string& detail)
    {
        nlohmann::json item;
        item["label"] = label;
        item["kind"] = kind;
        item["detail"] = detail;
        return item;
    }
    //=============================================================================
    nlohmann::json
    snippetItem(const std::string& label, const std::string& insertText, const std::string& detail)
    {
        nlohmann::json item = completionItem(label, 15, detail);
        item["insertText"] = insertText;
        item["insertTextFormat"] = 2;
        return item;
    }
    //=============================================================================
    nlohmann::json
    symbolRange(const LspModelSymbol& symbol)
    {
        return lspRange(symbol.line, symbol.column, symbol.endLine, symbol.endColumn);
    }
    //=============================================================================
} // namespace
//=============================================================================
const std::vector<std::string>&
lspKeywords()
{
    static const std::vector<std::string> keywords = { "break", "case", "catch", "classdef",
        "continue", "else", "elseif", "end", "for", "function", "global", "if", "import",
        "otherwise", "persistent", "return", "switch", "try", "while" };
    return keywords;
}
//=============================================================================
LspDocumentModel
buildLspDocumentModel(const std::string& uri, const std::string& text)
{
    LspDocumentModel model;
    model.uri = uri;
    model.text = text;
    model.lines = splitLines(text);
    const bool astModelUsed = appendAstModel(model);
    for (size_t k = 0; k < model.lines.size(); ++k) {
        const int lineNumber = static_cast<int>(k + 1);
        std::string code = codeWithoutComment(model.lines[k]);
        if (!astModelUsed) {
            collectFunction(model, code, lineNumber);
            collectClassdef(model, code, lineNumber);
        }
        collectImport(model, code, lineNumber);
        collectAssignments(model, code, lineNumber);
        collectDeclarations(model, code, lineNumber);
    }
    return model;
}
//=============================================================================
std::string
lspWordAt(const LspDocumentModel& model, int zeroBasedLine, int zeroBasedCharacter)
{
    if (zeroBasedLine < 0 || zeroBasedLine >= static_cast<int>(model.lines.size())) {
        return "";
    }
    const std::string& line = model.lines[static_cast<size_t>(zeroBasedLine)];
    size_t cursor = std::min(static_cast<size_t>(std::max(zeroBasedCharacter, 0)), line.size());
    size_t first = cursor;
    while (first > 0 && isIdentifierChar(line[first - 1])) {
        --first;
    }
    size_t last = cursor;
    while (last < line.size() && isIdentifierChar(line[last])) {
        ++last;
    }
    if (first == last || !isIdentifierStart(line[first])) {
        return "";
    }
    return line.substr(first, last - first);
}
//=============================================================================
std::string
lspPrefixAt(const LspDocumentModel& model, int zeroBasedLine, int zeroBasedCharacter)
{
    if (zeroBasedLine < 0 || zeroBasedLine >= static_cast<int>(model.lines.size())) {
        return "";
    }
    const std::string& line = model.lines[static_cast<size_t>(zeroBasedLine)];
    size_t cursor = std::min(static_cast<size_t>(std::max(zeroBasedCharacter, 0)), line.size());
    size_t first = cursor;
    while (first > 0 && isIdentifierChar(line[first - 1])) {
        --first;
    }
    return line.substr(first, cursor - first);
}
//=============================================================================
nlohmann::json
lspLocation(const std::string& uri, const LspModelSymbol& symbol)
{
    nlohmann::json location;
    location["uri"] = uri;
    location["range"] = symbolRange(symbol);
    return location;
}
//=============================================================================
nlohmann::json
lspDocumentSymbols(const LspDocumentModel& model)
{
    nlohmann::json result = nlohmann::json::array();
    for (const auto& symbol : model.symbols) {
        nlohmann::json item;
        item["name"] = symbol.name;
        item["detail"] = symbol.detail;
        item["kind"] = symbol.kind;
        item["range"] = symbolRange(symbol);
        item["selectionRange"] = symbolRange(symbol);
        result.push_back(item);
    }
    return result;
}
//=============================================================================
nlohmann::json
lspCompletionItems(const LspDocumentModel& model, const std::string& prefix)
{
    nlohmann::json result = nlohmann::json::array();
    std::set<std::string> emitted;
    auto append = [&](const std::string& label, int kind, const std::string& detail) {
        if (label.empty() || !matchesPrefix(label, prefix) || !emitted.insert(label).second) {
            return;
        }
        result.push_back(completionItem(label, kind, detail));
    };
    for (const auto& keyword : lspKeywords()) {
        append(keyword, 14, "keyword");
    }
    if (matchesPrefix("for", prefix)) {
        result.push_back(
            snippetItem("for loop", "for ${1:k} = ${2:range}\n  ${0}\nend", "snippet"));
    }
    if (matchesPrefix("if", prefix)) {
        result.push_back(snippetItem("if block", "if ${1:condition}\n  ${0}\nend", "snippet"));
    }
    if (matchesPrefix("function", prefix)) {
        result.push_back(
            snippetItem("function", "function ${1:y} = ${2:name}(${3:x})\n  ${0}\nend", "snippet"));
    }
    for (const auto& symbol : model.symbols) {
        if (symbol.parameters.empty()) {
            append(symbol.name, symbol.kind, symbol.detail);
        } else if (matchesPrefix(symbol.name, prefix) && emitted.insert(symbol.name).second) {
            std::ostringstream insertText;
            insertText << symbol.name << "(";
            for (size_t k = 0; k < symbol.parameters.size(); ++k) {
                if (k > 0) {
                    insertText << ", ";
                }
                insertText << "${" << k + 1 << ":" << symbol.parameters[k] << "}";
            }
            insertText << ")";
            result.push_back(snippetItem(symbol.name, insertText.str(), symbol.detail));
        }
    }
    for (const auto& variable : model.variables) {
        append(variable.name, 6, "local variable");
    }
    for (const auto& import : model.imports) {
        append(import.name, 9, import.detail);
    }
    for (const std::string& builtin : { "disp", "error", "warning", "plot", "zeros", "ones", "size",
             "length", "class", "help", "checkcode", "codeIssues" }) {
        append(builtin, 3, "Nelson function");
    }
    return result;
}
//=============================================================================
nlohmann::json
lspReferences(const LspDocumentModel& model, const std::string& name)
{
    nlohmann::json result = nlohmann::json::array();
    if (name.empty()) {
        return result;
    }
    for (size_t k = 0; k < model.lines.size(); ++k) {
        const std::string code = codeWithoutComment(model.lines[k]);
        size_t pos = code.find(name);
        while (pos != std::string::npos) {
            bool left = pos == 0 || !isIdentifierChar(code[pos - 1]);
            size_t end = pos + name.size();
            bool right = end >= code.size() || !isIdentifierChar(code[end]);
            if (left && right) {
                LspModelSymbol symbol;
                symbol.name = name;
                symbol.line = static_cast<int>(k + 1);
                symbol.column = static_cast<int>(pos + 1);
                symbol.endLine = symbol.line;
                symbol.endColumn = symbol.column + static_cast<int>(name.size());
                result.push_back(lspLocation(model.uri, symbol));
            }
            pos = code.find(name, pos + 1);
        }
    }
    return result;
}
//=============================================================================
nlohmann::json
lspDocumentHighlights(const LspDocumentModel& model, const std::string& name)
{
    nlohmann::json result = nlohmann::json::array();
    for (const auto& location : lspReferences(model, name)) {
        nlohmann::json item;
        item["range"] = location["range"];
        item["kind"] = 1;
        result.push_back(item);
    }
    return result;
}
//=============================================================================
nlohmann::json
lspFoldingRanges(const LspDocumentModel& model)
{
    nlohmann::json result = nlohmann::json::array();
    std::vector<int> stack;
    for (size_t k = 0; k < model.lines.size(); ++k) {
        std::string code = trim(codeWithoutComment(model.lines[k]));
        if (containsWord(code, "function") || containsWord(code, "classdef")
            || containsWord(code, "if") || containsWord(code, "for") || containsWord(code, "while")
            || containsWord(code, "switch") || containsWord(code, "try")) {
            stack.push_back(static_cast<int>(k));
        } else if (code == "end" && !stack.empty()) {
            int start = stack.back();
            stack.pop_back();
            if (static_cast<int>(k) > start) {
                nlohmann::json range;
                range["startLine"] = start;
                range["endLine"] = static_cast<int>(k);
                result.push_back(range);
            }
        }
    }
    return result;
}
//=============================================================================
nlohmann::json
lspSelectionRanges(const LspDocumentModel& model, int zeroBasedLine, int zeroBasedCharacter)
{
    nlohmann::json result = nlohmann::json::array();
    if (zeroBasedLine < 0 || zeroBasedLine >= static_cast<int>(model.lines.size())) {
        return result;
    }
    std::string word = lspWordAt(model, zeroBasedLine, zeroBasedCharacter);
    nlohmann::json item;
    if (!word.empty()) {
        int column = wordColumn(model.lines[static_cast<size_t>(zeroBasedLine)], word);
        item["range"] = lspRange(
            zeroBasedLine + 1, column, zeroBasedLine + 1, column + static_cast<int>(word.size()));
    } else {
        item["range"] = lspRange(zeroBasedLine + 1, 1, zeroBasedLine + 1,
            static_cast<int>(model.lines[static_cast<size_t>(zeroBasedLine)].size() + 1));
    }
    result.push_back(item);
    return result;
}
//=============================================================================
nlohmann::json
lspSemanticTokens(const LspDocumentModel& model)
{
    nlohmann::json data = nlohmann::json::array();
    int previousLine = 0;
    int previousStart = 0;
    auto appendToken = [&](int line, int start, int length, int type) {
        data.push_back(line - previousLine);
        data.push_back(line == previousLine ? start - previousStart : start);
        data.push_back(length);
        data.push_back(type);
        data.push_back(0);
        previousLine = line;
        previousStart = start;
    };
    for (size_t k = 0; k < model.lines.size(); ++k) {
        const std::string& line = model.lines[k];
        std::string code = codeWithoutComment(line);
        std::vector<bool> protectedColumns(line.size(), false);
        std::vector<std::tuple<int, int, int>> lineTokens;
        bool inString = false;
        char delimiter = 0;
        int tokenStart = 0;
        for (size_t column = 0; column < line.size(); ++column) {
            char c = line[column];
            if (inString) {
                protectedColumns[column] = true;
                if (c == delimiter) {
                    if (column + 1 < line.size() && line[column + 1] == delimiter) {
                        protectedColumns[column + 1] = true;
                        ++column;
                    } else {
                        lineTokens.emplace_back(
                            tokenStart, static_cast<int>(column) - tokenStart + 1, 6);
                        inString = false;
                    }
                }
                continue;
            }
            if (c == '\'' || c == '"') {
                inString = true;
                delimiter = c;
                tokenStart = static_cast<int>(column);
                protectedColumns[column] = true;
                continue;
            }
            if (c == '%') {
                for (size_t protectedColumn = column; protectedColumn < line.size();
                    ++protectedColumn) {
                    protectedColumns[protectedColumn] = true;
                }
                lineTokens.emplace_back(
                    static_cast<int>(column), static_cast<int>(line.size() - column), 7);
                break;
            }
        }
        if (inString) {
            lineTokens.emplace_back(tokenStart, static_cast<int>(line.size()) - tokenStart, 6);
        }
        for (const auto& keyword : lspKeywords()) {
            size_t pos = code.find(keyword);
            while (pos != std::string::npos) {
                bool left = pos == 0 || !isIdentifierChar(code[pos - 1]);
                size_t end = pos + keyword.size();
                bool right = end >= code.size() || !isIdentifierChar(code[end]);
                if (left && right && pos < protectedColumns.size() && !protectedColumns[pos]) {
                    lineTokens.emplace_back(
                        static_cast<int>(pos), static_cast<int>(keyword.size()), 0);
                }
                pos = code.find(keyword, pos + 1);
            }
        }
        for (const auto& symbol : model.symbols) {
            if (symbol.line == static_cast<int>(k + 1)) {
                lineTokens.emplace_back(
                    symbol.column - 1, symbol.endColumn - symbol.column, symbol.kind == 5 ? 3 : 1);
            }
        }
        for (const auto& variable : model.variables) {
            if (variable.line == static_cast<int>(k + 1)) {
                lineTokens.emplace_back(
                    variable.column - 1, variable.endColumn - variable.column, 2);
            }
        }
        std::sort(lineTokens.begin(), lineTokens.end());
        int previousEnd = -1;
        for (const auto& token : lineTokens) {
            int start = std::get<0>(token);
            int length = std::get<1>(token);
            if (start < previousEnd || length <= 0) {
                continue;
            }
            appendToken(static_cast<int>(k), start, length, std::get<2>(token));
            previousEnd = start + length;
        }
    }
    nlohmann::json result;
    result["data"] = data;
    return result;
}
//=============================================================================
nlohmann::json
lspDocumentLinks(const LspDocumentModel& model)
{
    nlohmann::json result = nlohmann::json::array();
    static const std::regex pathPattern(R"((['"])([^'"]+\.(m|json|xml))\1)");
    std::filesystem::path basePath = pathFromUri(model.uri).parent_path();
    for (size_t k = 0; k < model.lines.size(); ++k) {
        const std::string& line = model.lines[k];
        auto begin = std::sregex_iterator(line.begin(), line.end(), pathPattern);
        auto end = std::sregex_iterator();
        for (auto it = begin; it != end; ++it) {
            std::string target = (*it)[2].str();
            nlohmann::json link;
            link["range"] = lspRange(static_cast<int>(k + 1),
                static_cast<int>((*it).position(2) + 1), static_cast<int>(k + 1),
                static_cast<int>((*it).position(2) + target.size() + 1));
            std::filesystem::path targetPath(target);
            if (targetPath.is_relative() && !basePath.empty()) {
                targetPath = basePath / targetPath;
            }
            link["target"] = std::filesystem::exists(targetPath) ? pathToUri(targetPath) : target;
            result.push_back(link);
        }
    }
    for (const auto& import : model.imports) {
        nlohmann::json link;
        link["range"] = symbolRange(import);
        link["target"] = import.detail;
        result.push_back(link);
    }
    return result;
}
//=============================================================================
nlohmann::json
lspCodeLens(const LspDocumentModel& model)
{
    nlohmann::json result = nlohmann::json::array();
    for (const auto& symbol : model.symbols) {
        nlohmann::json run;
        run["range"] = symbolRange(symbol);
        run["command"]["title"] = "Run file";
        run["command"]["command"] = "nelson.runFile";
        run["command"]["arguments"] = nlohmann::json::array({ model.uri });
        result.push_back(run);
        nlohmann::json tests;
        tests["range"] = symbolRange(symbol);
        tests["command"]["title"] = "Run module tests";
        tests["command"]["command"] = "nelson.runTestsForModule";
        tests["command"]["arguments"] = nlohmann::json::array({ model.uri });
        result.push_back(tests);
        nlohmann::json issues;
        issues["range"] = symbolRange(symbol);
        issues["command"]["title"] = "Lint workspace";
        issues["command"]["command"] = "nelson.lintWorkspace";
        issues["command"]["arguments"] = nlohmann::json::array();
        result.push_back(issues);
    }
    return result;
}
//=============================================================================
nlohmann::json
lspSignatureHelp(const LspDocumentModel& model, int zeroBasedLine, int zeroBasedCharacter)
{
    nlohmann::json result;
    result["signatures"] = nlohmann::json::array();
    result["activeSignature"] = 0;
    result["activeParameter"] = 0;
    if (zeroBasedLine < 0 || zeroBasedLine >= static_cast<int>(model.lines.size())) {
        return result;
    }
    const std::string& line = model.lines[static_cast<size_t>(zeroBasedLine)];
    size_t cursor = std::min(static_cast<size_t>(std::max(zeroBasedCharacter, 0)), line.size());
    size_t paren = line.rfind('(', cursor);
    if (paren == std::string::npos || paren == 0) {
        return result;
    }
    size_t last = paren;
    size_t first = last;
    while (first > 0 && isIdentifierChar(line[first - 1])) {
        --first;
    }
    std::string name = line.substr(first, last - first);
    if (name.empty()) {
        return result;
    }
    std::vector<std::string> parameters;
    for (const auto& symbol : model.symbols) {
        if (symbol.name == name) {
            parameters = symbol.parameters;
            break;
        }
    }
    nlohmann::json signature;
    std::ostringstream label;
    label << name << "(";
    for (size_t k = 0; k < parameters.size(); ++k) {
        if (k > 0) {
            label << ", ";
        }
        label << parameters[k];
    }
    if (parameters.empty()) {
        label << "...";
    }
    label << ")";
    signature["label"] = label.str();
    signature["documentation"] = "Nelson function call";
    signature["parameters"] = nlohmann::json::array();
    if (parameters.empty()) {
        signature["parameters"].push_back({ { "label", "..." } });
    } else {
        for (const auto& parameter : parameters) {
            signature["parameters"].push_back({ { "label", parameter } });
        }
    }
    result["signatures"].push_back(signature);
    return result;
}
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
