//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerDataflowImports.hpp"
#include "CodeAnalyzerAstUtilities.hpp"
#include <algorithm>
#include <cctype>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
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
    std::string
    importedSymbolName(const std::string& spec);
    //=============================================================================
    CodeAnalyzerImportSpec
    makeImportSpec(const std::string& spec);
    //=============================================================================
    CodeAnalyzerImportSpec
    makeImportSpecRange(const std::string& spec, int lineNumber, size_t start, size_t end);
    //=============================================================================
    bool
    startsWithImportSyntax(const std::string& line)
    {
        std::string trimmed = trim(line);
        if (trimmed.size() < 6 || trimmed.compare(0, 6, "import") != 0) {
            return false;
        }
        return trimmed.size() == 6 || std::isspace(static_cast<unsigned char>(trimmed[6]))
            || trimmed[6] == '(';
    }
    //=============================================================================
    stringVector
    splitImportSpecs(const std::string& value)
    {
        stringVector specs;
        std::string current;
        for (char c : value) {
            if (c == ',' || std::isspace(static_cast<unsigned char>(c))) {
                std::string spec = trim(current);
                if (!spec.empty()) {
                    specs.push_back(spec);
                }
                current.clear();
            } else {
                current.push_back(c);
            }
        }
        std::string spec = trim(current);
        if (!spec.empty()) {
            specs.push_back(spec);
        }
        return specs;
    }
    //=============================================================================
    stringVector
    splitLiteralImportSpecs(const std::string& value)
    {
        stringVector specs;
        size_t cursor = 0;
        while (cursor < value.size()) {
            while (cursor < value.size()
                && (std::isspace(static_cast<unsigned char>(value[cursor]))
                    || value[cursor] == ',')) {
                ++cursor;
            }
            if (cursor >= value.size() || (value[cursor] != '\'' && value[cursor] != '"')) {
                break;
            }
            const char quote = value[cursor];
            std::string spec;
            ++cursor;
            while (cursor < value.size()) {
                if (value[cursor] == quote && cursor + 1 < value.size()
                    && value[cursor + 1] == quote) {
                    spec.push_back(quote);
                    cursor += 2;
                    continue;
                }
                if (value[cursor] == quote) {
                    ++cursor;
                    break;
                }
                spec.push_back(value[cursor++]);
            }
            spec = trim(spec);
            if (!spec.empty()) {
                specs.push_back(spec);
            }
        }
        return specs;
    }
    //=============================================================================
    std::string
    importTail(const std::string& line)
    {
        std::string trimmed = trim(CodeAnalyzerAst::stripComment(line));
        if (!startsWithImportSyntax(trimmed)) {
            return "";
        }
        std::string tail = trim(trimmed.substr(6));
        if (tail.empty() || tail[0] != '(') {
            return tail;
        }
        size_t close = tail.find_last_of(')');
        if (close == std::string::npos || close == 0) {
            return "";
        }
        return trim(tail.substr(1, close - 1));
    }
    //=============================================================================
    size_t
    commentStart(const std::string& line)
    {
        size_t comment = CodeAnalyzerAst::codeAnalyzerCommentStart(line);
        return comment == std::string::npos ? line.size() : comment;
    }
    //=============================================================================
    std::string
    codeBeforeComment(const std::string& line)
    {
        return CodeAnalyzerAst::stripComment(line);
    }
    //=============================================================================
    size_t
    importCommandStart(const std::string& line)
    {
        const size_t end = commentStart(line);
        size_t cursor = 0;
        while (cursor < end && std::isspace(static_cast<unsigned char>(line[cursor]))) {
            ++cursor;
        }
        if (cursor + 6 > end || line.compare(cursor, 6, "import") != 0) {
            return std::string::npos;
        }
        const size_t after = cursor + 6;
        if (after < end && !std::isspace(static_cast<unsigned char>(line[after]))
            && line[after] != '(') {
            return std::string::npos;
        }
        return cursor;
    }
    //=============================================================================
    bool
    hasContinuationMarker(const std::string& line)
    {
        std::string trimmed = trim(codeBeforeComment(line));
        return trimmed.size() >= 3 && trimmed.compare(trimmed.size() - 3, 3, "...") == 0;
    }
    //=============================================================================
    std::string
    withoutContinuationMarker(const std::string& line)
    {
        std::string code = codeBeforeComment(line);
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
    logicalImportLine(const stringVector& lines, int lineNumber)
    {
        if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
            return "";
        }
        size_t index = static_cast<size_t>(lineNumber - 1);
        std::string logical = withoutContinuationMarker(lines[index]);
        while (hasContinuationMarker(lines[index]) && index + 1 < lines.size()) {
            ++index;
            std::string continuation = withoutContinuationMarker(lines[index]);
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
    importStartsLiteralSyntax(const std::string& line)
    {
        std::string trimmed = trim(codeBeforeComment(line));
        if (!startsWithImportSyntax(trimmed)) {
            return false;
        }
        std::string tail = trim(trimmed.substr(6));
        return !tail.empty() && tail[0] == '(';
    }
    //=============================================================================
    std::vector<CodeAnalyzerImportSpec>
    normalImportSpecsFromLine(const std::string& line, int lineNumber, size_t cursor, size_t end)
    {
        std::vector<CodeAnalyzerImportSpec> imports;
        while (cursor < end) {
            while (cursor < end
                && (std::isspace(static_cast<unsigned char>(line[cursor]))
                    || line[cursor] == ',')) {
                ++cursor;
            }
            size_t start = cursor;
            while (cursor < end && !std::isspace(static_cast<unsigned char>(line[cursor]))
                && line[cursor] != ',') {
                ++cursor;
            }
            if (cursor > start) {
                imports.push_back(makeImportSpecRange(
                    line.substr(start, cursor - start), lineNumber, start, cursor));
            }
        }
        return imports;
    }
    //=============================================================================
    std::vector<CodeAnalyzerImportSpec>
    literalImportSpecsFromLine(const std::string& line, int lineNumber, size_t cursor, size_t end)
    {
        std::vector<CodeAnalyzerImportSpec> imports;
        while (cursor < end) {
            while (cursor < end
                && (std::isspace(static_cast<unsigned char>(line[cursor]))
                    || line[cursor] == ',')) {
                ++cursor;
            }
            if (cursor >= end || (line[cursor] != '\'' && line[cursor] != '"')) {
                break;
            }
            const char quote = line[cursor];
            size_t start = cursor;
            std::string spec;
            ++cursor;
            while (cursor < end) {
                if (line[cursor] == quote && cursor + 1 < end && line[cursor + 1] == quote) {
                    spec.push_back(quote);
                    cursor += 2;
                    continue;
                }
                if (line[cursor] == quote) {
                    ++cursor;
                    break;
                }
                spec.push_back(line[cursor++]);
            }
            spec = trim(spec);
            if (!spec.empty()) {
                imports.push_back(makeImportSpecRange(spec, lineNumber, start, cursor));
            }
        }
        return imports;
    }
    //=============================================================================
    std::vector<CodeAnalyzerImportSpec>
    importSpecsFromPhysicalLine(const std::string& line, int lineNumber)
    {
        size_t importStart = importCommandStart(line);
        if (importStart == std::string::npos) {
            return {};
        }
        size_t cursor = importStart + 6;
        while (cursor < line.size() && std::isspace(static_cast<unsigned char>(line[cursor]))) {
            ++cursor;
        }
        size_t end = commentStart(line);
        if (cursor >= end) {
            return {};
        }
        if (line[cursor] == '(') {
            ++cursor;
            size_t close = line.find_last_of(')');
            end = close == std::string::npos ? end : std::min(end, close);
            return literalImportSpecsFromLine(line, lineNumber, cursor, end);
        }
        return normalImportSpecsFromLine(line, lineNumber, cursor, end);
    }
    //=============================================================================
    stringVector
    importSpecsFromLine(const std::string& line)
    {
        std::string tail = importTail(line);
        if (tail.empty()) {
            return {};
        }
        if (tail[0] == '\'' || tail[0] == '"') {
            return splitLiteralImportSpecs(tail);
        }
        return splitImportSpecs(tail);
    }
    //=============================================================================
    std::string
    importedSymbolName(const std::string& spec)
    {
        size_t dot = spec.find_last_of('.');
        return dot == std::string::npos ? spec : spec.substr(dot + 1);
    }
    //=============================================================================
    CodeAnalyzerImportSpec
    makeImportSpec(const std::string& spec)
    {
        CodeAnalyzerImportSpec importSpec;
        importSpec.spec = spec;
        importSpec.name = importedSymbolName(spec);
        importSpec.wildcard = importSpec.name == "*";
        return importSpec;
    }
    //=============================================================================
    CodeAnalyzerImportSpec
    makeImportSpecRange(const std::string& spec, int lineNumber, size_t start, size_t end)
    {
        CodeAnalyzerImportSpec importSpec = makeImportSpec(spec);
        importSpec.lineStart = lineNumber;
        importSpec.lineEnd = lineNumber;
        importSpec.columnStart = static_cast<int>(start + 1);
        importSpec.columnEnd = static_cast<int>(end + 1);
        importSpec.hasRange = true;
        return importSpec;
    }
    //=============================================================================
    bool
    continuationLineContainsOnlySymbol(
        const std::string& line, const std::string& symbol, bool literal)
    {
        stringVector specs = literal ? splitLiteralImportSpecs(withoutContinuationMarker(line))
                                     : splitImportSpecs(withoutContinuationMarker(line));
        return specs.size() == 1 && importedSymbolName(specs[0]) == symbol;
    }
    //=============================================================================
    bool
    hasComment(const std::string& line)
    {
        return commentStart(line) < line.size();
    }
    //=============================================================================
    std::string
    continuationCommentSuffix(const std::string& line)
    {
        size_t comment = commentStart(line);
        if (comment >= line.size()) {
            return "";
        }
        std::string code = codeBeforeComment(line);
        size_t end = code.size();
        while (end > 0 && std::isspace(static_cast<unsigned char>(code[end - 1]))) {
            --end;
        }
        if (end < 3 || code.compare(end - 3, 3, "...") != 0) {
            return "";
        }
        return " " + trim(line.substr(comment));
    }
    //=============================================================================
    bool
    continuationImportRemovalEditFor(const stringVector& lines, int lineNumber,
        const std::string& symbol, int& lineStart, int& columnStart, int& lineEnd, int& columnEnd,
        std::string& replacementText)
    {
        if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
            return false;
        }
        const size_t first = static_cast<size_t>(lineNumber - 1);
        if (!hasContinuationMarker(lines[first])) {
            return false;
        }
        const bool literal = importStartsLiteralSyntax(lines[first]);
        size_t target = first + 1;
        while (target < lines.size()) {
            const bool targetHasContinuation = hasContinuationMarker(lines[target]);
            if (continuationLineContainsOnlySymbol(lines[target], symbol, literal)) {
                if (targetHasContinuation || hasComment(lines[target])) {
                    return false;
                }
                break;
            }
            if (!targetHasContinuation) {
                return false;
            }
            ++target;
        }
        if (target >= lines.size() || target == first) {
            return false;
        }
        const size_t previous = target - 1;
        if (!hasContinuationMarker(lines[previous])) {
            return false;
        }
        std::string previousCode = codeBeforeComment(lines[previous]);
        size_t end = previousCode.size();
        while (end > 0 && std::isspace(static_cast<unsigned char>(previousCode[end - 1]))) {
            --end;
        }
        if (end < 3 || previousCode.compare(end - 3, 3, "...") != 0) {
            return false;
        }
        const size_t ellipsis = end - 3;
        size_t start = previousCode.rfind(',', ellipsis);
        if (start == std::string::npos) {
            start = ellipsis;
        }
        lineStart = static_cast<int>(previous + 1);
        columnStart = static_cast<int>(start + 1);
        lineEnd = static_cast<int>(target + 2);
        columnEnd = 1;
        replacementText
            = literal && codeBeforeComment(lines[target]).find(')') != std::string::npos ? ")" : "";
        replacementText += continuationCommentSuffix(lines[previous]);
        replacementText += "\n";
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
codeAnalyzerStartsWithImport(const std::string& line)
{
    return startsWithImportSyntax(line);
}
//=============================================================================
std::string
codeAnalyzerImportedSymbolName(const std::string& spec)
{
    return importedSymbolName(spec);
}
//=============================================================================
std::vector<CodeAnalyzerImportSpec>
collectCodeAnalyzerImportSpecs(const stringVector& lines, int lineNumber)
{
    std::vector<CodeAnalyzerImportSpec> imports;
    if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
        return imports;
    }
    const std::string& line = lines[static_cast<size_t>(lineNumber - 1)];
    if (!hasContinuationMarker(line)) {
        return importSpecsFromPhysicalLine(line, lineNumber);
    }
    const std::string logicalLine = logicalImportLine(lines, lineNumber);
    for (const std::string& spec : importSpecsFromLine(logicalLine)) {
        CodeAnalyzerImportSpec importSpec = makeImportSpec(spec);
        if (importSpec.name.empty()) {
            continue;
        }
        imports.push_back(importSpec);
    }
    return imports;
}
//=============================================================================
void
collectCodeAnalyzerImportSymbols(
    const std::string& line, std::set<std::string>& importedNames, bool& wildcardImport)
{
    std::string trimmed = trim(line);
    if (!codeAnalyzerStartsWithImport(trimmed)) {
        return;
    }
    for (const CodeAnalyzerImportSpec& importSpec : importSpecsFromPhysicalLine(trimmed, 1)) {
        if (importSpec.wildcard) {
            wildcardImport = true;
        } else if (!importSpec.name.empty()) {
            importedNames.insert(importSpec.name);
        }
    }
}
//=============================================================================
void
collectCodeAnalyzerImportSymbols(const stringVector& lines, int lineNumber,
    std::set<std::string>& importedNames, bool& wildcardImport)
{
    collectCodeAnalyzerImportSymbols(
        logicalImportLine(lines, lineNumber), importedNames, wildcardImport);
}
//=============================================================================
bool
isSimpleCodeAnalyzerImportLineFor(const std::string& line, const std::string& symbol)
{
    if (hasComment(line)) {
        return false;
    }
    std::string trimmed = trim(line);
    if (!codeAnalyzerStartsWithImport(trimmed)) {
        return false;
    }
    std::vector<CodeAnalyzerImportSpec> imports = importSpecsFromPhysicalLine(trimmed, 1);
    if (imports.size() != 1) {
        return false;
    }
    return imports[0].name == symbol;
}
//=============================================================================
bool
codeAnalyzerImportRemovalEditFor(const std::string& line, const std::string& symbol,
    int& columnStart, int& columnEnd, std::string& replacementText)
{
    std::vector<CodeAnalyzerImportSpec> imports = importSpecsFromPhysicalLine(line, 1);
    if (imports.size() <= 1) {
        return false;
    }
    auto found = std::find_if(imports.begin(), imports.end(),
        [&symbol](const CodeAnalyzerImportSpec& importSpec) { return importSpec.name == symbol; });
    if (found == imports.end() || !found->hasRange) {
        return false;
    }
    size_t index = static_cast<size_t>(std::distance(imports.begin(), found));
    int start = found->columnStart;
    int end = found->columnEnd;
    if (index > 0) {
        start = imports[index - 1].columnEnd;
    } else if (index + 1 < imports.size()) {
        end = imports[index + 1].columnStart;
    }
    columnStart = start;
    columnEnd = end;
    replacementText.clear();
    return columnEnd > columnStart;
}
//=============================================================================
bool
codeAnalyzerImportRemovalEditFor(const stringVector& lines, int lineNumber,
    const std::string& symbol, int& lineStart, int& columnStart, int& lineEnd, int& columnEnd,
    std::string& replacementText)
{
    return continuationImportRemovalEditFor(
        lines, lineNumber, symbol, lineStart, columnStart, lineEnd, columnEnd, replacementText);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
