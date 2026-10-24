//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerTextRules.hpp"
#include <algorithm>
#include <cctype>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool
    isDirectionalFormattingUtf8(const std::string& value, size_t cursor)
    {
        if (cursor + 2 >= value.size()) {
            return false;
        }
        const unsigned char first = static_cast<unsigned char>(value[cursor]);
        const unsigned char second = static_cast<unsigned char>(value[cursor + 1]);
        const unsigned char third = static_cast<unsigned char>(value[cursor + 2]);
        return (first == 0xE2 && second == 0x80 && third >= 0xAA && third <= 0xAE)
            || (first == 0xE2 && second == 0x81 && third >= 0xA6 && third <= 0xA9);
    }
    //=============================================================================
    std::string
    toLowerAscii(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return value;
    }
    //=============================================================================
    CodeAnalyzerDiagnostic
    makeDiagnostic(const std::filesystem::path& path, const std::string& id,
        const std::string& severity, const std::string& category, const std::string& fixability,
        const std::string& message, int line, int column = 1)
    {
        CodeAnalyzerDiagnostic diagnostic;
        diagnostic.file = path.wstring();
        diagnostic.checkId = id;
        diagnostic.severity = severity;
        diagnostic.category = category;
        diagnostic.fixability = fixability;
        diagnostic.message = message;
        diagnostic.lineStart = line;
        diagnostic.lineEnd = line;
        diagnostic.columnStart = column;
        diagnostic.columnEnd = column;
        return diagnostic;
    }
    //=============================================================================
    CodeAnalyzerFix
    makeSingleEditFix(const std::string& title, int lineStart, int columnStart, int lineEnd,
        int columnEnd, const std::string& replacementText)
    {
        CodeAnalyzerTextEdit edit;
        edit.lineStart = lineStart;
        edit.lineEnd = lineEnd;
        edit.columnStart = columnStart;
        edit.columnEnd = columnEnd;
        edit.replacementText = replacementText;
        CodeAnalyzerFix fix;
        fix.title = title;
        fix.edits.push_back(edit);
        return fix;
    }
    //=============================================================================
    void
    appendDirectionalFormattingDiagnostic(const std::filesystem::path& path, int lineNumber,
        size_t column, const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        CodeAnalyzerDiagnostic diagnostic = makeDiagnostic(path, "NLS0016", "error", "Security",
            "safe", "Invisible directional formatting character.", lineNumber,
            static_cast<int>(column + 1));
        diagnostic.columnEnd = static_cast<int>(column + 4);
        diagnostic.fixes.push_back(
            makeSingleEditFix("Remove invisible directional formatting character", lineNumber,
                static_cast<int>(column + 1), lineNumber, diagnostic.columnEnd, ""));
        appendDiagnostic(std::move(diagnostic));
    }
    //=============================================================================
    void
    appendTrailingWhitespaceDiagnostic(const std::filesystem::path& path, const std::string& line,
        int lineNumber, const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        size_t firstTrailing = line.size();
        while (firstTrailing > 0
            && std::isspace(static_cast<unsigned char>(line[firstTrailing - 1]))) {
            --firstTrailing;
        }
        CodeAnalyzerDiagnostic diagnostic = makeDiagnostic(path, "NLS0010", "warning", "Style",
            "trivial", "Trailing whitespace.", lineNumber, static_cast<int>(firstTrailing + 1));
        diagnostic.columnEnd = static_cast<int>(line.size() + 1);
        diagnostic.fixes.push_back(makeSingleEditFix("Remove trailing whitespace", lineNumber,
            static_cast<int>(firstTrailing + 1), lineNumber, static_cast<int>(line.size() + 1),
            ""));
        appendDiagnostic(std::move(diagnostic));
    }
    //=============================================================================
    void
    appendFinalNewlineDiagnostic(const std::filesystem::path& path, const stringVector& lines,
        const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
    {
        int line = static_cast<int>(std::max<size_t>(lines.size(), 1));
        int column = lines.empty() ? 1 : static_cast<int>(lines.back().size() + 1);
        CodeAnalyzerDiagnostic diagnostic = makeDiagnostic(path, "NLS0011", "warning", "Style",
            "trivial", "File does not end with a newline.", line, column);
        diagnostic.fixes.push_back(
            makeSingleEditFix("Insert final newline", line, column, line, column, "\n"));
        appendDiagnostic(std::move(diagnostic));
    }
    //=============================================================================
} // namespace
//=============================================================================
void
applyCodeAnalyzerTextRules(const std::filesystem::path& path, const stringVector& lines,
    const std::string& content, const CodeAnalyzerOptions& options,
    const std::function<void(CodeAnalyzerDiagnostic)>& appendDiagnostic)
{
    for (size_t k = 0; k < lines.size(); ++k) {
        const std::string& line = lines[k];
        const int lineNumber = static_cast<int>(k + 1);
        for (size_t column = 0; column < line.size(); ++column) {
            if (isDirectionalFormattingUtf8(line, column)) {
                appendDirectionalFormattingDiagnostic(path, lineNumber, column, appendDiagnostic);
                column += 2;
            }
        }
        if (!line.empty() && std::isspace(static_cast<unsigned char>(line.back()))) {
            appendTrailingWhitespaceDiagnostic(path, line, lineNumber, appendDiagnostic);
        }
        if (static_cast<int>(line.size()) > options.maxLineLength) {
            appendDiagnostic(makeDiagnostic(path, "NLS0012", "warning", "Style", "none",
                "Line is longer than the configured limit.", lineNumber,
                options.maxLineLength + 1));
        }
        std::string lower = toLowerAscii(line);
        stringVector forbidden = { std::string("MAT") + "LAB", std::string("math") + "works" };
        for (const std::string& term : forbidden) {
            if (lower.find(toLowerAscii(term)) != std::string::npos) {
                appendDiagnostic(makeDiagnostic(path, "NLS0013", "error", "Repository", "none",
                    "Forbidden project term.", lineNumber, 1));
                break;
            }
        }
    }
    if (!content.empty() && content.back() != '\n') {
        appendFinalNewlineDiagnostic(path, lines, appendDiagnostic);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
