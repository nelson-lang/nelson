//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerDataflowFixes.hpp"
#include "CodeAnalyzerAstUtilities.hpp"
#include "CodeAnalyzerDataflowImports.hpp"
#include <cctype>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    CodeAnalyzerFix
    makeSingleEditFix(const std::string& title, int lineStart, int columnStart, int lineEnd,
        int columnEnd, const std::string& replacementText, bool safe = true)
    {
        CodeAnalyzerTextEdit edit;
        edit.lineStart = lineStart;
        edit.lineEnd = lineEnd;
        edit.columnStart = columnStart;
        edit.columnEnd = columnEnd;
        edit.replacementText = replacementText;
        CodeAnalyzerFix fix;
        fix.title = title;
        fix.safe = safe;
        fix.edits.push_back(edit);
        return fix;
    }
    //=============================================================================
    bool
    isContinuedLine(const std::string& line)
    {
        const std::string code = CodeAnalyzerAst::trim(CodeAnalyzerAst::stripComment(line));
        return code.size() >= 3 && code.compare(code.size() - 3, 3, "...") == 0;
    }
    //=============================================================================
    size_t
    skipSpaces(const std::string& value, size_t cursor)
    {
        while (cursor < value.size() && std::isspace(static_cast<unsigned char>(value[cursor]))) {
            ++cursor;
        }
        return cursor;
    }
    //=============================================================================
    size_t
    matchingParen(const std::string& line, size_t open)
    {
        int depth = 0;
        bool inString = false;
        for (size_t k = open; k < line.size(); ++k) {
            if (line[k] == '\'') {
                if (inString && k + 1 < line.size() && line[k + 1] == '\'') {
                    ++k;
                    continue;
                }
                inString = !inString;
                continue;
            }
            if (inString) {
                continue;
            }
            if (line[k] == '(') {
                ++depth;
            } else if (line[k] == ')') {
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
    diagnosticSprintfReplacement(const std::string& line, int outerColumn, int& columnStart,
        int& columnEnd, std::string& replacement)
    {
        if (outerColumn <= 0 || static_cast<size_t>(outerColumn - 1) >= line.size()) {
            return false;
        }
        size_t cursor = line.find('(', static_cast<size_t>(outerColumn - 1));
        if (cursor == std::string::npos) {
            return false;
        }
        cursor = skipSpaces(line, cursor + 1);
        const std::string token = "sprintf";
        if (cursor + token.size() > line.size() || line.compare(cursor, token.size(), token) != 0) {
            return false;
        }
        size_t innerOpen = skipSpaces(line, cursor + token.size());
        if (innerOpen >= line.size() || line[innerOpen] != '(') {
            return false;
        }
        size_t innerClose = matchingParen(line, innerOpen);
        if (innerClose == std::string::npos || innerClose <= innerOpen) {
            return false;
        }
        columnStart = static_cast<int>(cursor + 1);
        columnEnd = static_cast<int>(innerClose + 2);
        replacement = line.substr(innerOpen + 1, innerClose - innerOpen - 1);
        return true;
    }
    //=============================================================================
    size_t
    closingQuote(const std::string& value, size_t openQuote)
    {
        for (size_t k = openQuote + 1; k < value.size(); ++k) {
            if (value[k] != '\'') {
                continue;
            }
            if (k + 1 < value.size() && value[k + 1] == '\'') {
                ++k;
                continue;
            }
            return k;
        }
        return std::string::npos;
    }
    //=============================================================================
    bool
    displaySprintfReplacement(const std::string& line, int outerColumn, int& columnStart,
        int& columnEnd, std::string& replacement)
    {
        if (outerColumn <= 0 || static_cast<size_t>(outerColumn - 1) >= line.size()) {
            return false;
        }
        size_t cursor = static_cast<size_t>(outerColumn - 1);
        std::string outer = "disp";
        if (cursor + outer.size() > line.size() || line.compare(cursor, outer.size(), outer) != 0) {
            outer = "display";
        }
        if (cursor + outer.size() > line.size() || line.compare(cursor, outer.size(), outer) != 0) {
            return false;
        }
        size_t outerOpen = skipSpaces(line, cursor + outer.size());
        if (outerOpen >= line.size() || line[outerOpen] != '(') {
            return false;
        }
        size_t inner = skipSpaces(line, outerOpen + 1);
        const std::string token = "sprintf";
        if (inner + token.size() > line.size() || line.compare(inner, token.size(), token) != 0) {
            return false;
        }
        size_t innerOpen = skipSpaces(line, inner + token.size());
        if (innerOpen >= line.size() || line[innerOpen] != '(') {
            return false;
        }
        size_t innerClose = matchingParen(line, innerOpen);
        size_t outerClose = matchingParen(line, outerOpen);
        if (innerClose == std::string::npos || outerClose == std::string::npos) {
            return false;
        }
        if (skipSpaces(line, innerClose + 1) != outerClose) {
            return false;
        }
        std::string args = line.substr(innerOpen + 1, innerClose - innerOpen - 1);
        size_t literal = skipSpaces(args, 0);
        if (literal >= args.size() || args[literal] != '\'') {
            return false;
        }
        size_t literalEnd = closingQuote(args, literal);
        if (literalEnd == std::string::npos) {
            return false;
        }
        args.insert(literalEnd, "\\n");
        columnStart = static_cast<int>(cursor + 1);
        columnEnd = static_cast<int>(outerClose + 2);
        replacement = "fprintf(" + args + ")";
        return true;
    }
    //=============================================================================
    bool
    hasAssignmentOperator(const std::string& line, size_t& equals)
    {
        equals = line.find('=');
        if (equals == std::string::npos) {
            return false;
        }
        return !((equals > 0
                     && (line[equals - 1] == '<' || line[equals - 1] == '>'
                         || line[equals - 1] == '~' || line[equals - 1] == '='))
            || (equals + 1 < line.size() && line[equals + 1] == '='));
    }
    //=============================================================================
    bool
    containsCallableExpression(const std::string& code)
    {
        for (size_t k = 0; k < code.size();) {
            if (!CodeAnalyzerAst::isIdentifierStart(code[k])) {
                ++k;
                continue;
            }
            ++k;
            while (k < code.size() && CodeAnalyzerAst::isIdentifierChar(code[k])) {
                ++k;
            }
            size_t cursor = CodeAnalyzerAst::skipSpaces(code, k);
            if (cursor < code.size() && code[cursor] == '(') {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    unusedAssignmentCanBeRemoved(const std::string& line, const std::string& symbol)
    {
        std::string code = CodeAnalyzerAst::stripComment(line);
        if (CodeAnalyzerAst::trim(code) != CodeAnalyzerAst::trim(line)) {
            return false;
        }
        size_t equals = 0;
        if (!hasAssignmentOperator(code, equals)) {
            return false;
        }
        if (CodeAnalyzerAst::trim(code.substr(0, equals)) != symbol) {
            return false;
        }
        std::string rhs = CodeAnalyzerAst::trim(
            CodeAnalyzerAst::codeWithoutStringsAndComments(code.substr(equals + 1)));
        if (rhs.empty() || containsCallableExpression(rhs)) {
            return false;
        }
        return true;
    }
    //=============================================================================
    std::string
    leadingWhitespace(const std::string& line)
    {
        size_t cursor = 0;
        while (cursor < line.size() && (line[cursor] == ' ' || line[cursor] == '\t')) {
            ++cursor;
        }
        return line.substr(0, cursor);
    }
    //=============================================================================
    bool
    simpleLoadCommandReplacement(const std::string& line, std::string& replacement)
    {
        std::string code = CodeAnalyzerAst::stripComment(line);
        if (CodeAnalyzerAst::trim(code) != CodeAnalyzerAst::trim(line)) {
            return false;
        }
        std::string trimmed = CodeAnalyzerAst::trim(code);
        if (!CodeAnalyzerAst::startsWithWord(CodeAnalyzerAst::toLowerAscii(trimmed), "load")) {
            return false;
        }
        std::string filename = CodeAnalyzerAst::trim(trimmed.substr(4));
        if (filename.empty() || filename[0] == '(' || filename[0] == '-'
            || filename.find_first_of(" \t,;") != std::string::npos) {
            return false;
        }
        std::string escaped;
        for (char c : filename) {
            escaped.push_back(c);
            if (c == '\'') {
                escaped.push_back('\'');
            }
        }
        replacement = leadingWhitespace(line) + "loadedData = load('" + escaped + "');\n";
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
addDiagnosticSprintfFix(CodeAnalyzerDiagnostic& diagnostic, const std::string& line)
{
    int columnStart = 0;
    int columnEnd = 0;
    std::string replacement;
    if (diagnosticSprintfReplacement(
            line, diagnostic.columnStart, columnStart, columnEnd, replacement)) {
        diagnostic.fixability = "safe";
        diagnostic.columnEnd = columnEnd;
        diagnostic.fixes.push_back(makeSingleEditFix("Pass formatted arguments directly",
            diagnostic.lineStart, columnStart, diagnostic.lineStart, columnEnd, replacement));
    }
}
//=============================================================================
void
addDisplaySprintfFix(CodeAnalyzerDiagnostic& diagnostic, const std::string& line)
{
    int columnStart = 0;
    int columnEnd = 0;
    std::string replacement;
    if (displaySprintfReplacement(
            line, diagnostic.columnStart, columnStart, columnEnd, replacement)) {
        diagnostic.fixability = "safe";
        diagnostic.columnEnd = columnEnd;
        diagnostic.fixes.push_back(makeSingleEditFix("Use fprintf directly", diagnostic.lineStart,
            columnStart, diagnostic.lineStart, columnEnd, replacement));
    }
}
//=============================================================================
void
addUnreachableCodeFix(CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines,
    const CodeAnalyzerLineUsage& usage)
{
    const int lineNumber = diagnostic.lineStart;
    if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
        return;
    }
    const std::string& line = lines[static_cast<size_t>(lineNumber - 1)];
    if (CodeAnalyzerAst::trim(CodeAnalyzerAst::stripComment(line)) != CodeAnalyzerAst::trim(line)) {
        return;
    }
    // Removing a whole line is only ever safe when that line holds a complete simple
    // statement. Block openers, block terminators, declarations and continued statements
    // would leave the surrounding code unbalanced, so they get a diagnostic without a fix.
    if (usage.flowBlockStart || usage.flowBoundary || usage.endsBlock || usage.functionDeclaration
        || usage.startsArgumentsBlock) {
        return;
    }
    if (isContinuedLine(line)
        || (lineNumber > 1 && isContinuedLine(lines[static_cast<size_t>(lineNumber - 2)]))) {
        return;
    }
    diagnostic.fixability = "safe";
    diagnostic.fixes.push_back(
        makeSingleEditFix("Remove unreachable code", lineNumber, 1, lineNumber + 1, 1, ""));
}
//=============================================================================
void
addUnusedImportFix(
    CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines, const std::string& symbol)
{
    const int lineNumber = diagnostic.lineStart;
    if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
        return;
    }
    if (isSimpleCodeAnalyzerImportLineFor(lines[lineNumber - 1], symbol)) {
        diagnostic.fixability = "safe";
        diagnostic.fixes.push_back(
            makeSingleEditFix("Remove unused import", lineNumber, 1, lineNumber + 1, 1, ""));
        return;
    }
    int columnStart = 0;
    int columnEnd = 0;
    std::string replacementText;
    if (codeAnalyzerImportRemovalEditFor(
            lines[lineNumber - 1], symbol, columnStart, columnEnd, replacementText)) {
        diagnostic.fixability = "safe";
        diagnostic.fixes.push_back(makeSingleEditFix("Remove unused import", lineNumber,
            columnStart, lineNumber, columnEnd, replacementText));
        return;
    }
    int lineStart = 0;
    int lineEnd = 0;
    if (codeAnalyzerImportRemovalEditFor(lines, lineNumber, symbol, lineStart, columnStart, lineEnd,
            columnEnd, replacementText)) {
        diagnostic.fixability = "safe";
        diagnostic.fixes.push_back(makeSingleEditFix(
            "Remove unused import", lineStart, columnStart, lineEnd, columnEnd, replacementText));
    }
}
//=============================================================================
void
addUnusedAssignmentFix(
    CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines, const std::string& symbol)
{
    const int lineNumber = diagnostic.lineStart;
    if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
        return;
    }
    if (!unusedAssignmentCanBeRemoved(lines[lineNumber - 1], symbol)) {
        return;
    }
    diagnostic.fixability = "safe";
    diagnostic.fixes.push_back(
        makeSingleEditFix("Remove unused assignment", lineNumber, 1, lineNumber + 1, 1, ""));
}
//=============================================================================
void
addDynamicLoadCaptureFix(CodeAnalyzerDiagnostic& diagnostic, const stringVector& lines)
{
    const int lineNumber = diagnostic.lineStart;
    if (diagnostic.checkId != "NLS0022" || lineNumber <= 0
        || lineNumber > static_cast<int>(lines.size())) {
        return;
    }
    std::string replacement;
    if (!simpleLoadCommandReplacement(lines[static_cast<size_t>(lineNumber - 1)], replacement)) {
        return;
    }
    diagnostic.fixability = "unsafe";
    diagnostic.fixes.push_back(makeSingleEditFix(
        "Capture load output in a struct", lineNumber, 1, lineNumber + 1, 1, replacement, false));
}
//=============================================================================
void
addClassdefNameMismatchFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerClassdef& classdef,
    const std::string& expectedName)
{
    if (classdef.line <= 0 || classdef.column <= 0 || classdef.name.empty()
        || expectedName.empty()) {
        return;
    }
    diagnostic.fixability = "safe";
    diagnostic.fixes.push_back(
        makeSingleEditFix("Rename class to match file name", classdef.line, classdef.column,
            classdef.line, classdef.column + static_cast<int>(classdef.name.size()), expectedName));
}
//=============================================================================
void
addFunctionNameMismatchFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerFunction& function,
    const std::string& expectedName)
{
    if (function.line <= 0 || function.column <= 0 || function.name.empty()
        || expectedName.empty()) {
        return;
    }
    diagnostic.fixability = "safe";
    diagnostic.fixes.push_back(
        makeSingleEditFix("Rename function to match file name", function.line, function.column,
            function.line, function.column + static_cast<int>(function.name.size()), expectedName));
}
//=============================================================================
void
addUnusedLocalFunctionFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerFunction& function)
{
    if (function.line <= 0 || function.endLine < function.line) {
        return;
    }
    diagnostic.fixability = "safe";
    diagnostic.fixes.push_back(makeSingleEditFix(
        "Remove unused local function", function.line, 1, function.endLine + 1, 1, ""));
}
//=============================================================================
void
addMissingFunctionHelpFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerFunction& function,
    const stringVector& lines)
{
    const int lineNumber = function.line + 1;
    if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
        return;
    }
    diagnostic.fixability = "safe";
    diagnostic.fixes.push_back(makeSingleEditFix("Insert help comment", lineNumber, 1, lineNumber,
        1, "% " + function.name + " - Add help text.\n"));
}
//=============================================================================
void
addMissingClassdefHelpFix(CodeAnalyzerDiagnostic& diagnostic, const CodeAnalyzerClassdef& classdef,
    const stringVector& lines)
{
    const int lineNumber = classdef.line + 1;
    if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
        return;
    }
    diagnostic.fixability = "safe";
    diagnostic.fixes.push_back(makeSingleEditFix("Insert help comment", lineNumber, 1, lineNumber,
        1, "% " + classdef.name + " - Add help text.\n"));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
