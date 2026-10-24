//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <iostream>
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <numeric>
#include <set>
#include <sstream>
#include <string>
#include "CodeAnalyzer.hpp"
#include "CodeAnalyzerConfig.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace {
//=============================================================================
void
usage()
{
    std::cerr << "usage: nelson-lint [--format text|json|sarif] [--config file] "
                 "[--include-subfolders true|false] [--fail-on error|warning|info|none] "
                 "[--deny warnings] [--quiet] [--output file] [--stdin] "
                 "[--stdin-filename name] [--fix|--fix-dry-run|--diff] <path>...\n";
}
//=============================================================================
bool
readBool(const std::string& value, bool& result)
{
    if (value == "true" || value == "1") {
        result = true;
        return true;
    }
    if (value == "false" || value == "0") {
        result = false;
        return true;
    }
    return false;
}
//=============================================================================
bool
readFile(const std::filesystem::path& path, std::string& content)
{
    std::ifstream stream(path, std::ios::binary);
    if (!stream.good()) {
        return false;
    }
    std::ostringstream buffer;
    buffer << stream.rdbuf();
    content = buffer.str();
    return true;
}
//=============================================================================
bool
writeFile(const std::filesystem::path& path, const std::string& content)
{
    std::ofstream stream(path, std::ios::binary | std::ios::trunc);
    if (!stream.good()) {
        return false;
    }
    stream << content;
    return stream.good();
}
//=============================================================================
int
severityRank(const std::string& severity)
{
    if (severity == "error") {
        return 3;
    }
    if (severity == "warning") {
        return 2;
    }
    if (severity == "info") {
        return 1;
    }
    return 0;
}
//=============================================================================
bool
isFailOnValue(const std::string& value)
{
    return value == "error" || value == "warning" || value == "info" || value == "none";
}
//=============================================================================
bool
shouldFail(
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics, const std::string& failOn)
{
    if (failOn == "none") {
        return false;
    }
    int threshold = severityRank(failOn);
    for (const auto& diagnostic : diagnostics) {
        if (!diagnostic.suppressed && severityRank(diagnostic.severity) >= threshold) {
            return true;
        }
    }
    return false;
}
//=============================================================================
std::vector<std::string>
splitContentLines(const std::string& content)
{
    std::vector<std::string> lines;
    size_t first = 0;
    while (first < content.size()) {
        size_t next = content.find('\n', first);
        std::string line = next == std::string::npos ? content.substr(first)
                                                     : content.substr(first, next - first);
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        lines.push_back(line);
        if (next == std::string::npos) {
            break;
        }
        first = next + 1;
    }
    return lines;
}
//=============================================================================
std::string
joinContentLines(const std::vector<std::string>& lines, bool finalNewline)
{
    std::string content;
    for (size_t k = 0; k < lines.size(); ++k) {
        if (k > 0) {
            content += '\n';
        }
        content += lines[k];
    }
    if (!content.empty() && finalNewline) {
        content += '\n';
    }
    return content;
}
//=============================================================================
void
collectSafeEditsForFile(const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics,
    const std::filesystem::path& file, std::vector<Nelson::CodeAnalyzerTextEdit>& edits)
{
    for (const auto& diagnostic : diagnostics) {
        if (diagnostic.suppressed || std::filesystem::path(diagnostic.file) != file) {
            continue;
        }
        for (const auto& fix : diagnostic.fixes) {
            if (!fix.safe) {
                continue;
            }
            edits.insert(edits.end(), fix.edits.begin(), fix.edits.end());
        }
    }
}
//=============================================================================
size_t
textOffset(const std::vector<std::string>& lines, int lineNumber, int column)
{
    size_t offset = 0;
    int beforeLine = std::max(0, std::min(lineNumber - 1, static_cast<int>(lines.size())));
    for (int k = 0; k < beforeLine; ++k) {
        offset += lines[static_cast<size_t>(k)].size() + 1;
    }
    if (lineNumber > 0 && lineNumber <= static_cast<int>(lines.size())) {
        const std::string& line = lines[static_cast<size_t>(lineNumber - 1)];
        offset += static_cast<size_t>(
            std::min(std::max(column - 1, 0), static_cast<int>(line.size())));
    }
    return offset;
}
//=============================================================================
std::vector<Nelson::CodeAnalyzerTextEdit>
selectNonConflictingEdits(
    const std::vector<Nelson::CodeAnalyzerTextEdit>& edits, const std::vector<std::string>& lines)
{
    std::vector<size_t> order(edits.size());
    std::iota(order.begin(), order.end(), 0);
    std::vector<size_t> starts(edits.size());
    std::vector<size_t> ends(edits.size());
    for (size_t k = 0; k < edits.size(); ++k) {
        starts[k] = textOffset(lines, edits[k].lineStart, edits[k].columnStart);
        ends[k] = textOffset(lines, edits[k].lineEnd, edits[k].columnEnd);
    }
    std::sort(order.begin(), order.end(), [&](size_t left, size_t right) {
        return (ends[left] - starts[left]) > (ends[right] - starts[right]);
    });
    std::vector<size_t> kept;
    for (size_t index : order) {
        bool overlaps = false;
        for (size_t other : kept) {
            if (starts[index] < ends[other] && starts[other] < ends[index]) {
                overlaps = true;
                break;
            }
        }
        if (!overlaps) {
            kept.push_back(index);
        }
    }
    std::sort(kept.begin(), kept.end(),
        [&](size_t left, size_t right) { return starts[left] > starts[right]; });
    std::vector<Nelson::CodeAnalyzerTextEdit> selected;
    for (size_t index : kept) {
        selected.push_back(edits[index]);
    }
    return selected;
}
//=============================================================================
std::string
fixedContentFor(const std::string& content,
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics,
    const std::filesystem::path& file)
{
    bool finalNewline = !content.empty() && content.back() == '\n';
    std::vector<std::string> lines = splitContentLines(content);
    std::vector<Nelson::CodeAnalyzerTextEdit> edits;
    collectSafeEditsForFile(diagnostics, file, edits);
    edits = selectNonConflictingEdits(edits, lines);
    std::string text = joinContentLines(lines, finalNewline);
    for (const auto& edit : edits) {
        const size_t start = textOffset(lines, edit.lineStart, edit.columnStart);
        const size_t end = textOffset(lines, edit.lineEnd, edit.columnEnd);
        if (end < start || start > text.size()) {
            continue;
        }
        finalNewline = finalNewline || edit.replacementText == "\n";
        std::string replacement = edit.replacementText == "\n" ? "" : edit.replacementText;
        text.replace(start, std::min(end, text.size()) - start, replacement);
    }
    if (finalNewline && (text.empty() || text.back() != '\n')) {
        text += '\n';
    }
    return text;
}
//=============================================================================
bool
printFixDiff(const std::set<std::filesystem::path>& files,
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics)
{
    for (const auto& file : files) {
        std::string content;
        if (!readFile(file, content)) {
            return false;
        }
        std::string fixed = fixedContentFor(content, diagnostics, file);
        if (fixed == content) {
            continue;
        }
        std::cout << "--- " << file.generic_string() << "\n";
        std::cout << "+++ " << file.generic_string() << "\n";
        std::cout << "@@ safe code analyzer fixes @@\n";
        std::cout << fixed;
    }
    return true;
}
//=============================================================================
std::set<std::filesystem::path>
fixableFiles(const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics)
{
    std::set<std::filesystem::path> files;
    for (const auto& diagnostic : diagnostics) {
        if (diagnostic.suppressed) {
            continue;
        }
        if (std::any_of(diagnostic.fixes.begin(), diagnostic.fixes.end(),
                [](const Nelson::CodeAnalyzerFix& fix) { return fix.safe; })) {
            files.insert(std::filesystem::path(diagnostic.file));
        }
    }
    return files;
}
//=============================================================================
bool
applyFixes(const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics)
{
    std::set<std::filesystem::path> files = fixableFiles(diagnostics);
    for (const auto& file : files) {
        std::string content;
        if (!readFile(file, content)) {
            return false;
        }
        std::string fixed = fixedContentFor(content, diagnostics, file);
        if (fixed != content && !writeFile(file, fixed)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
struct CliOptions
{
    Nelson::CodeAnalyzerOptions analyzerOptions;
    std::string format = "text";
    bool fix = false;
    bool fixDryRun = false;
    bool diff = false;
    bool quiet = false;
    bool useStdin = false;
    bool failOnExplicit = false;
    std::string stdinFilename = "stdin.m";
    std::string outputFile;
    Nelson::wstringVector names;
};
//=============================================================================
int
parseArguments(int argc, char* argv[], CliOptions& command)
{
    for (int k = 1; k < argc; ++k) {
        std::string arg = argv[k];
        if (arg == "--format") {
            if (++k >= argc) {
                usage();
                return 2;
            }
            command.format = argv[k];
            if (command.format != "text" && command.format != "json" && command.format != "sarif") {
                usage();
                return 2;
            }
        } else if (arg == "--config") {
            if (++k >= argc) {
                usage();
                return 2;
            }
            command.analyzerOptions.configFile = Nelson::utf8_to_wstring(argv[k]);
        } else if (arg == "--include-subfolders") {
            if (++k >= argc || !readBool(argv[k], command.analyzerOptions.includeSubfolders)) {
                usage();
                return 2;
            }
        } else if (arg == "--deny") {
            if (++k >= argc || std::string(argv[k]) != "warnings") {
                usage();
                return 2;
            }
            command.analyzerOptions.failOn = "warning";
            command.failOnExplicit = true;
        } else if (arg == "--fail-on") {
            if (++k >= argc || !isFailOnValue(argv[k])) {
                usage();
                return 2;
            }
            command.analyzerOptions.failOn = argv[k];
            command.failOnExplicit = true;
        } else if (arg == "--quiet") {
            command.quiet = true;
        } else if (arg == "--output") {
            if (++k >= argc) {
                usage();
                return 2;
            }
            command.outputFile = argv[k];
        } else if (arg == "--stdin") {
            command.useStdin = true;
        } else if (arg == "--stdin-filename") {
            if (++k >= argc) {
                usage();
                return 2;
            }
            command.stdinFilename = argv[k];
        } else if (arg == "--fix") {
            command.fix = true;
        } else if (arg == "--fix-dry-run") {
            command.fixDryRun = true;
        } else if (arg == "--diff") {
            command.diff = true;
        } else if (!arg.empty() && arg[0] == '-') {
            usage();
            return 2;
        } else {
            command.names.push_back(Nelson::utf8_to_wstring(arg));
        }
    }
    if (command.useStdin
        && (!command.names.empty() || command.fix || command.fixDryRun || command.diff)) {
        usage();
        return 2;
    }
    if (!command.useStdin && command.names.empty()) {
        usage();
        return 2;
    }
    for (const std::wstring& name : command.names) {
        if (!std::filesystem::exists(std::filesystem::path(name))) {
            std::cerr << "path does not exist: " << Nelson::wstring_to_utf8(name) << "\n";
            return 2;
        }
    }
    return 0;
}
//=============================================================================
} // namespace
//=============================================================================
int
main(int argc, char* argv[])
{
    CliOptions command;
    command.analyzerOptions.includeCyclomatic = true;
    command.analyzerOptions.includeModifiedCyclomatic = true;
    int parseStatus = parseArguments(argc, argv, command);
    if (parseStatus != 0) {
        return parseStatus;
    }
    try {
        Nelson::CodeAnalyzerOptions resolvedOptions = command.analyzerOptions;
        Nelson::readCodeAnalyzerConfig(resolvedOptions);
        if (command.failOnExplicit) {
            resolvedOptions.failOn = command.analyzerOptions.failOn;
        }
        if (resolvedOptions.configError.empty()) {
            resolvedOptions.configFile.clear();
        }
        std::vector<Nelson::CodeAnalyzerDiagnostic> diagnostics;
        if (command.useStdin) {
            std::ostringstream buffer;
            buffer << std::cin.rdbuf();
            diagnostics = Nelson::analyzeCodeBuffer(
                Nelson::utf8_to_wstring(command.stdinFilename), buffer.str(), resolvedOptions);
        } else {
            diagnostics = Nelson::analyzeCode(command.names, resolvedOptions);
        }
        if (command.diff || command.fixDryRun) {
            if (!printFixDiff(fixableFiles(diagnostics), diagnostics)) {
                std::cerr << "failed to compute fixes\n";
                return 2;
            }
        }
        if (command.fix) {
            if (!applyFixes(diagnostics)) {
                std::cerr << "failed to apply fixes\n";
                return 2;
            }
            diagnostics = Nelson::analyzeCode(command.names, resolvedOptions);
        }
        std::string output;
        if (command.format == "json") {
            output = Nelson::codeAnalyzerDiagnosticsToJson(diagnostics) + "\n";
        } else if (command.format == "sarif") {
            output = Nelson::codeAnalyzerDiagnosticsToSarif(diagnostics) + "\n";
        } else {
            output = Nelson::codeAnalyzerDiagnosticsToText(diagnostics);
        }
        if (!command.outputFile.empty()) {
            if (!writeFile(command.outputFile, output)) {
                std::cerr << "failed to write output\n";
                return 2;
            }
        } else if (!command.quiet) {
            std::cout << output;
        }
        return shouldFail(diagnostics, resolvedOptions.failOn) ? 1 : 0;
    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 2;
    } catch (...) {
        std::cerr << "internal error\n";
        return 2;
    }
}
//=============================================================================
