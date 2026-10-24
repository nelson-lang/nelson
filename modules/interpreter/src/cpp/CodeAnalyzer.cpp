//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
#include "CodeAnalyzer.hpp"
#include <algorithm>
#include <cctype>
#include <cwctype>
#include <filesystem>
#include <functional>
#include <fstream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include "ArrayOf.hpp"
#include "CodeAnalyzerCache.hpp"
#include "CodeAnalyzerConfig.hpp"
#include "CodeAnalyzerDataflow.hpp"
#include "CodeAnalyzerRules.hpp"
#include "CodeAnalyzerSuppressions.hpp"
#include "CodeAnalyzerSyntaxRules.hpp"
#include "CodeAnalyzerTextRules.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    struct SourceFile
    {
        fs::path path;
        std::string content;
        stringVector lines;
    };
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
    toLowerAscii(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return value;
    }
    //=============================================================================
    std::string
    toUpperAscii(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
        return value;
    }
    //=============================================================================
    bool
    readSourceFile(const fs::path& path, SourceFile& source)
    {
        std::ifstream stream(path, std::ios::binary);
        if (!stream.good()) {
            return false;
        }
        std::ostringstream buffer;
        buffer << stream.rdbuf();
        source.path = path;
        source.content = buffer.str();
        size_t first = 0;
        while (first < source.content.size()) {
            size_t next = source.content.find('\n', first);
            std::string line = next == std::string::npos
                ? source.content.substr(first)
                : source.content.substr(first, next - first);
            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }
            source.lines.push_back(line);
            if (next == std::string::npos) {
                break;
            }
            first = next + 1;
        }
        if (source.content.empty()) {
            source.lines.clear();
        }
        return true;
    }
    //=============================================================================
    SourceFile
    sourceFromContent(const fs::path& path, const std::string& content)
    {
        SourceFile source;
        source.path = path;
        source.content = content;
        size_t first = 0;
        while (first < source.content.size()) {
            size_t next = source.content.find('\n', first);
            std::string line = next == std::string::npos
                ? source.content.substr(first)
                : source.content.substr(first, next - first);
            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }
            source.lines.push_back(line);
            if (next == std::string::npos) {
                break;
            }
            first = next + 1;
        }
        return source;
    }
    //=============================================================================
    bool
    isGeneratedOrVendoredPath(const fs::path& path, const CodeAnalyzerOptions& options)
    {
        static const std::set<std::string> ignored = { ".git", "build", "target", "bin", "x64",
            "node_modules", "thirdparty", "extern", "external", "generated" };
        for (const auto& part : path) {
            const std::string lower = toLowerAscii(part.generic_string());
            if (ignored.find(lower) != ignored.end()) {
                return true;
            }
        }
        const std::wstring generic = path.generic_wstring();
        for (const std::wstring& exclusion : options.exclusions) {
            if (!exclusion.empty() && generic.find(exclusion) != std::wstring::npos) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    matchesIncludePattern(const fs::path& path, const std::wstring& pattern)
    {
        if (pattern.empty()) {
            return false;
        }
        std::wstring lowerPattern = pattern;
        std::transform(lowerPattern.begin(), lowerPattern.end(), lowerPattern.begin(),
            [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
        const std::wstring generic = path.generic_wstring();
        std::wstring lowerPath = generic;
        std::transform(lowerPath.begin(), lowerPath.end(), lowerPath.begin(),
            [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
        if (lowerPattern.rfind(L"**/*.", 0) == 0 || lowerPattern.rfind(L"*.", 0) == 0) {
            std::wstring extension = lowerPattern.substr(lowerPattern.find(L'.'));
            std::wstring pathExtension = path.extension().generic_wstring();
            std::transform(pathExtension.begin(), pathExtension.end(), pathExtension.begin(),
                [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
            return pathExtension == extension;
        }
        return lowerPath.find(lowerPattern) != std::wstring::npos;
    }
    //=============================================================================
    bool
    isIncludedPath(const fs::path& path, const CodeAnalyzerOptions& options)
    {
        if (options.inclusions.empty()) {
            std::string ext = toLowerAscii(path.extension().generic_string());
            return ext == ".m" || ext == ".xml";
        }
        for (const std::wstring& inclusion : options.inclusions) {
            if (matchesIncludePattern(path, inclusion)) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    void
    collectFiles(
        const fs::path& path, const CodeAnalyzerOptions& options, std::vector<fs::path>& files)
    {
        if (!fs::exists(path) || isGeneratedOrVendoredPath(path, options)) {
            return;
        }
        if (fs::is_regular_file(path)) {
            if (isIncludedPath(path, options)) {
                files.push_back(path);
            }
            return;
        }
        if (!fs::is_directory(path)) {
            return;
        }
        if (options.includeSubfolders) {
            fs::recursive_directory_iterator iterator(
                path, fs::directory_options::skip_permission_denied);
            for (const auto& entry : iterator) {
                if (entry.is_directory() && isGeneratedOrVendoredPath(entry.path(), options)) {
                    iterator.disable_recursion_pending();
                    continue;
                }
                if (entry.is_regular_file()) {
                    collectFiles(entry.path(), options, files);
                }
            }
        } else {
            for (const auto& entry : fs::directory_iterator(path)) {
                collectFiles(entry.path(), options, files);
            }
        }
    }
    //=============================================================================
    std::string
    severityFor(
        const CodeAnalyzerOptions& options, const std::string& checkId, std::string fallback)
    {
        auto found = options.ruleSeverity.find(checkId);
        if (found == options.ruleSeverity.end()) {
            return fallback;
        }
        return found->second;
    }
    //=============================================================================
    std::string
    fingerprintFor(const CodeAnalyzerDiagnostic& diagnostic)
    {
        std::string value = wstring_to_utf8(diagnostic.file) + "|" + diagnostic.checkId + "|"
            + std::to_string(diagnostic.lineStart) + "|" + std::to_string(diagnostic.columnStart)
            + "|" + diagnostic.message;
        return std::to_string(std::hash<std::string> {}(value));
    }
    //=============================================================================
    void
    appendDiagnostic(std::vector<CodeAnalyzerDiagnostic>& diagnostics,
        const CodeAnalyzerOptions& options, const CodeAnalyzerSuppressions& suppressions,
        CodeAnalyzerDiagnostic diagnostic)
    {
        diagnostic.checkId = toUpperAscii(diagnostic.checkId);
        if (const auto* rule = codeAnalyzerRuleInfo(diagnostic.checkId)) {
            if (diagnostic.ruleName.empty()) {
                diagnostic.ruleName = rule->name;
            }
            if (diagnostic.category.empty()) {
                diagnostic.category = rule->category;
            }
            if (diagnostic.fixability.empty()) {
                diagnostic.fixability = rule->fixability;
            }
            if (diagnostic.helpUri.empty()) {
                diagnostic.helpUri = rule->helpUri;
            }
        }
        diagnostic.severity = severityFor(options, diagnostic.checkId, diagnostic.severity);
        if (diagnostic.severity == "allow") {
            return;
        }
        if (diagnostic.fingerprint.empty()) {
            diagnostic.fingerprint = fingerprintFor(diagnostic);
        }
        diagnostic.suppressed = isCodeAnalyzerDiagnosticSuppressed(
            suppressions, diagnostic.lineStart, diagnostic.checkId);
        if (!diagnostic.suppressed || options.includeSuppressed) {
            diagnostics.push_back(std::move(diagnostic));
        }
    }
    //=============================================================================
    CodeAnalyzerDiagnostic
    makeDiagnostic(const fs::path& path, const std::string& id, const std::string& severity,
        const std::string& category, const std::string& fixability, const std::string& message,
        int line, int column = 1)
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
    void
    applySizeRules(const SourceFile& source, const CodeAnalyzerOptions& options,
        const CodeAnalyzerSuppressions& suppressions,
        std::vector<CodeAnalyzerDiagnostic>& diagnostics)
    {
        if (options.maxFileLines > 0
            && static_cast<int>(source.lines.size()) > options.maxFileLines) {
            appendDiagnostic(diagnostics, options, suppressions,
                makeDiagnostic(source.path, "NLS0020", "warning", "Metrics", "none",
                    "File is longer than the configured maximum number of lines.", 1, 1));
        }
    }
    //=============================================================================
    void
    analyzeSource(SourceFile& source, bool parseFromText, const CodeAnalyzerOptions& options,
        std::vector<CodeAnalyzerDiagnostic>& diagnostics)
    {
        auto suppressions = collectCodeAnalyzerSuppressions(source.lines);
        applyCodeAnalyzerTextRules(source.path, source.lines, source.content, options,
            [&](CodeAnalyzerDiagnostic diagnostic) {
                appendDiagnostic(diagnostics, options, suppressions, std::move(diagnostic));
            });
        if (toLowerAscii(source.path.extension().generic_string()) != ".m") {
            return;
        }
        ParserContext parserContext = applyCodeAnalyzerSyntaxRules(
            source.path, source.content, parseFromText, [&](CodeAnalyzerDiagnostic diagnostic) {
                appendDiagnostic(diagnostics, options, suppressions, std::move(diagnostic));
            });
        if (parserContext.state() == ParseError) {
            return;
        }
        applySizeRules(source, options, suppressions, diagnostics);
        applyCodeAnalyzerDataflowRules(source.path, source.lines, parserContext, options,
            [&](CodeAnalyzerDiagnostic diagnostic) {
                appendDiagnostic(diagnostics, options, suppressions, std::move(diagnostic));
            });
    }
    //=============================================================================
} // namespace
//=============================================================================
std::vector<CodeAnalyzerDiagnostic>
analyzeCode(const wstringVector& names, const CodeAnalyzerOptions& rawOptions)
{
    CodeAnalyzerOptions options = rawOptions;
    readCodeAnalyzerConfig(options);
    std::vector<fs::path> files;
    std::vector<CodeAnalyzerDiagnostic> diagnostics;
    CodeAnalyzerSuppressions emptySuppressions;
    if (!options.configError.empty()) {
        fs::path configPath(options.configFile);
        appendDiagnostic(diagnostics, options, emptySuppressions,
            makeDiagnostic(
                configPath, "NLS0019", "error", "Config", "none", options.configError, 1, 1));
        return diagnostics;
    }
    for (const std::wstring& name : names) {
        fs::path path(name);
        if (!fs::exists(path)) {
            appendDiagnostic(diagnostics, options, emptySuppressions,
                makeDiagnostic(
                    path, "NLS0001", "error", "Input", "none", "Path does not exist.", 1, 1));
            continue;
        }
        collectFiles(path, options, files);
    }
    if (options.fullPath) {
        for (fs::path& file : files) {
            file = fs::absolute(file);
        }
    }
    std::sort(files.begin(), files.end());
    files.erase(std::unique(files.begin(), files.end()), files.end());

    for (const fs::path& file : files) {
        std::vector<CodeAnalyzerDiagnostic> fileDiagnostics;
        if (getCachedCodeAnalysis(file, options, fileDiagnostics)) {
            diagnostics.insert(diagnostics.end(), fileDiagnostics.begin(), fileDiagnostics.end());
            continue;
        }
        SourceFile source;
        if (!readSourceFile(file, source)) {
            appendDiagnostic(diagnostics, options, emptySuppressions,
                makeDiagnostic(
                    file, "NLS0001", "error", "Input", "none", "File cannot be read.", 1, 1));
            continue;
        }
        analyzeSource(source, false, options, fileDiagnostics);
        storeCachedCodeAnalysis(file, options, fileDiagnostics);
        diagnostics.insert(diagnostics.end(), fileDiagnostics.begin(), fileDiagnostics.end());
    }
    return diagnostics;
}
//=============================================================================
std::vector<CodeAnalyzerDiagnostic>
analyzeCodeBuffer(const std::wstring& displayFilename, const std::string& sourceText,
    const CodeAnalyzerOptions& rawOptions)
{
    CodeAnalyzerOptions options = rawOptions;
    readCodeAnalyzerConfig(options);
    std::vector<CodeAnalyzerDiagnostic> diagnostics;
    CodeAnalyzerSuppressions emptySuppressions;
    if (!options.configError.empty()) {
        fs::path configPath
            = options.configFile.empty() ? fs::path(displayFilename) : fs::path(options.configFile);
        appendDiagnostic(diagnostics, options, emptySuppressions,
            makeDiagnostic(
                configPath, "NLS0019", "error", "Config", "none", options.configError, 1, 1));
        return diagnostics;
    }
    fs::path displayPath(displayFilename);
    if (isGeneratedOrVendoredPath(displayPath, options) || !isIncludedPath(displayPath, options)) {
        return diagnostics;
    }
    SourceFile source = sourceFromContent(displayPath, sourceText);
    analyzeSource(source, true, options, diagnostics);
    return diagnostics;
}
//=============================================================================
wstringVector
codeAnalyzerIncludedFiles(const wstringVector& names, const CodeAnalyzerOptions& rawOptions)
{
    CodeAnalyzerOptions options = rawOptions;
    readCodeAnalyzerConfig(options);
    wstringVector result;
    if (!options.configError.empty()) {
        return result;
    }
    std::vector<fs::path> files;
    for (const std::wstring& name : names) {
        fs::path path(name);
        if (fs::exists(path)) {
            collectFiles(path, options, files);
        }
    }
    if (options.fullPath) {
        for (fs::path& file : files) {
            file = fs::absolute(file);
        }
    }
    std::sort(files.begin(), files.end());
    files.erase(std::unique(files.begin(), files.end()), files.end());
    for (const fs::path& file : files) {
        result.push_back(file.wstring());
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
