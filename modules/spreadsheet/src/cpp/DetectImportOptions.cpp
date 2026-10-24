//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <map>
#include <regex>
#include <algorithm>
#include <cctype>
#include "DetectImportOptions.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include <fast_float/fast_float.h>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    const stringVector DELIMITERS = { ",", "\t", ";", "|", " " };
    const stringVector LINE_ENDINGS = { "\r\n", "\n", "\r" };
    const stringVector COMMENT_STYLES = { "%", "#", "//", "--" };
    const size_t MIN_HEADER_SAMPLE = 5;
    //=============================================================================
} // namespace
//=============================================================================
static std::string
escapeSpecialCharacters(const std::string& input)
{
    static const std::map<char, std::string> escapeMap
        = { { '\n', "\\n" }, { '\r', "\\r" }, { '\t', "\\t" }, { '\\', "\\\\" } };

    std::string escaped;
    escaped.reserve(input.length());

    for (char c : input) {
        auto it = escapeMap.find(c);
        if (it != escapeMap.end()) {
            escaped += it->second;
        } else {
            escaped += c;
        }
    }
    return escaped;
}
//=============================================================================
static std::string
unescapeSpecialCharacters(const std::string& input)
{
    static const std::map<std::string, char> unescapeMap
        = { { "\\n", '\n' }, { "\\r", '\r' }, { "\\t", '\t' }, { "\\\\", '\\' } };

    std::string unescaped;
    unescaped.reserve(input.length());
    size_t i = 0;

    while (i < input.length()) {
        if (input[i] == '\\' && i + 1 < input.length()) {
            std::string potentialEscape = input.substr(i, 2);
            auto it = unescapeMap.find(potentialEscape);
            if (it != unescapeMap.end()) {
                unescaped += it->second;
                i += 2;
                continue;
            }
        }
        unescaped += input[i];
        ++i;
    }

    return unescaped;
}
//=============================================================================
static std::string
readFileContent(const std::wstring& filename, size_t sampleSize, std::string& errorMessage)
{
#ifdef _MSC_VER
    std::wifstream file(filename, std::ios::binary);
#else
    std::ifstream file(wstring_to_utf8(filename), std::ios::binary);
#endif

    if (!file.is_open()) {
        errorMessage = _("Unable to open file.");
        return "";
    }

#ifdef _MSC_VER
    std::wstring wcontent(sampleSize, L'\0');
    file.read(&wcontent[0], sampleSize);
    size_t actualSize = file.gcount();
    wcontent.resize(actualSize);
    // Read on to the end of the current line so detection never sees a truncated
    // line (the detected column count would disagree with the data and throw).
    if (actualSize == sampleSize) {
        std::wstring rest;
        if (std::getline(file, rest)) {
            wcontent += rest;
        }
    }
    return wstring_to_utf8(wcontent);
#else
    std::string content(sampleSize, '\0');
    file.read(&content[0], sampleSize);
    size_t actualSize = file.gcount();
    content.resize(actualSize);
    if (actualSize == sampleSize) {
        std::string rest;
        if (std::getline(file, rest)) {
            content += rest;
        }
    }
    return content;
#endif
}
//=============================================================================
static std::vector<std::string>
splitIntoLines(const std::string& content, const std::string& lineEnding)
{
    std::vector<std::string> lines;
    size_t start = 0;
    size_t end = content.find(lineEnding);

    while (end != std::string::npos) {
        lines.push_back(content.substr(start, end - start));
        start = end + lineEnding.length();
        end = content.find(lineEnding, start);
    }

    lines.push_back(content.substr(start));

    return lines;
}
//=============================================================================
static std::vector<std::string>
splitLine(const std::string& line, const std::string& delimiter)
{
    std::vector<std::string> tokens;
    size_t start = 0;
    size_t end = line.find(delimiter);

    while (end != std::string::npos) {
        std::string token = line.substr(start, end - start);
        tokens.push_back(token);
        start = end + delimiter.length();
        end = line.find(delimiter, start);
    }

    tokens.push_back(line.substr(start));
    return tokens;
}
//=============================================================================
struct DelimiterStats
{
    std::string delimiter;
    double averageCount;
    size_t consistentLines;
    size_t totalCount;
};
//=============================================================================
DelimiterStats
analyzeDelimiterInLine(const std::string& line, const std::string& delimiter)
{
    DelimiterStats stats { delimiter, 0.0, 0, 0 };

    if (line.empty()) {
        return stats;
    }

    size_t count = 0;
    size_t pos = 0;
    while ((pos = line.find(delimiter, pos)) != std::string::npos) {
        ++count;
        pos += delimiter.length();
    }

    stats.totalCount = count;
    return stats;
}
//=============================================================================
static void
detectDelimiter(const std::vector<std::string>& lines, detectImportOptions& options)
{
    std::map<std::string, std::vector<size_t>> delimiterCounts;

    size_t validLines = 0;
    for (const auto& line : lines) {
        if (line.empty()) {
            continue;
        }

        validLines++;
        for (const auto& delimiter : DELIMITERS) {
            size_t count = 0;
            size_t pos = 0;
            while ((pos = line.find(delimiter, pos)) != std::string::npos) {
                ++count;
                pos += delimiter.length();
            }
            delimiterCounts[delimiter].push_back(count);
        }
    }

    std::vector<DelimiterStats> delimiterStats;
    for (const auto& [delimiter, counts] : delimiterCounts) {
        if (counts.empty()) {
            continue;
        }

        DelimiterStats stats { delimiter, 0.0, 0, 0 };

        size_t total = 0;
        for (size_t count : counts) {
            total += count;
        }
        double mean = static_cast<double>(total) / counts.size();

        size_t consistentLines = 0;
        for (size_t count : counts) {
            if (std::abs(count - mean) <= 1.0) {
                consistentLines++;
            }
        }

        stats.averageCount = mean;
        stats.consistentLines = consistentLines;
        stats.totalCount = total;

        delimiterStats.push_back(stats);
    }

    auto bestDelimiter = std::max_element(delimiterStats.begin(), delimiterStats.end(),
        [validLines](const DelimiterStats& a, const DelimiterStats& b) {
            double aConsistency = static_cast<double>(a.consistentLines) / validLines;
            double bConsistency = static_cast<double>(b.consistentLines) / validLines;

            if (std::abs(aConsistency - bConsistency) > 0.1) {
                return aConsistency < bConsistency;
            }

            if (std::abs(a.averageCount - b.averageCount) > 0.5) {
                return a.averageCount < b.averageCount;
            }

            return a.delimiter.length() > b.delimiter.length();
        });

    if (bestDelimiter != delimiterStats.end() && bestDelimiter->totalCount > 0) {
        options.Delimiter = { bestDelimiter->delimiter };
        stringVector defaultVariableNames;
        size_t nbElements = (size_t)bestDelimiter->averageCount + 1;
        defaultVariableNames.resize(nbElements);
        for (size_t k = 0; k < nbElements; ++k) {
            defaultVariableNames[k] = "Var" + std::to_string((int)(k + 1));
        }
        options.VariableNames = defaultVariableNames;
    } else {
        options.Delimiter = { "," };
    }
}
//=============================================================================
static void
detectLineEndings(const std::string& content, detectImportOptions& options)
{
    size_t maxLineEndingsCount = 0;

    for (const auto& lineEnding : LINE_ENDINGS) {
        size_t count = 0;
        size_t pos = 0;

        while ((pos = content.find(lineEnding, pos)) != std::string::npos) {
            ++count;
            pos += lineEnding.length();
        }

        if (count > maxLineEndingsCount) {
            maxLineEndingsCount = count;
            options.LineEnding = { escapeSpecialCharacters(lineEnding) };
        }
    }
}
//=============================================================================
static void
detectCommentStyle(
    const std::vector<std::string>& lines, detectImportOptions& options, std::string& errorMessage)
{
    std::map<std::string, size_t> commentCounts;

    for (const auto& comment : COMMENT_STYLES) {
        std::string escapedComment
            = std::regex_replace(comment, std::regex("[\\[\\](){}.*+?^$\\\\|]"), "\\$&");
        std::string pattern = "^[ \\t]*" + escapedComment;

        try {
            std::regex commentRegex(pattern);
            size_t count = std::count_if(
                lines.begin(), lines.end(), [&commentRegex](const std::string& line) {
                    return std::regex_search(line, commentRegex);
                });

            if (count > 0) {
                commentCounts[comment] = count;
            }
        } catch (const std::regex_error&) {
            errorMessage = "Regex error for pattern: " + pattern;
            continue;
        }
    }

    std::vector<std::pair<std::string, size_t>> sortedComments(
        commentCounts.begin(), commentCounts.end());
    std::sort(sortedComments.begin(), sortedComments.end(),
        [](const auto& a, const auto& b) { return a.second > b.second; });

    options.CommentStyle.clear();
    for (const auto& [style, count] : sortedComments) {
        options.CommentStyle.push_back(style);
    }
}
//=============================================================================
static void
detectEmptyLineRule(const std::vector<std::string>& lines, detectImportOptions& options)
{
    bool hasEmptyLines = false;
    bool hasConsecutiveEmptyLines = false;
    size_t emptyLineCount = 0;

    for (const auto& line : lines) {
        std::string trimmedLine = line;
        trimmedLine.erase(0, trimmedLine.find_first_not_of(" \t\r\n"));
        trimmedLine.erase(trimmedLine.find_last_not_of(" \t\r\n") + 1);

        if (trimmedLine.empty()) {
            emptyLineCount++;
            hasEmptyLines = true;
            if (emptyLineCount > 1) {
                hasConsecutiveEmptyLines = true;
                break;
            }
        } else {
            emptyLineCount = 0;
        }
    }

    options.EmptyLineRule
        = (!hasEmptyLines || !hasConsecutiveEmptyLines) ? "skip" : "AllowConsecutiveEmpty";
}
//=============================================================================
static bool
isNumeric(const std::string& str)
{
    if (str.empty()) {
        return false;
    }
    fast_float::parse_options options { fast_float::chars_format::fortran };

    double val;
    const char* first = str.data();
    const char* last = str.data() + str.size();
    if (!str.empty() && str.front() == '+') {
        first += 1;
    }

    auto answer = fast_float::from_chars_advanced(first, last, val, options);

    if (answer.ec != std::errc() || answer.ptr != last) {
        return false;
    }
    return true;
}
//=============================================================================
static bool
isPotentialHeader(const std::string& str)
{
    if (str.empty()) {
        return false;
    }

    if (isNumeric(str)) {
        return false;
    }

    bool hasLetters = false;
    for (char c : str) {
        if (std::isalpha(c)) {
            hasLetters = true;
            break;
        }
    }

    return hasLetters;
}
//=============================================================================
static void
detectColumnsAndRowNames(std::vector<std::string>& lines, const std::string& delimiter,
    const std::string& lineEnding, detectImportOptions& options)
{
    std::vector<std::vector<std::string>> parsedLines;
    size_t maxColumns = 0;
    size_t headerLineIndex = -1;

    size_t sampleSize = std::min(lines.size(), MIN_HEADER_SAMPLE);
    for (size_t i = 0; i < sampleSize; i++) {
        if (lines[i].size() >= lineEnding.size()
            && lines[i].substr(lines[i].size() - lineEnding.size()) == lineEnding) {
            lines[i] = lines[i].substr(0, lines[i].size() - lineEnding.size());
        }
        if (lines[i].empty()) {
            continue;
        }

        auto tokens = splitLine(lines[i], delimiter);
        maxColumns = std::max(maxColumns, tokens.size());
        parsedLines.push_back(tokens);

        if (headerLineIndex == -1 && std::any_of(tokens.begin(), tokens.end(), isPotentialHeader)) {
            headerLineIndex = i;
        }
    }

    if (parsedLines.empty() || maxColumns == 0) {
        return;
    }

    std::vector<std::string> columnNames;
    bool hasColumnHeaders = false;
    if (!parsedLines.empty()) {
        size_t headerCandidates = 0;

        for (size_t col = 0; col < parsedLines[0].size(); col++) {
            if (isPotentialHeader(parsedLines[0][col])) {
                headerCandidates++;
                columnNames.push_back(parsedLines[0][col]);
            }
        }
        hasColumnHeaders = (headerCandidates > parsedLines[0].size() / 2);
        if (!hasColumnHeaders) {
            columnNames.clear();
            options.VariableNamesLine = 0;
        } else {
            options.VariableNames = columnNames;
            options.VariableNamesLine = (int)(headerLineIndex + 1);
        }
    }
}
//=============================================================================
static void
detectDataLines(std::vector<std::string>& lines, detectImportOptions& options)
{
    size_t dataLineStart = options.VariableNamesLine;

    while (dataLineStart < lines.size()) {
        std::string trimmedLine = lines[dataLineStart];
        trimmedLine.erase(0, trimmedLine.find_first_not_of(" \t\r\n"));
        trimmedLine.erase(trimmedLine.find_last_not_of(" \t\r\n") + 1);

        if (options.EmptyLineRule == "skip" && trimmedLine.empty()) {
            dataLineStart++;
            continue;
        }

        bool isCommentLine = false;
        for (const auto& comment : options.CommentStyle) {
            if (trimmedLine.find(comment) == 0) {
                isCommentLine = true;
                break;
            }
        }
        if (isCommentLine) {
            dataLineStart++;
            continue;
        }
        break;
    }
    options.DataLines[0] = (double)(dataLineStart + 1);
}
//=============================================================================
void
initializeDetectImportOptions(detectImportOptions& options)
{
    options.Delimiter.clear();
    options.LineEnding.clear();
    options.CommentStyle.clear();
    options.EmptyLineRule = "skip";
    options.TextType = "char";
    options.VariableNamesLine = 0;
    options.VariableNames.clear();
    options.RowNamesColumn = 0;
    options.DataLines.push_back(1);
    options.DataLines.push_back(std::numeric_limits<double>::infinity());
    options.VariableTypes.clear();
    options.VariableInputFormats.clear();
    options.VariableWhitespaceRules.clear();
}
//=============================================================================
void
analyzeFileFormatImportOptions(std::wstring filename, size_t sampleSize,
    detectImportOptions& options, std::string& errorMessage)
{

    std::string content = readFileContent(filename, sampleSize, errorMessage);
    if (content.empty() && !errorMessage.empty()) {
        return;
    }

    detectLineEndings(content, options);
    std::string lineEnding
        = options.LineEnding.empty() ? "\n" : unescapeSpecialCharacters(options.LineEnding[0]);

    std::vector<std::string> lines = splitIntoLines(content, lineEnding);

    detectDelimiter(lines, options);
    detectCommentStyle(lines, options, errorMessage);
    detectEmptyLineRule(lines, options);

    if (!options.Delimiter.empty()) {
        detectColumnsAndRowNames(lines, options.Delimiter[0], lineEnding, options);
    }
    detectDataLines(lines, options);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
