//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LspUtilities.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <iomanip>
#include <sstream>
//=============================================================================
namespace Nelson::Lsp {
//=============================================================================
namespace {
    //=============================================================================
    struct EditPosition
    {
        int line;
        int column;
    };
    //=============================================================================
    struct EditRange
    {
        EditPosition start;
        EditPosition end;
    };
    //=============================================================================
    int
    comparePosition(const EditPosition& left, const EditPosition& right)
    {
        if (left.line != right.line) {
            return left.line < right.line ? -1 : 1;
        }
        if (left.column != right.column) {
            return left.column < right.column ? -1 : 1;
        }
        return 0;
    }
    //=============================================================================
    EditRange
    normalizedRange(const CodeAnalyzerTextEdit& edit)
    {
        EditRange range { { std::max(edit.lineStart, 1), std::max(edit.columnStart, 1) },
            { std::max(edit.lineEnd, 1), std::max(edit.columnEnd, 1) } };
        if (comparePosition(range.end, range.start) < 0) {
            std::swap(range.start, range.end);
        }
        return range;
    }
    //=============================================================================
    bool
    emptyRange(const EditRange& range)
    {
        return comparePosition(range.start, range.end) == 0;
    }
    //=============================================================================
    bool
    pointInsideRange(const EditPosition& point, const EditRange& range)
    {
        return !emptyRange(range) && comparePosition(range.start, point) <= 0
            && comparePosition(point, range.end) < 0;
    }
    //=============================================================================
    bool
    editRangesOverlap(const CodeAnalyzerTextEdit& left, const CodeAnalyzerTextEdit& right)
    {
        const EditRange leftRange = normalizedRange(left);
        const EditRange rightRange = normalizedRange(right);
        if (emptyRange(leftRange) || emptyRange(rightRange)) {
            return comparePosition(leftRange.start, rightRange.start) == 0
                || pointInsideRange(leftRange.start, rightRange)
                || pointInsideRange(rightRange.start, leftRange);
        }
        return comparePosition(leftRange.start, rightRange.end) < 0
            && comparePosition(rightRange.start, leftRange.end) < 0;
    }
    //=============================================================================
    bool
    editOverlapsAny(
        const CodeAnalyzerTextEdit& edit, const std::vector<CodeAnalyzerTextEdit>& edits)
    {
        return std::any_of(edits.begin(), edits.end(),
            [&edit](const CodeAnalyzerTextEdit& other) { return editRangesOverlap(edit, other); });
    }
    //=============================================================================
    int
    lspSeverity(const std::string& severity)
    {
        if (severity == "error") {
            return 1;
        }
        if (severity == "warning") {
            return 2;
        }
        return 3;
    }
    //=============================================================================
    void
    appendHash(uint64_t& hash, const std::string& value)
    {
        for (unsigned char c : value) {
            hash ^= static_cast<uint64_t>(c);
            hash *= 1099511628211ULL;
        }
        hash ^= 0xffULL;
        hash *= 1099511628211ULL;
    }
    //=============================================================================
    std::string
    diagnosticResultIdFor(const std::string& uri, int version,
        const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics)
    {
        uint64_t hash = 14695981039346656037ULL;
        appendHash(hash, uri);
        appendHash(hash, std::to_string(version));
        for (const auto& diagnostic : diagnostics) {
            appendHash(hash, diagnostic.checkId);
            appendHash(hash, diagnostic.severity);
            appendHash(hash, std::to_string(diagnostic.lineStart));
            appendHash(hash, std::to_string(diagnostic.columnStart));
            appendHash(hash, std::to_string(diagnostic.lineEnd));
            appendHash(hash, std::to_string(diagnostic.columnEnd));
            appendHash(hash, diagnostic.message);
            appendHash(hash, diagnostic.fingerprint);
        }
        return std::to_string(hash);
    }
    //=============================================================================
    std::string
    percentDecode(const std::string& value)
    {
        std::string decoded;
        for (size_t k = 0; k < value.size(); ++k) {
            if (value[k] == '%' && k + 2 < value.size()) {
                const std::string hex = value.substr(k + 1, 2);
                char* end = nullptr;
                const long character = std::strtol(hex.c_str(), &end, 16);
                if (end != nullptr && *end == '\0') {
                    decoded.push_back(static_cast<char>(character));
                    k += 2;
                    continue;
                }
            }
            decoded.push_back(value[k]);
        }
        return decoded;
    }
    //=============================================================================
    std::string
    percentEncodePath(const std::string& value)
    {
        std::ostringstream encoded;
        encoded << std::uppercase << std::hex;
        for (unsigned char character : value) {
            const bool safe = std::isalnum(character) || character == '/' || character == ':'
                || character == '-' || character == '_' || character == '.' || character == '~';
            if (safe) {
                encoded << static_cast<char>(character);
            } else {
                encoded << '%' << std::setw(2) << std::setfill('0') << static_cast<int>(character);
            }
        }
        return encoded.str();
    }
    //=============================================================================
    size_t
    lineStartOffset(const std::string& text, int zeroBasedLine)
    {
        size_t offset = 0;
        for (int currentLine = 0; currentLine < zeroBasedLine && offset < text.size();
            ++currentLine) {
            const size_t newline = text.find('\n', offset);
            if (newline == std::string::npos) {
                return text.size();
            }
            offset = newline + 1;
        }
        return offset;
    }
    //=============================================================================
    size_t
    lineEndOffset(const std::string& text, size_t lineStart)
    {
        size_t lineEnd = text.find('\n', lineStart);
        if (lineEnd == std::string::npos) {
            lineEnd = text.size();
        }
        if (lineEnd > lineStart && text[lineEnd - 1] == '\r') {
            --lineEnd;
        }
        return lineEnd;
    }
    //=============================================================================
    size_t
    utf8SequenceLength(unsigned char leadByte)
    {
        if ((leadByte & 0x80) == 0) {
            return 1;
        }
        if ((leadByte & 0xE0) == 0xC0) {
            return 2;
        }
        if ((leadByte & 0xF0) == 0xE0) {
            return 3;
        }
        if ((leadByte & 0xF8) == 0xF0) {
            return 4;
        }
        return 1;
    }
    //=============================================================================
    int
    utf16UnitsForCodePoint(const std::string& text, size_t offset, size_t length)
    {
        if (length < 4 || offset + length > text.size()) {
            return 1;
        }
        const unsigned char first = static_cast<unsigned char>(text[offset]);
        const unsigned char second = static_cast<unsigned char>(text[offset + 1]);
        const unsigned char third = static_cast<unsigned char>(text[offset + 2]);
        const unsigned char fourth = static_cast<unsigned char>(text[offset + 3]);
        const unsigned int codePoint = ((first & 0x07) << 18) | ((second & 0x3F) << 12)
            | ((third & 0x3F) << 6) | (fourth & 0x3F);
        return codePoint > 0xFFFF ? 2 : 1;
    }
    //=============================================================================
    size_t
    positionOffset(const std::string& text, int zeroBasedLine, int zeroBasedCharacter)
    {
        const size_t start = lineStartOffset(text, std::max(zeroBasedLine, 0));
        const size_t end = lineEndOffset(text, start);
        size_t offset = start;
        int remainingCharacters = std::max(zeroBasedCharacter, 0);
        while (offset < end && remainingCharacters > 0) {
            const size_t sequenceLength = std::min(
                utf8SequenceLength(static_cast<unsigned char>(text[offset])), end - offset);
            const int utf16Units = utf16UnitsForCodePoint(text, offset, sequenceLength);
            if (remainingCharacters < utf16Units) {
                break;
            }
            remainingCharacters -= utf16Units;
            offset += sequenceLength;
        }
        return offset;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::string
optionalString(const nlohmann::json& value, const std::string& key)
{
    if (!value.contains(key) || !value[key].is_string()) {
        return "";
    }
    return value[key].get<std::string>();
}
//=============================================================================
std::string
pathToUri(const std::filesystem::path& path)
{
    std::filesystem::path absolutePath = std::filesystem::absolute(path).lexically_normal();
    std::string generic = Nelson::wstring_to_utf8(absolutePath.generic_wstring());
    return "file:///" + percentEncodePath(generic);
}
//=============================================================================
std::filesystem::path
pathFromUri(const std::string& uri)
{
    const std::string prefix = "file:///";
    if (uri.rfind(prefix, 0) == 0) {
        std::string path = percentDecode(uri.substr(prefix.size()));
        for (char& c : path) {
            if (c == '/') {
                c = std::filesystem::path::preferred_separator;
            }
        }
        return std::filesystem::path(Nelson::utf8_to_wstring(path));
    }
    return std::filesystem::path();
}
//=============================================================================
void
applyDocumentChange(std::string& text, const nlohmann::json& change)
{
    const std::string newText = change.value("text", "");
    if (!change.contains("range")) {
        text = newText;
        return;
    }
    const nlohmann::json& range = change["range"];
    const int startLine = range["start"].value("line", 0);
    const int startCharacter = range["start"].value("character", 0);
    const int endLine = range["end"].value("line", startLine);
    const int endCharacter = range["end"].value("character", startCharacter);
    const size_t startOffset = positionOffset(text, startLine, startCharacter);
    const size_t endOffset = std::max(startOffset, positionOffset(text, endLine, endCharacter));
    text.replace(startOffset, endOffset - startOffset, newText);
}
//=============================================================================
nlohmann::json
lspRange(int lineStart, int columnStart, int lineEnd, int columnEnd)
{
    nlohmann::json range;
    range["start"]["line"] = std::max(lineStart - 1, 0);
    range["start"]["character"] = std::max(columnStart - 1, 0);
    range["end"]["line"] = std::max(lineEnd - 1, 0);
    range["end"]["character"] = std::max(columnEnd - 1, 0);
    return range;
}
//=============================================================================
nlohmann::json
lspDiagnostic(const Nelson::CodeAnalyzerDiagnostic& diagnostic)
{
    nlohmann::json item;
    nlohmann::json range = lspRange(
        diagnostic.lineStart, diagnostic.columnStart, diagnostic.lineEnd, diagnostic.columnEnd);
    const std::string ruleName = diagnostic.ruleName;
    item["range"] = range;
    item["severity"] = lspSeverity(diagnostic.severity);
    item["code"] = diagnostic.checkId;
    item["source"] = "nelson-lint";
    item["message"] = diagnostic.message;
    item["data"]["code"] = diagnostic.checkId;
    item["data"]["fingerprint"] = diagnostic.fingerprint;
    item["data"]["ruleName"] = diagnostic.ruleName;
    item["data"]["fixability"] = diagnostic.fixability;
    if (!diagnostic.helpUri.empty()) {
        item["codeDescription"]["href"] = diagnostic.helpUri;
        item["data"]["helpUri"] = diagnostic.helpUri;
    }
    if (diagnostic.checkId == "NLS0004" || diagnostic.checkId == "NLS0007") {
        item["tags"] = nlohmann::json::array({ 1 });
    } else if (ruleName.find("deprecated") != std::string::npos
        || ruleName.find("obsolete") != std::string::npos) {
        item["tags"] = nlohmann::json::array({ 2 });
    }
    if (!diagnostic.checkId.empty()) {
        nlohmann::json related;
        related["location"]["uri"] = pathToUri(diagnostic.file);
        related["location"]["range"] = range;
        related["message"] = diagnostic.ruleName.empty() ? diagnostic.checkId
                                                         : diagnostic.checkId + ": " + ruleName;
        item["relatedInformation"] = nlohmann::json::array({ related });
    }
    return item;
}
//=============================================================================
nlohmann::json
documentDiagnosticReport(const std::string& uri, int version,
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics,
    const std::string& previousResultId)
{
    nlohmann::json report;
    report["uri"] = uri;
    if (version > 0) {
        report["version"] = version;
    } else {
        report["version"] = nullptr;
    }
    const std::string resultId = diagnosticResultIdFor(uri, version, diagnostics);
    report["resultId"] = resultId;
    if (!previousResultId.empty() && previousResultId == resultId) {
        report["kind"] = "unchanged";
        return report;
    }
    report["kind"] = "full";
    report["items"] = nlohmann::json::array();
    for (const auto& diagnostic : diagnostics) {
        report["items"].push_back(lspDiagnostic(diagnostic));
    }
    return report;
}
//=============================================================================
std::map<std::string, std::string>
previousWorkspaceResultIds(const nlohmann::json& params)
{
    std::map<std::string, std::string> previous;
    if (!params.contains("previousResultIds") || !params["previousResultIds"].is_array()) {
        return previous;
    }
    for (const auto& item : params["previousResultIds"]) {
        if (!item.is_object()) {
            continue;
        }
        std::string uri = optionalString(item, "uri");
        std::string value = optionalString(item, "value");
        if (!uri.empty() && !value.empty()) {
            previous[uri] = value;
        }
    }
    return previous;
}
//=============================================================================
void
addDiagnosticsByUri(std::map<std::string, std::vector<Nelson::CodeAnalyzerDiagnostic>>& byUri,
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& diagnostics)
{
    for (const auto& diagnostic : diagnostics) {
        const std::string uri = pathToUri(std::filesystem::path(diagnostic.file));
        byUri[uri].push_back(diagnostic);
    }
}
//=============================================================================
nlohmann::json
fullDocumentRange(const std::string& text)
{
    int line = 0;
    int character = 0;
    size_t offset = 0;
    while (offset < text.size()) {
        if (text[offset] == '\n') {
            ++line;
            character = 0;
            ++offset;
            continue;
        }
        if (text[offset] == '\r') {
            ++offset;
            continue;
        }
        const size_t sequenceLength = std::min(
            utf8SequenceLength(static_cast<unsigned char>(text[offset])), text.size() - offset);
        character += utf16UnitsForCodePoint(text, offset, sequenceLength);
        offset += sequenceLength;
    }
    nlohmann::json range;
    range["start"]["line"] = 0;
    range["start"]["character"] = 0;
    range["end"]["line"] = line;
    range["end"]["character"] = character;
    return range;
}
//=============================================================================
bool
codeAnalyzerFixConflictsWith(
    const CodeAnalyzerFix& fix, const std::vector<CodeAnalyzerTextEdit>& acceptedEdits)
{
    std::vector<CodeAnalyzerTextEdit> pendingEdits;
    for (const auto& edit : fix.edits) {
        if (editOverlapsAny(edit, acceptedEdits) || editOverlapsAny(edit, pendingEdits)) {
            return true;
        }
        pendingEdits.push_back(edit);
    }
    return false;
}
//=============================================================================
} // namespace Nelson::Lsp
//=============================================================================
