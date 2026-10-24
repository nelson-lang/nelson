//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cctype>
#include <cwctype>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <nlohmann/json.hpp>
#include "NotebookReader.hpp"
#include "Unzip.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
fileExtensionLower(const std::wstring& filename)
{
    const size_t dot = filename.find_last_of(L'.');
    if (dot == std::wstring::npos) {
        return L"";
    }
    std::wstring ext = filename.substr(dot);
    std::transform(ext.begin(), ext.end(), ext.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    return ext;
}
//=============================================================================
bool
isNotebookFile(const std::wstring& filename)
{
    return fileExtensionLower(filename) == L".nlx";
}
//=============================================================================
static std::string
readWholeFile(const std::filesystem::path& path)
{
    std::ifstream stream(path, std::ios::binary);
    if (!stream) {
        Error(L"Cannot read Notebook document: " + path.wstring());
    }
    std::ostringstream contents;
    contents << stream.rdbuf();
    return contents.str();
}
//=============================================================================
// The `%% [kind]` marker a Notebook .m line opens, or "" when it is not one.
static std::string
lineMarkerKind(const std::string& line)
{
    size_t i = 0;
    while (i < line.size() && (line[i] == ' ' || line[i] == '\t')) {
        i++;
    }
    if (i + 1 >= line.size() || line[i] != '%' || line[i + 1] != '%') {
        return "";
    }
    i += 2;
    while (i < line.size() && (line[i] == ' ' || line[i] == '\t')) {
        i++;
    }
    if (i >= line.size() || line[i] != '[') {
        return "";
    }
    i++;
    std::string kind;
    while (
        i < line.size() && (std::isalnum(static_cast<unsigned char>(line[i])) || line[i] == '_')) {
        kind += line[i];
        i++;
    }
    if (i >= line.size() || line[i] != ']') {
        return "";
    }
    return kind;
}
//=============================================================================
static std::vector<std::string>
splitLines(const std::string& text)
{
    std::vector<std::string> lines;
    std::string line;
    for (const char c : text) {
        if (c == '\n') {
            lines.push_back(line);
            line.clear();
        } else if (c != '\r') {
            line += c;
        }
    }
    lines.push_back(line);
    return lines;
}
//=============================================================================
static wstringVector
codeCellsFromM(const std::string& text)
{
    wstringVector codes;
    const std::vector<std::string> lines = splitLines(text);
    bool hasMarker = false;
    for (const auto& l : lines) {
        if (!lineMarkerKind(l).empty()) {
            hasMarker = true;
            break;
        }
    }
    if (!hasMarker) {
        // An ordinary script: run the whole file as a single cell.
        codes.push_back(utf8_to_wstring(text));
        return codes;
    }
    size_t i = 0;
    while (i < lines.size()) {
        const std::string kind = lineMarkerKind(lines[i]);
        i++;
        if (kind.empty()) {
            continue;
        }
        if (kind == "code") {
            std::string code;
            while (i < lines.size() && lineMarkerKind(lines[i]).empty()) {
                if (!code.empty()) {
                    code += "\n";
                }
                code += lines[i];
                i++;
            }
            codes.push_back(utf8_to_wstring(code));
        } else {
            while (i < lines.size() && lineMarkerKind(lines[i]).empty()) {
                i++;
            }
        }
    }
    return codes;
}
//=============================================================================
static wstringVector
codeCellsFromNlx(const std::wstring& filename)
{
    const auto unique = std::to_string(std::chrono::steady_clock::now().time_since_epoch().count());
    std::filesystem::path tempDir
        = std::filesystem::temp_directory_path() / ("nelson_nlx_" + unique);
    std::error_code ec;
    std::filesystem::create_directories(tempDir, ec);
    wstringVector entries;
    UnZip(filename, tempDir.wstring(), entries);
    std::string content;
    const std::filesystem::path docPath = tempDir / "document.json";
    if (std::filesystem::exists(docPath)) {
        content = readWholeFile(docPath);
    }
    std::filesystem::remove_all(tempDir, ec);
    if (content.empty()) {
        Error(L"Not a valid Notebook (.nlx) document: missing document.json.");
    }
    nlohmann::json doc = nlohmann::json::parse(content, nullptr, false);
    if (doc.is_discarded() || !doc.contains("blocks") || !doc["blocks"].is_array()) {
        Error(L"Not a valid Notebook (.nlx) document: malformed document.json.");
    }
    wstringVector codes;
    for (const auto& block : doc["blocks"]) {
        if (!block.is_object() || block.value("type", std::string()) != "codecell") {
            continue;
        }
        if (!block.contains("props") || !block["props"].is_object()) {
            continue;
        }
        const auto& props = block["props"];
        if (props.contains("code") && props["code"].is_string()) {
            codes.push_back(utf8_to_wstring(props["code"].get<std::string>()));
        }
    }
    return codes;
}
//=============================================================================
wstringVector
ReadNotebookCode(const std::wstring& filename)
{
    if (!std::filesystem::exists(filename)) {
        Error(L"Notebook document not found: " + filename);
    }
    if (fileExtensionLower(filename) == L".nlx") {
        return codeCellsFromNlx(filename);
    }
    return codeCellsFromM(readWholeFile(filename));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
