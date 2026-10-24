//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "AiMcpServerPrivate.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <set>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::AiMcp {
//=============================================================================
struct SymbolSearchResult
{
    std::string matchesJson = "[]";
    bool found = false;
    bool hasTests = false;
    bool hasHelp = false;
};
//=============================================================================
inline std::wstring
normalizeSymbolSearchPath(const std::filesystem::path& path)
{
    std::wstring value = path.wstring();
    std::replace(value.begin(), value.end(), L'\\', L'/');
    return value;
}
//=============================================================================
inline bool
symbolSearchFileContains(const std::filesystem::path& path, const std::string& needle)
{
    std::ifstream input(path, std::ios::binary);
    if (!input.good()) {
        return false;
    }
    std::string content((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
    return content.find(needle) != std::string::npos;
}
//=============================================================================
inline bool
isSymbolSearchFile(const std::filesystem::path& path)
{
    std::string ext = path.extension().string();
    return ext == ".cpp" || ext == ".hpp" || ext == ".m" || ext == ".xml";
}
//=============================================================================
inline bool
isLargeSymbolSearchFile(const std::filesystem::path& path)
{
    std::error_code ec;
    auto size = std::filesystem::file_size(path, ec);
    return !ec && size > 2 * 1024 * 1024;
}
//=============================================================================
inline bool
shouldSkipSymbolSearchDirectory(const std::filesystem::path& path)
{
    std::string name = path.filename().string();
    return name == ".git" || name == "build" || name == "dist" || name == "generated"
        || name == "node_modules" || name == "thirdparty" || name == "vendor" || name == "asio"
        || name == "nlohmann" || name == "simdutf" || name == "stduuid";
}
//=============================================================================
inline std::vector<std::filesystem::path>
symbolSearchRootsForModule(const std::filesystem::path& moduleRoot)
{
    static const char* directories[] = { "builtin", "functions", "help", "src", "tests" };
    std::vector<std::filesystem::path> roots;
    for (const char* directory : directories) {
        std::filesystem::path root = moduleRoot / directory;
        if (std::filesystem::is_directory(root)) {
            roots.push_back(root);
        }
    }
    return roots;
}
//=============================================================================
inline std::vector<std::filesystem::path>
symbolModuleRoots(const std::filesystem::path& modulesRoot)
{
    std::vector<std::filesystem::path> roots;
    if (!std::filesystem::is_directory(modulesRoot)) {
        return roots;
    }
    std::error_code ec;
    for (const auto& moduleEntry : std::filesystem::directory_iterator(
             modulesRoot, std::filesystem::directory_options::skip_permission_denied, ec)) {
        if (ec) {
            ec.clear();
            continue;
        }
        if (moduleEntry.is_directory(ec)) {
            roots.push_back(moduleEntry.path());
        }
    }
    return roots;
}
//=============================================================================
inline void
appendSymbolSearchMatch(const std::filesystem::path& path, bool nameMatch, bool contentMatch,
    std::string& json, bool& first, SymbolSearchResult& result)
{
    result.found = true;
    std::wstring normalizedWide = normalizeSymbolSearchPath(path);
    std::string normalized = wstring_to_utf8(normalizedWide);
    if (normalized.find("/tests/") != std::string::npos) {
        result.hasTests = true;
    }
    if (normalized.find("/help/") != std::string::npos) {
        result.hasHelp = true;
    }
    if (!first) {
        json += ",";
    }
    first = false;
    json += "{\"path\":" + jsonString(normalizedWide);
    json += ",\"name_match\":" + std::string(nameMatch ? "true" : "false");
    json += ",\"content_match\":" + std::string(contentMatch ? "true" : "false") + "}";
}
//=============================================================================
inline void
appendSymbolSearchMatches(const std::filesystem::path& root, const std::string& symbolUtf8,
    bool includeContent, std::vector<std::filesystem::path>& candidateModules,
    std::set<std::wstring>& candidateKeys, std::set<std::wstring>& matchedKeys, std::string& json,
    bool& first, SymbolSearchResult& result)
{
    std::error_code ec;
    std::filesystem::recursive_directory_iterator it(
        root, std::filesystem::directory_options::skip_permission_denied, ec);
    std::filesystem::recursive_directory_iterator end;
    for (; it != end; it.increment(ec)) {
        if (ec) {
            ec.clear();
            continue;
        }
        if (it->is_directory(ec) && shouldSkipSymbolSearchDirectory(it->path())) {
            it.disable_recursion_pending();
            continue;
        }
        if (!it->is_regular_file(ec)) {
            continue;
        }
        std::filesystem::path path = it->path();
        bool nameMatch
            = path.stem().string() == symbolUtf8 || path.filename().string() == symbolUtf8;
        if (nameMatch && !includeContent) {
            std::filesystem::path moduleRoot = root.parent_path();
            if (candidateKeys.insert(moduleRoot.wstring()).second) {
                candidateModules.push_back(moduleRoot);
            }
        }
        bool contentMatch = false;
        if (includeContent && isSymbolSearchFile(path) && !isLargeSymbolSearchFile(path)) {
            contentMatch = symbolSearchFileContains(path, symbolUtf8);
        }
        if (!nameMatch && !contentMatch) {
            continue;
        }
        std::wstring matchKey = path.wstring();
        if (!matchedKeys.insert(matchKey).second) {
            continue;
        }
        appendSymbolSearchMatch(path, nameMatch, contentMatch, json, first, result);
    }
}
//=============================================================================
inline SymbolSearchResult
searchNelsonSymbolMatches(const std::wstring& symbol)
{
    SymbolSearchResult result;
    std::string symbolUtf8 = wstring_to_utf8(symbol);
    std::filesystem::path modulesRoot = std::filesystem::path(getNelsonModulesRoot());
    std::string json = "[";
    bool first = true;
    std::vector<std::filesystem::path> candidateModules;
    std::set<std::wstring> candidateKeys;
    std::set<std::wstring> matchedKeys;
    for (const auto& moduleRoot : symbolModuleRoots(modulesRoot)) {
        for (const auto& root : symbolSearchRootsForModule(moduleRoot)) {
            appendSymbolSearchMatches(root, symbolUtf8, false, candidateModules, candidateKeys,
                matchedKeys, json, first, result);
        }
    }
    for (const auto& moduleRoot : candidateModules) {
        for (const auto& root : symbolSearchRootsForModule(moduleRoot)) {
            appendSymbolSearchMatches(root, symbolUtf8, true, candidateModules, candidateKeys,
                matchedKeys, json, first, result);
        }
    }
    json += "]";
    result.matchesJson = json;
    return result;
}
//=============================================================================
} // namespace Nelson::AiMcp
//=============================================================================
