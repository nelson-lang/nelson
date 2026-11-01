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
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <vector>
#include <nlohmann/json.hpp>
#include "i18nExtract.hpp"
#include "i18nSourceScanners.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isCppSourceExtension(const std::wstring& ext);
static bool
isExtractionCppExtension(const std::wstring& ext);
static bool
isInTestsDirectory(const std::wstring& path);
static bool
isWebDesktopSource(const std::wstring& path, const std::wstring& ext);
static void
scanTsFileForUiStrings(const std::string& text, std::set<std::string>& texts);
static bool
readFileContent(const std::filesystem::path& path, std::string& text);
static bool
writeJsonFile(const std::wstring& jsonFile, const nlohmann::ordered_json& jsonOutput,
    std::wstring& errorMessage);
//=============================================================================
bool
i18nExtractUi(
    const std::wstring& sourceRoot, const std::wstring& jsonFile, std::wstring& errorMessage)
{
    std::set<std::string> texts;
    std::error_code ec;
    std::filesystem::recursive_directory_iterator it(sourceRoot, ec), end;
    for (; it != end; it.increment(ec)) {
        if (ec) {
            break;
        }
        if (!it->is_regular_file(ec)) {
            continue;
        }
        std::wstring path = it->path().wstring();
        if (isInTestsDirectory(path)) {
            continue;
        }
        std::wstring ext = it->path().extension().wstring();
        bool cpp = isExtractionCppExtension(ext);
        bool mFile = (ext == L".m");
        bool tsFile = isWebDesktopSource(path, ext);
        if (!cpp && !mFile && !tsFile) {
            continue;
        }
        std::string text;
        if (!readFileContent(it->path(), text)) {
            continue;
        }
        if (cpp) {
            scanSourceForTextKeyed(text, texts);
        } else if (tsFile) {
            scanTsFileForUiStrings(text, texts);
        } else {
            scanMFileForUiStrings(text, texts);
        }
    }
    nlohmann::ordered_json jsonOutput;
    for (const std::string& text : texts) {
        jsonOutput[text] = "";
    }
    return writeJsonFile(jsonFile, jsonOutput, errorMessage);
}
//=============================================================================
bool
i18nExtractErrors(
    const std::wstring& sourceRoot, const std::wstring& jsonFile, std::wstring& errorMessage)
{
    std::map<std::string, std::vector<std::string>> idTemplates; // id -> distinct templates
    std::error_code ec;
    std::filesystem::recursive_directory_iterator it(sourceRoot, ec), end;
    for (; it != end; it.increment(ec)) {
        if (ec) {
            break;
        }
        if (!it->is_regular_file(ec)) {
            continue;
        }
        std::wstring path = it->path().wstring();
        if (isInTestsDirectory(path)) {
            continue;
        }
        std::wstring ext = it->path().extension().wstring();
        bool cpp = isExtractionCppExtension(ext);
        bool mFile = (ext == L".m");
        if (!cpp && !mFile) {
            continue;
        }
        std::string text;
        if (!readFileContent(it->path(), text)) {
            continue;
        }
        if (cpp) {
            scanSourceForIdTemplates(text, idTemplates);
        } else {
            scanMFileForIdTemplates(text, idTemplates);
        }
    }
    // An identifier used with several distinct templates is "overloaded"; it is
    // dropped from the catalog (its sites keep their inline template and stay
    // text-keyed) and reported. Single-template identifiers are emitted with
    // the English template as value so that message(id) always resolves a
    // non-empty template.
    nlohmann::ordered_json jsonOutput;
    std::vector<std::string> overloaded;
    for (const auto& entry : idTemplates) {
        if (entry.second.size() == 1) {
            jsonOutput[entry.first] = entry.second[0];
        } else {
            overloaded.push_back(entry.first);
        }
    }
    if (!overloaded.empty()) {
        std::string msg = "i18nExtractErrors: dropped " + std::to_string(overloaded.size())
            + " overloaded identifier(s) (mapped to several templates):";
        for (const std::string& id : overloaded) {
            msg += "\n  " + id;
        }
        std::cout << msg << std::endl;
    }
    return writeJsonFile(jsonFile, jsonOutput, errorMessage);
}
//=============================================================================
std::set<std::string>
collectRuntimeMessageIds(const std::wstring& sourceRoot)
{
    std::set<std::string> ids;
    std::error_code ec;
    std::filesystem::recursive_directory_iterator it(sourceRoot, ec), end;
    for (; it != end; it.increment(ec)) {
        if (ec) {
            break;
        }
        if (!it->is_regular_file(ec)) {
            continue;
        }
        std::wstring path = it->path().wstring();
        if (isInTestsDirectory(path)) {
            continue;
        }
        std::wstring ext = it->path().extension().wstring();
        if (isCppSourceExtension(ext)) {
            std::string text;
            if (readFileContent(it->path(), text)) {
                scanSourceForRuntimeIds(text, ids);
            }
        }
    }
    return ids;
}
//=============================================================================
std::set<std::string>
collectTextKeyedLiterals(const std::wstring& sourceRoot)
{
    std::set<std::string> texts;
    std::error_code ec;
    std::filesystem::recursive_directory_iterator it(sourceRoot, ec), end;
    for (; it != end; it.increment(ec)) {
        if (ec) {
            break;
        }
        if (!it->is_regular_file(ec)) {
            continue;
        }
        std::wstring ext = it->path().extension().wstring();
        if (isCppSourceExtension(ext)) {
            std::string text;
            if (readFileContent(it->path(), text)) {
                scanSourceForTextKeyed(text, texts);
            }
        }
    }
    return texts;
}
//=============================================================================
std::set<std::string>
collectErrorWarningMessages(const std::wstring& sourceRoot)
{
    std::set<std::string> msgs;
    std::error_code ec;
    std::filesystem::recursive_directory_iterator it(sourceRoot, ec), end;
    for (; it != end; it.increment(ec)) {
        if (ec) {
            break;
        }
        if (!it->is_regular_file(ec)) {
            continue;
        }
        std::wstring ext = it->path().extension().wstring();
        bool cpp = isCppSourceExtension(ext);
        bool mFile = (ext == L".m");
        if (!cpp && !mFile) {
            continue;
        }
        std::string text;
        if (!readFileContent(it->path(), text)) {
            continue;
        }
        scanSourceForErrorMessages(text, msgs, mFile);
    }
    return msgs;
}
//=============================================================================
static bool
isCppSourceExtension(const std::wstring& ext)
{
    return ext == L".cpp" || ext == L".hpp" || ext == L".cxx" || ext == L".hxx" || ext == L".h";
}
//=============================================================================
// The extraction passes also scan .c files; the maintenance collectors scan C++
// extensions only.
static bool
isExtractionCppExtension(const std::wstring& ext)
{
    return isCppSourceExtension(ext) || ext == L".c";
}
//=============================================================================
static bool
isInTestsDirectory(const std::wstring& path)
{
    return path.find(L"\\tests\\") != std::wstring::npos
        || path.find(L"/tests/") != std::wstring::npos;
}
//=============================================================================
// the web desktop sources translate their debugger strings with the t('...') helper
static bool
isWebDesktopSource(const std::wstring& path, const std::wstring& ext)
{
    if (ext != L".ts" && ext != L".tsx") {
        return false;
    }
    std::wstring normalized = path;
    std::replace(normalized.begin(), normalized.end(), L'\\', L'/');
    return normalized.find(L"/web_gui/frontend/src/") != std::wstring::npos;
}
//=============================================================================
// Collects the literal argument of t('...') and t("...") calls.
static void
scanTsFileForUiStrings(const std::string& text, std::set<std::string>& texts)
{
    size_t pos = 0;
    while ((pos = text.find("t(", pos)) != std::string::npos) {
        bool identifierBefore = pos > 0
            && (isalnum(static_cast<unsigned char>(text[pos - 1])) || text[pos - 1] == '_'
                || text[pos - 1] == '.');
        size_t cursor = pos + 2;
        pos = cursor;
        if (identifierBefore) {
            continue;
        }
        while (cursor < text.size() && (text[cursor] == ' ' || text[cursor] == '\n')) {
            cursor++;
        }
        if (cursor >= text.size() || (text[cursor] != '\'' && text[cursor] != '"')) {
            continue;
        }
        char quote = text[cursor];
        std::string literal;
        cursor++;
        bool closed = false;
        while (cursor < text.size()) {
            char c = text[cursor++];
            if (c == '\\' && cursor < text.size()) {
                literal.push_back(text[cursor++]);
                continue;
            }
            if (c == quote) {
                closed = true;
                break;
            }
            if (c == '\n') {
                break;
            }
            literal.push_back(c);
        }
        if (closed && !literal.empty()) {
            texts.insert(literal);
        }
        pos = cursor;
    }
}
//=============================================================================
static bool
readFileContent(const std::filesystem::path& path, std::string& text)
{
#ifdef _MSC_VER
    std::ifstream f(path.wstring(), std::ios::binary);
#else
    std::ifstream f(path.string(), std::ios::binary);
#endif
    if (!f) {
        return false;
    }
    text.assign((std::istreambuf_iterator<char>(f)), std::istreambuf_iterator<char>());
    return true;
}
//=============================================================================
static bool
writeJsonFile(const std::wstring& jsonFile, const nlohmann::ordered_json& jsonOutput,
    std::wstring& errorMessage)
{
#ifdef _MSC_VER
    std::ofstream outFile(jsonFile, std::ios::binary);
#else
    std::ofstream outFile(wstring_to_utf8(jsonFile), std::ios::binary);
#endif
    if (!outFile) {
        errorMessage = L"Cannot open output file: " + jsonFile;
        return false;
    }
    outFile << jsonOutput.dump(2) << "\n";
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
