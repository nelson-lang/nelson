//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerCache.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <mutex>
#include <sstream>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    namespace fs = std::filesystem;
    //=============================================================================
    struct CacheEntry
    {
        uintmax_t fileSize = 0;
        long long lastWrite = 0;
        uint64_t contentHash = 0;
        uint64_t optionsHash = 0;
        std::vector<CodeAnalyzerDiagnostic> diagnostics;
    };
    //=============================================================================
    std::mutex cacheMutex;
    std::unordered_map<std::wstring, CacheEntry> cache;
    //=============================================================================
    constexpr uint64_t FNV_OFFSET = 14695981039346656037ULL;
    constexpr uint64_t FNV_PRIME = 1099511628211ULL;
    constexpr int CACHE_VERSION = 3;
    //=============================================================================
    void
    hashBytes(uint64_t& seed, const std::string& value)
    {
        for (unsigned char c : value) {
            seed ^= c;
            seed *= FNV_PRIME;
        }
        seed ^= 0xff;
        seed *= FNV_PRIME;
    }
    //=============================================================================
    void
    hashNumber(uint64_t& seed, uint64_t value)
    {
        hashBytes(seed, std::to_string(value));
    }
    //=============================================================================
    void
    hashStrings(uint64_t& seed, const wstringVector& values)
    {
        for (const auto& value : values) {
            hashBytes(seed, wstring_to_utf8(value));
        }
    }
    //=============================================================================
    uint64_t
    analysisHash(const CodeAnalyzerOptions& options, const fs::path& displayPath)
    {
        uint64_t seed = FNV_OFFSET;
        hashBytes(seed, wstring_to_utf8(displayPath.lexically_normal().wstring()));
        hashNumber(seed, options.includeSuppressed ? 1 : 0);
        hashNumber(seed, options.fullPath ? 1 : 0);
        hashNumber(seed, options.includeCyclomatic ? 1 : 0);
        hashNumber(seed, options.includeModifiedCyclomatic ? 1 : 0);
        hashNumber(seed, static_cast<uint64_t>(options.maxLineLength));
        hashNumber(seed, static_cast<uint64_t>(options.maxFileLines));
        hashNumber(seed, static_cast<uint64_t>(options.maxFunctionLines));
        hashNumber(seed, static_cast<uint64_t>(options.cyclomaticWarningThreshold));
        hashNumber(seed, static_cast<uint64_t>(options.cyclomaticErrorThreshold));
        hashStrings(seed, options.inclusions);
        hashStrings(seed, options.exclusions);
        for (const auto& severity : options.ruleSeverity) {
            hashBytes(seed, severity.first);
            hashBytes(seed, severity.second);
        }
        return seed;
    }
    //=============================================================================
    bool
    fileContentHash(const fs::path& path, uint64_t& contentHash)
    {
        std::ifstream stream(path, std::ios::binary);
        if (!stream.good()) {
            return false;
        }
        uint64_t seed = FNV_OFFSET;
        char buffer[4096];
        while (stream.good()) {
            stream.read(buffer, sizeof(buffer));
            const std::streamsize count = stream.gcount();
            for (std::streamsize k = 0; k < count; ++k) {
                seed ^= static_cast<unsigned char>(buffer[k]);
                seed *= FNV_PRIME;
            }
        }
        contentHash = seed;
        return true;
    }
    //=============================================================================
    bool
    fileState(
        const fs::path& path, uintmax_t& fileSize, long long& lastWrite, uint64_t& contentHash)
    {
        try {
            fileSize = fs::file_size(path);
            lastWrite = fs::last_write_time(path).time_since_epoch().count();
            return fileContentHash(path, contentHash);
        } catch (...) {
            return false;
        }
    }
    //=============================================================================
    std::wstring
    cacheKey(const fs::path& path)
    {
        return fs::absolute(path).lexically_normal().wstring();
    }
    //=============================================================================
    std::string
    environmentVariable(const char* name)
    {
#ifdef _MSC_VER
        char* value = nullptr;
        size_t size = 0;
        if (_dupenv_s(&value, &size, name) != 0 || value == nullptr) {
            return "";
        }
        std::string result(value);
        free(value);
        return result;
#else
        const char* value = std::getenv(name);
        return value == nullptr ? "" : std::string(value);
#endif
    }
    //=============================================================================
    std::string
    hexHash(uint64_t value)
    {
        std::ostringstream stream;
        stream << std::hex << std::setw(16) << std::setfill('0') << value;
        return stream.str();
    }
    //=============================================================================
    fs::path
    cacheDirectory()
    {
        std::string configured = environmentVariable("NELSON_CODE_ANALYZER_CACHE_DIR");
        if (!configured.empty()) {
            return fs::path(utf8_to_wstring(configured));
        }
        return fs::temp_directory_path() / "nelson-code-analyzer-cache";
    }
    //=============================================================================
    fs::path
    cacheFilePath(const fs::path& path, uint64_t hash)
    {
        uint64_t seed = FNV_OFFSET;
        hashBytes(seed, wstring_to_utf8(cacheKey(path)));
        hashNumber(seed, hash);
        return cacheDirectory() / (hexHash(seed) + ".json");
    }
    //=============================================================================
    nlohmann::json
    editToJson(const CodeAnalyzerTextEdit& edit)
    {
        nlohmann::json item;
        item["lineStart"] = edit.lineStart;
        item["lineEnd"] = edit.lineEnd;
        item["columnStart"] = edit.columnStart;
        item["columnEnd"] = edit.columnEnd;
        item["replacementText"] = edit.replacementText;
        return item;
    }
    //=============================================================================
    nlohmann::json
    fixToJson(const CodeAnalyzerFix& fix)
    {
        nlohmann::json item;
        item["title"] = fix.title;
        item["safe"] = fix.safe;
        item["edits"] = nlohmann::json::array();
        for (const auto& edit : fix.edits) {
            item["edits"].push_back(editToJson(edit));
        }
        return item;
    }
    //=============================================================================
    nlohmann::json
    diagnosticToJson(const CodeAnalyzerDiagnostic& diagnostic)
    {
        nlohmann::json item;
        item["file"] = wstring_to_utf8(diagnostic.file);
        item["checkId"] = diagnostic.checkId;
        item["ruleName"] = diagnostic.ruleName;
        item["severity"] = diagnostic.severity;
        item["category"] = diagnostic.category;
        item["fixability"] = diagnostic.fixability;
        item["helpUri"] = diagnostic.helpUri;
        item["fingerprint"] = diagnostic.fingerprint;
        item["message"] = diagnostic.message;
        item["lineStart"] = diagnostic.lineStart;
        item["lineEnd"] = diagnostic.lineEnd;
        item["columnStart"] = diagnostic.columnStart;
        item["columnEnd"] = diagnostic.columnEnd;
        item["suppressed"] = diagnostic.suppressed;
        item["fixes"] = nlohmann::json::array();
        for (const auto& fix : diagnostic.fixes) {
            item["fixes"].push_back(fixToJson(fix));
        }
        return item;
    }
    //=============================================================================
    CodeAnalyzerTextEdit
    editFromJson(const nlohmann::json& item)
    {
        CodeAnalyzerTextEdit edit;
        edit.lineStart = item.value("lineStart", 0);
        edit.lineEnd = item.value("lineEnd", 0);
        edit.columnStart = item.value("columnStart", 0);
        edit.columnEnd = item.value("columnEnd", 0);
        edit.replacementText = item.value("replacementText", "");
        return edit;
    }
    //=============================================================================
    CodeAnalyzerFix
    fixFromJson(const nlohmann::json& item)
    {
        CodeAnalyzerFix fix;
        fix.title = item.value("title", "");
        fix.safe = item.value("safe", true);
        if (item.contains("edits") && item["edits"].is_array()) {
            for (const auto& edit : item["edits"]) {
                fix.edits.push_back(editFromJson(edit));
            }
        }
        return fix;
    }
    //=============================================================================
    CodeAnalyzerDiagnostic
    diagnosticFromJson(const nlohmann::json& item)
    {
        CodeAnalyzerDiagnostic diagnostic;
        diagnostic.file = utf8_to_wstring(item.value("file", ""));
        diagnostic.checkId = item.value("checkId", "");
        diagnostic.ruleName = item.value("ruleName", "");
        diagnostic.severity = item.value("severity", "");
        diagnostic.category = item.value("category", "");
        diagnostic.fixability = item.value("fixability", "");
        diagnostic.helpUri = item.value("helpUri", "");
        diagnostic.fingerprint = item.value("fingerprint", "");
        diagnostic.message = item.value("message", "");
        diagnostic.lineStart = item.value("lineStart", 0);
        diagnostic.lineEnd = item.value("lineEnd", 0);
        diagnostic.columnStart = item.value("columnStart", 0);
        diagnostic.columnEnd = item.value("columnEnd", 0);
        diagnostic.suppressed = item.value("suppressed", false);
        if (item.contains("fixes") && item["fixes"].is_array()) {
            for (const auto& fix : item["fixes"]) {
                diagnostic.fixes.push_back(fixFromJson(fix));
            }
        }
        return diagnostic;
    }
    //=============================================================================
    bool
    loadDiskCache(const fs::path& path, uintmax_t fileSize, long long lastWrite,
        uint64_t contentHash, uint64_t hash, std::vector<CodeAnalyzerDiagnostic>& diagnostics)
    {
        std::ifstream stream(cacheFilePath(path, hash), std::ios::binary);
        if (!stream.good()) {
            return false;
        }
        nlohmann::json data;
        try {
            data = nlohmann::json::parse(stream);
        } catch (...) {
            return false;
        }
        if (data.value("version", 0) != CACHE_VERSION
            || data.value("fileSize", uintmax_t { 0 }) != fileSize
            || data.value("lastWrite", 0LL) != lastWrite
            || data.value("contentHash", uint64_t { 0 }) != contentHash
            || data.value("optionsHash", uint64_t { 0 }) != hash) {
            return false;
        }
        if (!data.contains("diagnostics") || !data["diagnostics"].is_array()) {
            return false;
        }
        diagnostics.clear();
        for (const auto& item : data["diagnostics"]) {
            diagnostics.push_back(diagnosticFromJson(item));
        }
        return true;
    }
    //=============================================================================
    void
    storeDiskCache(const fs::path& path, const CacheEntry& entry)
    {
        try {
            fs::create_directories(cacheDirectory());
            nlohmann::json data;
            data["version"] = CACHE_VERSION;
            data["fileSize"] = entry.fileSize;
            data["lastWrite"] = entry.lastWrite;
            data["contentHash"] = entry.contentHash;
            data["optionsHash"] = entry.optionsHash;
            data["diagnostics"] = nlohmann::json::array();
            for (const auto& diagnostic : entry.diagnostics) {
                data["diagnostics"].push_back(diagnosticToJson(diagnostic));
            }
            std::ofstream stream(cacheFilePath(path, entry.optionsHash), std::ios::binary);
            stream << data.dump();
        } catch (...) {
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
getCachedCodeAnalysis(const fs::path& path, const CodeAnalyzerOptions& options,
    std::vector<CodeAnalyzerDiagnostic>& diagnostics)
{
    uintmax_t fileSize = 0;
    long long lastWrite = 0;
    uint64_t contentHash = 0;
    if (!fileState(path, fileSize, lastWrite, contentHash)) {
        return false;
    }
    const uint64_t hash = analysisHash(options, path);
    const std::wstring key = cacheKey(path);
    {
        std::lock_guard<std::mutex> lock(cacheMutex);
        auto found = cache.find(key);
        if (found != cache.end()) {
            const CacheEntry& entry = found->second;
            if (entry.fileSize == fileSize && entry.lastWrite == lastWrite
                && entry.contentHash == contentHash && entry.optionsHash == hash) {
                diagnostics = entry.diagnostics;
                return true;
            }
        }
    }
    if (loadDiskCache(path, fileSize, lastWrite, contentHash, hash, diagnostics)) {
        CacheEntry entry;
        entry.fileSize = fileSize;
        entry.lastWrite = lastWrite;
        entry.contentHash = contentHash;
        entry.optionsHash = hash;
        entry.diagnostics = diagnostics;
        std::lock_guard<std::mutex> lock(cacheMutex);
        cache[key] = std::move(entry);
        return true;
    }
    return false;
}
//=============================================================================
void
storeCachedCodeAnalysis(const fs::path& path, const CodeAnalyzerOptions& options,
    const std::vector<CodeAnalyzerDiagnostic>& diagnostics)
{
    uintmax_t fileSize = 0;
    long long lastWrite = 0;
    uint64_t contentHash = 0;
    if (!fileState(path, fileSize, lastWrite, contentHash)) {
        return;
    }
    CacheEntry entry;
    entry.fileSize = fileSize;
    entry.lastWrite = lastWrite;
    entry.contentHash = contentHash;
    entry.optionsHash = analysisHash(options, path);
    entry.diagnostics = diagnostics;
    {
        std::lock_guard<std::mutex> lock(cacheMutex);
        cache[cacheKey(path)] = entry;
    }
    storeDiskCache(path, entry);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
