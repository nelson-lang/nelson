//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cwctype>
#include <fstream>
#include <iostream>
#include <map>
#include <mutex>
#include <utility>
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <nlohmann/json.hpp>
#include "GatewaysManager.hpp"
#include "characters_encoding.hpp"
#include "FindDynamicLibraryName.hpp"
#include "DynamicLibrary.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "BuiltInFunctionDef.hpp"
#include "FileSystemWrapper.hpp"
#include "GetVariableEnvironment.hpp"
#include "NelsonConfiguration.hpp"
#include "Nelson_VERSION.h"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define GATEWAY_ENTRY "AddGateway"
#define REMOVEGATEWAY_ENTRY "RemoveGateway"
#define GATEWAY_DESCRIPTOR_ENTRY "GetGatewayDescriptor"
//=============================================================================
struct GatewayBuiltinCache
{
    std::string name;
    int nLhs = 0;
    int nRhs = 0;
    size_t builtinPrototype = BUILTIN_PROTOTYPE::CPP_BUILTIN;
    FunctionOverloadAutoMode overloadAutoMode = NLS_OVERLOAD_AUTO_ON;
};
//=============================================================================
struct GatewayCacheEntry
{
    std::wstring libraryPath;
    std::wstring moduleName;
    uintmax_t fileSize = 0;
    std::time_t lastWriteTime = 0;
    bool hasInitializer = false;
    bool hasFinalizer = false;
    bool requiresEagerLoad = false;
    std::vector<GatewayBuiltinCache> builtins;
};
//=============================================================================
struct ResolvedGatewayLibrary
{
    FileSystemWrapper::Path directory;
    std::wstring filename;
    std::wstring fullPath;
    std::wstring cacheKey;
    uintmax_t fileSize = 0;
    std::time_t lastWriteTime = 0;
};
//=============================================================================
class CurrentDirectoryGuard
{
public:
    explicit CurrentDirectoryGuard(const FileSystemWrapper::Path& newDirectory)
    {
        previousDirectory = FileSystemWrapper::Path::current_path();
        FileSystemWrapper::Path::current_path(newDirectory);
    }

    ~CurrentDirectoryGuard() { FileSystemWrapper::Path::current_path(previousDirectory); }

private:
    FileSystemWrapper::Path previousDirectory;
};
//=============================================================================
static std::mutex gatewayResolveMutex;
static std::map<std::wstring, library_handle> libraryMap;
static std::map<std::wstring, GatewayCacheEntry> lazyGatewayMap;
static std::map<std::wstring, GatewayCacheEntry> gatewayCacheMap;
static nlohmann::json gatewayCacheDocument;
static bool gatewayCacheLoaded = false;
//=============================================================================
using PROC_AddGateway = int (*)(void*, const wchar_t*);
using PROC_RemoveGateway = bool (*)(void*, const wchar_t*);
using PROC_GetGatewayDescriptor = const Nelson::nlsGatewayDescriptor* (*)();
//=============================================================================
static std::wstring
gatewayCacheFilename();
static bool
readJsonFile(const std::wstring& filename, nlohmann::json& data);
static bool
writeJsonFile(const std::wstring& filename, const nlohmann::json& data);
static nlohmann::json
emptyCacheDocument();
static bool
cacheHeaderIsValid(const nlohmann::json& data);
static bool
cacheEntryFromJson(const std::wstring& key, const nlohmann::json& entry, uintmax_t fileSize,
    std::time_t lastWriteTime, GatewayCacheEntry& cacheEntry);
static nlohmann::json
cacheEntryToJson(const GatewayCacheEntry& cacheEntry);
static bool
gatewayFileMetadata(const std::wstring& key, uintmax_t& fileSize, std::time_t& lastWriteTime);
static void
flushGatewayCache();
static void
loadGatewayCache();
static bool
readCacheEntry(const ResolvedGatewayLibrary& resolved, GatewayCacheEntry& cacheEntry);
static void
writeCacheEntry(const GatewayCacheEntry& cacheEntry);
static bool
resolveGatewayLibrary(const std::wstring& libraryFullName, ResolvedGatewayLibrary& resolved,
    std::wstring& errorMessage);
static bool
descriptorToCacheEntry(const ResolvedGatewayLibrary& resolved,
    const Nelson::nlsGatewayDescriptor* descriptor, GatewayCacheEntry& cacheEntry,
    std::wstring& errorMessage);
static library_handle
loadGatewayLibrary(const ResolvedGatewayLibrary& resolved);
static bool
registerLazyGateway(const GatewayCacheEntry& cacheEntry);
static bool
mustLoadGatewayEager(const GatewayCacheEntry& cacheEntry);
static bool
addGatewayEager(
    Evaluator* eval, const ResolvedGatewayLibrary& resolved, std::wstring& errorMessage);
static bool
lazyGatewayResolver(Evaluator* eval, const std::wstring& libraryFullName,
    const std::string& functionName, std::wstring& errorMessage);
static bool
gatewayEnvironmentFlag(const wchar_t* name);
static bool
gatewayTraceEnabled();
static bool
gatewayForceLoadedEnabled();
static void
traceGateway(const std::wstring& action, const std::wstring& detail);
//=============================================================================
GatewaysManager::GatewaysManager() { SetLazyGatewayResolver(lazyGatewayResolver); }
//=============================================================================
GatewaysManager*
GatewaysManager::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new GatewaysManager();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
GatewaysManager::destroy(Evaluator* eval)
{
    if (m_pInstance != nullptr) {
        // Let modules with live worker threads (e.g. the parfeval background
        // pool) drain them before any gateway frees the builtin definitions
        // those threads may still be resolving.
        RunPreModulesTeardownCallbacks();
        wstringVector names;
        for (auto it = libraryMap.rbegin(); it != libraryMap.rend(); ++it) {
            names.push_back(it->first);
        }
        for (const auto& n : names) {
            std::wstring errorMessage;
            removeGateway(eval, n, errorMessage);
        }
        for (const auto& lazyGateway : lazyGatewayMap) {
            BuiltInFunctionDefManager::getInstance()->removeByFilename(lazyGateway.first);
        }
        lazyGatewayMap.clear();
        SetLazyGatewayResolver(nullptr);
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
bool
GatewaysManager::addGateway(Evaluator* eval, const std::wstring& libraryFullName,
    std::wstring& errorMessage, bool forceLoaded)
{
    ResolvedGatewayLibrary resolved;
    if (!resolveGatewayLibrary(libraryFullName, resolved, errorMessage)) {
        return false;
    }

    const bool effectiveForceLoaded = forceLoaded || gatewayForceLoadedEnabled();

    if (libraryMap.find(resolved.cacheKey) != libraryMap.end()) {
        traceGateway(L"loaded", resolved.cacheKey);
        return true;
    }

    auto lazyFound = lazyGatewayMap.find(resolved.cacheKey);
    if (lazyFound != lazyGatewayMap.end()) {
        if (!effectiveForceLoaded) {
            traceGateway(L"lazy", resolved.cacheKey);
            return true;
        }
        BuiltInFunctionDefManager::getInstance()->removeByFilename(resolved.cacheKey);
        lazyGatewayMap.erase(lazyFound);
        traceGateway(L"force loaded", resolved.cacheKey);
    } else if (effectiveForceLoaded) {
        traceGateway(L"force loaded", resolved.cacheKey);
    }

    if (!effectiveForceLoaded) {
        GatewayCacheEntry cacheEntry;
        if (readCacheEntry(resolved, cacheEntry)) {
            if (!mustLoadGatewayEager(cacheEntry)) {
                traceGateway(L"cache hit lazy", resolved.cacheKey);
                return registerLazyGateway(cacheEntry);
            }
            traceGateway(L"cache hit loaded", resolved.cacheKey);
        } else {
            traceGateway(L"cache miss", resolved.cacheKey);
        }
    }

    return addGatewayEager(eval, resolved, errorMessage);
}
//=============================================================================
bool
GatewaysManager::resolveLazyGateway(Evaluator* eval, const std::wstring& libraryFullName,
    const std::string& functionName, std::wstring& errorMessage)
{
    std::lock_guard<std::mutex> lock(gatewayResolveMutex);
    ResolvedGatewayLibrary resolved;
    if (!resolveGatewayLibrary(libraryFullName, resolved, errorMessage)) {
        return false;
    }
    if (libraryMap.find(resolved.cacheKey) != libraryMap.end()) {
        traceGateway(L"loaded", resolved.cacheKey);
        return true;
    }
    if (lazyGatewayMap.find(resolved.cacheKey) == lazyGatewayMap.end()) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:modules_manager:lazyGatewayRegistered",
                L"Lazy gateway not registered: %s"),
            resolved.cacheKey);
        return false;
    }
    traceGateway(L"lazy resolve", resolved.cacheKey + L"!" + utf8_to_wstring(functionName));
    return addGatewayEager(eval, resolved, errorMessage);
}
//=============================================================================
bool
GatewaysManager::removeGateway(
    Evaluator* eval, const std::wstring& libraryFullName, std::wstring& errorMessage)
{
    ResolvedGatewayLibrary resolved;
    if (!resolveGatewayLibrary(libraryFullName, resolved, errorMessage)) {
        return false;
    }

    auto lazyFound = lazyGatewayMap.find(resolved.cacheKey);
    if (lazyFound != lazyGatewayMap.end()) {
        BuiltInFunctionDefManager::getInstance()->removeByFilename(resolved.cacheKey);
        lazyGatewayMap.erase(lazyFound);
        return true;
    }

    auto found = libraryMap.find(resolved.cacheKey);
    if (found == libraryMap.end()) {
        errorMessage = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedLibraryHandle",
            L"Module not loaded: library handle not found.");
        return false;
    }

    library_handle handle = found->second;
    if (handle == nullptr) {
        errorMessage = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedLibraryHandle",
            L"Module not loaded: library handle not found.");
        return false;
    }

    auto removeGatewayPtr
        = reinterpret_cast<PROC_RemoveGateway>(get_function(handle, REMOVEGATEWAY_ENTRY));
    if (removeGatewayPtr == nullptr) {
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedRemovegatewaySymbol",
                L"Module not loaded: RemoveGateway symbol not found.");
        return false;
    }

    bool res = removeGatewayPtr((void*)eval, resolved.cacheKey.c_str());
    libraryMap.erase(found);
    // Once the process is shutting down, the library must stay mapped: gateway
    // dependencies (e.g. Qt Quick pulled in by qml_engine) leave callbacks
    // registered in subsystems destroyed later (Qt accessibility factories),
    // and unmapping their code here turns that teardown into a jump into freed
    // pages. The loader reclaims everything at process exit anyway.
    if (!NelsonConfiguration::getInstance()->isShuttingDown()) {
        close_dynamic_library(handle);
    }
    return res;
}
//=============================================================================
wstringVector
GatewaysManager::getLibraryNames()
{
    wstringVector names;
    for (auto it = libraryMap.rbegin(); it != libraryMap.rend(); ++it) {
        names.push_back(it->first);
    }
    for (auto it = lazyGatewayMap.rbegin(); it != lazyGatewayMap.rend(); ++it) {
        names.push_back(it->first);
    }
    return names;
}
//=============================================================================
std::wstring
GatewaysManager::getGatewayState(const std::wstring& libraryFullName, std::wstring& errorMessage)
{
    ResolvedGatewayLibrary resolved;
    if (!resolveGatewayLibrary(libraryFullName, resolved, errorMessage)) {
        return L"";
    }
    if (libraryMap.find(resolved.cacheKey) != libraryMap.end()) {
        return L"loaded";
    }
    if (lazyGatewayMap.find(resolved.cacheKey) != lazyGatewayMap.end()) {
        return L"lazy";
    }
    return L"not_loaded";
}
//=============================================================================
bool
GatewaysManager::getLazyGatewayInfo(
    const std::wstring& libraryFullName, std::wstring& moduleName, stringVector& builtins)
{
    ResolvedGatewayLibrary resolved;
    std::wstring errorMessage;
    if (!resolveGatewayLibrary(libraryFullName, resolved, errorMessage)) {
        return false;
    }
    auto lazyFound = lazyGatewayMap.find(resolved.cacheKey);
    if (lazyFound == lazyGatewayMap.end()) {
        return false;
    }
    moduleName = lazyFound->second.moduleName;
    builtins.clear();
    builtins.reserve(lazyFound->second.builtins.size());
    for (const auto& builtin : lazyFound->second.builtins) {
        builtins.push_back(builtin.name);
    }
    return true;
}
//=============================================================================
static bool
gatewayEnvironmentFlag(const wchar_t* name)
{
    std::wstring flagValue = GetVariableEnvironment(name, L"");
    for (auto& character : flagValue) {
        character = static_cast<wchar_t>(std::towlower(character));
    }
    return !(flagValue.empty() || flagValue == L"0" || flagValue == L"false" || flagValue == L"off"
        || flagValue == L"no");
}
//=============================================================================
static bool
gatewayTraceEnabled()
{
    return gatewayEnvironmentFlag(L"NELSON_GATEWAY_TRACE");
}
//=============================================================================
static bool
gatewayForceLoadedEnabled()
{
    return gatewayEnvironmentFlag(L"NELSON_GATEWAY_FORCE_LOADED");
}
//=============================================================================
static void
traceGateway(const std::wstring& action, const std::wstring& detail)
{
    if (!gatewayTraceEnabled()) {
        return;
    }
    std::cerr << "[gateway] " << wstring_to_utf8(action) << ": " << wstring_to_utf8(detail)
              << std::endl;
}
//=============================================================================
static std::wstring
gatewayCacheFilename()
{
    std::wstring prefdir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    FileSystemWrapper::Path cachePath(prefdir);
    cachePath /= L"gateway_cache.json";
    return cachePath.generic_wstring();
}
//=============================================================================
static bool
readJsonFile(const std::wstring& filename, nlohmann::json& data)
{
    data = nlohmann::json::object();
    if (!FileSystemWrapper::Path::is_regular_file(filename)) {
        return true;
    }
#ifdef _MSC_VER
    std::ifstream jsonFile(filename);
#else
    std::ifstream jsonFile(wstring_to_utf8(filename));
#endif
    if (!jsonFile.good()) {
        return false;
    }
    try {
        data = nlohmann::json::parse(jsonFile);
    } catch (const nlohmann::json::exception&) {
        data = nlohmann::json::object();
        return false;
    }
    return true;
}
//=============================================================================
static bool
writeJsonFile(const std::wstring& filename, const nlohmann::json& data)
{
#ifdef _MSC_VER
    std::ofstream jsonFile(filename);
#else
    std::ofstream jsonFile(wstring_to_utf8(filename));
#endif
    if (!jsonFile.good()) {
        return false;
    }
    jsonFile << data.dump();
    return jsonFile.good();
}
//=============================================================================
static nlohmann::json
emptyCacheDocument()
{
    nlohmann::json data;
    data["schemaVersion"] = 1;
    data["gatewayAbi"] = 1;
    data["nelsonVersion"] = NELSON_SEMANTIC_VERSION_STRING;
    data["gateways"] = nlohmann::json::object();
    return data;
}
//=============================================================================
static bool
cacheHeaderIsValid(const nlohmann::json& data)
{
    return data.is_object() && data.value("schemaVersion", 0) == 1
        && data.value("gatewayAbi", 0) == 1
        && data.value("nelsonVersion", std::string()) == NELSON_SEMANTIC_VERSION_STRING
        && data.contains("gateways") && data["gateways"].is_object();
}
//=============================================================================
static bool
cacheEntryFromJson(const std::wstring& key, const nlohmann::json& entry, uintmax_t fileSize,
    std::time_t lastWriteTime, GatewayCacheEntry& cacheEntry)
{
    if (!entry.is_object() || entry.value("fileSize", uintmax_t(0)) != fileSize
        || entry.value("lastWriteTime", std::time_t(0)) != lastWriteTime
        || !entry.contains("builtins") || !entry["builtins"].is_array()) {
        return false;
    }
    cacheEntry = GatewayCacheEntry();
    cacheEntry.libraryPath = key;
    cacheEntry.moduleName = utf8_to_wstring(entry.value("moduleName", std::string()));
    cacheEntry.fileSize = fileSize;
    cacheEntry.lastWriteTime = lastWriteTime;
    cacheEntry.hasInitializer = entry.value("hasInitializer", false);
    cacheEntry.hasFinalizer = entry.value("hasFinalizer", false);
    cacheEntry.requiresEagerLoad = entry.value("requiresEagerLoad", false);
    for (const auto& builtin : entry["builtins"]) {
        if (!builtin.is_object()) {
            return false;
        }
        GatewayBuiltinCache builtinCache;
        builtinCache.name = builtin.value("name", std::string());
        builtinCache.nLhs = builtin.value("nLhs", 0);
        builtinCache.nRhs = builtin.value("nRhs", 0);
        builtinCache.builtinPrototype = builtin.value("prototype", 0);
        builtinCache.overloadAutoMode
            = static_cast<FunctionOverloadAutoMode>(builtin.value("overloadMode", 0));
        if (builtinCache.name.empty()) {
            return false;
        }
        cacheEntry.builtins.push_back(builtinCache);
    }
    return !cacheEntry.moduleName.empty() && !cacheEntry.builtins.empty();
}
//=============================================================================
static nlohmann::json
cacheEntryToJson(const GatewayCacheEntry& cacheEntry)
{
    nlohmann::json entry;
    entry["moduleName"] = wstring_to_utf8(cacheEntry.moduleName);
    entry["fileSize"] = cacheEntry.fileSize;
    entry["lastWriteTime"] = cacheEntry.lastWriteTime;
    entry["hasInitializer"] = cacheEntry.hasInitializer;
    entry["hasFinalizer"] = cacheEntry.hasFinalizer;
    entry["requiresEagerLoad"] = cacheEntry.requiresEagerLoad;
    entry["builtins"] = nlohmann::json::array();
    for (const auto& builtin : cacheEntry.builtins) {
        nlohmann::json builtinJson;
        builtinJson["name"] = builtin.name;
        builtinJson["nLhs"] = builtin.nLhs;
        builtinJson["nRhs"] = builtin.nRhs;
        builtinJson["prototype"] = builtin.builtinPrototype;
        builtinJson["overloadMode"] = static_cast<int>(builtin.overloadAutoMode);
        entry["builtins"].push_back(builtinJson);
    }
    return entry;
}
//=============================================================================
static bool
gatewayFileMetadata(const std::wstring& key, uintmax_t& fileSize, std::time_t& lastWriteTime)
{
    if (!FileSystemWrapper::Path::is_regular_file(key)) {
        return false;
    }
    FileSystemWrapper::Path path(key);
    fileSize = FileSystemWrapper::Path::file_size(path);
    std::string errorMessage;
    lastWriteTime = FileSystemWrapper::Path::last_write_time(path, errorMessage);
    return true;
}
//=============================================================================
static void
flushGatewayCache()
{
    writeJsonFile(gatewayCacheFilename(), gatewayCacheDocument);
}
//=============================================================================
static void
loadGatewayCache()
{
    if (gatewayCacheLoaded) {
        return;
    }
    gatewayCacheLoaded = true;
    gatewayCacheMap.clear();

    nlohmann::json data;
    const std::wstring cacheFilename = gatewayCacheFilename();
    const bool cacheFileExists = FileSystemWrapper::Path::is_regular_file(cacheFilename);
    if (!readJsonFile(cacheFilename, data) || !cacheHeaderIsValid(data)) {
        gatewayCacheDocument = emptyCacheDocument();
        if (cacheFileExists) {
            traceGateway(L"cache reset", cacheFilename);
            flushGatewayCache();
        }
        return;
    }

    gatewayCacheDocument = emptyCacheDocument();
    bool cacheChanged = false;
    for (const auto& item : data["gateways"].items()) {
        const std::wstring key = utf8_to_wstring(item.key());
        uintmax_t fileSize = 0;
        std::time_t lastWriteTime = 0;
        GatewayCacheEntry cacheEntry;
        if (gatewayFileMetadata(key, fileSize, lastWriteTime)
            && cacheEntryFromJson(key, item.value(), fileSize, lastWriteTime, cacheEntry)) {
            gatewayCacheMap[key] = cacheEntry;
            gatewayCacheDocument["gateways"][item.key()] = cacheEntryToJson(cacheEntry);
        } else {
            traceGateway(L"cache purge", key);
            cacheChanged = true;
        }
    }

    const auto compactCacheSize = static_cast<uintmax_t>(gatewayCacheDocument.dump().size());
    if (cacheFileExists && FileSystemWrapper::Path::file_size(cacheFilename) != compactCacheSize) {
        cacheChanged = true;
    }

    if (cacheChanged || gatewayCacheDocument != data) {
        traceGateway(L"cache write", cacheFilename);
        flushGatewayCache();
    }
}
//=============================================================================
static bool
readCacheEntry(const ResolvedGatewayLibrary& resolved, GatewayCacheEntry& cacheEntry)
{
    loadGatewayCache();
    auto found = gatewayCacheMap.find(resolved.cacheKey);
    if (found == gatewayCacheMap.end()) {
        return false;
    }
    if (found->second.fileSize != resolved.fileSize
        || found->second.lastWriteTime != resolved.lastWriteTime) {
        gatewayCacheMap.erase(found);
        gatewayCacheDocument["gateways"].erase(wstring_to_utf8(resolved.cacheKey));
        traceGateway(L"cache stale", resolved.cacheKey);
        flushGatewayCache();
        return false;
    }
    cacheEntry = found->second;
    return true;
}
//=============================================================================
static void
writeCacheEntry(const GatewayCacheEntry& cacheEntry)
{
    loadGatewayCache();
    const auto key = wstring_to_utf8(cacheEntry.libraryPath);
    const nlohmann::json entry = cacheEntryToJson(cacheEntry);
    if (gatewayCacheDocument["gateways"].contains(key)
        && gatewayCacheDocument["gateways"][key] == entry) {
        gatewayCacheMap[cacheEntry.libraryPath] = cacheEntry;
        return;
    }
    gatewayCacheMap[cacheEntry.libraryPath] = cacheEntry;
    gatewayCacheDocument["gateways"][key] = entry;
    traceGateway(L"cache update", cacheEntry.libraryPath);
    flushGatewayCache();
}
//=============================================================================
static bool
resolveGatewayLibrary(const std::wstring& libraryFullName, ResolvedGatewayLibrary& resolved,
    std::wstring& errorMessage)
{
    FileSystemWrapper::Path p(libraryFullName);
    p = p.generic_wstring();
    FileSystemWrapper::Path dir = p.parent_path();
    if (dir.generic_wstring().empty()) {
        dir = FileSystemWrapper::Path::current_path();
    }
    std::wstring filename
        = FindDynamicLibraryName(dir.generic_wstring(), p.filename().generic_wstring(), false);
    if (filename.empty()) {
        errorMessage
            = formatI18nMessage(Nelson::resolveMessageTemplate(
                                    L"Nelson:modules_manager:fileFound", L"File not found: %s"),
                p.generic_wstring());
        return false;
    }
    FileSystemWrapper::Path fullPath = dir / filename;
    std::string canonicalError;
    FileSystemWrapper::Path canonical
        = FileSystemWrapper::Path::canonical(fullPath, canonicalError);
    resolved.directory = dir;
    resolved.filename = filename;
    resolved.fullPath = fullPath.generic_wstring();
    resolved.cacheKey = canonicalError.empty()
        ? canonical.generic_wstring()
        : FileSystemWrapper::Path::normalize(fullPath.generic_wstring());
    resolved.fileSize = FileSystemWrapper::Path::file_size(fullPath);
    std::string lastWriteError;
    resolved.lastWriteTime = FileSystemWrapper::Path::last_write_time(fullPath, lastWriteError);
    return true;
}
//=============================================================================
static bool
descriptorToCacheEntry(const ResolvedGatewayLibrary& resolved,
    const Nelson::nlsGatewayDescriptor* descriptor, GatewayCacheEntry& cacheEntry,
    std::wstring& errorMessage)
{
    if (descriptor == nullptr || descriptor->gatewayName == nullptr
        || descriptor->gateway == nullptr || descriptor->nbBuiltins == 0) {
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedGatewayDescriptor",
                L"Module not loaded: gateway descriptor is invalid.");
        return false;
    }
    cacheEntry = GatewayCacheEntry();
    cacheEntry.libraryPath = resolved.cacheKey;
    cacheEntry.moduleName = descriptor->gatewayName;
    cacheEntry.fileSize = resolved.fileSize;
    cacheEntry.lastWriteTime = resolved.lastWriteTime;
    cacheEntry.hasInitializer = descriptor->hasInitializer;
    cacheEntry.hasFinalizer = descriptor->hasFinalizer;
    cacheEntry.requiresEagerLoad = descriptor->requiresEagerLoad;
    cacheEntry.builtins.reserve(descriptor->nbBuiltins);
    try {
        for (size_t k = 0; k < descriptor->nbBuiltins; k++) {
            const auto& gateway = descriptor->gateway[k];
            if (gateway.functionName == nullptr || gateway.functionName[0] == '\0') {
                errorMessage = Nelson::resolveMessageTemplate(
                    L"Nelson:modules_manager:moduleLoadedGatewayDescriptor",
                    L"Module not loaded: gateway descriptor is invalid.");
                return false;
            }
            GatewayBuiltinCache builtin;
            builtin.name = gateway.functionName;
            builtin.nLhs = gateway.nLhs;
            builtin.nRhs = gateway.nRhs;
            builtin.builtinPrototype = static_cast<size_t>(gateway.builtinPrototype);
            builtin.overloadAutoMode = gateway.builtinOverloadAutoMode;
            cacheEntry.builtins.push_back(builtin);
        }
    } catch (...) {
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedGatewayDescriptor",
                L"Module not loaded: incompatible gateway library (rebuild required).");
        return false;
    }
    return true;
}
//=============================================================================
static library_handle
loadGatewayLibrary(const ResolvedGatewayLibrary& resolved)
{
    CurrentDirectoryGuard guard(resolved.directory);
#ifdef _MSC_VER
    return load_dynamic_libraryW(resolved.filename);
#else
    return load_dynamic_library(wstring_to_utf8(resolved.filename));
#endif
}
//=============================================================================
static bool
registerLazyGateway(const GatewayCacheEntry& cacheEntry)
{
    for (const auto& builtin : cacheEntry.builtins) {
        BuiltInFunctionDefManager::getInstance()->addLazyGateway(builtin.name, builtin.nRhs,
            builtin.nLhs, cacheEntry.libraryPath, cacheEntry.moduleName, builtin.builtinPrototype,
            builtin.overloadAutoMode);
    }
    lazyGatewayMap[cacheEntry.libraryPath] = cacheEntry;
    traceGateway(L"lazy", cacheEntry.libraryPath);
    return true;
}
//=============================================================================
// A lazy gateway runs its initializer when the first builtin call resolves
// it (AddGateway registers the builtins then calls the initializer), and
// destroy() only finalizes libraries that were actually loaded. Only gateways
// whose initializer has effects consumed before any of their builtins run
// (class registration, IPC receiver, ...) must be loaded at startup.
static bool
mustLoadGatewayEager(const GatewayCacheEntry& cacheEntry)
{
    return cacheEntry.requiresEagerLoad;
}
//=============================================================================
static bool
addGatewayEager(Evaluator* eval, const ResolvedGatewayLibrary& resolved, std::wstring& errorMessage)
{
    library_handle handle = loadGatewayLibrary(resolved);
    if (handle == nullptr) {
        traceGateway(L"load failed", resolved.cacheKey);
        errorMessage = formatI18nMessage(Nelson::resolveMessageTemplate(
                                             L"Nelson:modules_manager:moduleLoadedLibraryLoaded",
                                             L"Module not loaded: library not loaded.\n%s\n%s"),
                           resolved.fullPath, utf8_to_wstring(get_dynamic_library_error()))
            + L"\n";
        return false;
    }

    auto descriptorPtr = reinterpret_cast<PROC_GetGatewayDescriptor>(
        get_function(handle, GATEWAY_DESCRIPTOR_ENTRY));
    if (descriptorPtr == nullptr) {
        close_dynamic_library(handle);
        traceGateway(L"descriptor missing", resolved.cacheKey);
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedGatewayDescriptor2",
                L"Module not loaded: gateway descriptor not found.");
        return false;
    }
    GatewayCacheEntry cacheEntry;
    if (!descriptorToCacheEntry(resolved, descriptorPtr(), cacheEntry, errorMessage)) {
        close_dynamic_library(handle);
        traceGateway(L"descriptor invalid", resolved.cacheKey);
        return false;
    }

    auto addGatewayPtr = reinterpret_cast<PROC_AddGateway>(get_function(handle, GATEWAY_ENTRY));
    if (addGatewayPtr == nullptr) {
        close_dynamic_library(handle);
        traceGateway(L"AddGateway missing", resolved.cacheKey);
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedAddgatewaySymbol",
                L"Module not loaded: AddGateway symbol not found.");
        return false;
    }

    if (addGatewayPtr((void*)eval, resolved.cacheKey.c_str()) == 0) {
        close_dynamic_library(handle);
        traceGateway(L"AddGateway failed", resolved.cacheKey);
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedAddgatewayFailed",
                L"Module not loaded: AddGateway failed.");
        return false;
    }

    libraryMap[resolved.cacheKey] = handle;
    lazyGatewayMap.erase(resolved.cacheKey);
    writeCacheEntry(cacheEntry);
    traceGateway(L"loaded", resolved.cacheKey);
    return true;
}
//=============================================================================
static bool
lazyGatewayResolver(Evaluator* eval, const std::wstring& libraryFullName,
    const std::string& functionName, std::wstring& errorMessage)
{
    return GatewaysManager::getInstance()->resolveLazyGateway(
        eval, libraryFullName, functionName, errorMessage);
}
//=============================================================================
GatewaysManager* GatewaysManager::m_pInstance = nullptr;
//=============================================================================
} // namespace Nelson
//=============================================================================
