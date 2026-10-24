//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PathFunctionIndexerManager.hpp"
#include "FileSystemWrapper.hpp"
#include "GetVariableEnvironment.hpp"
#include "NelsonConfiguration.hpp"
#include "NelsonException.hpp"
#include "characters_encoding.hpp"
#include <fstream>
#include <iterator>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
PathFunctionIndexerManager::setCurrentDirectorySearchEnabled(bool enabled)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    const bool previous = _currentDirectorySearchEnabled;
    if (previous == enabled) {
        return previous;
    }
    _currentDirectorySearchEnabled = enabled;
    if (enabled) {
        setCurrentUserPath(FileSystemWrapper::Path::current_path().generic_wstring());
    } else {
        delete _currentPath;
        _currentPath = nullptr;
        clearCurrentPathCache();
        refreshFunctionsMap();
    }
    return previous;
}
//=============================================================================
std::wstring
PathFunctionIndexerManager::getUserPath()
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    std::wstring _path = L"";
    if (_userPath != nullptr) {
        _path = _userPath->getPath();
    }
    return _path;
}
//=============================================================================
bool
PathFunctionIndexerManager::setCurrentUserPath(const std::wstring& path)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    if (!_currentDirectorySearchEnabled) {
        return true;
    }
    const std::wstring normalizedPath = FileSystemWrapper::Path::normalize(path);
    if (_currentPath != nullptr) {
        if (FileSystemWrapper::Path::equivalent(normalizedPath, _currentPath->getPath())) {
            return true;
        }
        cacheCurrentPath(_currentPath);
        _currentPath = nullptr;
    }
    _currentPath = takeCachedCurrentPath(normalizedPath);
    if (_currentPath == nullptr) {
        try {
            _currentPath = new PathFunctionIndexer(normalizedPath);
        } catch (const std::bad_alloc&) {
            _currentPath = nullptr;
        }
    }
    if (_currentPath != nullptr && _filesWatcherStarted) {
        _currentPath->startFileWatcher();
    }
    refreshFunctionsMap();
    return false;
}
//=============================================================================
// Returns the cached index of the directory when it still describes it, and
// drops it otherwise: an entry created, removed or renamed in the directory
// while it was not current moves the directory modification time.
PathFunctionIndexer*
PathFunctionIndexerManager::takeCachedCurrentPath(const std::wstring& path)
{
    for (auto it = _currentPathCache.rbegin(); it != _currentPathCache.rend(); ++it) {
        PathFunctionIndexer* indexer = *it;
        // A cached directory may have been removed since: not equivalent, no error.
        std::error_code errorCode;
        if (!nfs::equivalent(nfs::path(path), nfs::path(indexer->getPath()), errorCode)
            || errorCode) {
            continue;
        }
        _currentPathCache.erase(std::next(it).base());
        if (indexer->directoryUnchanged()) {
            return indexer;
        }
        delete indexer;
        return nullptr;
    }
    return nullptr;
}
//=============================================================================
void
PathFunctionIndexerManager::cacheCurrentPath(PathFunctionIndexer* indexer)
{
    constexpr size_t CURRENT_PATH_CACHE_SIZE = 8;
    _currentPathCache.push_back(indexer);
    if (_currentPathCache.size() > CURRENT_PATH_CACHE_SIZE) {
        delete _currentPathCache.front();
        _currentPathCache.erase(_currentPathCache.begin());
    }
}
//=============================================================================
void
PathFunctionIndexerManager::clearCurrentPathCache()
{
    for (PathFunctionIndexer* indexer : _currentPathCache) {
        delete indexer;
    }
    _currentPathCache.clear();
}
//=============================================================================
bool
PathFunctionIndexerManager::setUserPath(const std::wstring& path, bool saveToFile)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    clearUserPath();
    if (_userPath == nullptr) {
        const std::wstring normalizedPath = FileSystemWrapper::Path::normalize(path);
        _userPath = new PathFunctionIndexer(normalizedPath);
        if (_filesWatcherStarted) {
            _userPath->startFileWatcher();
        }
    }
    if (saveToFile) {
        saveUserPathToFile();
    }
    refreshFunctionsMap();
    return true;
}
//=============================================================================
void
PathFunctionIndexerManager::clearUserPath(bool saveToFile)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    if (_userPath != nullptr) {
        delete _userPath;
        _userPath = nullptr;
    }
    if (saveToFile) {
        saveUserPathToFile();
    }
    refreshFunctionsMap();
}
//=============================================================================
void
PathFunctionIndexerManager::resetUserPath()
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    FileSystemWrapper::Path p(userPathFile);
    FileSystemWrapper::Path::remove(p);
    userpathCompute();
    refreshFunctionsMap();
}
//=============================================================================
void
PathFunctionIndexerManager::userpathCompute()
{
    clearUserPath();
    std::wstring userpathEnv = GetVariableEnvironment(L"NELSON_USERPATH", L"");
    if (!userpathEnv.empty() && FileSystemWrapper::Path::is_directory(userpathEnv)) {
        setUserPath(userpathEnv);
        return;
    }
    std::wstring prefDir;
    try {
        prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    } catch (const NelsonException&) {
        prefDir.clear();
    }
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    if (FileSystemWrapper::Path::is_regular_file(userPathFile)) {
        std::wstring preferredUserPath = loadUserPathFromFile();
        if (!preferredUserPath.empty()
            && FileSystemWrapper::Path::is_directory(preferredUserPath)) {
            setUserPath(preferredUserPath);
            return;
        }
    }
#ifdef _MSC_VER
    std::wstring userprofileEnv = GetVariableEnvironment(L"USERPROFILE", L"");
    if (!userprofileEnv.empty()) {
        std::wstring userpathDir = userprofileEnv + std::wstring(L"/Documents/Nelson");
        if (!FileSystemWrapper::Path::is_directory(userpathDir)) {
            FileSystemWrapper::Path::create_directories(userpathDir);
        }
        if (FileSystemWrapper::Path::is_directory(userpathDir)) {
            setUserPath(userpathDir);
        }
    }
#else
    std::wstring homeEnv = GetVariableEnvironment(L"HOME", L"");
    if (homeEnv != L"") {
        std::wstring userpathDir = homeEnv + std::wstring(L"/Documents/Nelson");
        if (!FileSystemWrapper::Path::is_directory(userpathDir)) {
            FileSystemWrapper::Path::create_directories(userpathDir);
        }
        if (FileSystemWrapper::Path::is_directory(userpathDir)) {
            setUserPath(userpathDir);
        }
    }
#endif
}
//=============================================================================
std::wstring
PathFunctionIndexerManager::loadUserPathFromFile()
{
    std::wstring preferredUserPath = L"";
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(userPathFile);
    if (bIsFile) {
#ifdef _MSC_VER
        std::ifstream jsonFile(userPathFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(userPathFile));
#endif
        if (jsonFile.is_open()) {
            nlohmann::json data;
            try {
                data = nlohmann::json::parse(jsonFile);
                std::string _preferredUserPath = data["userpath"];
                preferredUserPath = utf8_to_wstring(_preferredUserPath);
            } catch (const nlohmann::json::exception&) {
            }
            jsonFile.close();
        }
    }
    return preferredUserPath;
}
//=============================================================================
bool
PathFunctionIndexerManager::saveUserPathToFile()
{
    std::wstring up = L"";
    if (_userPath != nullptr) {
        up = _userPath->getPath();
    }
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring userPathFile = prefDir + L"/userpath.conf";

    nlohmann::json data;
    data["userpath"] = wstring_to_utf8(up);
#ifdef _MSC_VER
    std::ofstream out(userPathFile);
#else
    std::ofstream out(wstring_to_utf8(userPathFile));
#endif
    if (out.is_open()) {
        out << data;
        out.close();
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
