//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cerrno>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cwctype>
#include <thread>
#include "nlsBuildConfig.h"
#include "PathFunctionIndexerManager.hpp"
#include "ClassdefParser.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserInterface.hpp"
#include "Error.hpp"
#include "NelsonConfiguration.hpp"
#include "FunctionsInMemory.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// set by the first prompt command: class registry changes are applied on that thread only
static std::atomic<std::thread::id> interpreterThreadId {};
//=============================================================================
// class files changed while another thread noticed them, applied at the next name lookup
static std::atomic<bool> classFilesPending { false };
//=============================================================================
static bool
isEngineSentinelName(const std::string& functionName);
//=============================================================================
static bool
mustPollWatchedDirectories();
//=============================================================================
struct LoadedFileStamp
{
    nfs::file_time_type writeTime {};
    std::uintmax_t size = 0;
    bool isClassdef = false;
    std::wstring loadedFilename;
};
//=============================================================================
static std::unordered_map<std::wstring, LoadedFileStamp>&
loadedFileStamps();
//=============================================================================
static std::mutex&
loadedFileStampsMutex();
//=============================================================================
static wstringVector&
pendingClassFiles();
//=============================================================================
static std::vector<LoadedFileStamp>
takeChangedLoadedFiles();
//=============================================================================
static void
dispatchChangedFiles(const std::vector<LoadedFileStamp>& changedFiles, bool onInterpreterThread);
//=============================================================================
static void
forgetClassFile(const std::wstring& filename);
//=============================================================================
static bool
isInterpreterThread();
//=============================================================================
static void
startLoadedFilesMonitor();
//=============================================================================
static void
stopLoadedFilesMonitor();
//=============================================================================
static bool
readFileStamp(const std::wstring& filename, LoadedFileStamp& stamp);
//=============================================================================
static bool
isInsideDirectory(const std::wstring& normalizedFilename, const std::wstring& directory);
//=============================================================================
PathFunctionIndexerManager* PathFunctionIndexerManager::m_pInstance = nullptr;
//=============================================================================
namespace {
    //=============================================================================
    std::wstring
    packageClassFilename(const std::wstring& rootPath, const std::string& qualifiedName)
    {
        if (qualifiedName.empty()) {
            return {};
        }
        std::wstring filename = rootPath;
        std::string::size_type first = 0;
        std::string::size_type dot = qualifiedName.find('.');
        while (dot != std::string::npos) {
            const std::string packagePart = qualifiedName.substr(first, dot - first);
            if (packagePart.empty()) {
                return {};
            }
            filename += L"/+" + utf8_to_wstring(packagePart);
            first = dot + 1;
            dot = qualifiedName.find('.', first);
        }
        const std::string className = qualifiedName.substr(first);
        if (className.empty()) {
            return {};
        }
        return filename + L"/" + utf8_to_wstring(className) + L".m";
    }
    //=============================================================================
    std::string
    overloadOwnerClassName(const std::string& functionName)
    {
        if (functionName.size() < 3 || functionName.front() != '@') {
            return {};
        }
        size_t slash = functionName.find('/');
        if (slash == std::string::npos || slash <= 1) {
            return {};
        }
        return functionName.substr(1, slash - 1);
    }
    //=============================================================================
    // The constructor of an @Class directory is registered under its plain name
    // (e.g. "myc" for @myc/myc.m), so overloadOwnerClassName cannot find the
    // owner. The parent @Class folder owns it, which lets a classic constructor
    // write the object's data fields directly after class(s, 'name').
    std::string
    ownerClassNameFromFilename(const std::wstring& filename)
    {
        if (filename.empty()) {
            return {};
        }
        FileSystemWrapper::Path parent = FileSystemWrapper::Path(filename).parent_path();
        std::string dir = parent.filename().generic_string();
        if (dir.size() < 2 || dir.front() != '@') {
            return {};
        }
        return dir.substr(1);
    }
    //=============================================================================
} // namespace
//=============================================================================
PathFunctionIndexerManager::PathFunctionIndexerManager()
{
    _filesWatcherStarted = false;
    _userPath = nullptr;
    _currentPath = nullptr;
    userpathCompute();
}
//=============================================================================
PathFunctionIndexerManager::~PathFunctionIndexerManager()
{
    if (_userPath != nullptr) {
        delete _userPath;
        _userPath = nullptr;
    }
    if (_currentPath != nullptr) {
        delete _currentPath;
        _currentPath = nullptr;
    }
    clearCurrentPathCache();
}
//=============================================================================
PathFunctionIndexerManager*
PathFunctionIndexerManager::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new PathFunctionIndexerManager();
    }
    return m_pInstance;
}
//=============================================================================
void
PathFunctionIndexerManager::destroy()
{
    stopLoadedFilesMonitor();
    clearUserPath();
    clear();
    FunctionsInMemory::getInstance()->clear();
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
void
PathFunctionIndexerManager::startFileWatcher()
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    for (auto& pf : _pathWatchFuncVector) {
        pf->startFileWatcher();
    }
    if (_userPath) {
        _userPath->startFileWatcher();
    }
    if (_currentPath) {
        _currentPath->startFileWatcher();
    }
    _filesWatcherStarted = true;
    // started by the thread that then runs the prompt loop
    interpreterThreadId.store(std::this_thread::get_id());
    if (NelsonConfiguration::getInstance()->isFileWatcherEnabled()) {
        startLoadedFilesMonitor();
    }
}
//=============================================================================
bool
PathFunctionIndexerManager::hashOnFileWatcher()
{
    if (!mustPollWatchedDirectories()) {
        return false;
    }
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    bool result = false;
    for (auto& pf : _pathWatchFuncVector) {
        if (pf->wasModified()) {
            pf->rehash();
            result = true;
        }
    }
    if (_userPath && _userPath->wasModified()) {
        _userPath->rehash();
        result = true;
    }
    if (_currentPath && _currentPath->wasModified()) {
        _currentPath->rehash();
        result = true;
    }

    if (result) {
        refreshFunctionsMap();
    }
    return result;
}
//=============================================================================
void
PathFunctionIndexerManager::recordLoadedFile(const std::wstring& filename, bool isClassdef)
{
    if (filename.empty()) {
        return;
    }
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    const std::wstring normalizedFilename = FileSystemWrapper::Path::normalize(filename);
    bool watched = (_currentPath != nullptr
                       && isInsideDirectory(normalizedFilename, _currentPath->getPath()))
        || (_userPath != nullptr && isInsideDirectory(normalizedFilename, _userPath->getPath()));
    for (size_t k = 0; !watched && k < _pathWatchFuncVector.size(); ++k) {
        watched = isInsideDirectory(normalizedFilename, _pathWatchFuncVector[k]->getPath());
    }
    if (!watched) {
        return;
    }
    LoadedFileStamp stamp;
    if (readFileStamp(normalizedFilename, stamp)) {
        stamp.isClassdef = isClassdef;
        stamp.loadedFilename = filename;
        std::lock_guard<std::mutex> stampsLock(loadedFileStampsMutex());
        loadedFileStamps()[normalizedFilename] = stamp;
    }
}
//=============================================================================
bool
PathFunctionIndexerManager::reloadChangedFiles(bool withClassdefs)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    if (withClassdefs && interpreterThreadId.load() == std::thread::id()) {
        interpreterThreadId.store(std::this_thread::get_id());
    }
    const bool onInterpreterThread = withClassdefs && isInterpreterThread();
    if (onInterpreterThread) {
        applyPendingClassFileChanges();
    }
    const std::vector<LoadedFileStamp> changedFiles = takeChangedLoadedFiles();
    if (changedFiles.empty()) {
        return false;
    }
    dispatchChangedFiles(changedFiles, onInterpreterThread);
    FunctionsInMemory::getInstance()->clearMapCache();
    return true;
}
//=============================================================================
void
PathFunctionIndexerManager::applyPendingClassFileChanges()
{
    if (!classFilesPending.load(std::memory_order_relaxed) || !isInterpreterThread()) {
        return;
    }
    wstringVector filenames;
    {
        std::lock_guard<std::mutex> lock(loadedFileStampsMutex());
        filenames.swap(pendingClassFiles());
        classFilesPending.store(false, std::memory_order_relaxed);
    }
    for (const auto& filename : filenames) {
        forgetClassFile(filename);
    }
}
//=============================================================================
void
PathFunctionIndexerManager::notifyFileWritten(const std::wstring& filename)
{
    if (filename.empty() || !NelsonConfiguration::getInstance()->isFileWatcherEnabled()) {
        return;
    }
    // data files written in a loop must not pay one stat per loaded function
    std::wstring extension = FileSystemWrapper::Path(filename).extension().generic_wstring();
    std::transform(extension.begin(), extension.end(), extension.begin(), ::towlower);
    if (!extension.empty() && extension != L".m" && extension != L".nbc") {
        return;
    }
    {
        std::lock_guard<std::mutex> lock(loadedFileStampsMutex());
        if (loadedFileStamps().empty()) {
            return;
        }
    }
    // the background check would see it too, but only after the next calls
    dispatchChangedFiles(takeChangedLoadedFiles(), isInterpreterThread());
}
//=============================================================================
void
PathFunctionIndexerManager::refreshFunctionsMap()
{
    // addpath, cd and rehash change what resolves: drop generation caches and
    // known misses (a miss recorded before cd hides a new @class overload).
    FunctionsInMemory::getInstance()->clearMapCache();
    std::unordered_map<std::string, FileFunction*> refreshed;
    auto gather = [&refreshed](const PathFunctionIndexer* pathFunc) {
        if (pathFunc == nullptr) {
            return;
        }
        const auto& fileFunctions = pathFunc->getAllFileFunctions();
        refreshed.reserve(refreshed.size() + fileFunctions.size());
        refreshed.insert(fileFunctions.begin(), fileFunctions.end());
    };
    // The current directory shadows the path, so it is gathered first (insert()
    // keeps the first key it sees). which(...,'-all') lists it first too.
    gather(_currentPath);
    for (auto& pathFunc : _pathFuncVector) {
        gather(pathFunc);
    }
    gather(_userPath);

    // The in-memory table is read before the indexes, so retire every name the
    // indexes dropped (file removed, rmpath, cd). Only the key is read: the old
    // file entries may already be freed.
    for (const auto& entry : _pathFuncMap) {
        if (refreshed.find(entry.first) == refreshed.end()) {
            FunctionsInMemory::getInstance()->retireUnresolvable(entry.first);
        }
    }

    // A name now mapped to a different file than the in-memory copy (current
    // directory changed, path reordered) must drop the stale definition, or the
    // table keeps answering with the old file while which() reports the new one.
    for (const std::string& name : FunctionsInMemory::getInstance()->getMacroAndMexNames()) {
        auto it = refreshed.find(name);
        if (it != refreshed.end() && it->second != nullptr) {
            FunctionsInMemory::getInstance()->retireIfFileReplaced(name, it->second->getFilename());
        }
    }
    // a builtin cached before a file of the same name became reachable must not hide it
    FunctionsInMemory::getInstance()->dropShadowedBuiltinReferences(
        [&refreshed](const std::string& name) { return refreshed.count(name) != 0; });
    _pathFuncMap = std::move(refreshed);
}
//=============================================================================
void
PathFunctionIndexerManager::clear()
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    for (std::vector<PathFunctionIndexer*>::reverse_iterator it = _pathFuncVector.rbegin();
        it != _pathFuncVector.rend(); ++it) {
        PathFunctionIndexer* pf = *it;
        if (pf) {
            delete pf;
            pf = nullptr;
        }
    }
    _pathFuncVector.clear();
    _pathWatchFuncVector.clear();
    refreshFunctionsMap();
}
//=============================================================================
wstringVector
PathFunctionIndexerManager::getMacrosList(bool withPrivate)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    wstringVector macros;

    size_t estimatedSize = 0;
    if (_userPath != nullptr) {
        estimatedSize += 100;
    }
    estimatedSize += _pathFuncVector.size() * 50;
    macros.reserve(estimatedSize);

    if (_currentPath != nullptr) {
        wstringVector currentMacros = _currentPath->getFunctionsName(L"", withPrivate);
        macros.insert(macros.end(), std::make_move_iterator(currentMacros.begin()),
            std::make_move_iterator(currentMacros.end()));
    }
    if (_userPath != nullptr) {
        wstringVector userMacros = _userPath->getFunctionsName(L"", withPrivate);
        macros.insert(macros.end(), std::make_move_iterator(userMacros.begin()),
            std::make_move_iterator(userMacros.end()));
    }
    for (auto it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it) {
        const auto* pf = *it;
        if (pf) {
            wstringVector names = pf->getFunctionsName(L"", withPrivate);
            macros.insert(macros.end(), std::make_move_iterator(names.begin()),
                std::make_move_iterator(names.end()));
        }
    }
    std::sort(macros.begin(), macros.end());
    macros.erase(std::unique(macros.begin(), macros.end()), macros.end());
    return macros;
}
//=============================================================================
FunctionDef*
PathFunctionIndexerManager::findAndProcessFile(const std::string& name)
{
    FunctionDef* ptr = nullptr;
    FileFunction* ff = nullptr;
    if (find(name, &ff)) {
        ptr = processFile(ff, name);
    }
    return ptr;
}
//=============================================================================
bool
PathFunctionIndexerManager::find(const std::string& name, FunctionDefPtr& ptr)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    if (findIndexed(name, ptr)) {
        return true;
    }
    if (rehashWatchedPathOwningFile(name)) {
        return findIndexed(name, ptr);
    }
    return false;
}
//=============================================================================
bool
PathFunctionIndexerManager::findIndexed(const std::string& name, FunctionDefPtr& ptr)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    ptr = findAndProcessFile(name);
    return ptr != nullptr;
}
//=============================================================================
// Change notifications are asynchronous, so a file written to a watched
// directory can be looked up before the watcher sees it. On a miss, probe the
// watched directories for a file defining the function and rehash the first
// owner. Only plain identifiers are probed (package, class and private
// functions have their own indexing) and frozen directories never are, so the
// cost is a few file stats on the miss path, and nothing when the watcher is
// disabled.
bool
PathFunctionIndexerManager::rehashWatchedPathOwningFile(const std::string& functionName)
{
    if (functionName.empty() || !NelsonConfiguration::getInstance()->isFileWatcherEnabled()
        || functionName.find_first_of("./\\@") != std::string::npos
        || isEngineSentinelName(functionName)) {
        return false;
    }
    auto probe = [&functionName](PathFunctionIndexer* indexer) {
        if (indexer == nullptr || !indexer->isWithWatcher()) {
            return false;
        }
        std::wstring filename;
        if (!indexer->findFuncName(functionName, filename, true)) {
            return false;
        }
        indexer->rehash();
        return true;
    };
    bool rehashed = probe(_currentPath) || probe(_userPath);
    if (!rehashed) {
        for (auto* pf : _pathWatchFuncVector) {
            if (probe(pf)) {
                rehashed = true;
                break;
            }
        }
    }
    if (rehashed) {
        refreshFunctionsMap();
    }
    return rehashed;
}
//=============================================================================
bool
PathFunctionIndexerManager::find(const std::string& name, FileFunction** ff)
{
    bool res = false;
    auto it = _pathFuncMap.find(name);
    if (it != _pathFuncMap.end()) {
        *ff = it->second;
        res = true;
    }
    return res;
}
//=============================================================================
bool
PathFunctionIndexerManager::find(const std::string& functionName, std::wstring& filename)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    FileFunction* ff = nullptr;
    bool res = find(functionName, &ff);
    if (res) {
        filename = ff->getFilename();
        return true;
    }
    return findPackageClassFile(functionName, &filename);
}
//=============================================================================
bool
PathFunctionIndexerManager::functionFileExists(const std::string& functionName)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    FileFunction* ff = nullptr;
    if (find(functionName, &ff)) {
        return !ff->getFilename().empty();
    }
    return findPackageClassFile(functionName, nullptr);
}
//=============================================================================
bool
PathFunctionIndexerManager::findPackageClassFile(
    const std::string& functionName, std::wstring* filename)
{
    if (functionName.find('.') == std::string::npos) {
        return false;
    }
    auto tryPath = [&](PathFunctionIndexer* indexer) {
        if (indexer == nullptr) {
            return false;
        }
        std::wstring candidate = packageClassFilename(indexer->getPath(), functionName);
        if (!candidate.empty() && FileSystemWrapper::Path::is_regular_file(candidate)) {
            if (filename != nullptr) {
                *filename = candidate;
            }
            return true;
        }
        return false;
    };
    if (tryPath(_currentPath) || tryPath(_userPath)) {
        return true;
    }
    for (auto it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it) {
        if (tryPath(*it)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
PathFunctionIndexerManager::find(const std::string& functionName, wstringVector& filenames)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    auto collect = [&]() {
        filenames.clear();
        filenames.reserve(4);

        std::wstring filename;
        if (_currentPath != nullptr && _currentPath->findFuncName(functionName, filename, false)) {
            filenames.emplace_back(std::move(filename));
        }
        if (_userPath != nullptr && _userPath->findFuncName(functionName, filename, false)) {
            filenames.emplace_back(std::move(filename));
        }
        for (auto it = _pathFuncVector.rbegin(); it != _pathFuncVector.rend(); ++it) {
            const auto* pf = *it;
            if (pf->findFuncName(functionName, filename, false)) {
                filenames.emplace_back(std::move(filename));
            }
        }
        return !filenames.empty();
    };

    // Reading the indexes is free. The file stats that find a file written since
    // the last rehash are paid only on a would-be miss, because which() and
    // exist() ask this question often.
    if (collect()) {
        return true;
    }
    if (rehashWatchedPathOwningFile(functionName)) {
        return collect();
    }
    return false;
}
//=============================================================================
bool
PathFunctionIndexerManager::addPath(const std::wstring& path, bool begin, bool frozen)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    if (!addPathWithoutRefresh(path, begin, frozen)) {
        return false;
    }
    refreshFunctionsMap();
    return true;
}
//=============================================================================
size_t
PathFunctionIndexerManager::addPaths(const wstringVector& paths, bool begin, bool frozen)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    size_t added = 0;
    try {
        for (const auto& path : paths) {
            if (addPathWithoutRefresh(path, begin, frozen)) {
                added++;
            }
        }
    } catch (...) {
        // Keep the map in step with the directories already added, as addPath does.
        if (added > 0) {
            refreshFunctionsMap();
        }
        throw;
    }
    if (added > 0) {
        refreshFunctionsMap();
    }
    return added;
}
//=============================================================================
bool
PathFunctionIndexerManager::addPathWithoutRefresh(const std::wstring& path, bool begin, bool frozen)
{
    const auto normalizedPath = FileSystemWrapper::Path::normalize(path);

    const std::string identity = PathFunctionIndexer::directoryIdentity(normalizedPath);
    auto it = std::find_if(_pathFuncVector.begin(), _pathFuncVector.end(),
        [&normalizedPath, &identity](
            const PathFunctionIndexer* x) { return x->isSameDirectory(normalizedPath, identity); });

    if (it != _pathFuncVector.end()) {
        return false;
    }

    try {
        auto pf = std::make_unique<PathFunctionIndexer>(normalizedPath, frozen ? false : true);

        if (_filesWatcherStarted) {
            pf->startFileWatcher();
        }

        auto* rawPtr = pf.release();
        if (begin) {
            _pathFuncVector.insert(_pathFuncVector.begin(), rawPtr);
        } else {
            _pathFuncVector.emplace_back(rawPtr);
        }

        if (!frozen) {
            _pathWatchFuncVector.emplace_back(rawPtr);
        }
        return true;
    } catch (const std::bad_alloc&) {
        return false;
    }
}
//=============================================================================
bool
PathFunctionIndexerManager::removePath(const std::wstring& path)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    const std::wstring normalizedPath = FileSystemWrapper::Path::normalize(path);
    const std::string identity = PathFunctionIndexer::directoryIdentity(normalizedPath);
    auto it = std::find_if(_pathFuncVector.begin(), _pathFuncVector.end(),
        [&normalizedPath, &identity](
            const PathFunctionIndexer* x) { return x->isSameDirectory(normalizedPath, identity); });

    if (it == _pathFuncVector.end()) {
        return false;
    }

    PathFunctionIndexer* pf = *it;
    if (pf != nullptr) {
        delete pf;
        _pathFuncVector.erase(it);

        _pathWatchFuncVector.clear();
        _pathWatchFuncVector.reserve(_pathFuncVector.size());
        for (const auto* pathFunc : _pathFuncVector) {
            if (pathFunc->isWithWatcher()) {
                _pathWatchFuncVector.emplace_back(const_cast<PathFunctionIndexer*>(pathFunc));
            }
        }
        refreshFunctionsMap();
        return true;
    }
    return false;
}
//=============================================================================
wstringVector
PathFunctionIndexerManager::getPathNameVector(bool watchedOnly)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    wstringVector list;

    if (watchedOnly) {
        list.reserve(_pathWatchFuncVector.size() + (_userPath != nullptr ? 1 : 0));
        if (_userPath != nullptr) {
            list.emplace_back(_userPath->getPath());
        }
        for (const auto* pf : _pathWatchFuncVector) {
            if (pf) {
                list.emplace_back(pf->getPath());
            }
        }
    } else {
        list.reserve(_pathFuncVector.size() + (_userPath != nullptr ? 1 : 0));
        if (_userPath != nullptr) {
            list.emplace_back(_userPath->getPath());
        }
        for (const auto* pf : _pathFuncVector) {
            if (pf) {
                list.emplace_back(pf->getPath());
            }
        }
    }
    return list;
}
//=============================================================================
void
PathFunctionIndexerManager::rehash()
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    clearCurrentPathCache();
    if (_currentPath != nullptr) {
        _currentPath->rehash();
    }
    if (_userPath != nullptr) {
        _userPath->rehash();
    }
    for (std::vector<PathFunctionIndexer*>::reverse_iterator it = _pathFuncVector.rbegin();
        it != _pathFuncVector.rend(); ++it) {
        PathFunctionIndexer* pf = *it;
        if (pf) {
            pf->rehash();
        }
    }
    refreshFunctionsMap();
    reloadChangedFiles(true);
}
//=============================================================================
void
PathFunctionIndexerManager::rehash(const std::wstring& path)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    if (_currentPath != nullptr) {
        FileSystemWrapper::Path p1 { _currentPath->getPath() }, p2 { path };
        if (FileSystemWrapper::Path::equivalent(p1, p2)) {
            _currentPath->rehash();
            return;
        }
    }
    if (_userPath != nullptr) {
        FileSystemWrapper::Path p1 { _userPath->getPath() }, p2 { path };
        if (FileSystemWrapper::Path::equivalent(p1, p2)) {
            _userPath->rehash();
            return;
        }
    }
    const std::wstring normalizedPath = FileSystemWrapper::Path::normalize(path);
    const std::string identity = PathFunctionIndexer::directoryIdentity(normalizedPath);
    for (std::vector<PathFunctionIndexer*>::reverse_iterator it = _pathFuncVector.rbegin();
        it != _pathFuncVector.rend(); ++it) {
        PathFunctionIndexer* pf = *it;
        if (pf) {
            if (pf->isSameDirectory(normalizedPath, identity)) {
                pf->rehash();
                return;
            }
        }
    }
    refreshFunctionsMap();
}
//=============================================================================
std::wstring
PathFunctionIndexerManager::getPathNameAsString()
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    std::wstring result;
    result.reserve(1024);

#ifdef _MSC_VER
    constexpr wchar_t separator = L';';
#else
    constexpr wchar_t separator = L':';
#endif

    if (_userPath != nullptr && !_userPath->getPath().empty()) {
        result += _userPath->getPath();
        result += separator;
    }

    for (const auto* pf : _pathFuncVector) {
        if (pf) {
            result += pf->getPath();
            result += separator;
        }
    }

    if (!result.empty() && result.back() == separator) {
        result.pop_back();
    }
    return result;
}
//=============================================================================
bool
PathFunctionIndexerManager::isAvailablePath(const std::wstring& path)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    if (_currentPath != nullptr && _currentPath->getPath() == path) {
        return true;
    }

    if (_userPath != nullptr && _userPath->getPath() == path) {
        return true;
    }

    return std::any_of(_pathFuncVector.begin(), _pathFuncVector.end(),
        [&path](const PathFunctionIndexer* x) { return x && x->getPath() == path; });
}
//=============================================================================
FunctionDef*
PathFunctionIndexerManager::processFile(FileFunction* ff, const std::string& functionName)
{
    FunctionDef* ptr = nullptr;
    if (ff != nullptr) {
        if (ff->isMex()) {
            ptr = processMexFile(ff->getFilename(), ff->getName(), ff->isOverload());
        } else if (!ff->isOverload() && classdefIsClassdefFile(ff->getFilename())) {
            ptr = classdefLoadConstructor(ff->getFilename(), functionName);
        } else {
            if (ff->getWithWatcher()) {
                // stamped before parsing: a write during the parse is seen as a change
                recordLoadedFile(ff->getFilename(), false);
            }
            ptr = processMacroFile(ff->getFilename(), ff->getWithWatcher(), ff->isOverload());
            auto* macro = dynamic_cast<MacroFunctionDef*>(ptr);
            std::string ownerClassName = overloadOwnerClassName(functionName);
            if (ownerClassName.empty() && ff != nullptr) {
                // Registered under its plain name inside an @Class directory:
                // the parent folder gives the owner.
                ownerClassName = ownerClassNameFromFilename(ff->getFilename());
            }
            // Also covers classic (struct-based) classes with no classdef file:
            // accesses inside a method then resolve against the object's own data
            // rather than its overloaded subsref.
            if (macro != nullptr && !ownerClassName.empty()) {
                macro->classdefOwnerClassName = ownerClassName;
            }
        }
    }
    return ptr;
}
//=============================================================================
MexFunctionDef*
PathFunctionIndexerManager::processMexFile(
    const std::wstring& filename, const std::wstring& functionName, bool isOverload)
{
    MexFunctionDef* fptr = nullptr;
    try {
        fptr = new MexFunctionDef(filename, functionName, isOverload);
        if (!fptr->isLoaded()) {
            delete fptr;
            fptr = nullptr;
        }
    } catch (const std::bad_alloc&) {
        fptr = nullptr;
    }
    return fptr;
}
//=============================================================================
MacroFunctionDef*
PathFunctionIndexerManager::processMacroFile(
    const std::wstring& script_filename, bool withWatcher, bool isOverload)
{
    MacroFunctionDef* fptr = nullptr;
    try {
        fptr = new MacroFunctionDef(script_filename, withWatcher, isOverload, true);
    } catch (const std::bad_alloc&) {
        fptr = nullptr;
    }
    return fptr;
}
//=============================================================================
void
PathFunctionIndexerManager::clearCache()
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    classdefClear();
    FunctionsInMemory::getInstance()->clear();
    refreshFunctionsMap();
}
//=============================================================================
void
PathFunctionIndexerManager::clearCache(const stringVector& exceptedFunctions)
{
    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    classdefClear();
    FunctionsInMemory::getInstance()->clear(exceptedFunctions);
    refreshFunctionsMap();
}
//=============================================================================
// A name wrapped in double underscores is an engine sentinel
// (__classdef_value__, __classdef_property_default__, ...): the interpreter
// dispatches it itself and it never names a file. Probing the watched
// directories for one costs two file stats per directory, and building a
// classdef object looks one up twice (122 us with the watcher on, 10 us off,
// at about 20 us per stat).
static bool
isEngineSentinelName(const std::string& functionName)
{
    return functionName.size() > 4 && functionName.compare(0, 2, "__") == 0
        && functionName.compare(functionName.size() - 2, 2, "__") == 0;
}
//=============================================================================
// The interpreter asks for a poll of the watched directories on every name
// resolution, and a poll costs one file stat per directory, so resolution time
// would grow with the length of the path. A new file is found by the probe on
// the miss path, and only a removed file needs the poll, promptly but not
// instantly. One poll per WATCHED_DIRECTORIES_POLL_INTERVAL bounds the cost to
// a fixed rate.
static bool
mustPollWatchedDirectories()
{
    constexpr std::chrono::steady_clock::duration WATCHED_DIRECTORIES_POLL_INTERVAL
        = std::chrono::milliseconds(500);
    static std::atomic<std::chrono::steady_clock::rep> lastPoll { 0 };
    const std::chrono::steady_clock::rep now
        = std::chrono::steady_clock::now().time_since_epoch().count();
    std::chrono::steady_clock::rep previous = lastPoll.load(std::memory_order_relaxed);
    if (now - previous < WATCHED_DIRECTORIES_POLL_INTERVAL.count()) {
        return false;
    }
    // A poll that loses the exchange has just been done by another thread.
    return lastPoll.compare_exchange_strong(previous, now, std::memory_order_relaxed);
}
//=============================================================================
// heap state never freed: the monitor thread may still run while statics are destroyed
static std::unordered_map<std::wstring, LoadedFileStamp>&
loadedFileStamps()
{
    static auto* stamps = new std::unordered_map<std::wstring, LoadedFileStamp>();
    return *stamps;
}
//=============================================================================
static std::mutex&
loadedFileStampsMutex()
{
    static auto* mutex = new std::mutex();
    return *mutex;
}
//=============================================================================
// guarded by loadedFileStampsMutex
static wstringVector&
pendingClassFiles()
{
    static auto* filenames = new wstringVector();
    return *filenames;
}
//=============================================================================
static bool
isInterpreterThread()
{
    return interpreterThreadId.load(std::memory_order_relaxed) == std::this_thread::get_id();
}
//=============================================================================
static void
forgetClassFile(const std::wstring& filename)
{
    FunctionsInMemory::getInstance()->retireFileDefinitions(filename);
    classdefForgetClassFile(filename);
    FunctionsInMemory::getInstance()->clearMapCache();
}
//=============================================================================
static void
dispatchChangedFiles(const std::vector<LoadedFileStamp>& changedFiles, bool onInterpreterThread)
{
    bool queued = false;
    for (const auto& changed : changedFiles) {
        if (!changed.isClassdef) {
            FunctionsInMemory::getInstance()->retireFileDefinitions(changed.loadedFilename);
        } else if (onInterpreterThread) {
            forgetClassFile(changed.loadedFilename);
        } else {
            std::lock_guard<std::mutex> lock(loadedFileStampsMutex());
            pendingClassFiles().push_back(changed.loadedFilename);
            classFilesPending.store(true, std::memory_order_relaxed);
            queued = true;
        }
    }
    if (queued) {
        // cached call sites look the names up again, which applies the pending classes
        FunctionsInMemory::getInstance()->clearMapCache();
    }
}
//=============================================================================
// Stats the loaded files outside the lock, then takes the ones still stamped as read.
static std::vector<LoadedFileStamp>
takeChangedLoadedFiles()
{
    std::vector<std::pair<std::wstring, LoadedFileStamp>> snapshot;
    {
        std::lock_guard<std::mutex> lock(loadedFileStampsMutex());
        snapshot.assign(loadedFileStamps().begin(), loadedFileStamps().end());
    }
    std::vector<LoadedFileStamp> changedFiles;
    for (const auto& entry : snapshot) {
        LoadedFileStamp current;
        if (readFileStamp(entry.first, current) && current.writeTime == entry.second.writeTime
            && current.size == entry.second.size) {
            continue;
        }
        std::lock_guard<std::mutex> lock(loadedFileStampsMutex());
        auto it = loadedFileStamps().find(entry.first);
        // a reload since the snapshot recorded a new stamp: that definition is current
        if (it == loadedFileStamps().end() || it->second.writeTime != entry.second.writeTime
            || it->second.size != entry.second.size) {
            continue;
        }
        changedFiles.push_back(it->second);
        loadedFileStamps().erase(it);
    }
    return changedFiles;
}
//=============================================================================
struct LoadedFilesMonitor
{
    std::mutex mutex;
    std::condition_variable wakeUp;
    std::thread worker;
    bool running = false;
    bool stopRequested = false;
};
//=============================================================================
static LoadedFilesMonitor&
loadedFilesMonitor()
{
    static auto* monitor = new LoadedFilesMonitor();
    return *monitor;
}
//=============================================================================
// An edited file changes no directory time: a background thread compares the loaded
// files and retires the changed definitions, so calls pay nothing and the next one reloads.
static void
startLoadedFilesMonitor()
{
#if WITH_FILE_WATCHER
    LoadedFilesMonitor& monitor = loadedFilesMonitor();
    std::lock_guard<std::mutex> lock(monitor.mutex);
    if (monitor.running) {
        return;
    }
    monitor.stopRequested = false;
    monitor.running = true;
    monitor.worker = std::thread([&monitor]() {
        constexpr std::chrono::milliseconds LOADED_FILES_POLL_INTERVAL(100);
        std::unique_lock<std::mutex> monitorLock(monitor.mutex);
        while (!monitor.wakeUp.wait_for(monitorLock, LOADED_FILES_POLL_INTERVAL,
            [&monitor]() { return monitor.stopRequested; })) {
            monitorLock.unlock();
            dispatchChangedFiles(takeChangedLoadedFiles(), false);
            monitorLock.lock();
        }
    });
#endif
}
//=============================================================================
static void
stopLoadedFilesMonitor()
{
    LoadedFilesMonitor& monitor = loadedFilesMonitor();
    {
        std::lock_guard<std::mutex> lock(monitor.mutex);
        if (!monitor.running) {
            return;
        }
        monitor.stopRequested = true;
        monitor.running = false;
    }
    monitor.wakeUp.notify_all();
    if (monitor.worker.joinable()) {
        monitor.worker.join();
    }
}
//=============================================================================
static bool
readFileStamp(const std::wstring& filename, LoadedFileStamp& stamp)
{
    std::error_code errorCode;
    const nfs::path path(FileSystemWrapper::Path(filename).native());
    stamp.writeTime = nfs::last_write_time(path, errorCode);
    if (errorCode) {
        return false;
    }
    stamp.size = nfs::file_size(path, errorCode);
    return !errorCode;
}
//=============================================================================
static bool
isInsideDirectory(const std::wstring& normalizedFilename, const std::wstring& directory)
{
    if (directory.empty()) {
        return false;
    }
    const std::wstring normalizedDirectory = FileSystemWrapper::Path::normalize(directory);
    if (normalizedFilename.size() <= normalizedDirectory.size()) {
        return false;
    }
    const wchar_t separator = normalizedFilename[normalizedDirectory.size()];
    if (separator != L'/' && separator != L'\\' && normalizedDirectory.back() != L'/'
        && normalizedDirectory.back() != L'\\') {
        return false;
    }
#ifdef _MSC_VER
    return std::equal(normalizedDirectory.begin(), normalizedDirectory.end(),
        normalizedFilename.begin(),
        [](wchar_t a, wchar_t b) { return towlower(a) == towlower(b); });
#else
    return normalizedFilename.compare(0, normalizedDirectory.size(), normalizedDirectory) == 0;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
