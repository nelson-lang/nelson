//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cerrno>
#include <algorithm>
#include <fstream>
#include <nlohmann/json.hpp>
#include "PathFunctionIndexerManager.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserInterface.hpp"
#include "GetVariableEnvironment.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "NelsonConfiguration.hpp"
#include "FunctionsInMemory.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PathFunctionIndexerManager* PathFunctionIndexerManager::m_pInstance = nullptr;
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
}
//=============================================================================
bool
PathFunctionIndexerManager::hashOnFileWatcher()
{
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
PathFunctionIndexerManager::refreshFunctionsMap()
{
    _pathFuncMap.clear();
    for (auto& pf : _pathFuncVector) {
        auto fileFunctions = pf->getAllFileFunctions();
        _pathFuncMap.reserve(_pathFuncMap.size() + fileFunctions.size());
        _pathFuncMap.insert(fileFunctions.begin(), fileFunctions.end());
    }
    if (_userPath) {
        auto fileFunctions = _userPath->getAllFileFunctions();
        _pathFuncMap.reserve(_pathFuncMap.size() + fileFunctions.size());
        _pathFuncMap.insert(fileFunctions.begin(), fileFunctions.end());
    }
    if (_currentPath) {
        auto fileFunctions = _currentPath->getAllFileFunctions();
        _pathFuncMap.reserve(_pathFuncMap.size() + fileFunctions.size());
        _pathFuncMap.insert(fileFunctions.begin(), fileFunctions.end());
    }
}
//=============================================================================
void
PathFunctionIndexerManager::clear()
{
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
PathFunctionIndexerManager::getMacrosList()
{
    wstringVector macros;
    if (_userPath != nullptr) {
        wstringVector userMacros = _userPath->getFunctionsName();
        macros.insert(macros.end(), userMacros.begin(), userMacros.end());
    }
    for (std::vector<PathFunctionIndexer*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunctionIndexer* pf = *it;
        if (pf) {
            wstringVector names = pf->getFunctionsName();
            macros.insert(macros.end(), names.begin(), names.end());
        }
    }
    sort(macros.begin(), macros.end());
    macros.erase(unique(macros.begin(), macros.end()), macros.end());
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
    ptr = findAndProcessFile(name);
    if (ptr != nullptr) {
        return true;
    }
    return false;
}
//=============================================================================
bool
PathFunctionIndexerManager::find(const std::string& functionName, FileFunction** ff)
{
    bool res = false;
    auto it = _pathFuncMap.find(functionName);
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
    FileFunction* ff = nullptr;
    bool res = find(functionName, &ff);
    if (res) {
        filename = ff->getFilename();
    }
    return res;
}
//=============================================================================
bool
PathFunctionIndexerManager::find(const std::string& functionName, wstringVector& filesname)
{
    filesname.clear();
    std::wstring filename;
    if (_currentPath != nullptr) {
        if (_currentPath->findFuncName(functionName, filename)) {
            filesname.push_back(filename);
        }
    }
    if (_userPath != nullptr) {
        if (_userPath->findFuncName(functionName, filename)) {
            filesname.push_back(filename);
        }
    }
    for (std::vector<PathFunctionIndexer*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunctionIndexer* pf = *it;
        if (pf->findFuncName(functionName, filename)) {
            filesname.push_back(filename);
        }
    }
    return (filesname.size() > 0);
}
//=============================================================================
bool
PathFunctionIndexerManager::addPath(const std::wstring& path, bool begin, bool frozen)
{
    bool res = false;
    auto it = std::find_if(
        _pathFuncVector.begin(), _pathFuncVector.end(), [path](PathFunctionIndexer* x) {
            return FileSystemWrapper::Path::equivalent(x->getPath(), path);
        });
    if (it != _pathFuncVector.end()) {
        return false;
    }

    try {
        auto pf = new PathFunctionIndexer(
            FileSystemWrapper::Path::normalize(path), frozen ? false : true);
        if (_filesWatcherStarted) {
            pf->startFileWatcher();
        }
        FunctionsInMemory::getInstance()->clearMapCache();
        if (begin) {
            _pathFuncVector.insert(_pathFuncVector.begin(), pf);
        } else {
            _pathFuncVector.push_back(pf);
        }
        if (!frozen) {
            _pathWatchFuncVector.push_back(pf);
        }
        refreshFunctionsMap();
        res = true;
    } catch (const std::bad_alloc&) {
        res = false;
    }
    return res;
}
//=============================================================================
bool
PathFunctionIndexerManager::removePath(const std::wstring& path)
{
    bool res = false;
    std::vector<PathFunctionIndexer*>::iterator it = std::find_if(
        _pathFuncVector.begin(), _pathFuncVector.end(), [path](PathFunctionIndexer* x) {
            return FileSystemWrapper::Path::equivalent(x->getPath(), path);
        });

    if (it != _pathFuncVector.end()) {
        PathFunctionIndexer* pf = *it;
        if (pf != nullptr) {
            FunctionsInMemory::getInstance()->clearMapCache();
            delete pf;
            _pathFuncVector.erase(it);
            _pathWatchFuncVector.clear();
            for (auto itw = _pathFuncVector.begin(); itw != _pathFuncVector.end(); ++itw) {
                PathFunctionIndexer* pf = *itw;
                if (pf->isWithWatcher()) {
                    _pathWatchFuncVector.push_back(pf);
                }
            }
            refreshFunctionsMap();
            return true;
        }
    }
    return res;
}
//=============================================================================
wstringVector
PathFunctionIndexerManager::getPathNameVector(bool watchedOnly)
{
    wstringVector list;
    if (_userPath != nullptr) {
        list.emplace_back(_userPath->getPath());
    }
    if (watchedOnly) {
        for (auto pf : _pathWatchFuncVector) {
            if (pf) {
                list.emplace_back(pf->getPath());
            }
        }
        return list;
    }
    for (auto pf : _pathFuncVector) {
        if (pf) {
            list.emplace_back(pf->getPath());
        }
    }
    return list;
}
//=============================================================================
std::wstring
PathFunctionIndexerManager::getUserPath()
{
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
    const std::wstring normalizedPath = FileSystemWrapper::Path::normalize(path);
    if (_currentPath != nullptr) {
        if (FileSystemWrapper::Path::equivalent(normalizedPath, _currentPath->getPath())) {
            return true;
        }
        delete _currentPath;
    }
    try {
        _currentPath = new PathFunctionIndexer(normalizedPath);
        _currentPath->rehash();
        if (_filesWatcherStarted) {
            _currentPath->startFileWatcher();
        }
    } catch (const std::bad_alloc&) {
        _currentPath = nullptr;
    }
    refreshFunctionsMap();
    return false;
}
//=============================================================================
bool
PathFunctionIndexerManager::setUserPath(const std::wstring& path, bool saveToFile)
{
    clearUserPath();
    if (_userPath == nullptr) {
        const std::wstring normalizedPath = FileSystemWrapper::Path::normalize(path);
        _userPath = new PathFunctionIndexer(normalizedPath);
        _userPath->rehash();
        if (_filesWatcherStarted) {
            _currentPath->startFileWatcher();
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
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    FileSystemWrapper::Path p(userPathFile);
    FileSystemWrapper::Path::remove(p);
    userpathCompute();
    refreshFunctionsMap();
}
//=============================================================================
void
PathFunctionIndexerManager::rehash()
{
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
}
//=============================================================================
void
PathFunctionIndexerManager::rehash(const std::wstring& path)
{
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
    for (std::vector<PathFunctionIndexer*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunctionIndexer* pf = *it;
        if (pf) {
            FileSystemWrapper::Path p1 { pf->getPath() }, p2 { path };
            if (FileSystemWrapper::Path::equivalent(p1, p2)) {
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
    std::wstring p = L"";
    if (_userPath != nullptr) {
        if (!_userPath->getPath().empty()) {
#ifdef _MSC_VER
            p = _userPath->getPath() + L";";
#else
            p = _userPath->getPath() + L":";
#endif
        }
    }
    for (auto pf : _pathFuncVector) {
        if (pf) {
#ifdef _MSC_VER
            p = p + pf->getPath() + L";";
#else
            p = p + pf->getPath() + L":";
#endif
        }
    }
#ifdef _MSC_VER
    if (StringHelpers::ends_with(p, L";"))
#else
    if (StringHelpers::ends_with(p, L":"))
#endif
    {
        p.pop_back();
    }
    return p;
}
//=============================================================================
bool
PathFunctionIndexerManager::isAvailablePath(const std::wstring& path)
{
    if (_currentPath != nullptr) {
        if (_currentPath->getPath() == path) {
            return true;
        }
    }

    if (_userPath != nullptr) {
        if (_userPath->getPath() == path) {
            return true;
        }
    }
    std::vector<PathFunctionIndexer*>::iterator it = std::find_if(_pathFuncVector.begin(),
        _pathFuncVector.end(), [path](PathFunctionIndexer* x) { return x->getPath() == path; });
    return (it != _pathFuncVector.end());
}
//=============================================================================
FunctionDef*
PathFunctionIndexerManager::processFile(FileFunction* ff, const std::string& functionName)
{
    FunctionDef* ptr = nullptr;
    if (ff != nullptr) {
        if (ff->isMex()) {
            ptr = processMexFile(ff->getFilename(), ff->getName(), ff->isOverload());
        } else {
            ptr = processMacroFile(ff->getFilename(), ff->getWithWatcher(), ff->isOverload());
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
        fptr = new MacroFunctionDef(script_filename, withWatcher, isOverload);
    } catch (const std::bad_alloc&) {
        fptr = nullptr;
    }
    return fptr;
}
//=============================================================================
void
PathFunctionIndexerManager::clearCache()
{
    FunctionsInMemory::getInstance()->clear();
    refreshFunctionsMap();
}
//=============================================================================
void
PathFunctionIndexerManager::clearCache(const stringVector& exceptedFunctions)
{
    FunctionsInMemory::getInstance()->clear(exceptedFunctions);
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
    } catch (const Exception&) {
        prefDir.clear();
    }
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    if (FileSystemWrapper::Path::is_regular_file(userPathFile)) {
        std::wstring preferedUserPath = loadUserPathFromFile();
        if (!preferedUserPath.empty() && FileSystemWrapper::Path::is_directory(preferedUserPath)) {
            setUserPath(preferedUserPath);
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
    std::wstring preferedUserPath = L"";
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
                std::string _preferedUserPath = data["userpath"];
                preferedUserPath = utf8_to_wstring(_preferedUserPath);
            } catch (const nlohmann::json::exception&) {
            }
            jsonFile.close();
        }
    }
    return preferedUserPath;
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
