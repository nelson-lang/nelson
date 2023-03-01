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
#include "PathFuncManager.hpp"
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
PathFuncManager* PathFuncManager::m_pInstance = nullptr;
//=============================================================================
PathFuncManager::PathFuncManager()
{
    _userPath = nullptr;
    _currentPath = nullptr;
    userpathCompute();
}
//=============================================================================
PathFuncManager::~PathFuncManager()
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
PathFuncManager*
PathFuncManager::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new PathFuncManager();
    }
    return m_pInstance;
}
//=============================================================================
void
PathFuncManager::destroy()
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
PathFuncManager::clear()
{
    for (std::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunc* pf = *it;
        if (pf) {
            delete pf;
            pf = nullptr;
        }
    }
    _pathFuncVector.clear();
}
//=============================================================================
wstringVector
PathFuncManager::getMacrosList(const std::wstring& prefix)
{
    wstringVector macros;
    if (_userPath != nullptr) {
        wstringVector userMacros = _userPath->getFunctionsName();
        macros.insert(macros.end(), userMacros.begin(), userMacros.end());
    }
    for (std::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunc* pf = *it;
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
PathFuncManager::findAndProcessFile(const std::string& name)
{
    FunctionDef* ptr = nullptr;
    FileFunction* ff = nullptr;
    std::wstring functionName = utf8_to_wstring(name);
    if (find(functionName, &ff)) {
        ptr = processFile(ff, name);
    }
    return ptr;
}
//=============================================================================
bool
PathFuncManager::find(const std::string& name, FunctionDefPtr& ptr)
{
    ptr = findAndProcessFile(name);
    if (ptr != nullptr) {
        return true;
    }
    return false;
}
//=============================================================================
bool
PathFuncManager::find(const std::wstring& functionName, FileFunction** ff)
{
    bool res = false;
    if (_currentPath != nullptr) {
        res = _currentPath->findFuncName(functionName, ff);
        if (res) {
            return res;
        }
    }
    if (_userPath != nullptr) {
        res = _userPath->findFuncName(functionName, ff);
        if (res) {
            return res;
        }
    }
    for (auto pf : _pathFuncVector) {
        if (pf) {
            res = pf->findFuncName(functionName, ff);
            if (res) {
                return res;
            }
        }
    }
    return res;
}
//=============================================================================
bool
PathFuncManager::find(const std::wstring& functionName, std::wstring& filename)
{
    if (_currentPath != nullptr) {
        if (_currentPath->findFuncName(functionName, filename)) {
            return true;
        }
    }
    if (_userPath != nullptr) {
        if (_userPath->findFuncName(functionName, filename)) {
            return true;
        }
    }
    for (std::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunc* pf = *it;
        if (pf->findFuncName(functionName, filename)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
PathFuncManager::find(const std::wstring& functionName, wstringVector& filesname)
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
    for (std::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunc* pf = *it;
        if (pf->findFuncName(functionName, filename)) {
            filesname.push_back(filename);
        }
    }
    return (filesname.size() > 0);
}
//=============================================================================
bool
PathFuncManager::addPath(const std::wstring& path, bool begin, bool frozen)
{
    bool res = false;

    std::vector<PathFunc*>::iterator it = std::find_if(_pathFuncVector.begin(),
        _pathFuncVector.end(),
        [path](PathFunc* x) { return FileSystemWrapper::Path::equivalent(x->getPath(), path); });
    if (it != _pathFuncVector.end()) {
        return false;
    }
    bool withWatch = frozen ? false : true;
    PathFunc* pf = nullptr;
    try {
        pf = new PathFunc(FileSystemWrapper::Path::normalize(path), withWatch);
    } catch (const std::bad_alloc&) {
        pf = nullptr;
    }
    if (pf != nullptr) {
        FunctionsInMemory::getInstance()->clearMapCache();
        if (begin) {
            _pathFuncVector.insert(_pathFuncVector.begin(), pf);
        } else {
            _pathFuncVector.push_back(pf);
        }
        res = true;
    }
    return res;
}
//=============================================================================
bool
PathFuncManager::removePath(const std::wstring& path)
{
    bool res = false;
    std::vector<PathFunc*>::iterator it = std::find_if(_pathFuncVector.begin(),
        _pathFuncVector.end(),
        [path](PathFunc* x) { return FileSystemWrapper::Path::equivalent(x->getPath(), path); });

    if (it != _pathFuncVector.end()) {
        PathFunc* pf = *it;
        if (pf != nullptr) {
            FunctionsInMemory::getInstance()->clearMapCache();
            PathFunc* pf = *it;
            delete pf;
            _pathFuncVector.erase(it);
            return true;
        }
    }
    return res;
}
//=============================================================================
wstringVector
PathFuncManager::getPathNameVector()
{
    wstringVector list;
    if (_userPath != nullptr) {
        list.emplace_back(_userPath->getPath());
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
PathFuncManager::getUserPath()
{
    std::wstring _path = L"";
    if (_userPath != nullptr) {
        _path = _userPath->getPath();
    }
    return _path;
}
//=============================================================================
bool
PathFuncManager::setCurrentUserPath(const std::wstring& path)
{
    const std::wstring normalizedPath = FileSystemWrapper::Path::normalize(path);
    if (_currentPath != nullptr) {
        if (FileSystemWrapper::Path::equivalent(normalizedPath, _currentPath->getPath())) {
            return true;
        }
        delete _currentPath;
    }
    try {
        _currentPath = new PathFunc(normalizedPath);
        _currentPath->rehash();
    } catch (const std::bad_alloc&) {
        _currentPath = nullptr;
    }
    return false;
}
//=============================================================================
bool
PathFuncManager::setUserPath(const std::wstring& path, bool saveToFile)
{
    clearUserPath();
    if (_userPath == nullptr) {
        const std::wstring normalizedPath = FileSystemWrapper::Path::normalize(path);
        _userPath = new PathFunc(normalizedPath);
    }
    if (saveToFile) {
        saveUserPathToFile();
    }
    return true;
}
//=============================================================================
void
PathFuncManager::clearUserPath(bool saveToFile)
{
    if (_userPath != nullptr) {
        delete _userPath;
        _userPath = nullptr;
    }
    if (saveToFile) {
        saveUserPathToFile();
    }
}
//=============================================================================
void
PathFuncManager::resetUserPath()
{
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    FileSystemWrapper::Path p(userPathFile);
    FileSystemWrapper::Path::remove(p);
    userpathCompute();
}
//=============================================================================
void
PathFuncManager::rehash()
{
    if (_currentPath != nullptr) {
        _currentPath->rehash();
    }
    if (_userPath != nullptr) {
        _userPath->rehash();
    }
    for (std::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunc* pf = *it;
        if (pf) {
            pf->rehash();
        }
    }
}
//=============================================================================
void
PathFuncManager::rehash(const std::wstring& path)
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
    for (std::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunc* pf = *it;
        if (pf) {
            FileSystemWrapper::Path p1 { pf->getPath() }, p2 { path };
            if (FileSystemWrapper::Path::equivalent(p1, p2)) {
                pf->rehash();
                return;
            }
        }
    }
}
//=============================================================================
std::wstring
PathFuncManager::getPathNameAsString()
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
PathFuncManager::isAvailablePath(const std::wstring& path)
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
    std::vector<PathFunc*>::iterator it = std::find_if(_pathFuncVector.begin(),
        _pathFuncVector.end(), [path](PathFunc* x) { return x->getPath() == path; });
    return (it != _pathFuncVector.end());
}
//=============================================================================
FunctionDef*
PathFuncManager::processFile(FileFunction* ff, const std::string& functionName)
{
    FunctionDef* ptr = nullptr;
    if (ff != nullptr) {
        if (ff->isMex()) {
            ptr = processMexFile(ff->getFilename(), ff->getName());
        } else {
            ptr = processMacroFile(ff->getFilename(), ff->getWithWatcher());
        }
    }
    return ptr;
}
//=============================================================================
MexFunctionDef*
PathFuncManager::processMexFile(const std::wstring& filename, const std::wstring& functionName)
{
    MexFunctionDef* fptr = nullptr;
    try {
        fptr = new MexFunctionDef(filename, functionName);
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
PathFuncManager::processMacroFile(const std::wstring& script_filename, bool withWatcher)
{
    MacroFunctionDef* fptr = nullptr;
    try {
        fptr = new MacroFunctionDef(script_filename, withWatcher);
    } catch (const std::bad_alloc&) {
        fptr = nullptr;
    }
    return fptr;
}
//=============================================================================
void
PathFuncManager::clearCache()
{
    FunctionsInMemory::getInstance()->clear();
}
//=============================================================================
void
PathFuncManager::clearCache(const stringVector& exceptedFunctions)
{
    FunctionsInMemory::getInstance()->clear(exceptedFunctions);
}
//=============================================================================
void
PathFuncManager::userpathCompute()
{
    clearUserPath();
    std::wstring userpathEnv = GetVariableEnvironment(L"NELSON_USERPATH", L"");
    bool bSet = false;
    if (!userpathEnv.empty()) {
        if (FileSystemWrapper::Path::is_directory(userpathEnv)) {
            setUserPath(userpathEnv);
            bSet = true;
        }
    }
    if (!bSet) {
        std::wstring prefDir;
        std::wstring userPathFile;
        try {
            prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
        } catch (const Exception&) {
            prefDir.clear();
        }

        userPathFile = prefDir + L"/userpath.conf";
        bool bIsFile = FileSystemWrapper::Path::is_regular_file(userPathFile);
        if (bIsFile) {
            std::wstring preferedUserPath = loadUserPathFromFile();
            if (!preferedUserPath.empty()) {
                if (FileSystemWrapper::Path::is_directory(preferedUserPath)) {
                    setUserPath(preferedUserPath);
                    bSet = true;
                }
            } else {
                bSet = true;
            }
        }
    }
    if (!bSet) {
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
}
//=============================================================================
std::wstring
PathFuncManager::loadUserPathFromFile()
{
    std::wstring preferedUserPath = L"";
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(userPathFile);
    if (bIsFile) {
        std::string tmpline;
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
PathFuncManager::saveUserPathToFile()
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
