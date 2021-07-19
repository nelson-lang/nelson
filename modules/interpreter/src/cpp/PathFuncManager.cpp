//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <cerrno>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <algorithm>
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
#include "MacroFunctionDef.hpp"
#include "ParserInterface.hpp"
#include "GetVariableEnvironment.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "NelsonConfiguration.hpp"
#include "FunctionsInMemory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PathFuncManager* PathFuncManager::m_pInstance = nullptr;
//=============================================================================
static bool
isSamePath(const std::wstring& p1, const std::wstring& p2)
{
    if (p1.compare(p2) == 0) {
        return true;
    }
    boost::filesystem::path _path1 = p1;
    boost::filesystem::path _path2 = p2;
    if (boost::filesystem::equivalent(p1, p2)) {
        return true;
    }
    return false;
}
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (myline.size() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
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
    for (boost::container::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
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
    for (boost::container::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
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
    for (boost::container::vector<PathFunc*>::iterator it = _pathFuncVector.begin();
         it != _pathFuncVector.end(); ++it) {
        PathFunc* pf = *it;
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
    for (boost::container::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
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
    for (boost::container::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
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

    boost::container::vector<PathFunc*>::iterator it = std::find_if(_pathFuncVector.begin(),
        _pathFuncVector.end(), [path](PathFunc* x) { return isSamePath(x->getPath(), path); });
    if (it != _pathFuncVector.end()) {
        return false;
    }
    bool withWatch = frozen ? false : true;
    PathFunc* pf = nullptr;
    try {
        pf = new PathFunc(path, withWatch);
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
    boost::container::vector<PathFunc*>::iterator it = std::find_if(_pathFuncVector.begin(),
        _pathFuncVector.end(), [path](PathFunc* x) { return isSamePath(x->getPath(), path); });

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
        list.push_back(_userPath->getPath());
    }
    for (boost::container::vector<PathFunc*>::iterator it = _pathFuncVector.begin();
         it != _pathFuncVector.end(); ++it) {
        PathFunc* pf = *it;
        if (pf) {
            list.push_back(pf->getPath());
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
#ifdef _MSC_VER
        _path = _userPath->getPath() + L";";
#else
        _path = _userPath->getPath() + L":";
#endif
    }
    return _path;
}
//=============================================================================
bool
PathFuncManager::setCurrentUserPath(const std::wstring& path)
{
    if (_currentPath != nullptr) {
        delete _currentPath;
    }
    _currentPath = new PathFunc(path);
    if (_currentPath != nullptr) {
        _currentPath->rehash();
    }
    return false;
}
//=============================================================================
bool
PathFuncManager::setUserPath(const std::wstring& path, bool saveToFile)
{
    clearUserPath();
    if (_userPath == nullptr) {
        _userPath = new PathFunc(path);
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
    std::wstring prefDir = getPreferencesPath();
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    try {
        boost::filesystem::path p = userPathFile;
        boost::filesystem::remove(p);
    } catch (const boost::filesystem::filesystem_error&) {
    }
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
    for (boost::container::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
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
        try {
            boost::filesystem::path p1{ _currentPath->getPath() }, p2{ path };
            if (boost::filesystem::equivalent(p1, p2)) {
                _currentPath->rehash();
                return;
            }
        } catch (const boost::filesystem::filesystem_error&) {
        }
    }
    if (_userPath != nullptr) {
        try {
            boost::filesystem::path p1{ _userPath->getPath() }, p2{ path };
            if (boost::filesystem::equivalent(p1, p2)) {
                _userPath->rehash();
                return;
            }
        } catch (const boost::filesystem::filesystem_error&) {
        }
    }
    for (boost::container::vector<PathFunc*>::reverse_iterator it = _pathFuncVector.rbegin();
         it != _pathFuncVector.rend(); ++it) {
        PathFunc* pf = *it;
        if (pf) {
            try {
                boost::filesystem::path p1{ pf->getPath() }, p2{ path };
                if (boost::filesystem::equivalent(p1, p2)) {
                    pf->rehash();
                    return;
                }
            } catch (const boost::filesystem::filesystem_error&) {
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
    for (boost::container::vector<PathFunc*>::iterator it = _pathFuncVector.begin();
         it != _pathFuncVector.end(); ++it) {
        PathFunc* pf = *it;
        if (pf) {
#ifdef _MSC_VER
            p = p + pf->getPath() + L";";
#else
            p = p + pf->getPath() + L":";
#endif
        }
    }
#ifdef _MSC_VER
    if (boost::algorithm::ends_with(p, L";"))
#else
    if (boost::algorithm::ends_with(p, L":"))
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
    boost::container::vector<PathFunc*>::iterator it = std::find_if(_pathFuncVector.begin(),
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
PathFuncManager::clearCache(stringVector exceptedFunctions)
{
    FunctionsInMemory::getInstance()->clear(exceptedFunctions);
}
//=============================================================================
bool
PathFuncManager::isDir(const std::wstring& pathname)
{
    boost::filesystem::path data_dir(pathname);
    bool bRes = false;
    try {
        bRes = boost::filesystem::exists(data_dir) && boost::filesystem::is_directory(data_dir);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            // ONLY FOR DEBUG
        }
        bRes = false;
    }
    return bRes;
}
//=============================================================================
bool
PathFuncManager::isFile(const std::wstring& filename)
{
    boost::filesystem::path data_dir(filename);
    bool bRes = false;
    try {
        bRes = boost::filesystem::exists(data_dir) && !boost::filesystem::is_directory(data_dir);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            // ONLY FOR DEBUG
        }
        bRes = false;
    }
    return bRes;
}
//=============================================================================
void
PathFuncManager::userpathCompute()
{
    clearUserPath();
    std::wstring userpathEnv = GetVariableEnvironment(L"NELSON_USERPATH", L"");
    bool bSet = false;
    if (!userpathEnv.empty()) {
        if (isDir(userpathEnv)) {
            setUserPath(userpathEnv);
            bSet = true;
        }
    }
    if (!bSet) {
        std::wstring prefDir;
        std::wstring userPathFile;
        try {
            prefDir = getPreferencesPath();
        } catch (const Exception&) {
            prefDir.clear();
        }
        try {
            userPathFile = prefDir + L"/userpath.conf";
            bool bIsFile = boost::filesystem::exists(userPathFile)
                && !boost::filesystem::is_directory(userPathFile);
            if (bIsFile) {
                std::wstring preferedUserPath = loadUserPathFromFile();
                if (!preferedUserPath.empty()) {
                    if (isDir(preferedUserPath)) {
                        setUserPath(preferedUserPath);
                        bSet = true;
                    }
                } else {
                    bSet = true;
                }
            }
        } catch (const boost::filesystem::filesystem_error&) {
        }
    }
    if (!bSet) {
#ifdef _MSC_VER
        std::wstring userprofileEnv = GetVariableEnvironment(L"USERPROFILE", L"");
        if (!userprofileEnv.empty()) {
            std::wstring userpathDir = userprofileEnv + std::wstring(L"/Documents/Nelson");
            if (!isDir(userpathDir)) {
                try {
                    boost::filesystem::create_directories(userpathDir);
                } catch (const boost::filesystem::filesystem_error&) {
                }
            }
            if (isDir(userpathDir)) {
                setUserPath(userpathDir);
            }
        }
#else
        std::wstring homeEnv = GetVariableEnvironment(L"HOME", L"");
        if (homeEnv != L"") {
            std::wstring userpathDir = homeEnv + std::wstring(L"/Documents/Nelson");
            if (!isDir(userpathDir)) {
                try {
                    boost::filesystem::create_directories(userpathDir);
                } catch (const boost::filesystem::filesystem_error&) {
                }
            }
            if (isDir(userpathDir)) {
                setUserPath(userpathDir);
            }
        }
#endif
    }
}
//=============================================================================
std::wstring
PathFuncManager::getPreferencesPath()
{
#define NELSON_PREFERENCES_PATH_ENV L"NELSON_PREFERENCES_PATH"
    std::wstring prefPath = GetVariableEnvironment(NELSON_PREFERENCES_PATH_ENV, L"");
    return prefPath;
}
//=============================================================================
std::wstring
PathFuncManager::loadUserPathFromFile()
{
    std::wstring preferedUserPath = L"";
    std::wstring prefDir = getPreferencesPath();
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    bool bIsFile
        = boost::filesystem::exists(userPathFile) && !boost::filesystem::is_directory(userPathFile);
    if (bIsFile) {
        std::string tmpline;
#ifdef _MSC_VER
        std::ifstream jsonFile(userPathFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(userPathFile));
#endif
        if (jsonFile.is_open()) {
            std::string jsonString;
            while (safegetline(jsonFile, tmpline)) {
                jsonString += tmpline + '\n';
            }
            jsonFile.close();
            boost::property_tree::ptree pt;
            std::istringstream is(jsonString);
            try {
                boost::property_tree::read_json(is, pt);
                preferedUserPath = utf8_to_wstring(pt.get<std::string>("userpath"));
            } catch (const boost::property_tree::json_parser::json_parser_error& je) {
                je.message();
            }
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
    std::wstring prefDir = getPreferencesPath();
    std::wstring userPathFile = prefDir + L"/userpath.conf";
    boost::property_tree::ptree pt;
    pt.put("userpath", wstring_to_utf8(up));
    std::ostringstream buf;
    boost::property_tree::write_json(buf, pt, false);
    std::string json = buf.str();
#ifdef _MSC_VER
    std::ofstream out(userPathFile);
#else
    std::ofstream out(wstring_to_utf8(userPathFile));
#endif
    out << json;
    out.close();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
