//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_map>
#include <map>
#include <algorithm>
#include <boost/container/vector.hpp>
#include <boost/algorithm/string.hpp>
#include "ModulesManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ModulesManager ModulesManager::m_pInstance = ModulesManager();
//=============================================================================
ModulesManager&
ModulesManager::Instance()
{
    return m_pInstance;
}
//=============================================================================
ModulesManager::ModulesManager(){
#define MODULETAB 4096
    // modulesMap.reserve(MODULETAB);
} //=============================================================================
size_t ModulesManager::getNumberOfModules()
{
    return modulesMap.size();
}
//=============================================================================
wstringVector
ModulesManager::getModulesPathList(bool bReverse)
{
    wstringVector retlist;
    if (bReverse) {
        for (auto it = modulesMap.rbegin(); it != modulesMap.rend(); ++it) {
            retlist.push_back(it->second);
        }
    } else {
        for (auto it = modulesMap.begin(); it != modulesMap.end(); ++it) {
            retlist.push_back(it->second);
        }
    }
    return retlist;
}
//=============================================================================
wstringVector
ModulesManager::getModulesList(bool bReverse)
{
    wstringVector retlist;
    if (bReverse) {
        for (auto it = modulesMap.rbegin(); it != modulesMap.rend(); ++it) {
            retlist.push_back(it->first);
        }
    } else {
        for (auto it = modulesMap.begin(); it != modulesMap.end(); ++it) {
            retlist.push_back(it->first);
        }
    }
    return retlist;
}
//=============================================================================
void
ModulesManager::insertModule(const std::wstring& modulename, const std::wstring& path)
{
    modulesMap.push_back(std::make_pair(modulename, path));
}
//=============================================================================
void
ModulesManager::deleteAllModules()
{
    modulesMap.clear();
    // modulesMap.reserve(MODULETAB);
}
//=============================================================================
bool
ModulesManager::deleteModule(const std::wstring& modulename)
{
    auto it = std::find_if(modulesMap.begin(), modulesMap.end(),
        [&modulename](std::pair<std::wstring, std::wstring> const& elem) {
            return elem.first == modulename;
        });
    if (it != modulesMap.end()) {
        modulesMap.erase(it);
        return true;
    }
    return false;
}
//=============================================================================
bool
ModulesManager::findModule(const std::wstring& modulename, std::wstring& path)
{
    auto it = std::find_if(modulesMap.begin(), modulesMap.end(),
        [&modulename](std::pair<std::wstring, std::wstring> const& elem) {
            return elem.first == modulename;
        });
    if (it != modulesMap.end()) {
        path = it->second;
        return true;
    }
    return false;
}
//=============================================================================
std::wstring
ModulesManager::findModuleNameByPath(const std::wstring& filename)
{
    std::wstring moduleName = L"";
    for (auto it = modulesMap.rbegin(); it != modulesMap.rend(); ++it) {
        if (boost::algorithm::starts_with(filename, it->second)) {
            moduleName = it->first;
            return moduleName;
        }
    }
    return moduleName;
}
//=============================================================================
bool
RegisterModule(const std::wstring& moduleshortname, const std::wstring& modulerootpath)
{
    ModulesManager::Instance().insertModule(moduleshortname, modulerootpath);
    return true;
}
//=============================================================================
bool
UnregisterModule(const std::wstring& moduleshortname)
{
    return ModulesManager::Instance().deleteModule(moduleshortname);
}
//=============================================================================
bool
IsExistingModuleName(const std::wstring& moduleshortname)
{
    std::wstring modulerootpath;
    return ModulesManager::Instance().findModule(moduleshortname, modulerootpath);
}
//=============================================================================
bool
IsExistingModulePath(const std::wstring& modulerootpath)
{
    wstringVector listPaths = GetModulesPath(false);
    for (size_t k = 0; k < listPaths.size(); k++) {
        if (listPaths[k] == modulerootpath) {
            return true;
        }
    }
    return false;
}
//=============================================================================
boost::container::vector<module>
GetModules(bool bReverse)
{
    wstringVector listPaths = GetModulesPath(bReverse);
    wstringVector listNames = GetModulesName(bReverse);
    boost::container::vector<module> modules;
    modules.reserve(listNames.size());
    for (size_t k = 0; k < listNames.size(); k++) {
        module m;
        m.modulename = listNames[k];
        m.modulepath = listPaths[k];
        modules.push_back(m);
    }
    return modules;
}
//=============================================================================
wstringVector
GetModulesName(bool bReverse)
{
    return ModulesManager::Instance().getModulesList(bReverse);
}
//=============================================================================
wstringVector
GetModulesPath(bool bReverse)
{
    return ModulesManager::Instance().getModulesPathList(bReverse);
}
//=============================================================================
std::wstring
GetModulePath(const std::wstring& moduleshortname)
{
    std::wstring res;
    ModulesManager::Instance().findModule(moduleshortname, res);
    return res;
}
//=============================================================================
}
//=============================================================================
