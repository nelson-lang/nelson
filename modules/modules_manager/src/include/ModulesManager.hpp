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
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "nlsModules_manager_exports.h"
#include <boost/container/vector.hpp>
#include <string>
//=============================================================================
typedef struct
{
    std::wstring modulename;
    std::wstring modulepath;
} module;
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSMODULES_MANAGER_IMPEXP ModulesManager
{
public:
    static ModulesManager&
    Instance();
    size_t
    getNumberOfModules();
    wstringVector
    getModulesPathList(bool bReverse);
    wstringVector
    getModulesList(bool bReverse);
    void
    insertModule(const std::wstring& modulename, const std::wstring& path);
    bool
    findModule(const std::wstring& modulename, std::wstring& path);
    void
    deleteAllModules();
    bool
    deleteModule(const std::wstring& modulename);
    std::wstring
    findModuleNameByPath(const std::wstring& filename);

private:
    ModulesManager();
    ModulesManager(ModulesManager const&){};
    static ModulesManager m_pInstance;
    boost::container::vector<std::pair<std::wstring, std::wstring>> modulesMap;
};
//=============================================================================
NLSMODULES_MANAGER_IMPEXP bool
RegisterModule(const std::wstring& moduleshortname, const std::wstring& modulerootpath);
NLSMODULES_MANAGER_IMPEXP bool
UnregisterModule(const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP bool
IsExistingModuleName(const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP bool
IsExistingModulePath(const std::wstring& modulerootpath);
NLSMODULES_MANAGER_IMPEXP boost::container::vector<module>
GetModules(bool bReverse = false);
NLSMODULES_MANAGER_IMPEXP wstringVector
GetModulesName(bool bReverse = false);
NLSMODULES_MANAGER_IMPEXP wstringVector
GetModulesPath(bool bReverse = false);
NLSMODULES_MANAGER_IMPEXP std::wstring
GetModulePath(const std::wstring& moduleshortname);
//=============================================================================
} // namespace Nelson
//=============================================================================
