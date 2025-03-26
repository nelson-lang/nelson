//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <tuple>
#include <vector>
#include "Evaluator.hpp"
#include "nlsModules_manager_exports.h"
//=============================================================================
typedef struct
{
    std::wstring modulename;
    std::wstring modulepath;
    bool isprotected;
    std::wstring librarypath;
} module;
//=============================================================================
namespace Nelson {
//=============================================================================
using versionElement = std::tuple<double, double, double>;
using mapElement = std::tuple<std::wstring, std::wstring, std::wstring, bool, versionElement>;
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
    wstringVector
    getLibraryList(bool bReverse);
    std::vector<bool>
    getModulesProtectedList(bool bReverse);
    std::vector<versionElement>
    getModulesVersionList(bool bReverse);
    void
    insertModule(const std::wstring& modulename, const std::wstring& path, bool protectedModule);
    bool
    findModule(const std::wstring& modulename, std::wstring& path);
    bool
    isModule(const std::wstring& modulename);
    void
    deleteAllModules();
    bool
    deleteModule(const std::wstring& modulename);
    std::wstring
    findModuleNameByPath(const std::wstring& filename);
    bool
    isProtectedModule(const std::wstring& modulename);
    bool
    setLibraryPath(const std::wstring& modulename, const std::wstring& libraryFullname);

private:
    ModulesManager();
    ModulesManager(ModulesManager const& /*unused*/) {};
    static ModulesManager m_pInstance;
    std::vector<mapElement> modulesMap;
    versionElement
    readVersionFromJson(const std::wstring& path);
};
//=============================================================================
NLSMODULES_MANAGER_IMPEXP bool
RegisterModule(
    const std::wstring& moduleshortname, const std::wstring& modulerootpath, bool protectedModule);
NLSMODULES_MANAGER_IMPEXP bool
UnregisterModule(const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP bool
IsExistingModuleName(const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP bool
IsProtectedModuleName(const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP bool
IsExistingModulePath(const std::wstring& modulerootpath);
NLSMODULES_MANAGER_IMPEXP std::vector<module>
GetModules(bool bReverse = false);
NLSMODULES_MANAGER_IMPEXP wstringVector
GetModulesName(bool bReverse = false);
NLSMODULES_MANAGER_IMPEXP wstringVector
GetModulesPath(bool bReverse = false);
NLSMODULES_MANAGER_IMPEXP std::vector<bool>
GetModulesProtected(bool bReverse = false);
NLSMODULES_MANAGER_IMPEXP std::vector<versionElement>
GetModulesVersion(bool bReverse = false);
NLSMODULES_MANAGER_IMPEXP std::wstring
GetModulePath(const std::wstring& moduleshortname);
//=============================================================================
} // namespace Nelson
//=============================================================================
