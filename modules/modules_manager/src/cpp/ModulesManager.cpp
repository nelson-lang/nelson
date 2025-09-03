//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_map>
#include <map>
#include <algorithm>
#include <fstream>
#include <nlohmann/json.hpp>
#include <semver.h>
#include "StringHelpers.hpp"
#include "ModulesManager.hpp"
#include "Nelson_VERSION.h"
#include "characters_encoding.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
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
ModulesManager::ModulesManager()
{
#define MODULETAB 128
    modulesMap.reserve(MODULETAB);
}
//=============================================================================
size_t
ModulesManager::getNumberOfModules()
{
    return modulesMap.size();
}
//=============================================================================
wstringVector
ModulesManager::getModulesPathList(bool bReverse)
{
    wstringVector retlist;
    if (bReverse) {
        if (!modulesMap.empty()) {
            for (int64 i = (int64)modulesMap.size() - 1; i >= 0; i--) {
                mapElement elem = modulesMap[i];
                retlist.push_back(std::get<1>(elem));
            }
        }
    } else {
        for (auto elem : modulesMap) {
            retlist.push_back(std::get<1>(elem));
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
        if (!modulesMap.empty()) {
            for (int64 i = (int64)modulesMap.size() - 1; i >= 0; i--) {
                mapElement elem = modulesMap[i];
                retlist.push_back(std::get<0>(elem));
            }
        }
    } else {
        for (auto elem : modulesMap) {
            retlist.push_back(std::get<0>(elem));
        }
    }
    return retlist;
}
//=============================================================================
std::vector<bool>
ModulesManager::getModulesProtectedList(bool bReverse)
{
    std::vector<bool> retlist;
    if (bReverse) {
        if (!modulesMap.empty()) {
            for (int64 i = (int64)modulesMap.size() - 1; i >= 0; i--) {
                mapElement elem = modulesMap[i];
                retlist.push_back(std::get<3>(elem));
            }
        }
    } else {
        for (auto elem : modulesMap) {
            retlist.push_back(std::get<3>(elem));
        }
    }
    return retlist;
}
//=============================================================================
versionElement
ModulesManager::readVersionFromJson(const std::wstring& path)
{
    std::vector<double> version;
    std::wstring moduleJsonFilename;
    if (StringHelpers::ends_with(path, L"\\") || StringHelpers::ends_with(path, L"/")) {
        moduleJsonFilename = path + L"module.json";
    } else {
        moduleJsonFilename = path + L"/module.json";
    }
#ifdef _MSC_VER
    std::ifstream jsonFile(moduleJsonFilename);
#else
    std::ifstream jsonFile(wstring_to_utf8(moduleJsonFilename));
#endif
    if (jsonFile.is_open()) {
        nlohmann::json data;
        try {
            data = nlohmann::json::parse(jsonFile);
            std::string versionString = data["version"];
            semver_t semVersion = {};
            if (semver_parse(versionString.c_str(), &semVersion) != 0) {
                version.clear();
            } else {
                semver_free(&semVersion);
                version.push_back(semVersion.major);
                version.push_back(semVersion.minor);
                version.push_back(semVersion.patch);
            }
        } catch (const nlohmann::json::exception&) {
        }
        jsonFile.close();
    }
    if (version.size() == 3) {
        return std::make_tuple(version[0], version[1], version[2]);
    }
    Warning(L"module_manager:modulejson", _W("Please check: ") + moduleJsonFilename);

    return std::make_tuple(std::nan(""), std::nan(""), std::nan(""));
}
//=============================================================================
std::vector<versionElement>
ModulesManager::getModulesVersionList(bool bReverse)
{
    std::vector<versionElement> retlist;
    if (bReverse) {
        if (!modulesMap.empty()) {
            for (int64 i = (int64)modulesMap.size() - 1; i >= 0; i--) {
                mapElement elem = modulesMap[i];
                retlist.push_back(std::get<4>(elem));
            }
        }
    } else {
        for (auto elem : modulesMap) {
            retlist.push_back(std::get<4>(elem));
        }
    }
    return retlist;
}
//=============================================================================
void
ModulesManager::insertModule(
    const std::wstring& modulename, const std::wstring& path, bool protectedModule)
{
    versionElement version;
    if (protectedModule) {
        version = std::make_tuple((double)NELSON_VERSION_MAJOR, (double)NELSON_VERSION_MINOR,
            (double)NELSON_VERSION_MAINTENANCE);
    } else {
        version = readVersionFromJson(path);
    }
    modulesMap.emplace_back(modulename, path, L"", protectedModule, version);
}
//=============================================================================
void
ModulesManager::deleteAllModules()
{
    modulesMap.clear();
}
//=============================================================================
bool
ModulesManager::deleteModule(const std::wstring& modulename)
{
    for (auto it = modulesMap.begin(); it != modulesMap.end(); ++it) {
        if (std::get<0>(*it) == modulename) {
            modulesMap.erase(it);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
ModulesManager::findModule(const std::wstring& modulename, std::wstring& path)
{
    for (auto& it : modulesMap) {
        if (std::get<0>(it) == modulename) {
            path = std::get<1>(it);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
ModulesManager::isModule(const std::wstring& modulename)
{
    for (auto& it : modulesMap) {
        if (std::get<0>(it) == modulename) {
            return true;
        }
    }
    return false;
}
//=============================================================================
std::wstring
ModulesManager::findModuleNameByPath(const std::wstring& filename)
{
    if (!modulesMap.empty()) {
        for (int64 i = (int64)modulesMap.size() - 1; i >= 0; i--) {
            const mapElement& elem = modulesMap[i];
            const std::wstring& modulePath = std::get<1>(elem);
            if (StringHelpers::starts_with(filename, modulePath)) {
                size_t len = modulePath.length();
                if (filename.length() == len || filename[len] == L'/' || filename[len] == L'\\') {
                    return std::get<0>(elem);
                }
            }
        }
    }
    return {};
}
//=============================================================================
bool
ModulesManager::isProtectedModule(const std::wstring& modulename)
{
    if (!modulesMap.empty()) {
        for (int64 i = (int64)modulesMap.size() - 1; i >= 0; i--) {
            mapElement elem = modulesMap[i];
            if (std::get<0>(elem) == modulename) {
                return std::get<3>(elem);
            }
        }
    }
    return false;
}
//=============================================================================
bool
ModulesManager::setLibraryPath(const std::wstring& modulename, const std::wstring& libraryFullname)
{
    for (int64 i = (int64)modulesMap.size() - 1; i >= 0; i--) {
        mapElement elem = modulesMap[i];
        if (std::get<0>(elem) == modulename) {
            std::wstring libraryPath = std::get<2>(elem);
            if (libraryPath.empty()) {
                std::get<2>(elem) = libraryFullname;
                modulesMap[i] = elem;
                return true;
            }
            return false;
        }
    }
    return false;
}
//=============================================================================
bool
RegisterModule(
    const std::wstring& moduleshortname, const std::wstring& modulerootpath, bool protectedModule)
{
    ModulesManager::Instance().insertModule(moduleshortname, modulerootpath, protectedModule);
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
    return ModulesManager::Instance().findModuleNameByPath(modulerootpath) != L"";
}
//=============================================================================
bool
IsProtectedModuleName(const std::wstring& moduleshortname)
{
    return ModulesManager::Instance().isProtectedModule(moduleshortname);
}
//=============================================================================
std::vector<module>
GetModules(bool bReverse)
{
    wstringVector listPaths = GetModulesPath(bReverse);
    wstringVector listNames = GetModulesName(bReverse);
    std::vector<bool> listProtected = GetModulesProtected(bReverse);
    std::vector<module> modules;
    modules.reserve(listNames.size());
    for (size_t k = 0; k < listNames.size(); k++) {
        module m;
        m.modulename = listNames[k];
        m.modulepath = listPaths[k];
        m.isprotected = listProtected[k];
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
std::vector<bool>
GetModulesProtected(bool bReverse)
{
    return ModulesManager::Instance().getModulesProtectedList(bReverse);
}
//=============================================================================
std::vector<versionElement>
GetModulesVersion(bool bReverse)
{
    return ModulesManager::Instance().getModulesVersionList(bReverse);
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
} // namespace Nelson
//=============================================================================
