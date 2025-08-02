//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "ModulePath.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FindDynamicLibraryName.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
ModulePath(const std::wstring& moduleshortname)
{
    return ModulePath(moduleshortname, GET_ROOT_PATH);
}
//=============================================================================
static bool
isInternalModule(const std::wstring& modulePath, const std::wstring& nelsonModulePath)
{
    return StringHelpers::starts_with(modulePath, nelsonModulePath);
}
//=============================================================================
static std::wstring
ModulePathNelson(MODULEPATH_OPTION option)
{
    std::wstring returnedPath;
    switch (option) {
    case Nelson::GET_BINARY_PATH:
        returnedPath = NelsonConfiguration::getInstance()->getNelsonBinaryDirectory();
        break;
    case Nelson::GET_ROOT_PATH:
        returnedPath = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
        break;
    case Nelson::GET_ETC_PATH:
        returnedPath = NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/etc";
        break;
    case Nelson::GET_LIBRARY_FULLPATH:
        returnedPath = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        break;
    case Nelson::GET_FUNCTIONS_PATH:
    case Nelson::GET_TESTS_PATH:
    default:
        Error(_W("Invalid option."));
        break;
    }
    return returnedPath;
}
//=============================================================================
std::wstring
ModulePath(const std::wstring& moduleshortname, MODULEPATH_OPTION option)
{
    if (moduleshortname == L"nelson") {
        return ModulePathNelson(option);
    }
    std::wstring moduleRootPath;
    if (!ModulesManager::Instance().findModule(moduleshortname, moduleRootPath)) {
        Error(_W("invalid module name."));
    }
    bool isNelsonInternalModule = isInternalModule(
        moduleRootPath, NelsonConfiguration::getInstance()->getNelsonModulesDirectory());
    FileSystemWrapper::Path p;
    switch (option) {
    case Nelson::GET_BINARY_PATH: {
        if (isNelsonInternalModule) {
            p = NelsonConfiguration::getInstance()->getNelsonBinaryDirectory();
        } else {
            p = moduleRootPath + L"/" + moduleshortname + L"/bin";
            if (!p.is_directory()) {
                Error(_W("Path does not exist:") + L"\n" + p.generic_wstring());
            }
        }
    } break;
    case Nelson::GET_ROOT_PATH: {
        return moduleRootPath;
    } break;
    case Nelson::GET_ETC_PATH: {
        if (isNelsonInternalModule) {
            p = NelsonConfiguration::getInstance()->getNelsonModulesDirectory() + L"/"
                + moduleshortname + L"/etc";
        } else {
            p = moduleRootPath + L"/" + moduleshortname + L"/etc";
        }
    } break;
    case Nelson::GET_LIBRARY_FULLPATH: {
        p = ConstructDynamicLibraryFullname(isNelsonInternalModule
                ? NelsonConfiguration::getInstance()->getNelsonLibraryDirectory()
                : moduleRootPath,
            moduleshortname, isNelsonInternalModule);
    } break;
    case Nelson::GET_FUNCTIONS_PATH: {
        if (isNelsonInternalModule) {
            p = NelsonConfiguration::getInstance()->getNelsonModulesDirectory() + L"/"
                + moduleshortname + L"/functions";
        } else {
            p = moduleRootPath + L"/" + moduleshortname + L"/functions";
        }
    } break;
    case Nelson::GET_TESTS_PATH: {
        if (isNelsonInternalModule) {
            p = NelsonConfiguration::getInstance()->getNelsonModulesDirectory() + L"/"
                + moduleshortname + L"/tests";
        } else {
            p = moduleRootPath + L"/" + moduleshortname + L"/tests";
        }
    } break;
    default:
        break;
    }
    return p.generic_wstring();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
