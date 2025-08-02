//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "FileSystemWrapper.hpp"
#include "ComputeNelsonBinariesPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ComputeNelsonRootPath(std::wstring& errorMessage)
{
    // Possible cases:
    // 1] GNU
    // ../prefix/bin/   --> binaries path
    // ../prefix/share/nelson --> nelson root path
    // ../prefix/share/nelson/modules --> nelson modules path
    // ../prefix/lib/nelson --> nelson libraries path
    //
    // 2] local build linux and/or Windows
    // ../nelson/bin/arch  --> binaries path
    // ../nelson --> nelson root path
    // ../nelson/modules --> nelson modules path
    // ../nelson/bin/arch  --> nelson libraries path

#define NELSON_ROOT_PATH_ENV L"NELSON_ROOT_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_ROOT_PATH_ENV, L"");
    if (penv != L"") {
        FileSystemWrapper::Path path(penv);
        if (path.is_directory()) {
            NelsonConfiguration::getInstance()->setNelsonRootDirectory(path.generic_wstring());
            return true;
        }
    }
    FileSystemWrapper::Path libraryPath
        = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
    FileSystemWrapper::Path parent = libraryPath.parent_path();
    FileSystemWrapper::Path modulesPath = parent.generic_wstring() + L"/modules";
    FileSystemWrapper::Path modulesFile = modulesPath.generic_wstring() + L"/modules.m";
    bool found = false;
    while (!found && !parent.generic_wstring().empty() && parent.generic_wstring() != L"/") {
        if (modulesFile.is_regular_file()) {
            found = true;
            break;
        } else {
            parent = parent.parent_path();
            modulesPath = parent.generic_wstring() + L"/modules";
            modulesFile = modulesPath.generic_wstring() + L"/modules.m";
        }
    }
    if (!found) {
        //  ../prefix/lib/nelson
        FileSystemWrapper::Path prefixDirectory = libraryPath.parent_path().parent_path();
        FileSystemWrapper::Path shareDirectory = prefixDirectory.generic_wstring() + L"/share";
        if (shareDirectory.is_directory()) {
            modulesPath = shareDirectory.generic_wstring() + L"/"
                + std::wstring(NELSON_PROJECT_NAME) + L"/modules";
        }
    }

    if (modulesPath.is_directory()) {
        NelsonConfiguration::getInstance()->setNelsonModulesDirectory(
            modulesPath.getFinalPathname().generic_wstring());
        FileSystemWrapper::Path nelsonRootPath = modulesPath.parent_path();
        NelsonConfiguration::getInstance()->setNelsonRootDirectory(
            nelsonRootPath.getFinalPathname().generic_wstring());
        SetVariableEnvironmentW(
            NELSON_ROOT_PATH_ENV, nelsonRootPath.getFinalPathname().generic_wstring());
        return true;
    }
    errorMessage = _W("Cannot find Nelson root path.");
    NelsonConfiguration::getInstance()->setNelsonRootDirectory(L"");
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
