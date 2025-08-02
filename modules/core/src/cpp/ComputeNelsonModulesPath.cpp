//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ComputeNelsonModulesPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ComputeNelsonModulesPath(std::wstring& errorMessage)
{
#define NELSON_MODULES_PATH_ENV L"NELSON_MODULES_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_MODULES_PATH_ENV, L"");
    if (penv != L"") {
        FileSystemWrapper::Path path(penv);
        if (path.is_directory()) {
            NelsonConfiguration::getInstance()->setNelsonModulesDirectory(path.generic_wstring());
            return true;
        }
    }
    FileSystemWrapper::Path nelsonRootPath
        = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
    FileSystemWrapper::Path modulesPath = nelsonRootPath.generic_wstring() + L"/modules";

    if (modulesPath.is_directory()) {
        NelsonConfiguration::getInstance()->setNelsonModulesDirectory(
            modulesPath.getFinalPathname().generic_wstring());
        SetVariableEnvironmentW(
            NELSON_MODULES_PATH_ENV, modulesPath.getFinalPathname().generic_wstring());
        return true;
    }
    errorMessage = _W("Cannot find Nelson modules path.");
    NelsonConfiguration::getInstance()->setNelsonModulesDirectory(L"");
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
