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
#include "GetExternalModulesPath.hpp"
#include "Nelson_VERSION.h"
#include "characters_encoding.hpp"
#include "GetVariableEnvironment.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring externalModulesPath;
//=============================================================================
static std::wstring
getUserDir()
{
    std::wstring envValue;
#ifdef _MSC_VER
    envValue = GetVariableEnvironment(L"USERPROFILE");
#else
    envValue = GetVariableEnvironment(L"HOME");
#endif
    FileSystemWrapper::Path pwd(envValue);
    std::wstring userDir = pwd.generic_wstring();
    if (!StringHelpers::ends_with(userDir, L"\\") && (!StringHelpers::ends_with(userDir, L"/"))) {
        userDir.append(L"/");
    }
    return userDir;
}
//=============================================================================
bool
CreateIfRequiredExternalModulesPath()
{
    std::wstring defaultExternalModulesDirectory = getUserDir() + std::wstring(NELSON_PROJECT_NAME)
        + L"/" + utf8_to_wstring(NELSON_SEMANTIC_VERSION_STRING);
    externalModulesPath
        = GetVariableEnvironment(L"NELSON_EXTERNAL_MODULES_PATH", defaultExternalModulesDirectory);
    FileSystemWrapper::Path modulesPath(externalModulesPath);
    externalModulesPath = modulesPath.generic_wstring();
    if (!StringHelpers::ends_with(externalModulesPath, L"\\")
        && (!StringHelpers::ends_with(externalModulesPath, L"/"))) {
        externalModulesPath = externalModulesPath + L"/";
    }

    bool bOK = false;
    bool bIsDir = FileSystemWrapper::Path::is_directory(externalModulesPath);
    if (!bIsDir) {
        bOK = FileSystemWrapper::Path::create_directories(externalModulesPath);
    } else {
        bOK = true;
    }
    return bOK;
}
//=============================================================================
std::wstring
GetExternalModulesPath()
{
    return externalModulesPath;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
