//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include "GetExternalModulesPath.hpp"
#include "Nelson_VERSION.h"
#include "characters_encoding.hpp"
#include "GetVariableEnvironment.hpp"
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
    boost::filesystem::path pwd = boost::filesystem::path(envValue);
    std::wstring userDir = pwd.generic_wstring();
    if (!boost::algorithm::ends_with(userDir, L"\\")
        && (!boost::algorithm::ends_with(userDir, L"/"))) {
        userDir.append(L"/");
    }
    return userDir;
}
//=============================================================================
bool
CreateIfRequiredExternalModulesPath()
{
    std::wstring defaultExternalModulesDirectory
        = getUserDir() + std::wstring(L"nelson/") + utf8_to_wstring(NELSON_SEMANTIC_VERSION_STRING);
    externalModulesPath
        = GetVariableEnvironment(L"NELSON_EXTERNAL_MODULES_PATH", defaultExternalModulesDirectory);
    boost::filesystem::path modulesPath = boost::filesystem::path(externalModulesPath);
    externalModulesPath = modulesPath.generic_wstring();
    if (!boost::algorithm::ends_with(externalModulesPath, L"\\")
        && (!boost::algorithm::ends_with(externalModulesPath, L"/"))) {
        externalModulesPath = externalModulesPath + L"/";
    }

    bool bOK = false;
    try {
        bool bIsDir = boost::filesystem::is_directory(externalModulesPath);
        if (!bIsDir) {
            bOK = boost::filesystem::create_directories(externalModulesPath);
        } else {
            bOK = true;
        }

    } catch (const boost::filesystem::filesystem_error&) {
        bOK = false;
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
