//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Shlobj.h>
#include <Windows.h>
#else
#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>
#endif
#include "FileSystemWrapper.hpp"
#include "FileSystemHelpers.hpp"
#include "GetPreferencesPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "Nelson_VERSION.h"
#include "SetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring preferencesPath;
//=============================================================================
static std::wstring
buildPreferencesPath()
{
    std::wstring prefPath;
#define NELSON_PREFERENCES_PATH_ENV L"NELSON_PREFERENCES_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_PREFERENCES_PATH_ENV, L"");
    bool bSet = false;
    if (penv != L"") {
        Nelson::FileSystemWrapper::Path path(penv);
        if (isDirectory(path)) {
            prefPath = path.generic_wstring();
            bSet = true;
        } else {
            bSet = false;
        }
    }
    if (!bSet) {
#ifdef _MSC_VER
        std::wstring strPath;
        LPWSTR wszPath = nullptr;
        HRESULT hr = SHGetKnownFolderPath(FOLDERID_RoamingAppData, 0, nullptr, &wszPath);
        bool bOK = SUCCEEDED(hr);
        if (bOK) {
            strPath = wszPath;
        }
        CoTaskMemFree(static_cast<void*>(wszPath));
        if (bOK) {
            Nelson::FileSystemWrapper::Path path_1(strPath);
            std::wstring NelSonDir = utf8_to_wstring(NELSON_PRODUCT_NAME) + std::wstring(L"\\")
                + utf8_to_wstring(NELSON_SEMANTIC_VERSION_STRING);
            Nelson::FileSystemWrapper::Path path_2(NelSonDir);
            Nelson::FileSystemWrapper::Path path = path_1;
            path /= path_2;
            prefPath = path.generic_wstring();
            bool permissionDenied;
            if (!isDirectory(path, permissionDenied)) {
                if (permissionDenied) {
                    prefPath.clear();
                } else {
                    if (!Nelson::FileSystemWrapper::Path::create_directories(path)) {
                        fprintf(
                            stderr, "%s\n", _("Error: we cannot create preferences path.").c_str());
                        prefPath.clear();
                    }
                }
            }
        }
#else
        const char* homedir;
        if ((homedir = getenv("HOME")) == nullptr) {
            homedir = getpwuid(getuid())->pw_dir;
        }
        if (homedir != nullptr) {
            std::string NelSonDir = std::string(".") + std::string(NELSON_PRODUCT_NAME)
                + std::string("/") + std::string(NELSON_SEMANTIC_VERSION_STRING);
            Nelson::FileSystemWrapper::Path path_1 = std::string(homedir);
            if (path_1.is_directory()) {
                Nelson::FileSystemWrapper::Path path_2(NelSonDir);
                Nelson::FileSystemWrapper::Path path { path_1 };
                path /= path_2;
                prefPath = path.generic_wstring();
                if (!isDirectory(path)) {
                    if (!Nelson::FileSystemWrapper::Path::create_directories(path)) {
                        fprintf(stderr, "%s\n",
                            _("Error: we cannot find Nelson preferences path.").c_str());
                        prefPath = L"";
                    }
                }
            }
        }
#endif
        SetVariableEnvironmentW(NELSON_PREFERENCES_PATH_ENV, prefPath);
    }
    return prefPath;
}
//=============================================================================
std::wstring
GetPreferencesPath()
{
    return preferencesPath;
}
//=============================================================================
bool
ComputePreferencesPath()
{
    preferencesPath = buildPreferencesPath();
    return (!preferencesPath.empty());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
static bool bFirstCall = true;
//=============================================================================
std::wstring
GetNelsonPreferencesPath()
{
    if (bFirstCall) {
        Nelson::ComputePreferencesPath();
        bFirstCall = false;
    }
    return Nelson::GetPreferencesPath();
}
//=============================================================================
