//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "GetVariableEnvironment.hpp"
#include "Nelson_VERSION.h"
#include "SetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
buildPreferencesPath()
{
    std::wstring prefPath;
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
        FileSystemWrapper::Path path_1(strPath);
        std::wstring NelSonDir = utf8_to_wstring(NELSON_PRODUCT_NAME) + std::wstring(L"\\")
            + utf8_to_wstring(NELSON_SEMANTIC_VERSION_STRING);
        FileSystemWrapper::Path path_2(NelSonDir);
        FileSystemWrapper::Path path = FileSystemWrapper::Path(path_1);
        path /= path_2;
        prefPath = path.generic_wstring();
        bool permissionDenied;
        if (!FileSystemWrapper::Path::is_directory(path, permissionDenied)) {
            if (permissionDenied) {
                prefPath.clear();
            } else {
                if (!FileSystemWrapper::Path::create_directories(path)) {
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
        std::string _homedir = std::string(homedir);
        FileSystemWrapper::Path path_1(_homedir);
        if (path_1.is_directory()) {
            FileSystemWrapper::Path path_2(NelSonDir);
            FileSystemWrapper::Path path { path_1 };
            path /= path_2;
            prefPath = path.generic_wstring();
            if (!FileSystemWrapper::Path::is_directory(path)) {
                if (!FileSystemWrapper::Path::create_directories(path)) {
                    prefPath = L"";
                }
            }
        }
    }
#endif
    return prefPath;
}
//=============================================================================
bool
ComputePreferencesPath(std::wstring& errorMessage)
{
#define NELSON_PREFERENCES_PATH_ENV L"NELSON_PREFERENCES_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_PREFERENCES_PATH_ENV, L"");
    if (!penv.empty()) {
        FileSystemWrapper::Path path(penv);
        if (path.is_directory()) {
            NelsonConfiguration::getInstance()->setNelsonPreferencesDirectory(
                path.generic_wstring());
            return true;
        }
    }
    std::wstring preferencesPath = buildPreferencesPath();
    if (!preferencesPath.empty()) {
        NelsonConfiguration::getInstance()->setNelsonPreferencesDirectory(preferencesPath);
        SetVariableEnvironmentW(NELSON_PREFERENCES_PATH_ENV, preferencesPath);
        return true;
    }
    errorMessage = _W("Cannot find Nelson preferences path.");
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
