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
#include <Shlobj.h>
#include <Windows.h>
#else
#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>
#endif
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
        std::filesystem::path path = createFileSystemPath(penv);
        try {
            if (std::filesystem::is_directory(path)) {
                prefPath = convertFileSytemPathToGenericWString(path);
                bSet = true;
            }
        } catch (const std::filesystem::filesystem_error&) {
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
            std::filesystem::path path_1 = createFileSystemPath(strPath);
            std::wstring NelSonDir = utf8_to_wstring(NELSON_PRODUCT_NAME) + std::wstring(L"\\")
                + utf8_to_wstring(NELSON_SEMANTIC_VERSION_STRING);
            std::filesystem::path path_2 = createFileSystemPath(NelSonDir);
            std::filesystem::path path = path_1;
            path /= path_2;
            prefPath = path.generic_wstring();
            try {
                if (!std::filesystem::is_directory(path)) {
                    try {
                        if (!std::filesystem::create_directories(path)) {
                            fprintf(stderr, "%s\n",
                                _("Error: we cannot create preferences path.").c_str());
                            prefPath.clear();
                        }
                    } catch (const std::filesystem::filesystem_error&) {
                        prefPath.clear();
                    }
                }
            } catch (const std::filesystem::filesystem_error&) {
                prefPath.clear();
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
            std::filesystem::path path_1 = std::string(homedir);
            try {
                if (std::filesystem::is_directory(path_1)) {
                    std::filesystem::path path_2 = NelSonDir;
                    std::filesystem::path path = path_1;
                    path /= path_2;
                    prefPath = convertFileSytemPathToGenericWString(path);
                    try {
                        if (!std::filesystem::is_directory(path)) {
                            try {
                                if (!std::filesystem::create_directories(path)) {
                                    fprintf(stderr, "%s\n",
                                        _("Error: we cannot find Nelson preferences path.")
                                            .c_str());
                                    prefPath = L"";
                                }
                            } catch (const std::filesystem::filesystem_error& e) {
                                if (e.code() == std::errc::permission_denied) {
                                    prefPath = L"";
                                }
                                prefPath = L"";
                            }
                        }
                    } catch (const std::filesystem::filesystem_error&) {
                        prefPath = L"";
                    }
                }
            } catch (const std::filesystem::filesystem_error&) {
                prefPath = L"";
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
