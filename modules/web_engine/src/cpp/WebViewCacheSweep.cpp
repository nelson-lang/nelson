//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _WIN32
#include <Windows.h>
#include <cstdlib>
#include <cwchar>
#include <filesystem>
#include <string>
#include <system_error>
#include <vector>
#include "FileSystemWrapper.hpp"
#endif
#include "WebViewCacheSweep.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _WIN32
static void
removeFolder(const std::filesystem::path& folder);
static bool
isCacheFolderOfLiveProcess(const std::wstring& folderName);
static bool
isProcessAlive(const std::wstring& processIdText);
#endif
//=============================================================================
void
SweepStaleWebViewCacheFolders()
{
#ifdef _WIN32
    wchar_t* userProfile = nullptr;
    size_t userProfileLen = 0;
    if (_wdupenv_s(&userProfile, &userProfileLen, L"USERPROFILE") != 0 || userProfile == nullptr) {
        return;
    }
    std::filesystem::path base = std::filesystem::path(userProfile) / L".WebUI";
    free(userProfile);
    std::error_code ec;
    if (!std::filesystem::is_directory(base, ec)) {
        return;
    }
    std::vector<std::filesystem::path> candidates;
    std::vector<std::filesystem::path> leftovers;
    // Collect first: moving entries while iterating the same directory is unsafe.
    for (std::filesystem::directory_iterator it(base, ec), end; it != end; it.increment(ec)) {
        if (ec) {
            break;
        }
        const std::wstring name = it->path().filename().wstring();
        if (name.rfind(L"WebUIWebViewCache_", 0) == 0) {
            candidates.push_back(it->path());
        } else if (name.rfind(L".nlsWebUITrash_", 0) == 0) {
            leftovers.push_back(it->path());
        }
    }
    // A leftover quarantine is safe to remove, unless its owner is still filling it.
    const size_t trashPrefixLength = std::wcslen(L".nlsWebUITrash_");
    for (const std::filesystem::path& old : leftovers) {
        if (!isProcessAlive(old.filename().wstring().substr(trashPrefixLength))) {
            removeFolder(old);
        }
    }
    std::filesystem::path trash
        = base / (L".nlsWebUITrash_" + std::to_wstring(GetCurrentProcessId()));
    bool trashReady = false;
    unsigned int counter = 0;
    for (const std::filesystem::path& entry : candidates) {
        std::error_code localEc;
        if (!std::filesystem::is_directory(entry, localEc)) {
            continue;
        }
        // A starting instance does not hold its folder yet: the rename would succeed.
        if (isCacheFolderOfLiveProcess(entry.filename().wstring())) {
            continue;
        }
        if (!trashReady) {
            std::filesystem::create_directory(trash, localEc);
            trashReady = std::filesystem::is_directory(trash, localEc);
            if (!trashReady) {
                return;
            }
        }
        // Atomic rename fails while a live instance holds the folder, so only dead runs move.
        std::filesystem::rename(entry, trash / std::to_wstring(counter++), localEc);
    }
    if (trashReady) {
        removeFolder(trash);
    }
#endif
}
//=============================================================================
#ifdef _WIN32
// Removes a folder and its content; a failure leaves it to a later sweep.
static void
removeFolder(const std::filesystem::path& folder)
{
    std::string errorMessage;
    FileSystemWrapper::Path::remove_all(FileSystemWrapper::Path(folder.wstring()), errorMessage);
}
//=============================================================================
// True when the folder name carries the id of a process that is still running.
static bool
isCacheFolderOfLiveProcess(const std::wstring& folderName)
{
    // WebUIWebViewCache_<process id>_<random>: older names have no process id.
    const std::wstring prefix = L"WebUIWebViewCache_";
    const size_t separator = folderName.find(L'_', prefix.size());
    if (separator == std::wstring::npos) {
        return false;
    }
    return isProcessAlive(folderName.substr(prefix.size(), separator - prefix.size()));
}
//=============================================================================
// True when the text is the id of a process that is still running.
static bool
isProcessAlive(const std::wstring& processIdText)
{
    if (processIdText.empty()
        || processIdText.find_first_not_of(L"0123456789") != std::wstring::npos) {
        return false;
    }
    const DWORD processId = static_cast<DWORD>(std::wcstoul(processIdText.c_str(), nullptr, 10));
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, processId);
    if (process == nullptr) {
        // Access denied means the process exists.
        return GetLastError() == ERROR_ACCESS_DENIED;
    }
    DWORD exitCode = 0;
    const bool alive = GetExitCodeProcess(process, &exitCode) && exitCode == STILL_ACTIVE;
    CloseHandle(process);
    return alive;
}
#endif
//=============================================================================
}
//=============================================================================
