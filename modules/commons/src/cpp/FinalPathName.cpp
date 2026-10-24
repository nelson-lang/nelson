//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
namespace Nelson::FileSystemWrapper {
#ifdef _MSC_VER
static std::wstring
finalPathForHandle(HANDLE file)
{
    wchar_t shortPath[MAX_PATH];
    DWORD length = GetFinalPathNameByHandleW(file, shortPath, MAX_PATH, VOLUME_NAME_DOS);
    if (length == 0) {
        return {};
    }
    if (length < MAX_PATH) {
        return std::wstring(shortPath, length);
    }
    if (length > 32768) {
        return {};
    }
    // On overflow the API returns the required capacity, including the terminator.
    std::wstring path(length, L'\0');
    length = GetFinalPathNameByHandleW(
        file, path.data(), static_cast<DWORD>(path.size()), VOLUME_NAME_DOS);
    if (length == 0 || length >= path.size()) {
        return {};
    }
    path.resize(length);
    return path;
}
#endif
std::wstring
Path::getFinalPathname(const std::wstring& path)
{
#ifdef _MSC_VER
    HANDLE file = CreateFileW(path.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
        nullptr, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, nullptr);
    if (file == INVALID_HANDLE_VALUE) {
        return path;
    }
    const auto resolved = finalPathForHandle(file);
    CloseHandle(file);
    if (resolved.compare(0, 8, L"\\\\?\\UNC\\") == 0) {
        return L"\\\\" + resolved.substr(8);
    }
    if (resolved.compare(0, 4, L"\\\\?\\") == 0) {
        return resolved.substr(4);
    }
#endif
    return path;
}
//=============================================================================
} // namespace Nelson::FileSystemWrapper
//=============================================================================
