//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "SharedRuntimeRegistryPosix.hpp"
#include <filesystem>
#include <string>
#include <vector>
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <Windows.h>
#endif
namespace Nelson::SharedRuntimeRegistry {
inline std::vector<std::filesystem::path>
roots(const std::string& fingerprint)
{
    std::vector<std::filesystem::path> result;
#ifdef _WIN32
#if defined(_M_ARM64) || defined(__aarch64__)
    const std::wstring architecture = L"woa64";
#else
    const std::wstring architecture = L"win64";
#endif
    const auto name = L"Software\\Nelson\\Runtime\\" + architecture + L"\\"
        + std::wstring(fingerprint.begin(), fingerprint.end());
    for (auto hive : { HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE }) {
        HKEY key = nullptr;
        if (RegOpenKeyExW(hive, name.c_str(), 0, KEY_QUERY_VALUE | KEY_WOW64_64KEY, &key)
            != ERROR_SUCCESS) {
            continue;
        }
        DWORD type = 0, size = 0;
        auto status = RegQueryValueExW(key, L"Root", nullptr, &type, nullptr, &size);
        if (status == ERROR_SUCCESS && type == REG_SZ && size >= sizeof(wchar_t)
            && size <= 32768 * sizeof(wchar_t) && size % sizeof(wchar_t) == 0) {
            std::wstring value(size / sizeof(wchar_t), L'\0');
            const auto capacity = size;
            status = RegQueryValueExW(
                key, L"Root", nullptr, &type, reinterpret_cast<BYTE*>(value.data()), &size);
            if (status == ERROR_SUCCESS && type == REG_SZ && size <= capacity
                && size >= sizeof(wchar_t) && size % sizeof(wchar_t) == 0) {
                value.resize(size / sizeof(wchar_t));
                if (value.back() == L'\0') {
                    value.pop_back();
                    if (value.find(L'\0') == std::wstring::npos) {
                        result.emplace_back(value);
                    }
                }
            }
        }
        RegCloseKey(key);
    }
#elif defined(__linux__) || defined(__APPLE__)
    return Posix::roots(fingerprint);
#else
    (void)fingerprint;
#endif
    return result;
}
//=============================================================================
} // namespace Nelson::SharedRuntimeRegistry
//=============================================================================
