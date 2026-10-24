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
#ifdef _WIN32
#include <windows.h>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
class ApplicationWindowsResources
{
public:
    explicit ApplicationWindowsResources(const std::wstring& filename)
        : handle(BeginUpdateResourceW(filename.c_str(), FALSE))
    {
        if (!handle) {
            fail("Cannot open application launcher resources.");
        }
    }
    ~ApplicationWindowsResources()
    {
        if (handle) {
            EndUpdateResourceW(handle, TRUE);
        }
    }
    ApplicationWindowsResources(const ApplicationWindowsResources&) = delete;
    ApplicationWindowsResources&
    operator=(const ApplicationWindowsResources&)
        = delete;
    void
    set(LPCWSTR type, LPCWSTR name, WORD language, const std::vector<uint8_t>& data)
    {
        if (!UpdateResourceW(handle, type, name, language,
                data.empty() ? nullptr : const_cast<uint8_t*>(data.data()),
                static_cast<DWORD>(data.size()))) {
            fail("Cannot update application resources.");
        }
    }
    void
    commit()
    {
        const auto current = handle;
        handle = nullptr;
        if (!EndUpdateResourceW(current, FALSE)) {
            fail("Cannot save application resources.");
        }
    }

private:
    HANDLE handle;
    [[noreturn]] static void
    fail(const char* message)
    {
        const auto code = GetLastError();
        throw std::runtime_error(
            std::string(message) + " (Windows error " + std::to_string(code) + ").");
    }
};
//=============================================================================
} // namespace Nelson
#endif
//=============================================================================
