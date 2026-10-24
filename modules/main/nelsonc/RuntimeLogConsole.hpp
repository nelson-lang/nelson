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
#include "RuntimeLogPlatform.hpp"
//=============================================================================
namespace Nelson::RuntimeLogging {
class OutputForwarder
{
public:
    explicit OutputForwarder(NativeHandle handle) : handle(handle)
    {
#ifdef _WIN32
        DWORD mode = 0;
        console = GetConsoleMode(handle, &mode) != 0;
#endif
    }
    bool
    write(const char* bytes, size_t length)
    {
#ifdef _WIN32
        if (console) {
            pending.append(bytes, length);
            return flushConsole(false);
        }
#endif
        return writeAll(handle, bytes, length);
    }
    void
    finish()
    {
#ifdef _WIN32
        if (console) {
            flushConsole(true);
        }
#endif
    }

private:
    NativeHandle handle;
#ifdef _WIN32
    bool console = false;
    std::string pending;
    bool
    flushConsole(bool final)
    {
        const size_t length
            = final ? pending.size() : completeUtf8Prefix(pending.data(), pending.size());
        if (!length) {
            return true;
        }
        const int count
            = MultiByteToWideChar(CP_UTF8, 0, pending.data(), static_cast<int>(length), nullptr, 0);
        if (count <= 0) {
            return false;
        }
        std::wstring wide(static_cast<size_t>(count), L'\0');
        if (!MultiByteToWideChar(
                CP_UTF8, 0, pending.data(), static_cast<int>(length), wide.data(), count)) {
            return false;
        }
        size_t offset = 0;
        while (offset < wide.size()) {
            DWORD written = 0;
            if (!WriteConsoleW(handle, wide.data() + offset,
                    static_cast<DWORD>(wide.size() - offset), &written, nullptr)
                || written == 0) {
                return false;
            }
            offset += written;
        }
        pending.erase(0, length);
        return true;
    }
#endif
};
//=============================================================================
} // namespace Nelson::RuntimeLogging
//=============================================================================
