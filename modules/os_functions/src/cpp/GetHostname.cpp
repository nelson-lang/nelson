//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GetHostname.hpp"
#include "characters_encoding.hpp"

#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#include <limits.h>
#include <sys/types.h>
#endif

//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GetHostname()
{
#ifdef _MSC_VER
    wchar_t buf[MAX_COMPUTERNAME_LENGTH + 1];
    DWORD size = (DWORD)std::size(buf);
    if (GetComputerNameW(buf, &size)) {
        return std::wstring(buf, static_cast<size_t>(size));
    }
    // fallback to ANSI name if wide call fails
    char abuf[MAX_COMPUTERNAME_LENGTH + 1];
    DWORD asize = (DWORD)std::size(abuf);
    if (GetComputerNameA(abuf, &asize)) {
        return utf8_to_wstring(std::string(abuf, static_cast<size_t>(asize)));
    }
    return std::wstring();
#else
    long host_max = sysconf(_SC_HOST_NAME_MAX);
    size_t len = 0;
    if (host_max > 0) {
        len = static_cast<size_t>(host_max) + 1;
    } else {
#ifdef HOST_NAME_MAX
        len = HOST_NAME_MAX + 1;
#else
        len = 256;
#endif
    }
    std::vector<char> buf(len);
    if (gethostname(buf.data(), buf.size()) == 0) {
        buf[buf.size() - 1] = '\0';
        return utf8_to_wstring(std::string(buf.data()));
    }
    return std::wstring();
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
