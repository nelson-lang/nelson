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
#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GetVariableEnvironment(const std::wstring& envVarName, const std::wstring& defaultValue)
{
    std::wstring str(defaultValue);
#ifdef _MSC_VER
    const DWORD DEFAULT_SIZE_ENV = 4096;
    std::unique_ptr<wchar_t[]> buf(new (std::nothrow) wchar_t[DEFAULT_SIZE_ENV]);
    if (!buf) {
        return str;
    }

    DWORD dwRet = ::GetEnvironmentVariableW(envVarName.c_str(), buf.get(), DEFAULT_SIZE_ENV);
    if (dwRet == 0) {
        // error
        return str;
    }
    if (dwRet > DEFAULT_SIZE_ENV) {
        // we resize the buffer
        buf.reset(new (std::nothrow) wchar_t[dwRet + 1]);
        if (!buf) {
            return str;
        }
        dwRet = ::GetEnvironmentVariableW(envVarName.c_str(), buf.get(), dwRet + 1);
        if (dwRet == 0) {
            // error
            return str;
        }
    }
    str.assign(buf.get(), dwRet);
#else
    std::string s1 = wstring_to_utf8(envVarName);
    str = defaultValue;
    char const* tmp = std::getenv(s1.c_str());
    if (tmp != nullptr) {
        str = utf8_to_wstring(tmp);
    }
#endif
    return str;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
