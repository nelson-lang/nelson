//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#ifdef _MSC_VER
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
#define DEFAULT_SIZE_ENV 4096
    wchar_t* buf = nullptr;
    try {
        buf = new wchar_t[DEFAULT_SIZE_ENV];
    } catch (const std::bad_alloc&) {
        return str;
    }
    DWORD dwRet = ::GetEnvironmentVariableW(envVarName.c_str(), buf, DEFAULT_SIZE_ENV);
    if (dwRet == 0) {
        // error
        if (buf != nullptr) {
            delete[] buf;
            buf = nullptr;
        }
        return str;
    }
    if (dwRet > DEFAULT_SIZE_ENV) {
        // we resize the buffer
        delete[] buf;
        try {
            buf = new wchar_t[dwRet + 1];
        } catch (const std::bad_alloc&) {
            return str;
        }
        dwRet = ::GetEnvironmentVariableW(envVarName.c_str(), buf, dwRet);
    }
    if (dwRet == 0) {
        // error
        if (buf != nullptr) {
            delete[] buf;
            buf = nullptr;
        }
        return str;
    }
    str = buf;
    delete[] buf;

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
