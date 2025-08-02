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
#include <Windows.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#endif
#include "GetUsername.hpp"
#include "GetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring username;
//=============================================================================
std::wstring
GetUsername()
{
    if (username.empty()) {
#ifdef _MSC_VER
#define INFO_BUFFER_SIZE 32767
        TCHAR infoBuf[INFO_BUFFER_SIZE];
        DWORD bufCharCount = INFO_BUFFER_SIZE;
        if (!GetUserName(infoBuf, &bufCharCount)) {
            username = GetVariableEnvironment(L"USERNAME");
        } else {
            username = infoBuf;
        }
#else
        username = GetVariableEnvironment(L"USER");
        if (username.empty()) {
            username = GetVariableEnvironment(L"LOGNAME");
        }
        if (username.empty()) {
            uid_t uid = geteuid();
            struct passwd* pw = getpwuid(uid);
            if (pw != nullptr) {
                username = utf8_to_wstring(std::string(pw->pw_name));
            }
        }
#endif
    }
    return username;
}
//=============================================================================
}
