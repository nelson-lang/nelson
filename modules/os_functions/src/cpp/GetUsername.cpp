//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
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
