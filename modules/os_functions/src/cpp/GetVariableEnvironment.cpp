//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
GetVariableEnvironment(std::wstring envVarName, std::wstring defaultValue)
{
    std::wstring str(defaultValue);
#ifdef _MSC_VER
#define DEFAULT_SIZE_ENV 4096
    wchar_t* buf = nullptr;
    try {
        buf = new wchar_t[DEFAULT_SIZE_ENV];
    } catch (const std::bad_alloc& e) {
        e.what();
        return str;
    }
    DWORD dwRet = ::GetEnvironmentVariableW(envVarName.c_str(), buf, DEFAULT_SIZE_ENV);
    if (dwRet == 0) {
        // error
        if (buf) {
            delete[] buf;
            buf = nullptr;
        }
        return str;
    } else {
        if (dwRet > DEFAULT_SIZE_ENV) {
            // we resize the buffer
            delete[] buf;
            try {
                buf = new wchar_t[dwRet + 1];
            } catch (const std::bad_alloc& e) {
                e.what();
                return str;
            }
            dwRet = ::GetEnvironmentVariableW(envVarName.c_str(), buf, dwRet);
        }
        if (dwRet == 0) {
            // error
            if (buf) {
                delete[] buf;
                buf = nullptr;
            }
            return str;
        }
        str = buf;
        delete[] buf;
    }
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
}
//=============================================================================
