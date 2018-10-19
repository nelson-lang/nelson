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
#pragma once
//=============================================================================
#include <cstdarg>
#include <string>
#include <vector>
#include <wchar.h>
//=============================================================================
namespace Nelson {
//=============================================================================
inline std::wstring
StringFormat(const wchar_t* format, ...)
{
    if (!format) {
        return L"";
    }
    std::vector<wchar_t> buff;
    size_t len = wcslen(format);
    size_t size = 1024;
    if (len >= 1024) {
        size = len * 2;
    }
    buff.resize(size);
    va_list ap;
    va_start(ap, format);
    while (true) {
#ifdef _MSC_VER
        int ret = _vsnwprintf_s(buff.data(), size, _TRUNCATE, format, ap);
#else
        int ret = vswprintf(buff.data(), size, format, ap);
#endif
        if (ret != -1) {
            break;
        } else {
            size *= 2;
            buff.resize(size);
        }
    }
    va_end(ap);
    return std::wstring(buff.data());
}
//=============================================================================
inline std::string
StringFormat(const char* format, ...)
{
    if (!format) {
        return "";
    }
    std::vector<char> buff;
    size_t len = strlen(format);
    size_t size = 1024;
    if (len >= 1024) {
        size = len * 2;
    }
    buff.resize(size);
    va_list ap;
    va_start(ap, format);
    while (true) {
#ifdef _MSC_VER
        int ret = _vsnprintf_s(buff.data(), size, _TRUNCATE, format, ap);
#else
        int ret = vsnprintf(buff.data(), size, format, ap);
#endif
        if (ret != -1) {
            break;
        } else {
            size *= 2;
            buff.resize(size);
        }
    }
    va_end(ap);
    return std::string(buff.data());
}
//=============================================================================
}
//=============================================================================
