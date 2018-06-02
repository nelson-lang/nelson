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
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "characters_encoding.hpp"
#include <boost/container/vector.hpp>
#include <unicode/ustring.h>
//=============================================================================
#ifdef _MSC_VER
#pragma comment(lib, "icuuc.lib")
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring wcacheRes;
static std::string wcacheSrc;
//=============================================================================
// convert UTF-8 string to wstring
std::wstring
utf8_to_wstring(const std::string& str)
{
    std::wstring result;
    if (str.empty()) {
        return result;
    } else {
        if (wcacheSrc == str) {
            return wcacheRes;
        }
    }
    boost::container::vector<UChar> buffer;
    result.resize(str.size());
    buffer.resize(str.size());
    UErrorCode status = U_ZERO_ERROR;
    int32_t len = 0;
    u_strFromUTF8(&buffer[0], (int32_t)buffer.size(), &len, &str[0], (int32_t)str.size(), &status);
    if (U_FAILURE(status)) {
#ifdef _MSC_VER
        int size
            = MultiByteToWideChar(CP_ACP, MB_COMPOSITE, str.c_str(), (int)str.length(), nullptr, 0);
        if (size > 0) {
            std::wstring utf16_str(size, '\0');
            int res = MultiByteToWideChar(
                CP_ACP, MB_COMPOSITE, str.c_str(), (int)str.length(), &utf16_str[0], size);
            if (res > 0) {
                return utf16_str;
            }
        }
#endif
        return std::wstring(L"!!!ERROR ICU!!!");
    }
    buffer.resize(len);
    u_strToWCS(
        &result[0], (int32_t)result.size(), &len, &buffer[0], (int32_t)buffer.size(), &status);
    if (U_FAILURE(status)) {
        return std::wstring(L"!!!ERROR ICU!!!");
    }
    result.resize(len);
    wcacheSrc = str;
    wcacheRes = result;
    return result;
}
//=============================================================================
static std::wstring ucacheSrc;
static std::string ucacheRes;
//=============================================================================
// convert wstring to UTF-8 string
std::string
wstring_to_utf8(const std::wstring& str)
{
    std::string result;
    if (str.empty()) {
        return result;
    } else {
        if (ucacheSrc == str) {
            return ucacheRes;
        }
    }
    boost::container::vector<UChar> buffer;
    result.resize(str.size() * 4); // UTF-8 uses max 4 bytes per char
    buffer.resize(str.size() * 2); // UTF-16 uses 2 code-points per char
    UErrorCode status = U_ZERO_ERROR;
    int32_t len = 0;
    u_strFromWCS(&buffer[0], (int32_t)buffer.size(), &len, &str[0], (int32_t)str.size(), &status);
    if (U_FAILURE(status)) {
        return std::string("!!!ERROR ICU!!!");
    }
    buffer.resize(len);
    u_strToUTF8(
        &result[0], (int32_t)result.size(), &len, &buffer[0], (int32_t)buffer.size(), &status);
    if (U_FAILURE(status)) {
        return std::string("!!!ERROR ICU!!!");
    }
    result.resize(len);
    ucacheSrc = str;
    ucacheRes = result;
    return result;
}
//=============================================================================
std::wstring
utf8_to_wstring(const char* str)
{
    if (str == nullptr) {
        return std::wstring(L"");
    }
    return utf8_to_wstring(std::string(str));
}
//=============================================================================
std::string
wstring_to_utf8(const wchar_t* str)
{
    if (str == nullptr) {
        return std::string("");
    }
    return wstring_to_utf8(std::wstring(str));
}
//=============================================================================
}
//=============================================================================
