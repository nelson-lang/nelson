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
#endif
#include <unicode/unistr.h>
#include <unicode/ucsdet.h>
#include <unicode/ucnv.h>
#include <boost/container/vector.hpp>
#include <boost/locale.hpp>
#include <unicode/ustring.h>
#include <algorithm>
#include "characters_encoding.hpp"
//=============================================================================
#ifdef _MSC_VER
#pragma comment(lib, "icuuc.lib")
#pragma comment(lib, "icuin.lib")
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isSupportedEncoding(const std::string& encoding)
{
    UErrorCode status = U_ZERO_ERROR;
    UConverter* conv = ucnv_open(encoding.c_str(), &status);
    if (!U_SUCCESS(status)) {
        return false;
    }
    if (conv) {
        ucnv_close(conv);
    }
    return true;
}
//=============================================================================
bool
isSupportedEncoding(const std::wstring& encoding)
{
    std::string asUtf8 = wstring_to_utf8(encoding);
    return isSupportedEncoding(asUtf8);
}
//=============================================================================
std::string
detectBestEncoding(const std::string& data)
{
    std::string encoding = "UTF-8";
    if (!data.empty()) {
        UErrorCode status = U_ZERO_ERROR;
        UCharsetDetector* detector = ucsdet_open(&status);
        std::string s = data;
        ucsdet_setText(detector, s.c_str(), (int32_t)s.length(), &status);
        const UCharsetMatch* match = ucsdet_detect(detector, &status);
        if (match != nullptr) {
            encoding = ucsdet_getName(match, &status);
        }
        ucsdet_close(detector);
    }
    return encoding;
}
//=============================================================================
std::vector<std::string>
detectEncodings(const std::string& data)
{
    // Collect all candidates, most confident comes first
    std::vector<std::string> encodings;
    if (!data.empty()) {
        UErrorCode status = U_ZERO_ERROR;
        UCharsetDetector* detector = ucsdet_open(&status);
        std::string s = data;
        ucsdet_setText(detector, s.c_str(), (int32_t)s.length(), &status);
        int32_t matches_count;
        const UCharsetMatch** matches = ucsdet_detectAll(detector, &matches_count, &status);
        if (matches != nullptr) {
            for (int i = 0; i < matches_count; ++i) {
                std::string encoding = ucsdet_getName(matches[i], &status);
                encodings.push_back(encoding);
            }
        }
        ucsdet_close(detector);
    }
    return encodings;
}
//=============================================================================
bool
utf8ToCharsetConverter(
    const std::string& utf8str, std::string& outputStr, const std::string& codeOut)
{
    icu::UnicodeString uStr = icu::UnicodeString::fromUTF8(utf8str.c_str());
    if (uStr.isBogus()) {
        outputStr.clear();
        return false;
    }
    int32_t l = uStr.extract(0, uStr.length(), nullptr, 0, codeOut.c_str());
    std::vector<char> tmp(l + 1);
    char* target = &tmp[0];
    int32_t s = uStr.extract(0, uStr.length(), target, l, codeOut.c_str());
    if (l != s) {
        outputStr.clear();
        return false;
    }
    outputStr = std::string(target);
    return true;
}
//=============================================================================
bool
charsetToUtf8Converter(const std::string& data, const std::string& codeIn, std::string& asUtf8)
{
    icu::UnicodeString uStr = icu::UnicodeString(data.c_str(), codeIn.c_str());
    if (uStr.isBogus()) {
        asUtf8.clear();
        return false;
    }
    uStr.toUTF8String(asUtf8);
    const uint8_t* src = reinterpret_cast<const uint8_t*>(asUtf8.data());

    if (uStr.isBogus()) {
        asUtf8.clear();
        return false;
    }
    return true;
}
//=============================================================================
static std::wstring wcacheRes;
static std::string wcacheSrc;
//=============================================================================
bool
utf8_to_wstring(const std::string& str, std::wstring& wstr)
{
    // Boost.Locale slower than direct ICU4C here
    std::wstring result;
    if (str.empty()) {
        wstr.clear();
        return true;
    }
    if (wcacheSrc == str) {
        wstr = wcacheRes;
        return true;
    }
    boost::container::vector<UChar> buffer;
    result.resize(str.size());
    buffer.resize(str.size());
    UErrorCode status = U_ZERO_ERROR;
    int32_t len = 0;
    u_strFromUTF8(&buffer[0], (int32_t)buffer.size(), &len, &str[0], (int32_t)str.size(), &status);
    if (U_FAILURE(status)) {
#ifdef _MSC_VER
        int size = MultiByteToWideChar(
            CP_ACP, MB_COMPOSITE, str.c_str(), static_cast<int>(str.length()), nullptr, 0);
        if (size > 0) {
            std::wstring utf16_str(size, '\0');
            int res = MultiByteToWideChar(CP_ACP, MB_COMPOSITE, str.c_str(),
                static_cast<int>(str.length()), &utf16_str[0], size);
            if (res > 0) {
                wstr = utf16_str;
                return true;
            }
        }
#endif
        return false;
    }
    buffer.resize(len);
    u_strToWCS(
        &result[0], (int32_t)result.size(), &len, &buffer[0], (int32_t)buffer.size(), &status);
    if (U_FAILURE(status)) {
        return false;
    }
    result.resize((size_t)len);
    wcacheSrc = str;
    wcacheRes = result;
    wstr = result;
    return true;
}
//=============================================================================
// convert UTF-8 string to wstring
std::wstring
utf8_to_wstring(const std::string& str)
{
    std::wstring result;
    if (!utf8_to_wstring(str, result)) {
        return std::wstring(L"!!!ERROR CHARACTER SET CONVERSION!!!");
    }
    return result;
}
//=============================================================================
static std::wstring ucacheSrc;
static std::string ucacheRes;
//=============================================================================
bool
wstring_to_utf8(const std::wstring& wstr, std::string& asUft8)
{
    if (wstr.empty()) {
        asUft8.clear();
        return true;
    }
    if (ucacheSrc == wstr) {
        asUft8 = ucacheRes;
        return true;
    }
    // Boost.Locale slower than direct ICU4C here
    boost::container::vector<UChar> buffer;
    std::string result;
    result.resize(wstr.size() * 4); // UTF-8 uses max 4 bytes per char
    buffer.resize(wstr.size() * 2); // UTF-16 uses 2 code-points per char
    UErrorCode status = U_ZERO_ERROR;
    int32_t len = 0;
    u_strFromWCS(&buffer[0], (int32_t)buffer.size(), &len, &wstr[0], (int32_t)wstr.size(), &status);
    if (U_FAILURE(status)) {
        asUft8.clear();
        return false;
    }
    buffer.resize(len);
    u_strToUTF8(
        &result[0], (int32_t)result.size(), &len, &buffer[0], (int32_t)buffer.size(), &status);
    if (U_FAILURE(status)) {
        asUft8.clear();
        return false;
    }
    result.resize(static_cast<size_t>(len));
    ucacheSrc = wstr;
    ucacheRes = result;
    asUft8 = result;
    return true;
}
//=============================================================================
// convert wstring to UTF-8 string
std::string
wstring_to_utf8(const std::wstring& str)
{
    std::string result;
    if (!wstring_to_utf8(str, result)) {
        return std::string("!!!ERROR CHARACTER SET CONVERSION!!!");
    }
    return result;
}
//=============================================================================
std::wstring
utf8_to_wstring(const char* str)
{
    if (str == nullptr) {
        return std::wstring();
    }
    return utf8_to_wstring(std::string(str));
}
//=============================================================================
std::vector<std::wstring>
utf8_to_wstring(const std::vector<std::string>& strs)
{
    std::vector<std::wstring> wideVector;
    wideVector.reserve(strs.size());
    for (const auto& s : strs) {
        wideVector.push_back(utf8_to_wstring(s));
    }
    return wideVector;
}
//=============================================================================
std::string
wstring_to_utf8(const wchar_t* str)
{
    if (str == nullptr) {
        return std::string();
    }
    return wstring_to_utf8(std::wstring(str));
}
//=============================================================================
std::vector<std::string>
wstring_to_utf8(const std::vector<std::wstring>& strs)
{
    std::vector<std::string> utf8Vector;
    utf8Vector.reserve(strs.size());
    for (const auto& s : strs) {
        utf8Vector.push_back(wstring_to_utf8(s));
    }
    return utf8Vector;
}
//=============================================================================
std::string
getSystemEncoding()
{
    std::string res = ucnv_getDefaultName();
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
