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
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include <vector>
#include <algorithm>
#include <unicode/unistr.h>
#include <unicode/ucsdet.h>
#include <unicode/ucnv.h>
#include <unicode/uchar.h>
#include <unicode/ustring.h>
#include "characters_encoding.hpp"
//=============================================================================
#define USE_SIMDUTF
//=============================================================================
#ifdef USE_SIMDUTF
#include "simdutf.h"
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
getSystemEncoding()
{
    std::string res = ucnv_getDefaultName();
    return res;
}
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
        return {};
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
        return {};
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
static inline bool
utf8_to_wstring_ICU(const std::string& str, std::wstring& wstr)
{
    // Boost.Locale slower than direct ICU4C here
    std::wstring result;
    if (str.empty()) {
        wstr.clear();
        return true;
    }
    std::vector<UChar> buffer;
    result.resize(str.size());
    buffer.resize(str.size());
    UErrorCode status = U_ZERO_ERROR;
    int32_t len = 0;
    u_strFromUTF8(&buffer[0], (int32_t)buffer.size(), &len, &str[0], (int32_t)str.size(), &status);
    if (U_FAILURE(status)) {
        return false;
    }
    buffer.resize(len);
    u_strToWCS(
        &result[0], (int32_t)result.size(), &len, &buffer[0], (int32_t)buffer.size(), &status);
    if (U_FAILURE(status)) {
        return false;
    }
    result.resize((size_t)len);
    wstr = result;
    return true;
}
//=============================================================================
static inline bool
wstring_to_utf8_ICU(const std::wstring& wstr, std::string& asUft8)
{
    if (wstr.empty()) {
        asUft8.clear();
        return true;
    }
    // Boost.Locale slower than direct ICU4C here
    std::vector<UChar> buffer;
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
    asUft8 = result;
    return true;
}
//=============================================================================
static inline bool
wstring_utf16le_to_utf8_SIMD(const std::wstring& wstr, std::string& asUft8)
{
    if (wstr.empty()) {
        asUft8.clear();
        return true;
    }
    asUft8.clear();
    asUft8.resize(wstr.size() * 4);
    size_t validSize = simdutf::convert_valid_utf16le_to_utf8(
        (char16_t*)wstr.c_str(), wstr.length(), (char*)asUft8.data());
    if (validSize > 0) {
        asUft8.resize(validSize);
    }
    return validSize > 0;
}
//=============================================================================
static inline bool
wstring_utf16be_to_utf8_SIMD(const std::wstring& wstr, std::string& asUft8)
{
    if (wstr.empty()) {
        asUft8.clear();
        return true;
    }
    asUft8.clear();
    asUft8.resize(wstr.size() * 4);
    size_t validSize = simdutf::convert_valid_utf16be_to_utf8(
        (char16_t*)wstr.c_str(), wstr.length(), (char*)asUft8.data());
    if (validSize > 0) {
        asUft8.resize(validSize);
    }
    return validSize > 0;
}
//=============================================================================
static inline bool
wstring_utf32_to_utf8_SIMD(const std::wstring& wstr, std::string& asUft8)
{
    if (wstr.empty()) {
        asUft8.clear();
        return true;
    }
    asUft8.clear();
    asUft8.resize(wstr.size() * 4);
    size_t validSize = simdutf::convert_valid_utf32_to_utf8(
        (char32_t*)wstr.c_str(), wstr.length(), (char*)asUft8.data());
    if (validSize > 0) {
        asUft8.resize(validSize);
    }
    return validSize > 0;
}
//=============================================================================
static inline bool
utf8_to_wstring_utf16le_SIMD(const std::string& str, std::wstring& wstr)
{
    if (str.empty()) {
        wstr.clear();
        return true;
    }
    wstr.clear();
    wstr.resize(str.size());
    size_t validSize
        = simdutf::convert_valid_utf8_to_utf16le(str.c_str(), str.length(), (char16_t*)wstr.data());
    if (validSize > 0) {
        wstr.resize(validSize);
    }
    return validSize > 0;
}
//=============================================================================
static inline bool
utf8_to_wstring_utf16be_SIMD(const std::string& str, std::wstring& wstr)
{
    if (str.empty()) {
        wstr.clear();
        return true;
    }
    wstr.clear();
    wstr.resize(str.size());
    size_t validSize
        = simdutf::convert_valid_utf8_to_utf16be(str.c_str(), str.length(), (char16_t*)wstr.data());
    if (validSize > 0) {
        wstr.resize(validSize);
    }
    return validSize > 0;
}
//=============================================================================
static inline bool
utf8_to_wstring_utf32_SIMD(const std::string& str, std::wstring& wstr)
{
    if (str.empty()) {
        wstr.clear();
        return true;
    }
    wstr.clear();
    wstr.resize(str.size());
    size_t validSize
        = simdutf::convert_valid_utf8_to_utf32(str.c_str(), str.length(), (char32_t*)wstr.data());
    if (validSize > 0) {
        wstr.resize(validSize);
    }
    return validSize > 0;
}
//=============================================================================
#ifdef _MSC_VER
bool
utf8_to_wstring_Windows(const std::string& str, std::wstring& wstr)
{
    int size
        = MultiByteToWideChar(CP_ACP, 0, str.c_str(), static_cast<int>(str.length()), nullptr, 0);
    if (size > 0) {
        std::wstring utf16_str(size, '\0');
        int res = MultiByteToWideChar(
            CP_ACP, 0, str.c_str(), static_cast<int>(str.length()), &utf16_str[0], size);
        if (res > 0) {
            wstr = utf16_str;
            return true;
        }
    }
    return false;
}
#endif
//=============================================================================
bool
utf8_to_wstring(const std::string& str, std::wstring& wstr)
{
    bool bResult = false;
#ifdef USE_SIMDUTF
#ifdef _MSC_VER
    bResult = utf8_to_wstring_utf16le_SIMD(str, wstr);
#else
    bResult = utf8_to_wstring_utf32_SIMD(str, wstr);
#endif
#else
    bResult = utf8_to_wstring_ICU(str, wstr);
#endif
#ifdef _MSC_VER
    if (!bResult) {
        bResult = utf8_to_wstring_Windows(str, wstr);
    }
#endif
    return bResult;
}
//=============================================================================
bool
wstring_to_utf8(const std::wstring& wstr, std::string& asUft8)
{
#ifdef USE_SIMDUTF
#ifdef _MSC_VER
    return wstring_utf16le_to_utf8_SIMD(wstr, asUft8);
#else
    return wstring_utf32_to_utf8_SIMD(wstr, asUft8);
#endif
#else
    return wstring_to_utf8_ICU(wstr, asUft8);
#endif
}
//=============================================================================
bool
isUnicodeLetter(wchar_t character)
{
    return (bool)u_isalpha((UChar32)(character));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
