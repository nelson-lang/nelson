//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdint>
#include <string>
#include <cwctype>
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
appendCodePointToWideString(uint32_t codePoint, std::wstring& wstr)
{
    if (codePoint > 0x10FFFF || (codePoint >= 0xD800 && codePoint <= 0xDFFF)) {
        return false;
    }
    if constexpr (sizeof(wchar_t) == 2) {
        if (codePoint <= 0xFFFF) {
            wstr.push_back(static_cast<wchar_t>(codePoint));
        } else {
            codePoint -= 0x10000;
            wstr.push_back(static_cast<wchar_t>(0xD800 + (codePoint >> 10)));
            wstr.push_back(static_cast<wchar_t>(0xDC00 + (codePoint & 0x3FF)));
        }
    } else {
        wstr.push_back(static_cast<wchar_t>(codePoint));
    }
    return true;
}
//=============================================================================
static void
appendCodePointToUtf8(uint32_t codePoint, std::string& asUtf8)
{
    if (codePoint <= 0x7F) {
        asUtf8.push_back(static_cast<char>(codePoint));
    } else if (codePoint <= 0x7FF) {
        asUtf8.push_back(static_cast<char>(0xC0 | (codePoint >> 6)));
        asUtf8.push_back(static_cast<char>(0x80 | (codePoint & 0x3F)));
    } else if (codePoint <= 0xFFFF) {
        asUtf8.push_back(static_cast<char>(0xE0 | (codePoint >> 12)));
        asUtf8.push_back(static_cast<char>(0x80 | ((codePoint >> 6) & 0x3F)));
        asUtf8.push_back(static_cast<char>(0x80 | (codePoint & 0x3F)));
    } else {
        asUtf8.push_back(static_cast<char>(0xF0 | (codePoint >> 18)));
        asUtf8.push_back(static_cast<char>(0x80 | ((codePoint >> 12) & 0x3F)));
        asUtf8.push_back(static_cast<char>(0x80 | ((codePoint >> 6) & 0x3F)));
        asUtf8.push_back(static_cast<char>(0x80 | (codePoint & 0x3F)));
    }
}
//=============================================================================
static bool
isUtf8EncodingName(const std::string& encoding)
{
    return encoding.empty() || encoding == "UTF-8" || encoding == "utf-8";
}
//=============================================================================
std::string
getSystemEncoding()
{
    return "UTF-8";
}
//=============================================================================
bool
isSupportedEncoding(const std::string& encoding)
{
    return ("UTF-8" == encoding);
}
//=============================================================================
bool
isSupportedEncoding(const std::wstring& encoding)
{
    return (L"UTF-8" == encoding);
}
//=============================================================================
std::string
detectBestEncoding(const std::string& data)
{
    std::string encoding = "UTF-8";
    return encoding;
}
//=============================================================================
std::vector<std::string>
detectEncodings(const std::string& data)
{
    std::vector<std::string> encodings;
    return encodings;
}
//=============================================================================
bool
utf8ToCharsetConverter(
    const std::string& utf8str, std::string& outputStr, const std::string& codeOut)
{
    if (!isUtf8EncodingName(codeOut)) {
        outputStr.clear();
        return false;
    }
    outputStr = utf8str;
    return true;
}
//=============================================================================
bool
charsetToUtf8Converter(const std::string& data, const std::string& codeIn, std::string& asUtf8)
{
    if (!isUtf8EncodingName(codeIn)) {
        asUtf8.clear();
        return false;
    }
    asUtf8 = data;
    return true;
}
//=============================================================================
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
#ifdef _MSC_VER
bool
utf8_to_wstring_Windows(const std::string& str, std::wstring& wstr)
{
    wstr = utf8_to_wstring(str);
    return true;
}
#endif
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
bool
utf8_to_wstring(const std::string& str, std::wstring& wstr)
{
    wstr.clear();
    wstr.reserve(str.size());
    for (size_t k = 0; k < str.size();) {
        const unsigned char first = static_cast<unsigned char>(str[k]);
        uint32_t codePoint = 0;
        size_t extraBytes = 0;
        if (first <= 0x7F) {
            codePoint = first;
        } else if (first >= 0xC2 && first <= 0xDF) {
            codePoint = first & 0x1F;
            extraBytes = 1;
        } else if (first >= 0xE0 && first <= 0xEF) {
            codePoint = first & 0x0F;
            extraBytes = 2;
        } else if (first >= 0xF0 && first <= 0xF4) {
            codePoint = first & 0x07;
            extraBytes = 3;
        } else {
            wstr.clear();
            return false;
        }
        if (k + extraBytes >= str.size()) {
            wstr.clear();
            return false;
        }
        for (size_t n = 1; n <= extraBytes; ++n) {
            const unsigned char current = static_cast<unsigned char>(str[k + n]);
            if ((current & 0xC0) != 0x80) {
                wstr.clear();
                return false;
            }
            codePoint = (codePoint << 6) | (current & 0x3F);
        }
        const bool overlong = (extraBytes == 1 && codePoint <= 0x7F)
            || (extraBytes == 2 && codePoint <= 0x7FF) || (extraBytes == 3 && codePoint <= 0xFFFF);
        if (overlong || !appendCodePointToWideString(codePoint, wstr)) {
            wstr.clear();
            return false;
        }
        k += extraBytes + 1;
    }
    return true;
}
//=============================================================================
bool
wstring_to_utf8(const std::wstring& wstr, std::string& asUft8)
{
    asUft8.clear();
    asUft8.reserve(wstr.size());
    for (size_t k = 0; k < wstr.size(); ++k) {
        uint32_t codePoint = static_cast<uint32_t>(wstr[k]);
        if constexpr (sizeof(wchar_t) == 2) {
            if (codePoint >= 0xD800 && codePoint <= 0xDBFF) {
                if (++k >= wstr.size()) {
                    asUft8.clear();
                    return false;
                }
                const uint32_t low = static_cast<uint32_t>(wstr[k]);
                if (low < 0xDC00 || low > 0xDFFF) {
                    asUft8.clear();
                    return false;
                }
                codePoint = 0x10000 + (((codePoint - 0xD800) << 10) | (low - 0xDC00));
            } else if (codePoint >= 0xDC00 && codePoint <= 0xDFFF) {
                asUft8.clear();
                return false;
            }
        }
        if (codePoint > 0x10FFFF || (codePoint >= 0xD800 && codePoint <= 0xDFFF)) {
            asUft8.clear();
            return false;
        }
        appendCodePointToUtf8(codePoint, asUft8);
    }
    return true;
}
//=============================================================================
bool
isUnicodeLetter(wchar_t character)
{
    return (bool)std::iswalpha(character);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
