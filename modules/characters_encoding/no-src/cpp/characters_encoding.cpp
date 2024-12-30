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
#define _SILENCE_CXX17_CODECVT_HEADER_DEPRECATION_WARNING
#endif
//=============================================================================
#include <cwctype>
#include <locale>
#include <codecvt>
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
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
    return false;
}
//=============================================================================
bool
charsetToUtf8Converter(const std::string& data, const std::string& codeIn, std::string& asUtf8)
{
    return false;
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
    using convert_typeX = std::codecvt_utf8<wchar_t>;
    std::wstring_convert<convert_typeX, wchar_t> converterX;

    wstr = converterX.from_bytes(str);
    return true;
}
//=============================================================================
bool
wstring_to_utf8(const std::wstring& wstr, std::string& asUft8)
{
    using convert_typeX = std::codecvt_utf8<wchar_t>;
    std::wstring_convert<convert_typeX, wchar_t> converterX;

    asUft8 = converterX.to_bytes(wstr);
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
