//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include "nlsCharacters_encoding_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum CHARSETENCODING
{
    NLS_CHARSET_UTF8 = 0,
    NLS_CHARSET_UNICODE = 1,
    NLS_CHARSET_LATIN1 = 2,
    NLS_CHARSET_SHIFT_JIS = 3
};
//=============================================================================
NLSCHARACTERS_ENCODING_IMPEXP bool
isSupportedEncoding(const std::string& encoding);
//=============================================================================
NLSCHARACTERS_ENCODING_IMPEXP bool
isSupportedEncoding(const std::wstring& encoding);
//=============================================================================
NLSCHARACTERS_ENCODING_IMPEXP std::string
detectBestEncoding(const std::string& data);
//=============================================================================
NLSCHARACTERS_ENCODING_IMPEXP std::vector<std::string>
detectEncodings(const std::string& data);
//=============================================================================
NLSCHARACTERS_ENCODING_IMPEXP bool
utf8ToCharsetConverter(
    const std::string& utf8str, std::string& outputStr, const std::string& codeOut);
//=============================================================================
NLSCHARACTERS_ENCODING_IMPEXP bool
charsetToUtf8Converter(const std::string& data, const std::string& codeIn, std::string& asUtf8);
//=============================================================================
// convert UTF-8 string to wstring
NLSCHARACTERS_ENCODING_IMPEXP bool
utf8_to_wstring(const std::string& str, std::wstring& wstr);
NLSCHARACTERS_ENCODING_IMPEXP std::wstring
utf8_to_wstring(const std::string& str);
NLSCHARACTERS_ENCODING_IMPEXP std::wstring
utf8_to_wstring(const char* str);
NLSCHARACTERS_ENCODING_IMPEXP std::vector<std::wstring>
utf8_to_wstring(const std::vector<std::string>& strs);
//=============================================================================
// convert wstring to UTF-8 string
NLSCHARACTERS_ENCODING_IMPEXP bool
wstring_to_utf8(const std::wstring& wstr, std::string& asUft8);
NLSCHARACTERS_ENCODING_IMPEXP std::string
wstring_to_utf8(const std::wstring& str);
NLSCHARACTERS_ENCODING_IMPEXP std::string
wstring_to_utf8(const wchar_t* str);
NLSCHARACTERS_ENCODING_IMPEXP std::vector<std::string>
wstring_to_utf8(const std::vector<std::wstring>& strs);
//=============================================================================
#ifdef _MSC_BUILD
NLSCHARACTERS_ENCODING_IMPEXP
bool
utf8_to_wstring_Windows(const std::string& str, std::wstring& wstr);
#endif
//=============================================================================
NLSCHARACTERS_ENCODING_IMPEXP bool
isUnicodeLetter(wchar_t character);
//=============================================================================
} // namespace Nelson
//=============================================================================
