//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <regex>
#include "StringHelpers.hpp"
#include "MakeValidFieldname.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
MakeValidFieldname(const std::string& fieldname, const std::string& defaultPrefix)
{
    return wstring_to_utf8(
        MakeValidFieldname(utf8_to_wstring(fieldname), utf8_to_wstring(defaultPrefix)));
}
//=============================================================================
std::wstring
MakeValidFieldname(const std::wstring& fieldname, const std::wstring& defaultPrefix)
{
    if (fieldname.empty()) {
        return defaultPrefix;
    }
    std::wstring modifiedFieldname = fieldname;
    std::wregex re(L"[^a-zA-Z_0-9]");
    modifiedFieldname = std::regex_replace(fieldname, re, L"_");
    if (StringHelpers::starts_with(modifiedFieldname, L"_")) {
        modifiedFieldname = defaultPrefix + modifiedFieldname;
    }
    if (iswdigit(modifiedFieldname[0])) {
        modifiedFieldname = defaultPrefix + L"_" + modifiedFieldname;
    }
    if (!iswalpha(modifiedFieldname[0])) {
        modifiedFieldname = defaultPrefix + modifiedFieldname;
    }
    return modifiedFieldname;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
