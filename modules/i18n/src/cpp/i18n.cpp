//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/locale.hpp>
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
gettextw(const std::wstring& S)
{
    if (S.empty()) {
        return {};
    }
    return boost::locale::translate<wchar_t>(S.c_str());
}
//=============================================================================
std::wstring
gettextw(const std::string& S)
{
    if (S.empty()) {
        return {};
    }
    return utf8_to_wstring(boost::locale::translate(S).str());
}
//=============================================================================
std::string
gettext(const std::string& S)
{
    if (S.empty()) {
        return {};
    }
    return boost::locale::translate(S).str();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
