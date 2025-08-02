//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsValidVariableName(const std::string& varname, bool withUnderscore)
{
    if (varname.empty()) {
        return false;
    }
    int c = varname[0];
    if (withUnderscore) {
        if (c >= 48 && c <= 57) {
            return false;
        }
    } else {
        if ((c >= 48 && c <= 57) || (c == '_')) {
            return false;
        }
    }
    for (int c : varname) {
        bool bSupportedChar
            = (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || (c == '_') || (c >= 48 && c <= 57);
        if (!bSupportedChar) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
IsValidVariableName(const std::wstring& varname, bool withUnderscore)
{
    return IsValidVariableName(wstring_to_utf8(varname), withUnderscore);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
