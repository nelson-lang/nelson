//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsValidFieldname.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The names MakeValidFieldname leaves unchanged, checked without building them.
template <typename Text>
static bool
isValidFieldnameText(const Text& fieldname)
{
    auto isLetter = [](auto c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); };
    if (fieldname.empty() || !isLetter(fieldname[0])) {
        return false;
    }
    for (auto c : fieldname) {
        if (!isLetter(c) && !(c >= '0' && c <= '9') && c != '_') {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
IsValidFieldname(const std::string& fieldname)
{
    return isValidFieldnameText(fieldname);
}
//=============================================================================
bool
IsValidFieldname(const std::wstring& fieldname)
{
    return isValidFieldnameText(fieldname);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
