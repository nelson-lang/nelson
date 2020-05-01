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
#include "IsValidVariableName.hpp"
#include "Types.hpp"
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
