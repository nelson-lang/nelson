//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "IsValidVariableName.hpp"
#include "Types.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsValidVariableName(std::string varname)
{
    if (!varname.size()) {
        return false;
    }
    int c = varname[0];
    if ((c >= 48 && c <= 57) || (c == '_')) {
        return false;
    }
    for (sizeType k = 0; k < (sizeType)varname.size(); k++) {
        int c = varname[k];
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
IsValidVariableName(std::wstring varname)
{
    return IsValidVariableName(wstring_to_utf8(varname));
}
//=============================================================================
}
//=============================================================================
