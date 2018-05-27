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
#include "Assert_IsFalse.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
logical
Assert_IsFalse(logical value, std::wstring modifiedmsg, std::wstring& msg)
{
    if (value != 0) {
        value = (logical)0;
        if (!modifiedmsg.empty()) {
            msg = modifiedmsg;
        } else {
            msg = _W("Assertion failed: found false entry in condition = true.");
        }
    } else {
        value = (logical)1;
        msg = L"";
    }
    return value;
}
//=============================================================================
}
//=============================================================================
