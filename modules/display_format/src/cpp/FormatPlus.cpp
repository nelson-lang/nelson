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
#include <boost/algorithm/string.hpp>
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "IEEEFP.hpp"
#include "FormatPlus.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatPlus(double number, bool trim)
{
    std::wstring result;
    if (number == 0) {
        if (trim) {
            result = L"";
        } else {
            result = L" ";
        }
    } else if (number < 0) {
        result = L"-";
    } else {
        result = L"+";
    }
    return result;
}
//=============================================================================
std::wstring
formatPlus(single number, bool trim)
{
    return formatPlus((double)number, trim);
}
//=============================================================================
std::wstring
formatComplexPlus(double realPart, double imagPart, bool trim)
{
    return formatPlus(realPart, trim);
}
//=============================================================================
std::wstring
formatComplexPlus(single realPart, single imagPart, bool trim)
{
    return formatPlus(realPart, trim);
}
//=============================================================================
}
//=============================================================================
