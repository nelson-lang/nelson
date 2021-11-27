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
#include "FormatBank.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatBank(double number, bool trim)
{
    std::wstring result;

    if (std::isnan(number)) {
        std::wstring format = L"%*s";
        result = fmt::sprintf(format, 13, L"NaN");
    } else if (std::isinf(number)) {
        std::wstring format = L"%*s";
        if (number < 0) {
            result = fmt::sprintf(format, 13, L"-Inf");
        } else {
            result = fmt::sprintf(format, 13, L"Inf");
        }
    } else {
        std::wstring format = L"%13.2f";
        result = fmt::sprintf(format, number);
    }
    if (result != L" ") {
        result = L" " + result;
    }
    if (trim) {
        boost::trim_left(result);
    }
    return result;
}
//=============================================================================
std::wstring
formatBank(single number, bool trim)
{
    return formatBank((double)number, trim);
}
//=============================================================================
std::wstring
formatComplexBank(double realPart, double imagPart, bool trim)
{
    return formatBank(realPart, trim);
}
//=============================================================================
std::wstring
formatComplexBank(single realPart, single imagPart, bool trim)
{
    return formatBank(realPart, trim);
}
//=============================================================================
}
//=============================================================================
