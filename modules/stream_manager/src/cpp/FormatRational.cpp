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
#include "FormatLong.hpp"
#include "IEEEFP.hpp"
#include "FloatNumberToRational.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatRational(double number, bool trim)
{
    std::wstring str;
    if (IsIntegerForm(number)) {
        str = fmt::sprintf(L"%16.f", number);
        boost::trim_left(str);
    } else if (std::abs(number) < 1e-10) {
        str = floatNumberToApproxRational<double, int64>(number, 10);
    } else {
        str = floatNumberToApproxRational<double, int>(number, 9);
    }
    if (trim) {
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatRational(single number, bool trim)
{
    std::wstring str;
    if (std::abs(number) < 1e-10) {
        str = floatNumberToApproxRational<double, int64>(number, 10);
    } else {
        str = floatNumberToApproxRational<double, int>(number, 9);
    }
    if (trim) {
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatComplexRational(double realPart, double imagPart, bool trim)
{
    std::wstring signStr;
    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }
    return formatRational(realPart, trim) + signStr + formatRational(fabs(imagPart), trim) + L"i";
}
//=============================================================================
std::wstring
formatComplexRational(single realPart, single imagPart, bool trim)
{
    return formatComplexRational((double)realPart, (double)imagPart, trim);
}
//=============================================================================
}
//=============================================================================
