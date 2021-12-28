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
#include "FormatLongEng.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatLongEng(double number, bool trim)
{
    std::wstring str;
    if (IsInfinite(number)) {
        std::wstring format = L"%*s";
        if (number < 0) {
            str = L"                     -Inf";
        } else {
            str = L"                      Inf";
        }
    } else if (IsNaN(number)) {
        str = L"                      NaN";
    } else {
        int exponent = 0;
        if (number != 0) {
            double absval = (number < 0 ? -number : number);
            int logabsval = static_cast<int>(std::floor(log10(absval)));
            if (logabsval < 0) {
                exponent = logabsval - 2 + ((-logabsval + 2) % 3);
            } else {
                exponent = logabsval - (logabsval % 3);
            }
        }
        double mantissa = number / std::pow(static_cast<double>(10), exponent);
        std::wstring expStr;
        expStr.reserve(8);
        if (exponent >= 0) {
            expStr = L"e+";
        } else {
            exponent = -exponent;
            expStr = L"e-";
        }
        std::wstring exponentAsString = fmt::to_wstring(exponent);
        if (exponentAsString.length() < 3) {
            expStr.append(3 - exponentAsString.length(), L'0');
        }
        std::wstring format = L"%.15f";
        str = fmt::sprintf(format, mantissa);
        if (str.find(L'.') != std::string::npos) {
            str = str.substr(0, 16);

        } else {
            str = str.substr(0, 15);
        }
        format = L"%s%s%s";
        str = fmt::sprintf(format, str, expStr, exponentAsString);
        if (mantissa < 0) {
            str = L"   " + str;

        } else {
            str = L"    " + str;
        }
    }
    if (trim) {
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatLongEng(single number, bool trim)
{
    return formatLongEng((double)number, trim);
}
//=============================================================================
std::wstring
formatComplexLongEng(double realPart, double imagPart, bool trim)
{
    std::wstring signStr;
    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }
    return formatLongEng(realPart, trim) + signStr + formatLongEng(fabs(imagPart), trim) + L"i";
}
//=============================================================================
std::wstring
formatComplexLongEng(single realPart, single imagPart, bool trim)
{
    return formatComplexLongEng((double)realPart, (double)imagPart, trim);
}
//=============================================================================
}
//=============================================================================
