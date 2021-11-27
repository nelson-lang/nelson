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
#include "FormatShortEng.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatShortEng(double x, bool forceFormat, bool trim)
{
    std::wstring str;
    if (IsInfinite(x)) {
        std::wstring format = L"%*s";
        if (x < 0) {
            str = fmt::sprintf(format, 15, L"-Inf");
        } else {
            str = fmt::sprintf(format, 15, L"Inf");
        }

    } else if (IsNaN(x)) {
        std::wstring format = L"%*s";
        str = fmt::sprintf(format, 15, L"NaN");
    } else {
        int exponent = 0;
        if (x != 0) {
            double absval = (x < 0 ? -x : x);
            int logabsval = static_cast<int>(std::floor(log10(absval)));
            if (logabsval < 0) {
                exponent = logabsval - 2 + ((-logabsval + 2) % 3);
            } else {
                exponent = logabsval - (logabsval % 3);
            }
        }
        double mantissa = x / std::pow(static_cast<double>(10), exponent);
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
        std::wstring format = L"%.4f%s%s";
        str = fmt::sprintf(format, mantissa, expStr, exponentAsString);
        if (mantissa < 10) {
            str = L" " + str;
        }
    }
    if (trim) {
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatShortEng(single x, bool forceFormat, bool trim)
{
    return formatShortEng((double)x, forceFormat, trim);
}
//=============================================================================
std::wstring
formatComplexShortEng(double realPart, double imagPart, bool forceFormat, bool trim)
{
    std::wstring signStr;
    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }
    return formatShortEng(realPart, forceFormat, trim) + signStr
        + formatShortEng(fabs(imagPart), forceFormat, false) + L"i";
}
//=============================================================================
std::wstring
formatComplexShortEng(single realPart, single imagPart, bool forceFormat, bool trim)
{
    return formatComplexShortEng((double)realPart, (double)imagPart, forceFormat, trim);
}
//=============================================================================
}
//=============================================================================
