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
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatAsIntegerLong(double number, bool forceFormat, bool trim)
{
    std::wstring msg;
    if (number >= 1e9) {
        msg = fmt::sprintf(L"%*.*e", 29, 15, number);
    } else {
        if (number > 0) {
            if (number >= 1e3) {
                msg = fmt::sprintf(L"%*lu", 15, (long long)number);
            } else {
                msg = fmt::sprintf(L"%*lu", 9, (long long)number);
            }
        } else {
            if (abs(number) >= 1e3) {
                msg = fmt::sprintf(L"%*ld", 15, (long int)number);
            } else {
                msg = fmt::sprintf(L"%*ld", 9, (long long)number);
            }
        }
    }
    if (trim) {
        boost::trim_left(msg);
    }
    return msg;
}
//=============================================================================
std::wstring
formatAsIntegerLong(single number, bool forceFormat, bool trim)
{
    std::wstring msg;
    return msg;
}
//=============================================================================
std::wstring
formatLong(double number, bool forceFormat, bool trim)
{
    std::wstring str;
    if (IsInfinite(number)) {
        std::wstring format = L"%*s";
        if (number < 0) {
            str = fmt::sprintf(format, 9, L"-Inf");
        } else {
            str = fmt::sprintf(format, 9, L" Inf");
        }
    } else if (IsNaN(number)) {
        std::wstring format = L"%*s";
        str = fmt::sprintf(format, 9, L"NaN");
    } else {
        if (forceFormat) {
            std::wstring format = L"%*.*f";
            str = fmt::sprintf(format, 23, 15, number);
        } else {
            if (fabs(number) > 1e-3 && number < 1e9) {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 23, 15, number);
            } else {
                std::wstring format = L"%*.*e";
                str = fmt::sprintf(format, 29, 15, number);
            }
        }
    }
    if (trim) {
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatLong(single number, bool forceFormat, bool trim)
{
    std::wstring str;
    if (IsInfinite(number)) {
        std::wstring format = L"%*s";
        if (number < 0) {
            str = fmt::sprintf(format, 9, L"-Inf");
        } else {
            str = fmt::sprintf(format, 9, L" Inf");
        }
    } else if (IsNaN(number)) {
        std::wstring format = L"%*s";
        str = fmt::sprintf(format, 9, L"NaN");
    } else {
        if (forceFormat) {
            std::wstring format = L"%*.*f";
            str = fmt::sprintf(format, 9, 4, number);
        } else {
            if (fabs(number) > 1e-4 && number < 1e9) {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 13, 4, number);
            } else {
                std::wstring format = L"%*.*e";
                str = fmt::sprintf(format, 16, 4, number);
            }
        }
    }
    if (trim) {
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatComplexLong(double realPart, double imagPart, bool forceFormat, bool trim)
{
    std::wstring signStr;
    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }
    std::wstring imagPartStr = formatLong(fabs(imagPart), forceFormat, trim);
    if (imagPartStr == L"NaN") {
        imagPartStr = L" " + imagPartStr;
    }
    return formatLong(realPart, forceFormat, trim) + signStr + imagPartStr + L"i";
}
//=============================================================================
std::wstring
formatComplexLong(single realPart, single imagPart, bool forceFormat, bool trim)
{
    std::wstring signStr;
    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }
    std::wstring imagPartStr = formatLong(fabs(imagPart), forceFormat, trim);
    if (imagPartStr == L"NaN") {
        imagPartStr = L" " + imagPartStr;
    }
    return formatLong(realPart, forceFormat, trim) + signStr + imagPartStr + L"i";
}
//=============================================================================
}
//=============================================================================
