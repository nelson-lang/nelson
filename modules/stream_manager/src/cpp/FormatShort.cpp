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
#include "FormatShort.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatAsIntegerShort(double number, bool forceFormat, bool trim)
{
    std::wstring msg;
    if (std::isnan(number)) {
        msg = fmt::sprintf(L"%*s", 6, L"NaN");
    } else if (!std::isfinite(number)) {
        if (number < 0) {
            msg = fmt::sprintf(L"%*s", 6, L"-Inf");
        } else {
            msg = fmt::sprintf(L"%*s", 6, L"Inf");
        }
    } else {
        double absoluteValue = abs(number);
        if (absoluteValue <= 999) {
            msg = fmt::sprintf(L"%*ld", 6, (long int)number);
        } else if (absoluteValue <= 999999999) {
            msg = fmt::sprintf(L"%*ld", 12, (long int)number);
        } else {
            std::wstring format = L"%*.*e";
            msg = fmt::sprintf(format, 13, 4, number);
        }
    }
    if (trim) {
        boost::trim_left(msg);
    }
    return msg;
}
//=============================================================================
std::wstring
formatAsIntegerShort(single number, bool forceFormat, bool trim)
{
    std::wstring msg;
    return msg;
}
//=============================================================================
std::wstring
formatScalarShort(double number, bool forceFormat, bool trim)
{
    return formatShort(number, forceFormat, trim);
}
//=============================================================================
std::wstring
formatScalarShort(single number, bool forceFormat, bool trim)
{
    std::wstring msg;
    return msg;
}
//=============================================================================
std::wstring
formatShort(double number, bool forceFormat, bool trim)
{
    std::wstring str;
    str.reserve(64);
    if (IsInfinite(number)) {
        std::wstring format = L"%*s";
        if (forceFormat) {
            if (number < 0) {
                str = fmt::sprintf(format, 13, L"-Inf");
            } else {
                str = fmt::sprintf(format, 13, L" Inf");
            }
        } else {
            if (number < 0) {
                str = fmt::sprintf(format, 6, L"-Inf");
            } else {
                str = fmt::sprintf(format, 6, L" Inf");
            }
        }
    } else if (IsNaN(number)) {
        std::wstring format = L"%*s";
        if (forceFormat) {
            str = fmt::sprintf(format, 13, L"NaN");
        } else {
            str = fmt::sprintf(format, 6, L"NaN");
        }
    } else {
        if (forceFormat) {
            double absoluteValue = fabs(number);
            if (absoluteValue <= 1e-3 || absoluteValue > 1e3) {
                std::wstring format = L"%*.*e";
                str = fmt::sprintf(format, 13, 4, number);
            } else {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 10, 4, number);
            }
        } else {
            double absoluteValue = fabs(number);
            if (absoluteValue == 0) {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 10, 4, number);
            } else if (absoluteValue < 1e-4) {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 10, 4, number);
            } else if (absoluteValue <= 999) {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 10, 4, number);
            } else {
                std::wstring format = L"%*.*e";
                str = fmt::sprintf(format, 13, 4, number);
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
formatShort(single number, bool forceFormat, bool trim)
{
    return formatShort((double)number, forceFormat, trim);
}
//=============================================================================
std::wstring
formatScalarComplexShort(double realPart, double imagPart, bool forceFormat, bool trim)
{
    std::wstring signStr;

    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }

    std::wstring realPartStr = formatScalarShort(realPart, true, false);
    std::wstring imagPartStr = formatScalarShort(fabs(imagPart), true, true);
    return realPartStr + signStr + imagPartStr + L"i";

    /*

    std::wstring signStr;

    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }
    std::wstring imagPartStr;
    std::wstring realPartStr;

    if (!std::isfinite(realPart) && !std::isfinite(imagPart)) {
        if (IsInfinite(realPart)) {
            std::wstring format = L"%*s";
            if (realPart < 0) {
                realPartStr = fmt::sprintf(format, 13, L"-Inf");
            } else {
                realPartStr = fmt::sprintf(format, 13, L" Inf");
            }
        } else if (IsNaN(realPart)) {
            std::wstring format = L"%*s";
            realPartStr = fmt::sprintf(format, 13, L" NaN");
        }
        if (IsInfinite(imagPart)) {
            std::wstring format = L"%*s";
            imagPartStr = fmt::sprintf(format, 13, L" Inf");
        } else if (IsNaN(imagPart)) {
            std::wstring format = L"%*s";
            imagPartStr = fmt::sprintf(format, 13, L" NaN");
        }
    } else {
        double absoluteValueRealPart = fabs(realPart);
        double absoluteValueImagPart = fabs(imagPart);
        if (!std::isfinite(realPart)) {
            std::wstring format = L"%*s";
            if (std::isnan(realPart)) {
                realPartStr = fmt::sprintf(format, 9, L" NaN");
            } else {

                realPartStr = fmt::sprintf(format, 9, L" Inf");
            }

        } else {
            std::wstring format = L"%*.*f";
            realPartStr = fmt::sprintf(format,9, 5,realPart);
        }
        if (!std::isfinite(imagPart)) {
            std::wstring format = L"%*s";
            if (std::isnan(imagPart)) {
                imagPartStr = fmt::sprintf(format, 6, L" NaN");
            } else {
                imagPartStr = fmt::sprintf(format, 6, L" Inf");
            }
        } else {
            std::wstring format = L"%*.*f";
            imagPartStr = fmt::sprintf(format, 9,5, absoluteValueImagPart);
        }
    }
    if (trim) {
        boost::trim_left(realPartStr);
        boost::trim_left(imagPartStr);
    }
    return realPartStr + signStr + imagPartStr + L"i";
    */
}
//=============================================================================
}
//=============================================================================
