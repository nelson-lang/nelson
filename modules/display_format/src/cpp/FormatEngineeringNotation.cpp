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
#include <type_traits>
#include "FormatEngineeringNotation.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
std::wstring
formatLongEngineering(T number, bool trim, const std::wstring& zero, const std::wstring& format,
    size_t nbBlanksMantissa, size_t widthMantissa, size_t widthFull)
{
    std::wstring str;
    if (number == 0.) {
        str = std::wstring(nbBlanksMantissa + 1, L' ') + zero;
    } else if (IsInfinite(number)) {
        if (number < 0) {
            str = std::wstring(widthFull - (size_t)5, L' ') + L"-Inf";

        } else {
            str = std::wstring(widthFull - (size_t)4, L' ') + L"Inf";
        }
    } else if (IsNaN(number)) {
        str = std::wstring(widthFull - (size_t)4, L' ') + L"NaN";
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
        str = fmt::sprintf(format, mantissa);
        size_t pointPos = str.find(L'.');
        if (pointPos == std::string::npos) {
            str = str + L".";
        }
        int nbZerosToAdd = (int)widthMantissa - (int)str.size() - (int)1;
        if (nbZerosToAdd > 0) {
            for (int k = 0; k < nbZerosToAdd; ++k) {
                str = str + L"0";
            }
        }
        str = fmt::sprintf(L"%s%s%s", str, expStr, exponentAsString);
        if (mantissa < 0) {
            str = std::wstring(nbBlanksMantissa, L' ') + str;

        } else {
            str = std::wstring(nbBlanksMantissa + (size_t)1, L' ') + str;
        }
    }
    if (trim) {
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatLongEng(double number, bool trim)
{
    return formatLongEngineering<double>(
        number, trim, L"0.00000000000000e+000", L"%.15g", 3, 17, 26);
}
//=============================================================================
std::wstring
formatLongEng(single number, bool trim)
{
    return formatLongEngineering<single>(number, trim, L"0.000000e+000", L"%.7g", 2, 9, 17);
}
//=============================================================================
template <class T>
std::wstring
formatComplexLongEng(T realPart, T imagPart, bool trim)
{
    std::wstring result;
    result.append(formatLongEng(realPart, trim));
    if (imagPart < 0) {
        result.append(L" -");
    } else {
        result.append(L" +");
    }
    if (std::isfinite(imagPart)) {
        std::wstring imgStr = formatLongEng(fabs(imagPart), false);
        size_t nbMaxBlanks = std::is_same<T, float>::value ? 2 : 3;
        for (size_t k = 0; k < nbMaxBlanks; ++k) {
            if (imgStr[0] == L' ') {
                imgStr.erase(0, 1);
            }
        }
        result.append(imgStr);
    } else {
        result.append(formatLongEng(fabs(imagPart), false));
    }
    result.append(L"i");
    return result;
}
//=============================================================================
std::wstring
formatComplexLongEng(double realPart, double imagPart, bool trim)
{
    return formatComplexLongEng<double>(realPart, imagPart, trim);
}
//=============================================================================
std::wstring
formatComplexLongEng(single realPart, single imagPart, bool trim)
{
    return formatComplexLongEng<single>(realPart, imagPart, trim);
}
//=============================================================================
template <class T>
std::wstring
formatShortEngineer(T x, bool trim, const std::wstring& format, size_t width, size_t nbBlanks)
{
    std::wstring str;
    if (IsInfinite(x)) {
        if (x < 0) {
            str = fmt::sprintf(L"%*s", width, L"-Inf");
        } else {
            str = fmt::sprintf(L"%*s", width, L"Inf");
        }
    } else if (IsNaN(x)) {
        str = fmt::sprintf(L"%*s", width, L"NaN");
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
        str = fmt::sprintf(format, mantissa, expStr, exponentAsString);
        std::wstring integerPart = std::to_wstring(int(mantissa));
        nbBlanks = nbBlanks - integerPart.length();
        std::wstring blanks(nbBlanks, L' ');
        str = blanks + str;
    }
    if (trim) {
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatShortEng(double number, bool trim)
{
    return formatShortEngineer<double>(number, trim, L"%.4f%s%s", 16, 6);
}
//=============================================================================
std::wstring
formatShortEng(single number, bool trim)
{
    return formatShortEngineer<single>(number, trim, L"%.4f%s%s", 16, 6);
}
//=============================================================================
template <class T>
std::wstring
formatComplexShortEng(T realPart, T imagPart, bool trim)
{
    std::wstring result;
    result.append(formatShortEng(realPart, trim));
    if (imagPart < 0) {
        result.append(L" -");
    } else {
        result.append(L" +");
    }
    if (std::isfinite(imagPart)) {
        std::wstring imgStr = formatShortEng(fabs(imagPart), false);
        size_t nbMaxBlanks = 3;
        for (size_t k = 0; k < nbMaxBlanks; ++k) {
            if (imgStr[0] == L' ') {
                imgStr.erase(0, 1);
            }
        }
        result.append(imgStr);
    } else {
        result.append(formatShortEng(fabs(imagPart), false));
    }
    result.append(L"i");
    return result;
}
//=============================================================================
std::wstring
formatComplexShortEng(double realPart, double imagPart, bool trim)
{
    return formatComplexShortEng<double>(realPart, imagPart, trim);
}
//=============================================================================
std::wstring
formatComplexShortEng(single realPart, single imagPart, bool trim)
{
    return formatComplexShortEng<single>(realPart, imagPart, trim);
}
//=============================================================================
}
//=============================================================================
