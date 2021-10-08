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
#include "DisplayFloatingNumberHelpers.hpp"
#include "FloatNumberToRational.hpp"
#include "hexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
isInteger(T val)
{
    int truncated = (int)val;
    return (T)truncated == (T)val && val < 1e9;
}
//=============================================================================
std::wstring
formatShortPrivate(double number, bool trim, bool forceFormat)
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
            if (number == 0. || (fabs(number) > 1e-4 && number < 1e9)) {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 9, 4, number);
            } else {
                std::wstring format = L"%*.*e";
                str = fmt::sprintf(format, 9, 4, number);
            }
        } else {
            if (isInteger<double>(number)) {
                std::wstring format = L"%d";
                str = fmt::sprintf(format, (int64)number);
            } else if (fabs(number) > 1e-4 && number < 1e9) {
                std::wstring format = L"%*.*f";
                str = fmt::sprintf(format, 9, 4, number);
            } else {
                std::wstring format = L"%*.*e";
                str = fmt::sprintf(format, 9, 4, number);
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
formatShort(double number, bool trim)
{
    return formatShortPrivate(number, trim, false);
}
//=============================================================================
std::wstring
formatShort(single number, bool trim)
{
    return formatShortPrivate((double)number, trim, false);
}
//=============================================================================
std::wstring
formatComplexShort(double realPart, double imagPart, bool trim)
{
    std::wstring signStr;
    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }
    std::wstring imagPartStr = formatShortPrivate(fabs(imagPart), trim, true);
    if (imagPartStr == L"NaN") {
        imagPartStr = L" " + imagPartStr;
    }
    return formatShortPrivate(realPart, trim, true) + signStr + imagPartStr + L"i";
}
//=============================================================================
std::wstring
formatComplexShort(single realPart, single imagPart, bool trim)
{
    return formatComplexShort((double)realPart, (double)imagPart, trim);
}
//=============================================================================
std::wstring
formatShortEng(double x, bool trim)
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
formatShortEng(single x, bool trim)
{
    return formatShortEng((double)x, trim);
}
//=============================================================================
std::wstring
formatComplexShortEng(double realPart, double imagPart, bool trim)
{
    std::wstring signStr;
    if (imagPart < 0) {
        signStr = L" - ";
    } else {
        signStr = L" + ";
    }
    return formatShortEng(realPart, trim) + signStr + formatShortEng(fabs(imagPart), false) + L"i";
}
//=============================================================================
std::wstring
formatComplexShortEng(single realPart, single imagPart, bool trim)
{
    return formatComplexShortEng((double)realPart, (double)imagPart, trim);
}
//=============================================================================
std::wstring
formatLongEng(double number, bool trim)
{
    std::wstring str;
    if (IsInfinite(number)) {
        std::wstring format = L"%*s";
        if (number < 0) {
            str = fmt::sprintf(format, 18, L"-Inf");
        } else {
            str = fmt::sprintf(format, 18, L"Inf");
        }
    } else if (IsNaN(number)) {
        std::wstring format = L"%*s";
        str = fmt::sprintf(format, 18, L"NaN");
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

std::wstring
formatHex(double number, bool trim)
{
    std::wstring result;
    result = double2hexastr(number);
    if (trim) {
        boost::trim_left(result);
    }
    return result;
}
//=============================================================================
std::wstring
formatHex(single number, bool trim)
{
    std::wstring result;
    result = single2hexastr(number);
    if (trim) {
        boost::trim_left(result);
    }
    return result;
}
//=============================================================================
std::wstring
formatComplexHex(double realPart, double imgPart, bool trim)
{
    return formatHex(realPart, trim) + L"   " + formatHex(imgPart, trim) + L"i";
}
//=============================================================================
std::wstring
formatComplexHex(single realPart, single imgPart, bool trim)
{
    return formatHex(realPart, trim) + L"   " + formatHex(imgPart, trim) + L"i";
}
//=============================================================================
std::wstring
formatRational(double number, bool trim)
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
std::wstring
formatBank(double number, bool trim)
{
    std::wstring result;

    if (std::isnan(number)) {
        std::wstring format = L"%*s";
        result = fmt::sprintf(format, 10, L"NaN");
    } else if (std::isinf(number)) {
        std::wstring format = L"%*s";
        if (number < 0) {
            result = fmt::sprintf(format, 10, L"-Inf");
        } else {
            result = fmt::sprintf(format, 10, L"Inf");
        }
    } else {
        std::wstring format = L"%10.2f";
        result = fmt::sprintf(format, number);
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
} // namespace Nelson
//=============================================================================
