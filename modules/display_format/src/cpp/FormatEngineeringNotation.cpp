//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "FormatEngineeringNotation.hpp"
#include "IEEEFP.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    template <class T>
    void
    compute_exponent_mantissa(T number, int& exponent, double& mantissa)
    {
        exponent = 0;
        if (number != 0) {
            double absval = (number < 0 ? -number : number);
            int logabsval = static_cast<int>(std::floor(log10(absval)));
            int rem = logabsval % 3;
            if (rem < 0) {
                rem += 3;
            }
            exponent = logabsval - rem;
        }
        mantissa = number / std::pow(static_cast<double>(10), exponent);
    }
    //=============================================================================
    static std::wstring
    make_exponent_string(int exponent)
    {
        std::wstring expStr;
        expStr.reserve(8);
        int absExp = exponent;
        if (exponent >= 0) {
            expStr = L"e+";
        } else {
            absExp = -exponent;
            expStr = L"e-";
        }
        std::wstring exponentAsString = fmt::to_wstring(absExp);
        if (exponentAsString.length() < 3) {
            expStr.append(3 - exponentAsString.length(), L'0');
        }
        expStr.append(exponentAsString);
        return expStr;
    }
    //=============================================================================
    static std::pair<std::wstring, std::wstring>
    make_exponent_components(int exponent)
    {
        std::wstring prefix;
        int absExp = exponent;
        if (exponent >= 0) {
            prefix = L"e+";
        } else {
            absExp = -exponent;
            prefix = L"e-";
        }
        std::wstring digits = fmt::to_wstring(absExp);
        if (digits.length() < 3) {
            digits.insert(0, 3 - digits.length(), L'0');
        }
        return { prefix, digits };
    }
    //=============================================================================
    static void
    trim_leading_spaces(std::wstring& s, size_t max)
    {
        size_t removed = 0;
        while (removed < max && !s.empty() && s.front() == L' ') {
            s.erase(0, 1);
            ++removed;
        }
    }
    //=============================================================================
} // namespace (internal)
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
        double mantissa = 0.0;
        compute_exponent_mantissa<T>(number, exponent, mantissa);
        std::wstring mantissaStr = fmt::format(fmt::runtime(format), mantissa);
        if (mantissaStr.find(L'.') == std::wstring::npos) {
            mantissaStr += L'.';
        }
        int nbZerosToAdd
            = static_cast<int>(widthMantissa) - static_cast<int>(mantissaStr.size()) - 1;
        if (nbZerosToAdd > 0) {
            mantissaStr.append((size_t)nbZerosToAdd, L'0');
        }
        std::wstring expFull = make_exponent_string(exponent);
        str = fmt::format(L"{}{}", mantissaStr, expFull);
        if (mantissa < 0) {
            str = std::wstring(nbBlanksMantissa, L' ') + str;
        } else {
            str = std::wstring(nbBlanksMantissa + (size_t)1, L' ') + str;
        }
    }
    if (trim) {
        StringHelpers::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatLongEng(double number, bool trim)
{
    return formatLongEngineering<double>(
        number, trim, L"0.00000000000000e+000", L"{:.15g}", 3, 17, 26);
}
//=============================================================================
std::wstring
formatLongEng(single number, bool trim)
{
    return formatLongEngineering<single>(number, trim, L"0.000000e+000", L"{:.7g}", 2, 9, 17);
}
//=============================================================================
template <class T>
std::wstring
formatComplexLongEng(T realPart, T imagPart, bool trim, size_t nbMaxBlanks)
{
    std::wstring result;
    result.append(formatLongEng(realPart, trim));
    if (imagPart < 0) {
        result.append(L" -");
    } else {
        result.append(L" +");
    }
    std::wstring imgStr = formatLongEng((T)fabs(imagPart), false);
    if (std::isfinite(imagPart)) {
        trim_leading_spaces(imgStr, nbMaxBlanks);
    }
    result.append(imgStr);
    result.append(L"i");
    return result;
}
//=============================================================================
std::wstring
formatComplexLongEng(double realPart, double imagPart, bool trim)
{
    return formatComplexLongEng<double>((double)realPart, (double)imagPart, trim, 3);
}
//=============================================================================
std::wstring
formatComplexLongEng(single realPart, single imagPart, bool trim)
{
    return formatComplexLongEng<single>((single)realPart, (single)imagPart, trim, 2);
}
//=============================================================================
template <class T>
std::wstring
formatShortEngineer(T x, bool trim, const std::wstring& format, size_t width, size_t nbBlanks)
{
    std::wstring str;
    if (IsInfinite(x)) {
        if (x < 0) {
            str = fmt::format(L"{:>{}}", L"-Inf", width);
        } else {
            str = fmt::format(L"{:>{}}", L"Inf", width);
        }
    } else if (IsNaN(x)) {
        str = fmt::format(L"{:>{}}", L"NaN", width);
    } else {
        int exponent = 0;
        double mantissa = 0.0;
        compute_exponent_mantissa<T>(x, exponent, mantissa);
        auto [expPrefix, expDigits] = make_exponent_components(exponent);
        str = fmt::format(fmt::runtime(format), mantissa, expPrefix, expDigits);
        std::wstring integerPart = std::to_wstring(static_cast<int>(mantissa));
        if (nbBlanks > integerPart.length()) {
            nbBlanks = nbBlanks - integerPart.length();
        } else {
            nbBlanks = 0;
        }
        std::wstring blanks(nbBlanks, L' ');
        str = blanks + str;
    }
    if (trim) {
        StringHelpers::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatShortEng(double number, bool trim)
{
    return formatShortEngineer<double>(number, trim, L"{:.4f}{}{}", 16, 6);
}
//=============================================================================
std::wstring
formatShortEng(single number, bool trim)
{
    return formatShortEngineer<single>(number, trim, L"{:.4f}{}{}", 16, 6);
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
        trim_leading_spaces(imgStr, 3);
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