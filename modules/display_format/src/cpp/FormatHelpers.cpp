//===========================================================================
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
#include "StringHelpers.hpp"
#include "FormatHelpers.hpp"
#include "FormatPlus.hpp"
#include "FormatRational.hpp"
#include "FormatHex.hpp"
#include "FormatEngineeringNotation.hpp"
#include "Types.hpp"
#include "IEEEFP.hpp"
#include "ScaleFactor.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    inline double
    getScaleDivisor(int scaleFactor)
    {
        if (scaleFactor == 1) {
            return 1.0;
        }
        static double pow10_cache[37];
        static bool pow10_init = false;
        if (!pow10_init) {
            for (int i = -18; i <= 18; ++i) {
                pow10_cache[i + 18] = std::pow(10.0, i);
            }
            pow10_init = true;
        }
        if (scaleFactor >= -18 && scaleFactor <= 18) {
            return pow10_cache[scaleFactor + 18];
        }
        return std::pow(10.0, scaleFactor);
    }
}
//=============================================================================
static std::wstring
padRight(const std::wstring& s, indexType width)
{
    size_t sw = s.size();
    if (static_cast<indexType>(sw) >= width) {
        return s;
    }
    std::wstring out(static_cast<size_t>(width) - sw, L' ');
    out += s;
    return out;
}
//=============================================================================
std::wstring
centerText(const std::wstring& text, size_t width)
{
    size_t padLength = (width - text.length()) / 2;
    return fmt::format(fmt::runtime(L"{:>{}}{}{:>{}}"), L"", padLength, text, L"", padLength);
}
//=============================================================================
std::wstring
formatScaleFactor(const FormatDisplayInformation& formatInfo)
{
    std::wstring scaleFactorAsString;
    if (formatInfo.scaleFactor != 1) {
        int absCommonLogarithm = std::abs(formatInfo.scaleFactor);
        wchar_t sign = (formatInfo.scaleFactor > 0) ? L'+' : L'-';

        std::wstring fmt;
        if (absCommonLogarithm < 10) {
            fmt = L"1.0e{}{:02d} *";
        } else {
            fmt = L"1.0e{}{:d} *";
        }
        scaleFactorAsString = fmt::format(fmt::runtime(fmt), sign, absCommonLogarithm);
    }
    return scaleFactorAsString;
}
//=============================================================================
indexType
getNominalWidth(const FormatDisplayInformation& formatInfo)
{
    indexType nominalWidth = formatInfo.widthReal;
    if (formatInfo.isComplex) {
        switch (formatInfo.numericFormatDisplay) {
        case NLS_NUMERIC_FORMAT_LONGENG: {
            nominalWidth = formatInfo.widthReal;
        } break;
        case NLS_NUMERIC_FORMAT_SHORTENG:
        case NLS_NUMERIC_FORMAT_SHORTE:
        case NLS_NUMERIC_FORMAT_SHORT:
        case NLS_NUMERIC_FORMAT_LONG:
        case NLS_NUMERIC_FORMAT_LONGE:
        case NLS_NUMERIC_FORMAT_SHORTG:
        case NLS_NUMERIC_FORMAT_LONGG: {
            nominalWidth = formatInfo.widthReal + formatInfo.widthImag + 3;
        } break;
        case NLS_NUMERIC_FORMAT_BANK: {
            nominalWidth = formatInfo.widthReal;
        } break;
        case NLS_NUMERIC_FORMAT_HEX: {
            nominalWidth = formatInfo.widthReal + formatInfo.widthImag + 1;
        } break;
        case NLS_NUMERIC_FORMAT_RATIONAL: {
            nominalWidth = formatInfo.widthReal + formatInfo.widthImag + 1;
        } break;
        case NLS_NUMERIC_FORMAT_PLUS:
            nominalWidth = 1;
        default: {
        } break;
        }
    }
    return nominalWidth;
}
//=============================================================================
template <class T>
static std::wstring
formatSpecialSigned(T val, indexType width, bool padded)
{
    if (std::isnan(static_cast<double>(val))) {
        return padded ? padRight(L"NaN", width) : std::wstring(L"NaN");
    }
    if (std::isinf(static_cast<double>(val))) {
        if (val > 0) {
            return padded ? padRight(L"Inf", width) : std::wstring(L"Inf");
        } else {
            return padded ? padRight(L"-Inf", width) : std::wstring(L"-Inf");
        }
    }
    return std::wstring();
}
//=============================================================================
template <class T>
static std::wstring
formatIntegerReal(T val, const FormatDisplayInformation& formatInfo)
{
    std::wstring s = formatSpecialSigned(val, 0, false);
    if (!s.empty()) {
        return s;
    }
    if (formatInfo.decimalsReal != 0) {
        return fmt::format(formatInfo.formatReal, val);
    }
    return fmt::format(formatInfo.formatReal, (long int)val);
}
//=============================================================================
static void
fixFormatNonFinite(std::wstring& s)
{
    // nan/inf always land at the tail of a formatted float string
    // e.g. "   nan", "  -inf" — no need to scan, just index from end
    if (s.size() < 3) {
        return;
    }
    const size_t tail = s.size() - 3;
    wchar_t a = s[tail], b = s[tail + 1], c = s[tail + 2];

    if (a == L'n' && b == L'a' && c == L'n') {
        s[tail] = L'N';
        s[tail + 2] = L'N';
        // 'a' stays
        return;
    }
    if (a == L'i' && b == L'n' && c == L'f') {
        s[tail] = L'I';
        // 'n', 'f' stay — only first char changes
    }
}
//=============================================================================
template <class T>
static std::wstring
formatReal(T val, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    if (std::isnan(val)) {
        // force NaN representation
        val = std::nan("NaN");
    }
    if (val == 0) {
        result = fmt::format(formatInfo.formatReal, 0.);
    } else {
        if (formatInfo.scaleFactor != 1) {
            double divisor = getScaleDivisor(formatInfo.scaleFactor);
            result = fmt::format(formatInfo.formatReal, val / divisor);
        } else {
            result = fmt::format(formatInfo.formatReal, val);
        }
    }
#ifdef __APPLE__
    fixFormatNonFinite(result);
#endif
    return result;
}
//=============================================================================
template <class T>
static std::wstring
formatSpecialUnsignedInf(T val, indexType width, bool& isPositive, bool padded)
{
    isPositive = true;
    if (std::isnan(static_cast<double>(val))) {
        return padded ? padRight(L"NaN", width) : std::wstring(L"NaN");
    }
    if (std::isinf(static_cast<double>(val))) {
        isPositive = val >= 0;
        return padded ? padRight(L"Inf", width) : std::wstring(L"Inf");
    }
    return std::wstring();
}
//=============================================================================
template <class T>
static std::wstring
formatComplex(T realPart, T imagPart, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    bool IsImagPartPositive = true;
    std::wstring resultRealPart;
    std::wstring resultImagPart;
    bool trimImagPart = true;

    if (imagPart == 0) {
        imagPart = 0;
    }
    if (formatInfo.scaleFactor != 1) {
        double divisor = getScaleDivisor(formatInfo.scaleFactor);
        T scaledRealPart = realPart / divisor;
        T scaledImagPart = imagPart / divisor;

        resultRealPart = fmt::format(formatInfo.formatReal, scaledRealPart);
        if (scaledImagPart < 0) {
            IsImagPartPositive = false;
            resultImagPart = fmt::format(formatInfo.formatImag, fabs(scaledImagPart));
        } else {
            resultImagPart = fmt::format(formatInfo.formatImag, scaledImagPart);
        }

    } else {
        if (std::isfinite(realPart)) {
            resultRealPart = fmt::format(formatInfo.formatReal, realPart);
        } else {
            resultRealPart = formatSpecialSigned(realPart, formatInfo.widthReal, true);
        }

        if (std::isfinite(imagPart)) {
            if (imagPart < 0) {
                IsImagPartPositive = false;
                resultImagPart = fmt::format(formatInfo.formatImag, fabs(imagPart));
            } else {
                resultImagPart = fmt::format(formatInfo.formatImag, imagPart);
            }
        } else {
            trimImagPart = false;
            resultImagPart = formatSpecialUnsignedInf(
                imagPart, formatInfo.widthImag, IsImagPartPositive, true);
        }
    }
    if (formatInfo.numericFormatDisplay == NLS_NUMERIC_FORMAT_BANK) {
        result = resultRealPart;
        if (formatInfo.trim) {
            StringHelpers::trim_left(result);
        }
    } else {
        if (IsImagPartPositive) {
            result = resultRealPart + L" +" + resultImagPart + L"i";
        } else {
            result = resultRealPart + L" -" + resultImagPart + L"i";
        }
    }
    return result;
}
//=============================================================================
static std::wstring
formatNumber(double val, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    if (formatInfo.floatAsInteger) {
        result = formatIntegerReal<double>(val, formatInfo);
    } else {
        result = formatReal<double>(val, formatInfo);
    }
    if (formatInfo.trim) {
        StringHelpers::trim_left(result);
    }
    return result;
}
//=============================================================================
template <class T>
static std::wstring
formatNumberComplex(T realPart, T imagPart, const FormatDisplayInformation& formatInfo)
{
    std::wstring result = formatComplex<T>(realPart, imagPart, formatInfo);
    if (formatInfo.trim) {
        StringHelpers::trim_left(result);
    }
    return result;
}
//=============================================================================
template <class T>
static std::wstring
formatComplexHexElement(T realPart, T imagPart, const FormatDisplayInformation& formatInfo)
{
    std::wstring partReal = fmt::format(formatInfo.formatReal, formatHex(realPart, true));
    std::wstring partImag = fmt::format(formatInfo.formatImag, formatHex(imagPart, true));
    std::wstring result;
    result.append(partReal);
    result.append(partImag);
    result.append(L"i");
    return result;
}
//=============================================================================
template <class T>
static std::wstring
formatComplexHexScalar(T realPart, T imagPart, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    result.append(formatHex(static_cast<double>(realPart), formatInfo.trim));
    result.append(L"   ");
    result.append(formatHex(static_cast<double>(imagPart), true));
    result.append(L"i");
    return result;
}
//=============================================================================
template <class T>
static std::wstring
formatComplexRational(T realPart, T imagPart, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    result.append(formatRational(static_cast<double>(realPart), formatInfo.widthReal,
        formatInfo.widthReal - 1, formatInfo.trim));
    if (imagPart < 0) {
        result.append(L" - ");
    } else {
        result.append(L" + ");
    }
    result.append(formatRational(
        fabs(static_cast<double>(imagPart)), formatInfo.widthImag, formatInfo.widthImag - 1, true));
    result.append(L"i");
    return result;
}
//=============================================================================
template <class T>
static std::wstring
formatElementT(T val, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (formatInfo.numericFormatDisplay) {
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        result = formatShortEng(val, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        result = formatLongEng(val, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        size_t width = formatInfo.widthReal;
        size_t lengthWithoutBlanks = formatInfo.decimalsReal - 1;
        result = formatRational(val, width, lengthWithoutBlanks, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result = formatPlus(val, false);
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        result = fmt::format(formatInfo.formatReal, formatHex(val, false));
    } break;
    case NLS_NUMERIC_FORMAT_BANK:
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_LONGG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_SHORT: {
        bool haveDecimals = formatInfo.decimalsReal != 0;
        if (formatInfo.scaleFactor != 1) {
            double divisor = getScaleDivisor(formatInfo.scaleFactor);
            if (haveDecimals) {
                double value = static_cast<double>(val) / divisor;
                if (value == 0.) {
                    result = padRight(L"0", formatInfo.widthReal);
                } else {
                    result = fmt::format(formatInfo.formatReal, value);
                }
            } else {
                if (formatInfo.floatAsInteger) {
                    result = fmt::format(
                        formatInfo.formatReal, (long int)(static_cast<long int>(val) / divisor));
                } else {
                    result = fmt::format(formatInfo.formatReal, static_cast<double>(val) / divisor);
                }
            }
        } else {
            if (haveDecimals) {
                if (val == 0) {
                    result = padRight(L"0", formatInfo.widthReal);
                } else {
                    result = fmt::format(formatInfo.formatReal, static_cast<double>(val));
                }
            } else {
                if (formatInfo.floatAsInteger) {
                    if (!std::isfinite(static_cast<double>(val))) {
                        result = formatSpecialSigned(
                            static_cast<double>(val), formatInfo.widthReal, true);
                    } else {
                        result = fmt::format(formatInfo.formatReal, (long int)val);
                    }
                } else {
                    result = fmt::format(formatInfo.formatReal, static_cast<double>(val));
                }
            }
        }
    } break;
    }
#ifdef __APPLE__
    fixFormatNonFinite(result);
#endif
    return result;
}
//=============================================================================
std::wstring
formatElement(double val, const FormatDisplayInformation& formatInfo)
{
    return formatElementT<double>(val, formatInfo);
}
//=============================================================================
template <class T>
static std::wstring
formatElementComplexT(T realPart, T ImagPart, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (formatInfo.numericFormatDisplay) {
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        result = formatComplexShortEng(realPart, ImagPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        result = formatComplexLongEng(realPart, ImagPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_SHORT:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_LONGG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_SHORTG: {
        result = formatComplex<T>(realPart, ImagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result = formatPlus(realPart, false);
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        result = formatComplexHexElement(realPart, ImagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        result = formatElement(static_cast<double>(realPart), formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        result = formatComplexRational(realPart, ImagPart, formatInfo);

    } break;
    default: {
    } break;
    }
    return result;
}
//=============================================================================
std::wstring
formatElementComplex(double realPart, double ImagPart, const FormatDisplayInformation& formatInfo)
{
    return formatElementComplexT<double>(realPart, ImagPart, formatInfo);
}
//=============================================================================
template <class T>
static std::wstring
formatScalarNumberT(T val, bool asSingle, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (formatInfo.numericFormatDisplay) {
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        if (asSingle) {
            result = formatShortEng((single)val, formatInfo.trim);
        } else {
            result = formatShortEng((double)val, formatInfo.trim);
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        if (asSingle) {
            result = formatLongEng((single)val, formatInfo.trim);
        } else {
            result = formatLongEng((double)val, formatInfo.trim);
        }
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        if (IsIntegerForm(static_cast<double>(val))) {
            result = L"   " + fmt::format(L"{:.0f}", static_cast<double>(val));
        } else {
            result = L"   "
                + formatRational(
                    static_cast<double>(val), formatInfo.widthReal, formatInfo.widthReal - 1, true);
        }
        if (formatInfo.trim) {
            StringHelpers::trim_left(result);
        }
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result.append(formatPlus(static_cast<double>(val), formatInfo.trim));
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        if (asSingle) {
            result = fmt::format(formatInfo.formatReal, formatHex((single)val, false));
        } else {
            result = fmt::format(formatInfo.formatReal, formatHex(static_cast<double>(val), false));
        }
        if (formatInfo.trim) {
            StringHelpers::trim_left(result);
        }
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        if (!formatInfo.trim) {
            result.append(L" ");
        }
        result.append(formatNumber(static_cast<double>(val), formatInfo));
    } break;
    default: {
        result.append(formatNumber(static_cast<double>(val), formatInfo));
    } break;
    }
    return result;
}
//=============================================================================
template <class T>
static std::wstring
formatScalarComplexNumberT(
    T realPart, T imagPart, bool asSingle, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (formatInfo.numericFormatDisplay) {
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        if (asSingle) {
            result.append(
                formatComplexShortEng((single)realPart, (single)imagPart, formatInfo.trim));
        } else {
            result.append(formatComplexShortEng(
                static_cast<double>(realPart), static_cast<double>(imagPart), formatInfo.trim));
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        if (asSingle) {
            result.append(
                formatComplexLongEng((single)realPart, (single)imagPart, formatInfo.trim));
        } else {
            result.append(formatComplexLongEng(
                static_cast<double>(realPart), static_cast<double>(imagPart), formatInfo.trim));
        }
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result = formatPlus(static_cast<double>(realPart), formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        result = formatComplexHexScalar(realPart, imagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        result = formatComplexRational(realPart, imagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_SHORT:
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_LONGG:
    default: {
        if (asSingle) {
            result.append(
                formatNumberComplex<single>((single)realPart, (single)imagPart, formatInfo));
        } else {
            result.append(formatNumberComplex<double>(
                static_cast<double>(realPart), static_cast<double>(imagPart), formatInfo));
        }

    } break;
    }
    return result;
}
//=============================================================================
std::wstring
formatElement(single val, const FormatDisplayInformation& formatInfo)
{
    return formatElementT<single>(val, formatInfo);
}
//=============================================================================
std::wstring
formatElementComplex(single realPart, single ImagPart, const FormatDisplayInformation& formatInfo)
{
    return formatElementComplexT<single>(realPart, ImagPart, formatInfo);
}
//=============================================================================
std::wstring
formatScalarNumber(double val, bool asSingle, const FormatDisplayInformation& formatInfo)
{
    return formatScalarNumberT<double>(val, asSingle, formatInfo);
}
//=============================================================================
std::wstring
formatScalarComplexNumber(
    double realPart, double imagPart, bool asSingle, const FormatDisplayInformation& formatInfo)
{
    return formatScalarComplexNumberT<double>(realPart, imagPart, asSingle, formatInfo);
}
//=============================================================================
}
//=============================================================================
