//===========================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
std::wstring
centerText(const std::wstring& text, size_t width)
{
    size_t padLength = (width - text.length()) / 2;
    return fmt::sprintf(L"%*s%s%*s", padLength, L"", text, padLength, L"");
}
//=============================================================================
std::wstring
formatScaleFactor(const FormatDisplayInformation& formatInfo)
{
    std::wstring scaleFactorAsString;
    if (formatInfo.scaleFactor != 1) {
        int absCommonLogarithm = (int)abs(formatInfo.scaleFactor);
        std::wstring sign;
        if (formatInfo.scaleFactor > 0) {
            sign = L"+";
        } else {
            sign = L"-";
        }
        std::wstring fmt;
        if (absCommonLogarithm < 10) {
            fmt = L"1.0e%s0%d *";
        } else {
            fmt = L"1.0e%s%d *";
        }
        scaleFactorAsString = fmt::sprintf(fmt, sign, absCommonLogarithm);
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
formatIntegerReal(T val, const FormatDisplayInformation& formatInfo)
{
    if (std::isnan(val)) {
        return L"NaN";
    }
    if (std::isinf(val)) {
        if (val > 0) {
            return L"Inf";
        } else {
            return L"-Inf";
        }
    }
    if (formatInfo.decimalsReal != 0) {
        return fmt::sprintf(
            formatInfo.formatReal, formatInfo.widthReal, formatInfo.decimalsReal, val);
    }
    return fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, (long int)val);
}
//=============================================================================
template <class T>
static std::wstring
formatReal(T val, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    if (val == 0) {
        result = fmt::sprintf(
            formatInfo.formatReal, formatInfo.widthReal, formatInfo.decimalsReal, 0.);
    } else {
        if (formatInfo.scaleFactor != 1) {
            result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal,
                formatInfo.decimalsReal, val / pow(10, formatInfo.scaleFactor));
        } else {
            bool haveDecimals = formatInfo.decimalsReal != 0;
            if (haveDecimals) {
                result = fmt::sprintf(
                    formatInfo.formatReal, formatInfo.widthReal, formatInfo.decimalsReal, val);
            } else {
                result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, val);
            }
        }
    }
    return result;
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
        T scaledRealPart = realPart / (T)pow(10, formatInfo.scaleFactor);
        T scaledImagPart = imagPart / (T)pow(10, formatInfo.scaleFactor);

        resultRealPart = fmt::sprintf(
            formatInfo.formatReal, formatInfo.widthReal, formatInfo.decimalsReal, scaledRealPart);
        if (scaledImagPart < 0) {
            IsImagPartPositive = false;
            resultImagPart = fmt::sprintf(formatInfo.formatImag, formatInfo.widthImag,
                formatInfo.decimalsImag, fabs(scaledImagPart));
        } else {
            resultImagPart = fmt::sprintf(formatInfo.formatImag, formatInfo.widthImag,
                formatInfo.decimalsImag, scaledImagPart);
        }

    } else {
        if (std::isfinite(realPart)) {
            resultRealPart = fmt::sprintf(
                formatInfo.formatReal, formatInfo.widthReal, formatInfo.decimalsReal, realPart);
        } else {
            if (std::isnan(realPart)) {
                resultRealPart = fmt::sprintf(L"%*s", formatInfo.widthReal, L"NaN");
            } else {
                if (realPart > 0) {
                    resultRealPart = fmt::sprintf(L"%*s", formatInfo.widthReal, L"Inf");
                } else {
                    resultRealPart = fmt::sprintf(L"%*s", formatInfo.widthReal, L"-Inf");
                }
            }
        }

        if (std::isfinite(imagPart)) {
            if (imagPart < 0) {
                IsImagPartPositive = false;
                resultImagPart = fmt::sprintf(formatInfo.formatImag, formatInfo.widthImag,
                    formatInfo.decimalsImag, fabs(imagPart));
            } else {
                resultImagPart = fmt::sprintf(
                    formatInfo.formatImag, formatInfo.widthImag, formatInfo.decimalsImag, imagPart);
            }
        } else {
            trimImagPart = false;
            if (std::isnan(imagPart)) {
                resultImagPart = fmt::sprintf(L"%*s", formatInfo.widthImag, L"NaN");
            } else {
                if (imagPart < 0) {
                    IsImagPartPositive = false;
                }
                resultImagPart = fmt::sprintf(L"%*s", formatInfo.widthImag, L"Inf");
            }
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
std::wstring
formatElement(double val, const FormatDisplayInformation& formatInfo)
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
        result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, formatHex(val, false));
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
            if (haveDecimals) {
                double value = val / pow(10, formatInfo.scaleFactor);
                if (value == 0.) {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, 0, value);
                } else {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal,
                        formatInfo.decimalsReal, value);
                }
            } else {
                if (formatInfo.floatAsInteger) {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal,
                        (long int)(val / pow(10, formatInfo.scaleFactor)));
                } else {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal,
                        val / pow(10, formatInfo.scaleFactor));
                }
            }
        } else {
            if (haveDecimals) {
                if (val == 0.) {
                    if (formatInfo.numericFormatDisplay == NLS_NUMERIC_FORMAT_SHORTE
                        || formatInfo.numericFormatDisplay == NLS_NUMERIC_FORMAT_LONGE) {
                        result = fmt::sprintf(L"%*s", formatInfo.widthReal, L"0");
                    } else {
                        result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, 0, val);
                    }
                } else {
                    result = fmt::sprintf(
                        formatInfo.formatReal, formatInfo.widthReal, formatInfo.decimalsReal, val);
                }
            } else {
                if (formatInfo.floatAsInteger) {
                    if (!std::isfinite(val)) {
                        if (std::isnan(val)) {
                            result = fmt::sprintf(L"%*s", formatInfo.widthReal, L"NaN");
                        } else if (val < 0) {
                            result = fmt::sprintf(L"%*s", formatInfo.widthReal, L"-Inf");
                        } else {
                            result = fmt::sprintf(L"%*s", formatInfo.widthReal, L"Inf");
                        }
                    } else {
                        result = fmt::sprintf(
                            formatInfo.formatReal, formatInfo.widthReal, (long int)val);
                    }
                } else {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, val);
                }
            }
        }
    } break;
    }
    return result;
}
//=============================================================================
std::wstring
formatElementComplex(double realPart, double ImagPart, const FormatDisplayInformation& formatInfo)
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
        result = formatComplex<double>(realPart, ImagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result = formatPlus(realPart, false);
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        std::wstring partReal
            = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, formatHex(realPart, true));
        std::wstring partImag
            = fmt::sprintf(formatInfo.formatImag, formatInfo.widthImag, formatHex(ImagPart, true));
        result.append(partReal);
        result.append(partImag);
        result.append(L"i");
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        result = formatElement(realPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        result.append(formatRational(
            realPart, formatInfo.widthReal, formatInfo.widthReal - 1, formatInfo.trim));
        if (ImagPart < 0) {
            result.append(L" - ");
        } else {
            result.append(L" + ");
        }
        result.append(
            formatRational(fabs(ImagPart), formatInfo.widthImag, formatInfo.widthImag - 1, true));
        result.append(L"i");

    } break;
    default: {
    } break;
    }
    return result;
}
//=============================================================================
std::wstring
formatElement(single val, const FormatDisplayInformation& formatInfo)
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
        result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, formatHex(val, false));
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
            if (haveDecimals) {
                double value = val / pow(10, formatInfo.scaleFactor);
                if (value == 0.) {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, 0, value);

                } else {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal,
                        formatInfo.decimalsReal, value);
                }
            } else {
                if (formatInfo.floatAsInteger) {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal,
                        (long int)(val / pow(10, formatInfo.scaleFactor)));
                } else {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal,
                        val / pow(10, formatInfo.scaleFactor));
                }
            }
        } else {
            if (haveDecimals) {
                if (val == 0.) {
                    if (formatInfo.numericFormatDisplay == NLS_NUMERIC_FORMAT_SHORTE
                        || formatInfo.numericFormatDisplay == NLS_NUMERIC_FORMAT_LONGE) {
                        result = fmt::sprintf(L"%*s", formatInfo.widthReal, L"0");
                    } else {
                        result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, 0, val);
                    }
                } else {
                    result = fmt::sprintf(
                        formatInfo.formatReal, formatInfo.widthReal, formatInfo.decimalsReal, val);
                }
            } else {
                if (formatInfo.floatAsInteger) {
                    if (!std::isfinite(val)) {
                        if (std::isnan(val)) {
                            result = fmt::sprintf(L"%*s", formatInfo.widthReal, L"NaN");
                        } else if (val < 0) {
                            result = fmt::sprintf(L"%*s", formatInfo.widthReal, L"-Inf");
                        } else {
                            result = fmt::sprintf(L"%*s", formatInfo.widthReal, L"Inf");
                        }
                    } else {
                        result = fmt::sprintf(
                            formatInfo.formatReal, formatInfo.widthReal, (long int)val);
                    }
                } else {
                    result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, val);
                }
            }
        }
    } break;
    }
    return result;
}
//=============================================================================
std::wstring
formatElementComplex(single realPart, single ImagPart, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (formatInfo.numericFormatDisplay) {
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        result = formatComplexShortEng(realPart, ImagPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        result = formatComplexLongEng(realPart, ImagPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result = formatPlus(realPart, false);
    } break;
    case NLS_NUMERIC_FORMAT_LONG:
    case NLS_NUMERIC_FORMAT_LONGG:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_SHORT: {
        result = formatComplex<single>(realPart, ImagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        std::wstring partReal
            = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, formatHex(realPart, true));
        std::wstring partImag
            = fmt::sprintf(formatInfo.formatImag, formatInfo.widthImag, formatHex(ImagPart, true));
        result.append(partReal);
        result.append(partImag);
        result.append(L"i");
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        result = formatElement(realPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        result.append(formatRational(
            realPart, formatInfo.widthReal, formatInfo.widthReal - 1, formatInfo.trim));
        if (ImagPart < 0) {
            result.append(L" - ");
        } else {
            result.append(L" + ");
        }
        result.append(
            formatRational(fabs(ImagPart), formatInfo.widthImag, formatInfo.widthImag - 1, true));
        result.append(L"i");
    } break;
    default: {
    } break;
    }
    return result;
}
//=============================================================================
std::wstring
formatScalarNumber(double val, bool asSingle, const FormatDisplayInformation& formatInfo)
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
        if (IsIntegerForm(val)) {
            result = L"   " + fmt::sprintf(L"%.f", val);
        } else {
            result = L"   "
                + formatRational(val, formatInfo.widthReal, formatInfo.widthReal - 1, true);
        }
        if (formatInfo.trim) {
            StringHelpers::trim_left(result);
        }
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result.append(formatPlus(val, formatInfo.trim));
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        if (asSingle) {
            result = fmt::sprintf(
                formatInfo.formatReal, formatInfo.widthReal, formatHex((single)val, false));
        } else {
            result
                = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, formatHex(val, false));
        }
        if (formatInfo.trim) {
            StringHelpers::trim_left(result);
        }
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        if (!formatInfo.trim) {
            result.append(L" ");
        }
        result.append(formatNumber(val, formatInfo));
    } break;
    default: {
        result.append(formatNumber(val, formatInfo));
    } break;
    }
    return result;
}
//=============================================================================
std::wstring
formatScalarComplexNumber(
    double realPart, double imagPart, bool asSingle, const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (formatInfo.numericFormatDisplay) {
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        if (asSingle) {
            result.append(
                formatComplexShortEng((single)realPart, (single)imagPart, formatInfo.trim));
        } else {
            result.append(formatComplexShortEng(realPart, imagPart, formatInfo.trim));
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        if (asSingle) {
            result.append(
                formatComplexLongEng((single)realPart, (single)imagPart, formatInfo.trim));
        } else {
            result.append(formatComplexLongEng(realPart, imagPart, formatInfo.trim));
        }
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result = formatPlus(realPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        result.append(formatHex(realPart, formatInfo.trim));
        result.append(L"   ");
        result.append(formatHex(imagPart, true));
        result.append(L"i");
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        result.append(formatRational(
            realPart, formatInfo.widthReal, formatInfo.widthReal - 1, formatInfo.trim));
        if (imagPart < 0) {
            result.append(L" - ");
        } else {
            result.append(L" + ");
        }
        result.append(
            formatRational(fabs(imagPart), formatInfo.widthImag, formatInfo.widthImag - 1, true));
        result.append(L"i");
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
            result.append(formatNumberComplex<double>(realPart, imagPart, formatInfo));
        }

    } break;
    }
    return result;
}
//=============================================================================
}
//=============================================================================
