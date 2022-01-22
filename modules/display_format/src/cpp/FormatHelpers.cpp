//===========================================================================
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
#include <Eigen/Dense>
#include <Eigen/Sparse>
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
        int absCommonLogarithm = abs(formatInfo.scaleFactor);
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
static FormatDisplayInformation
getArrayOfFormatInfoDouble(NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    formatInfo.numericFormatDisplay = currentNumericFormat;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORTE: {
        formatInfo.widthReal = 13;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 4;
        formatInfo.formatReal = L"%*.*e";
    } break;

    case NLS_NUMERIC_FORMAT_SHORT: {
        formatInfo.widthReal = 9;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 4;
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        formatInfo.widthReal = 18;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 18;
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
        formatInfo.widthReal = 26;
        formatInfo.formatReal = L"%*.*g";
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 17;
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        formatInfo.widthReal = 17;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 17;

    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        formatInfo.widthReal = 26;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 26;
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        formatInfo.widthReal = 13;
        formatInfo.formatReal = L"%*.*f";
        formatInfo.decimalsReal = 2;
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        formatInfo.widthReal = 20;
        formatInfo.formatReal = L"%*s";
        formatInfo.decimalsReal = 0;
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        formatInfo.widthReal = 9;
        formatInfo.formatReal = L"%*s";
    } break;
    }
    return formatInfo;
}
//=============================================================================
static FormatDisplayInformation
getArrayOfFormatInfoSingle(NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    formatInfo.numericFormatDisplay = currentNumericFormat;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORTE: {
        formatInfo.widthReal = 12;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 4;
        formatInfo.formatReal = L"%*.*e";
    } break;

    case NLS_NUMERIC_FORMAT_SHORT: {
        formatInfo.widthReal = 9;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 4;
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        formatInfo.widthReal = 18;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 7;
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
        formatInfo.widthReal = 27;
        formatInfo.floatAsInteger = true;
        formatInfo.decimalsReal = 15;
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        formatInfo.widthReal = 17;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 17;
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        formatInfo.widthReal = 17;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 17;
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        formatInfo.widthReal = 13;
        formatInfo.formatReal = L"%*.*f";
        formatInfo.decimalsReal = 2;
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        formatInfo.widthReal = 20;
        formatInfo.formatReal = L"%*s";
        formatInfo.decimalsReal = 0;
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        formatInfo.widthReal = 9;
        formatInfo.formatReal = L"%*s";
    } break;
    }
    return formatInfo;
}
//=============================================================================
static FormatDisplayInformation
getArrayOfFormatInfoDoubleComplex(NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    formatInfo.numericFormatDisplay = currentNumericFormat;
    formatInfo.isComplex = true;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT: {
        formatInfo.floatAsInteger = false;
        formatInfo.widthReal = 9;
        formatInfo.decimalsReal = 4;
        formatInfo.widthImag = 9;
        formatInfo.decimalsImag = 4;
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE: {
        formatInfo.floatAsInteger = false;
        formatInfo.widthReal = 13;
        formatInfo.decimalsReal = 4;
        formatInfo.formatReal = L"%*.*e";
        formatInfo.widthImag = 11;
        formatInfo.decimalsImag = 4;
        formatInfo.formatImag = L"%*.*e";
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        formatInfo.floatAsInteger = false;
        formatInfo.widthReal = (17 * 2) + 3 + 1;
        formatInfo.decimalsReal = 0;
        formatInfo.widthImag = (17 * 2) + 3 + 1;
        formatInfo.decimalsImag = 0;

    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        formatInfo.floatAsInteger = false;
        formatInfo.widthReal = 51;
        formatInfo.decimalsReal = 0;
        formatInfo.widthImag = 51;
        formatInfo.decimalsImag = 0;
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
    } break;
    }
    return formatInfo;
}
//=============================================================================
static FormatDisplayInformation
getArrayOfFormatInfoSingleComplex(NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT: {
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE: {
        formatInfo.floatAsInteger = false;
        formatInfo.widthReal = 12;
        formatInfo.decimalsReal = 4;
        formatInfo.formatReal = L"%*.*e";
        formatInfo.widthImag = 10;
        formatInfo.decimalsImag = 4;
        formatInfo.formatImag = L"%*.*e";
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        formatInfo.widthReal = 34;
        formatInfo.floatAsInteger = false;
        formatInfo.decimalsReal = 34;

    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        formatInfo.floatAsInteger = false;
        formatInfo.widthReal = 34;
        formatInfo.decimalsReal = 0;
        formatInfo.widthImag = 34;
        formatInfo.decimalsImag = 0;

    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
    } break;
    }
    return formatInfo;
}
//=============================================================================
static FormatDisplayInformation
computeFormatInfo(
    double realPart, double imagPart, bool asSingle, NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    if (asSingle) {
        formatInfo = getArrayOfFormatInfoSingleComplex(currentNumericFormat);
    } else {
        formatInfo = getArrayOfFormatInfoDoubleComplex(currentNumericFormat);
    }
    if (currentNumericFormat == NLS_NUMERIC_FORMAT_SHORTE) {
    } else if (currentNumericFormat == NLS_NUMERIC_FORMAT_RATIONAL) {
        if (fabs(realPart) < 1e-10) {
            formatInfo.widthReal = 50;
            formatInfo.formatReal = L"%*s";
            formatInfo.trim = true;
        }
        if (fabs(realPart) > 1e10) {
            formatInfo.widthReal = 100;
            formatInfo.formatReal = L"%*s";
            formatInfo.trim = true;
        }

        if (fabs(imagPart) < 1e-10) {
            formatInfo.widthImag = 50;
            formatInfo.formatImag = L"%*s";
            formatInfo.trim = true;
        }
        if (fabs(imagPart) > 1e10) {
            formatInfo.widthImag = 100;
            formatInfo.formatImag = L"%*s";
            formatInfo.trim = true;
        }

    } else if (currentNumericFormat == NLS_NUMERIC_FORMAT_BANK) {
        formatInfo.formatReal = L"%*.*f";
        formatInfo.widthReal = 13;
        formatInfo.decimalsReal = 2;

        formatInfo.formatImag = L"%*.*f";
        formatInfo.widthImag = 13;
        formatInfo.decimalsImag = 2;
    } else {
        if (std::isfinite(realPart) || std::isfinite(imagPart)) {
            bool isSmallReal = (fabs(realPart) <= 1e-3);
            bool isSmallImag = (fabs(imagPart) <= 1e-3);
            bool isAllSmall = isSmallReal && isSmallImag;

            bool isBigReal = (fabs(realPart) >= 1e2);
            bool isBigImag = (fabs(imagPart) >= 1e2);
            bool isBig = isBigReal || isBigImag;
            bool engineer = false;
            if (realPart == 0 && imagPart == 0) {
                engineer = false;
            } else if (isBig || isAllSmall) {
                engineer = true;
            } else if (std::hypot(realPart, imagPart) == realPart
                || std::hypot(realPart, imagPart) == imagPart) {
                engineer = false;
            }
            if (engineer) {
                formatInfo.formatReal = L"%*.*e";
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 4;

                formatInfo.formatImag = L"%*.*e";
                formatInfo.widthImag = 11;
                formatInfo.decimalsImag = 4;

            } else {
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 9;
                formatInfo.decimalsReal = 4;

                formatInfo.formatImag = L"%*.*f";
                formatInfo.widthImag = 7;
                formatInfo.decimalsImag = 4;
            }
        } else {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 9;
            formatInfo.decimalsReal = 4;

            formatInfo.formatImag = L"%*.*f";
            formatInfo.widthImag = 7;
            formatInfo.decimalsImag = 4;
        }
    }
    return formatInfo;
}
//=============================================================================
static FormatDisplayInformation
computeFormatInfo(double val, bool asSingle, NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    if (asSingle) {
        formatInfo = getArrayOfFormatInfoSingle(currentNumericFormat);
    } else {
        formatInfo = getArrayOfFormatInfoDouble(currentNumericFormat);
    }
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORTE: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                if (asSingle) {
                    formatInfo.widthReal = 12;
                } else {
                    formatInfo.widthReal = 13;
                }
                formatInfo.formatReal = L"%*.*e";
                formatInfo.decimalsReal = 4;
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORT: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                formatInfo.formatReal = L"%*.*e";
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 4;
            }
        } else {
            if (absoluteValue <= 1e-3 || absoluteValue > 1e3) {
                formatInfo.formatReal = L"%*.*e";
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 4;
            } else {
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 10;
                formatInfo.decimalsReal = 4;
            }
        }
    } break;

    case NLS_NUMERIC_FORMAT_LONGE: {

    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                formatInfo.formatReal = L"%*.*e";
                if (asSingle) {
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
            }
        } else {
            if (asSingle) {
                if (absoluteValue <= 1e-3 || absoluteValue > 1e2) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 7;
                }
            } else {
                if (absoluteValue <= 1e-3 || absoluteValue > 1e3) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 20;
                    formatInfo.decimalsReal = 15;
                }
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        if (fabs(val) < 1e-10) {
            formatInfo.widthReal = 50;
            formatInfo.formatReal = L"%*s";
            formatInfo.trim = true;
        }
        if (fabs(val) > 1e10) {
            formatInfo.widthReal = 100;
            formatInfo.formatReal = L"%*s";
            formatInfo.trim = true;
        }
    } break;
    default: { } break; }
    return formatInfo;
}
//=============================================================================
template <class T>
static std::wstring
formatIntegerReal(T val, const FormatDisplayInformation& formatInfo)
{
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
        result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, 0);
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
        T scaledRealPart = realPart / pow(10, formatInfo.scaleFactor);
        T scaledImagPart = imagPart / pow(10, formatInfo.scaleFactor);

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
    if (trimImagPart) {
        //        boost::trim_left(resultImagPart);
    }
    if (formatInfo.numericFormatDisplay == NLS_NUMERIC_FORMAT_BANK) {
        result = resultRealPart;
        if (formatInfo.trim) {
            boost::trim_left(result);
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
        boost::trim_left(result);
    }
    return result;
}
//=============================================================================
template <class T>
bool
getFiniteMinMax(const T* val, indexType nbElements, T& min, T& max, bool& isFinite)
{
    T minValue = std::nan("NaN");
    T maxValue = std::nan("NaN");
    T shared_max = std::nan("NaN");
    T shared_min = std::nan("NaN");
    isFinite = true;

    for (indexType k = 0; k < nbElements; ++k) {
        if (std::isfinite(val[k])) {
            minValue = val[k];
            maxValue = val[k];
            shared_max = maxValue;
            shared_min = minValue;
            break;
        }
    }
    if (std::isnan(shared_min) && std::isnan(shared_max)) {
        min = shared_min;
        max = shared_max;
        isFinite = false;
        return false;
    }
#pragma omp parallel
    {
#pragma omp for nowait
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            if (std::isfinite(val[idx])) {
                maxValue = std::max(val[idx], maxValue);
                minValue = std::min(val[idx], minValue);
            } else {
                isFinite = false;
            }
        }
#pragma omp critical
        {
            shared_max = std::max(shared_max, maxValue);
            shared_min = std::min(shared_min, minValue);
        }
    }
    min = shared_min;
    max = shared_max;
    return true;
}
//=============================================================================
template <class T>
static std::wstring
formatNumberComplex(T realPart, T imagPart, const FormatDisplayInformation& formatInfo)
{
    std::wstring result = formatComplex<T>(realPart, imagPart, formatInfo);
    if (formatInfo.trim) {
        boost::trim_left(result);
    }
    return result;
}
//=============================================================================
FormatDisplayInformation
computeFormatInfo(const ArrayOf& A, NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    bool isFinite = false;
    bool allInteger = false;
    double absoluteValue;
    if (A.isSingleClass()) {
        single minValue = 0;
        single maxValue = 0;

        if (A.isComplex()) {
            formatInfo = getArrayOfFormatInfoSingleComplex(currentNumericFormat);
        } else {
            formatInfo = getArrayOfFormatInfoSingle(currentNumericFormat);
        }
        if (A.isSparse()) {
            if (A.isComplex()) {
                Eigen::SparseMatrix<std::complex<single>, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<std::complex<single>, 0, signedIndexType>*)
                          A.getSparseDataPointer();
                std::complex<single>* zdata = spMat->valuePtr();
                const single* data = reinterpret_cast<single*>(zdata);
                allInteger = IsIntegerFormOrNotFinite(data, spMat->nonZeros() * 2);
                getFiniteMinMax<single>(data, spMat->nonZeros() * 2, minValue, maxValue, isFinite);
            } else {
                Eigen::SparseMatrix<single, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<single, 0, signedIndexType>*)A.getSparseDataPointer();
                const single* data = spMat->valuePtr();
                allInteger = IsIntegerFormOrNotFinite(data, spMat->nonZeros());
                getFiniteMinMax<single>(data, spMat->nonZeros(), minValue, maxValue, isFinite);
            }
        } else {
            if (A.isComplex()) {
                const single* pValues = (const single*)A.getDataPointer();
                allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount() * 2);
                getFiniteMinMax<single>(
                    pValues, A.getElementCount() * 2, minValue, maxValue, isFinite);
            } else {
                const single* pValues = (const single*)A.getDataPointer();
                allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount());
                getFiniteMinMax<single>(pValues, A.getElementCount(), minValue, maxValue, isFinite);
            }
        }
        absoluteValue = std::max(fabs(maxValue), fabs(minValue));

    } else {
        double minValue = 0;
        double maxValue = 0;

        if (A.isComplex()) {
            formatInfo = getArrayOfFormatInfoDoubleComplex(currentNumericFormat);
        } else {
            formatInfo = getArrayOfFormatInfoDouble(currentNumericFormat);
        }
        if (A.isSparse()) {
            if (A.isComplex()) {
                Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)
                          A.getSparseDataPointer();
                std::complex<double>* zdata = spMat->valuePtr();
                const double* data = reinterpret_cast<double*>(zdata);
                allInteger = IsIntegerFormOrNotFinite(data, spMat->nonZeros() * 2);
                getFiniteMinMax<double>(data, spMat->nonZeros() * 2, minValue, maxValue, isFinite);
            } else {
                Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
                const double* data = spMat->valuePtr();
                allInteger = IsIntegerFormOrNotFinite(data, spMat->nonZeros());
                getFiniteMinMax<double>(data, spMat->nonZeros(), minValue, maxValue, isFinite);
            }
        } else {
            if (A.isComplex()) {
                const double* pValues = (const double*)A.getDataPointer();
                allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount() * 2);
                getFiniteMinMax<double>(
                    pValues, A.getElementCount() * 2, minValue, maxValue, isFinite);
            } else {
                const double* pValues = (const double*)A.getDataPointer();
                allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount());
                getFiniteMinMax<double>(pValues, A.getElementCount(), minValue, maxValue, isFinite);
            }
        }
        absoluteValue = (double)std::max(fabs(maxValue), fabs(minValue));
    }
    if (A.isComplex()) {
        switch (currentNumericFormat) {
        case NLS_NUMERIC_FORMAT_LONG: {
            ComputeScaleFactor(A, allInteger, formatInfo);
            formatInfo.isComplex = true;
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 11;
            formatInfo.decimalsReal = 7;

            formatInfo.formatImag = L"%*.*f";
            formatInfo.widthImag = 10;
            formatInfo.decimalsImag = 7;

        } break;
        case NLS_NUMERIC_FORMAT_SHORT: {
            ComputeScaleFactor(A, allInteger, formatInfo);
            formatInfo.isComplex = true;
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 9;
            formatInfo.decimalsReal = 4;

            formatInfo.formatImag = L"%*.*f";
            formatInfo.widthImag = 7;
            formatInfo.decimalsImag = 4;
        } break;
        default: { } break; }
    } else {
        switch (currentNumericFormat) {
        case NLS_NUMERIC_FORMAT_HEX: {
        } break;

        case NLS_NUMERIC_FORMAT_SHORTE: {
            if (allInteger) {
                if (!std::isfinite(absoluteValue)) {
                    formatInfo.floatAsInteger = true;
                    formatInfo.formatReal = L"%*ld";
                    formatInfo.widthReal = 6;
                    formatInfo.decimalsReal = 0;
                } else if (absoluteValue <= 999) {
                    formatInfo.floatAsInteger = true;
                    formatInfo.formatReal = L"%*ld";
                    formatInfo.widthReal = 6;
                    formatInfo.decimalsReal = 0;
                } else if (absoluteValue <= 999999999) {
                    formatInfo.floatAsInteger = true;
                    formatInfo.formatReal = L"%*ld";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 0;
                } else {
                    formatInfo.floatAsInteger = false;
                    formatInfo.formatReal = L"%*.*e";
                    if (A.isSingleClass()) {
                        formatInfo.widthReal = 12;
                    } else {
                        formatInfo.widthReal = 13;
                    }
                    formatInfo.decimalsReal = 4;
                }
            } else {
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 4;
                } else {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 13;
                    formatInfo.decimalsReal = 4;
                }
            }

        } break;
        case NLS_NUMERIC_FORMAT_LONGE: {
            if (allInteger) {
                if (absoluteValue <= 999) {
                    formatInfo.floatAsInteger = true;
                    formatInfo.formatReal = L"%*ld";
                    formatInfo.widthReal = 6;
                    formatInfo.decimalsReal = 0;
                } else if (absoluteValue <= 999999999) {
                    formatInfo.floatAsInteger = true;
                    formatInfo.formatReal = L"%*ld";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 0;
                } else {
                    formatInfo.floatAsInteger = false;
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 13;
                    formatInfo.decimalsReal = 4;
                }
            } else {
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
            }
        } break;
        case NLS_NUMERIC_FORMAT_LONG: {
            ComputeScaleFactor(A, allInteger, formatInfo);
            if (allInteger) {
                if (formatInfo.scaleFactor == 1) {
                    if (absoluteValue <= 999) {
                        formatInfo.floatAsInteger = true;
                        formatInfo.formatReal = L"%*ld";
                        formatInfo.widthReal = 6;
                        formatInfo.decimalsReal = 0;
                    } else if (absoluteValue <= 999999999) {
                        formatInfo.floatAsInteger = true;
                        formatInfo.formatReal = L"%*ld";
                        formatInfo.widthReal = 12;
                        formatInfo.decimalsReal = 0;
                    } else {
                        formatInfo.floatAsInteger = false;
                        formatInfo.formatReal = L"%*.*e";
                        formatInfo.widthReal = 13;
                        formatInfo.decimalsReal = 4;
                    }
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    if (A.isSingleClass()) {
                        formatInfo.widthReal = 12;
                        formatInfo.decimalsReal = 7;
                    } else {
                        formatInfo.widthReal = 20;
                        formatInfo.decimalsReal = 15;
                    }
                }
            } else {
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 20;
                    formatInfo.decimalsReal = 15;
                }
            }
        } break;

        case NLS_NUMERIC_FORMAT_SHORT: {
            ComputeScaleFactor(A, allInteger, formatInfo);
            if (allInteger) {
                if (formatInfo.scaleFactor == 1) {
                    if (absoluteValue <= 999) {
                        formatInfo.floatAsInteger = true;
                        formatInfo.formatReal = L"%*ld";
                        formatInfo.widthReal = 6;
                        formatInfo.decimalsReal = 0;
                    } else if (absoluteValue <= 999999999) {
                        formatInfo.floatAsInteger = true;
                        formatInfo.formatReal = L"%*ld";
                        formatInfo.widthReal = 12;
                        formatInfo.decimalsReal = 0;
                    } else {
                        formatInfo.floatAsInteger = false;
                        formatInfo.formatReal = L"%*.*e";
                        formatInfo.widthReal = 13;
                        formatInfo.decimalsReal = 4;
                    }
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 10;
                    formatInfo.decimalsReal = 4;
                }
            } else {
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 10;
                formatInfo.decimalsReal = 4;
            }
        } break;
        case NLS_NUMERIC_FORMAT_RATIONAL: {
            formatInfo.widthReal = 11;
            formatInfo.decimalsReal = 9;
            formatInfo.formatReal = L"%*s";

        } break;
        default: { } break; }
    }
    return formatInfo;
}
//=============================================================================
std::wstring
formatElement(double val, NumericFormatDisplay currentNumericFormat,
    const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (currentNumericFormat) {
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
    case NLS_NUMERIC_FORMAT_SHORTE:
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
                    if (formatInfo.numericFormatDisplay == NLS_NUMERIC_FORMAT_SHORTE) {
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
formatElementComplex(double realPart, double ImagPart, NumericFormatDisplay currentNumericFormat,
    const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_LONG: {
        result = formatComplex<double>(realPart, ImagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        result = formatComplexShortEng(realPart, ImagPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        result = formatComplexLongEng(realPart, ImagPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_SHORT: {
        result = formatComplex<double>(realPart, ImagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE: {
        result = formatComplex<double>(realPart, ImagPart, formatInfo);
    } break;
    default: { } break; }
    return result;
}
//=============================================================================
std::wstring
formatElement(single val, NumericFormatDisplay currentNumericFormat,
    const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (currentNumericFormat) {
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
    case NLS_NUMERIC_FORMAT_SHORTE:
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
                    if (formatInfo.numericFormatDisplay == NLS_NUMERIC_FORMAT_SHORTE) {
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
formatElementComplex(single realPart, single ImagPart, NumericFormatDisplay currentNumericFormat,
    const FormatDisplayInformation& formatInfo)
{
    std::wstring result;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_LONG: {
        result = formatComplex<single>(realPart, ImagPart, formatInfo);
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        result = formatComplexShortEng(realPart, ImagPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        result = formatComplexLongEng(realPart, ImagPart, formatInfo.trim);
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_SHORT: {
        result = formatComplex<single>(realPart, ImagPart, formatInfo);
    } break;
    default: { } break; }
    return result;
}
//=============================================================================
std::wstring
formatScalarNumber(
    double val, bool asSingle, NumericFormatDisplay currentNumericFormat, bool forceLeftTrim)
{
    std::wstring result;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        if (asSingle) {
            result = formatShortEng((single)val, forceLeftTrim);
        } else {
            result = formatShortEng((double)val, forceLeftTrim);
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        if (asSingle) {
            result = formatLongEng((single)val, forceLeftTrim);
        } else {
            result = formatLongEng((double)val, forceLeftTrim);
        }
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        FormatDisplayInformation formatInfo
            = computeFormatInfo(val, asSingle, currentNumericFormat);
        if (IsIntegerForm(val)) {
            result = L"   " + fmt::sprintf(L"%.f", val);
        } else {
            result = L"   "
                + formatRational(val, formatInfo.widthReal, formatInfo.widthReal - 1, true);
        }
        if (forceLeftTrim) {
            boost::trim_left(result);
        }
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result.append(formatPlus(val, forceLeftTrim));
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        FormatDisplayInformation formatInfo
            = computeFormatInfo(val, asSingle, currentNumericFormat);
        result = fmt::sprintf(formatInfo.formatReal, formatInfo.widthReal, formatHex(val, false));
        if (forceLeftTrim) {
            boost::trim_left(result);
        }
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        FormatDisplayInformation formatInfo
            = computeFormatInfo(val, asSingle, currentNumericFormat);
        formatInfo.trim = forceLeftTrim;
        if (!forceLeftTrim) {
            result.append(L" ");
        }
        result.append(formatNumber(val, formatInfo));
    } break;
    default: {
        FormatDisplayInformation formatInfo
            = computeFormatInfo(val, asSingle, currentNumericFormat);
        formatInfo.trim = forceLeftTrim;
        result.append(formatNumber(val, formatInfo));
    } break;
    }
    return result;
}
//=============================================================================
std::wstring
formatScalarComplexNumber(double realPart, double imagPart, bool asSingle,
    NumericFormatDisplay currentNumericFormat, bool forceLeftTrim)
{
    std::wstring result;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        result.append(formatShortEng(realPart, forceLeftTrim));
        if (imagPart < 0) {
            result.append(L" -");
        } else {
            result.append(L" +");
        }
        if (std::isfinite(imagPart)) {
            result.append(L" ");
            result.append(formatShortEng(fabs(imagPart), true));
        } else {
            result.append(formatShortEng(fabs(imagPart), false));
        }
        result.append(L"i");
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        result.append(formatLongEng(realPart, forceLeftTrim));
        if (imagPart < 0) {
            result.append(L" -");
        } else {
            result.append(L" +");
        }
        if (std::isfinite(imagPart)) {
            result.append(L" ");
            result.append(formatLongEng(fabs(imagPart), true));
        } else {
            result.append(formatLongEng(fabs(imagPart), false));
        }
        result.append(L"i");
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        result = formatPlus(realPart, forceLeftTrim);
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        result.append(formatHex(realPart, true));
        result.append(L"   ");
        result.append(formatHex(imagPart, true));
        result.append(L"i");
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        FormatDisplayInformation formatInfo
            = computeFormatInfo(realPart, imagPart, asSingle, currentNumericFormat);
        result.append(formatRational(
            realPart, formatInfo.widthReal, formatInfo.widthReal - 1, forceLeftTrim));
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
        FormatDisplayInformation formatInfo
            = computeFormatInfo(realPart, imagPart, asSingle, currentNumericFormat);
        formatInfo.trim = forceLeftTrim;
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
