//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include <cstdio>
#include <cmath>
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include "StringHelpers.hpp"
#include "NumberToString.hpp"
#include "nlsBuildConfig.h"
#include "characters_encoding.hpp"
#include "RealPart.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum NUM2STR_ENUM
{
    AUTO = 0,
    PRECISION = 1,
    FORMAT = 2
};
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperLogical(const ArrayOf& A, NUM2STR_ENUM formatType, const std::wstring& format)
{
    std::string uformat;
    if (formatType == NUM2STR_ENUM::FORMAT) {
        uformat = wstring_to_utf8(format);
        StringHelpers::replace_all(uformat, "%%", "%");
    } else if (formatType == NUM2STR_ENUM::PRECISION) {
        uformat = wstring_to_utf8(format);
    } else if (formatType == NUM2STR_ENUM::AUTO) {
        uformat = "%2d";
    }
    T* dp = (T*)A.getDataPointer();
    size_t maxlen = 1;
    stringVector rows;
    Dimensions dimsA = A.getDimensions();
    indexType m = dimsA.getRows();
    indexType n = dimsA.getColumns();
    std::string s;
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            try {
                s = fmt::sprintf(uformat, (int)dp[j * m + i]);
            } catch (fmt::format_error&) {
                Error(_W("Wrong format string."));
            }
            if (formatType == NUM2STR_ENUM::AUTO) {
                int l = (int)(maxlen - s.length() + 2);
                row += std::string(l > 0 ? l : 2, ' ');
            }
            row += s;
        }
        StringHelpers::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterVectorToCharacterArray(rows);
}
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperInteger(const ArrayOf& A)
{
    T* dp = (T*)A.getDataPointer();
    Dimensions dimsA = A.getDimensions();
    long double maxAbsValue = fabsl(static_cast<long double>(dp[0]));
    for (indexType i = 0; i < dimsA.getElementCount(); i++) {
        maxAbsValue = std::max(maxAbsValue, fabsl(static_cast<long double>(dp[i])));
    }
    std::string maxString = fmt::to_string(static_cast<T>(maxAbsValue));
    size_t maxlen = maxString.size();
    stringVector rows;
    indexType m = dimsA.getRows();
    indexType n = dimsA.getColumns();
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            std::string s = fmt::to_string(dp[j * m + i]);
            int l = (int)(maxlen - s.length() + 2);
            row += std::string(l > 0 ? l : 2, ' ');
            row += s;
        }
        StringHelpers::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterVectorToCharacterArray(rows);
}
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperComplex(
    const ArrayOf& A, NUM2STR_ENUM formatType, const std::wstring& format, int precision)
{
    std::string uformat;
    T* dp = (T*)A.getDataPointer();
    std::complex<T>* dpz = reinterpret_cast<std::complex<T>*>(dp);
    Dimensions dimsA = A.getDimensions();
    bool allint = true;
    size_t maxlen = 0;

    if (formatType == NUM2STR_ENUM::AUTO) {
        long double maxAbsValue = std::nan("");
        for (indexType i = 0; i < dimsA.getElementCount(); i++) {
            long double value_real = fabsl((long double)dpz[i].real());
            long double value_imag = fabsl((long double)dpz[i].imag());
            if (std::isfinite(value_real)) {
                if (std::isnan(maxAbsValue)) {
                    maxAbsValue = value_real;
                }
                maxAbsValue = std::max(maxAbsValue, value_real);
                allint = allint && (dpz[i].real() == round(dpz[i].real()));
            } else {
                allint = false;
            }
            if (std::isfinite(value_imag)) {
                if (std::isnan(maxAbsValue)) {
                    maxAbsValue = value_imag;
                }
                maxAbsValue = std::max(maxAbsValue, value_imag);
                allint = allint && (dpz[i].imag() == round(dpz[i].imag()));
            } else {
                allint = false;
            }
        }
        int ndigit = 0;
        if (std::isfinite(maxAbsValue)) {
            ndigit = (int)std::floor(log10(maxAbsValue));
        }

        if (ndigit > 15 || !allint) {
            if (dimsA.isScalar()) {
                ndigit = std::max(ndigit + 6, 6);
            } else {

                ndigit = std::max(ndigit + 5, 5);
            }
            ndigit = std::min(ndigit, 16);
            uformat = "%" + fmt::to_string(ndigit + 7) + "." + fmt::to_string(ndigit) + "g";
        } else {
            ndigit += 3;
            if (!std::isfinite(maxAbsValue)) {
                ndigit = std::max(ndigit, 5);
            }
            uformat = "%" + fmt::to_string(ndigit) + ".0f";
        }
        std::string s;
        try {
            if (allint) {
                s = fmt::sprintf(uformat, static_cast<long double>(maxAbsValue));
            } else {
                s = fmt::sprintf(uformat, maxAbsValue);
            }
        } catch (fmt::format_error&) {
            Error(_W("Wrong format string."));
        }

        precision = ndigit;
        maxlen = s.size();
    } else if (formatType == NUM2STR_ENUM::FORMAT) {
        uformat = wstring_to_utf8(format);
        StringHelpers::replace_all(uformat, "%%", "%");
    } else if (formatType == NUM2STR_ENUM::PRECISION) {
        uformat = wstring_to_utf8(format);
    }
    stringVector rows;
    indexType m = dimsA.getRows();
    indexType n = dimsA.getColumns();
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            std::string strRealPart;
            try {
                if (allint) {
                    strRealPart
                        = fmt::sprintf(uformat, static_cast<long double>(dpz[j * m + i].real()));
                } else {
                    strRealPart = fmt::sprintf(uformat, dpz[j * m + i].real());
                }
            } catch (fmt::format_error&) {
                Error(_W("Wrong format string."));
            }
            std::string strImagPart;
            try {
                if (allint) {
                    strImagPart
                        = fmt::sprintf(uformat, static_cast<long double>(dpz[j * m + i].imag()));
                } else {
                    strImagPart = fmt::sprintf(uformat, dpz[j * m + i].imag());
                }
            } catch (fmt::format_error&) {
                Error(_W("Wrong format string."));
            }
            StringHelpers::erase_all(strImagPart, " ");
            std::string s;
            if (dpz[j * m + i].imag() < 0) {
                s = strRealPart + strImagPart + "i";
            } else {
                s = strRealPart + "+" + strImagPart + "i";
            }
            StringHelpers::replace_all(s, "inf", "Inf");
            StringHelpers::replace_all(s, "nan", "NaN");
            size_t nbSpace = 0;
            if (formatType == NUM2STR_ENUM::FORMAT) {
                nbSpace = 0;
            } else if (formatType == NUM2STR_ENUM::AUTO) {
                if (allint) {
                    nbSpace = (size_t)1;
                } else {
                    nbSpace = (size_t)10;
                }
            } else if (formatType == NUM2STR_ENUM::PRECISION) {
                nbSpace = (size_t)5 + (size_t)precision;
            }
            row += std::string(nbSpace, ' ');
            row += s;
        }
        StringHelpers::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterVectorToCharacterArray(rows);
}
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperReal(
    const ArrayOf& A, NUM2STR_ENUM formatType, const std::wstring& format, int precision)
{
    std::string uformat;
    T* dp = (T*)A.getDataPointer();
    Dimensions dimsA = A.getDimensions();
    bool allint = true;
    size_t maxlen = 0;

    if (formatType == NUM2STR_ENUM::AUTO) {
        long double maxAbsValue = std::nan("");
        for (indexType i = 0; i < dimsA.getElementCount(); i++) {
            long double value_real = fabsl((long double)dp[i]);
            if (std::isfinite(value_real)) {
                if (std::isnan(maxAbsValue)) {
                    maxAbsValue = value_real;
                }
                maxAbsValue = std::max(maxAbsValue, value_real);
                allint = allint && (dp[i] == round(dp[i]));
            } else {
                allint = false;
            }
        }
        int ndigit = 0;
        if (std::isfinite(maxAbsValue)) {
            if (maxAbsValue == 0.) {
                ndigit = 0;
            } else {
                ndigit = (int)std::floor(log10(maxAbsValue));
            }
        }
        if (ndigit > 15 || !allint) {
            if (dimsA.isScalar()) {
                ndigit = std::max(ndigit + 6, 6);
            } else {
                ndigit = std::max(ndigit + 5, 5);
            }
            ndigit = std::min(ndigit, 16);
            uformat = "%" + fmt::to_string(ndigit + 7) + "." + fmt::to_string(ndigit) + "g";
        } else {
            ndigit += 3;
            if (!std::isfinite(maxAbsValue)) {
                ndigit = std::max(ndigit, 5);
            }
            uformat = "%" + fmt::to_string(ndigit) + ".0f";
        }
        std::string s;
        try {
            if (allint) {
                s = fmt::sprintf(uformat, static_cast<long double>(maxAbsValue));
            } else {
                s = fmt::sprintf(uformat, maxAbsValue);
            }
        } catch (fmt::format_error&) {
            Error(_W("Wrong format string."));
        }
        precision = ndigit;
        maxlen = s.size();
    } else if (formatType == NUM2STR_ENUM::FORMAT) {
        uformat = wstring_to_utf8(format);
        StringHelpers::replace_all(uformat, "%%", "%");
    } else if (formatType == NUM2STR_ENUM::PRECISION) {
        uformat = wstring_to_utf8(format);
    }
    stringVector rows;
    indexType m = dimsA.getRows();
    indexType n = dimsA.getColumns();
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            std::string s;
            try {
                if (allint) {
                    s = fmt::sprintf(uformat, static_cast<long double>(dp[j * m + i]));
                } else {
                    s = fmt::sprintf(uformat, dp[j * m + i]);
                }
            } catch (fmt::format_error&) {
                Error(_W("Wrong format string."));
            }
            StringHelpers::replace_all(s, "inf", "Inf");
            StringHelpers::replace_all(s, "nan", "NaN");
            row += s;
        }
        StringHelpers::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterVectorToCharacterArray(rows);
}
//=============================================================================
static ArrayOf
NumberToString(const ArrayOf& A, NUM2STR_ENUM formatType, const std::wstring& format, int precision,
    bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isEmpty()) {
        return ArrayOf::characterArrayConstructor(L"");
    }
    ArrayOf as2D;
    if (!A.is2D() && !A.isScalar()) {
        as2D = A;
        Dimensions dimsA = A.getDimensions();
        indexType N = dimsA.getElementCount();
        Dimensions dims2d(dimsA.getRows(), N / dimsA.getRows());
        as2D.reshape(dims2d);
    } else if (A.isSparse()) {
        as2D = A;
        as2D.makeDense();
    } else {
        as2D = A;
    }
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        res = NumberToStringHelperReal<double>(as2D, formatType, format, precision);
    } break;
    case NLS_DCOMPLEX: {
        if (A.allReal()) {
            ArrayOf B = RealPart(as2D);
            res = NumberToStringHelperReal<double>(B, formatType, format, precision);
        } else {
            res = NumberToStringHelperComplex<double>(as2D, formatType, format, precision);
        }
    } break;
    case NLS_SINGLE: {
        res = NumberToStringHelperReal<single>(as2D, formatType, format, precision);
    } break;
    case NLS_SCOMPLEX: {
        if (as2D.allReal()) {
            ArrayOf B = RealPart(as2D);
            res = NumberToStringHelperReal<single>(as2D, formatType, format, precision);
        } else {
            res = NumberToStringHelperComplex<single>(as2D, formatType, format, precision);
        }
    } break;
    case NLS_LOGICAL: {
        res = NumberToStringHelperLogical<logical>(as2D, formatType, format);
    } break;
    case NLS_UINT8: {
        res = NumberToStringHelperInteger<uint8>(as2D);
    } break;
    case NLS_INT8: {
        res = NumberToStringHelperInteger<int8>(as2D);
    } break;
    case NLS_UINT16: {
        res = NumberToStringHelperInteger<uint16>(as2D);
    } break;
    case NLS_INT16: {
        res = NumberToStringHelperInteger<int16>(as2D);
    } break;
    case NLS_UINT32: {
        res = NumberToStringHelperInteger<uint32>(as2D);
    } break;
    case NLS_INT32: {
        res = NumberToStringHelperInteger<int32>(as2D);
    } break;
    case NLS_UINT64: {
        res = NumberToStringHelperInteger<uint64>(as2D);
    } break;
    case NLS_INT64: {
        res = NumberToStringHelperInteger<int64>(as2D);
    } break;
    case NLS_CHAR: {
        res = A;
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
NumberToString(const ArrayOf& A, bool& needToOverload)
{
    return NumberToString(A, NUM2STR_ENUM::AUTO, L"", 0, needToOverload);
}
//=============================================================================
ArrayOf
NumberToString(const ArrayOf& A, const std::wstring& format, bool& needToOverload)
{
    return NumberToString(A, NUM2STR_ENUM::FORMAT, format, 0, needToOverload);
}
//=============================================================================
ArrayOf
NumberToString(const ArrayOf& A, double N, bool& needToOverload)
{
    std::wstring format;
    if (!std::isfinite(N)) {
        Error(_("PRECISION must be a scalar integer >= 0."));
    }
    int precision = (int)N;
    if (A.getDataClass() == NLS_LOGICAL) {
        format = L"%" + std::to_wstring(precision + 7) + L"d";
    } else {
        if (N < 0) {
            Error(_("PRECISION must be a scalar integer >= 0."));
        }
        format = L"%" + std::to_wstring(precision + 7) + L"." + std::to_wstring(precision) + L"g";
    }
    return NumberToString(A, NUM2STR_ENUM::PRECISION, format, precision, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
