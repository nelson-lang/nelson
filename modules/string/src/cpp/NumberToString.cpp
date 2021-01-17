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
#define _CRT_SECURE_NO_WARNINGS
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include <cstdio>
//=============================================================================
#include <boost/algorithm/string.hpp>
#include "NumberToString.hpp"
#include "nlsConfig.h"
#include "characters_encoding.hpp"
#include "RealPart.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BUFFER_LEN 2048
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperComplex(const ArrayOf& A, bool withPrecision, const std::wstring& format)
{
    ArrayOf res;
    char buffer_re[BUFFER_LEN];
    char buffer_im[BUFFER_LEN];
    T* dp = (T*)A.getDataPointer();
    std::complex<T>* dpz = reinterpret_cast<std::complex<T>*>(dp);
    Dimensions dimsA = A.getDimensions();
    size_t maxlen = 0;
    bool allint = true;
    std::string uformat = wstring_to_utf8(format);
    boost::replace_all(uformat, "%%", "%");
    if (!withPrecision) {
        for (indexType i = 0; i < dimsA.getElementCount(); i++) {
            allint = allint && (dpz[i].real() == round(dpz[i].real()));
            allint = allint && (dpz[i].imag() == round(dpz[i].imag()));
            sprintf(buffer_re, uformat.c_str(), fabs(static_cast<double>(dpz[i].real())));
            sprintf(buffer_im, uformat.c_str(), fabs(static_cast<double>(dpz[i].imag())));
            maxlen = std::max(maxlen, strlen(buffer_re) + strlen(buffer_im));
        }
        if (!allint) {
            maxlen = std::max(maxlen, (size_t)20);
        }
    }
    stringVector rows;
    indexType m = dimsA.getRows();
    indexType n = dimsA.getColumns();
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            sprintf(buffer_re, uformat.c_str(), dpz[j * m + i].real());
            sprintf(buffer_im, uformat.c_str(), dpz[j * m + i].imag());
            std::string sr(buffer_re);
            std::string si(buffer_im);
            boost::replace_all(sr, "inf", "Inf");
            boost::replace_all(sr, "nan", "NaN");
            boost::replace_all(si, "inf", "Inf");
            boost::replace_all(si, "nan", "NaN");
            if (dpz[j * m + i].imag() > 0) {
                si = "+" + si;
            }
            std::string s = sr + si + "i";
            if (j != 0 && !withPrecision) {
                int l = maxlen - s.length() + 5;
                row += std::string(l > 0 ? l : 5, ' ');
            }
            row += s;
        }
        boost::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterVectorToCharacterArray(rows);
}
//=============================================================================
template <class T>
static ArrayOf
NumberToStringHelperReal(const ArrayOf& A, bool withPrecision, const std::wstring& format)
{
    ArrayOf res;
    char buffer[BUFFER_LEN];
    T* dp = (T*)A.getDataPointer();
    Dimensions dimsA = A.getDimensions();
    size_t maxlen = 0;
    bool allint = true;
    std::string uformat = wstring_to_utf8(format);
    boost::replace_all(uformat, "%%", "%");
    if (!withPrecision) {
        for (indexType i = 0; i < dimsA.getElementCount(); i++) {
            allint = allint && (dp[i] == round(dp[i]));
            sprintf(buffer, uformat.c_str(), fabs(static_cast<double>(dp[i])));
            maxlen = std::max(maxlen, strlen(buffer));
        }
        if (!allint) {
            maxlen = std::max(maxlen, (size_t)10);
        }
    }
    stringVector rows;
    indexType m = dimsA.getRows();
    indexType n = dimsA.getColumns();
    for (indexType i = 0; i < m; i++) {
        std::string row;
        for (indexType j = 0; j < n; j++) {
            if (allint) {
                sprintf(buffer, uformat.c_str(), static_cast<double>(dp[j * m + i]));
            } else {
                sprintf(buffer, uformat.c_str(), dp[j * m + i]);
            }
            std::string s(buffer);
            boost::replace_all(s, "inf", "Inf");
            boost::replace_all(s, "nan", "NaN");
            if (j != 0 && !withPrecision) {
                int l = maxlen - s.length() + 2;
                row += std::string(l > 0 ? l : 2, ' ');
            }
            row += s;
        }
        boost::trim_left(row);
        rows.push_back(row);
    }
    return ArrayOf::characterVectorToCharacterArray(rows);
}
//=============================================================================
static ArrayOf
NumberToString(
    const ArrayOf& A, bool withPrecision, const std::wstring& format, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isEmpty()) {
        return ArrayOf::characterArrayConstructor(L"");
    }
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        res = NumberToStringHelperReal<double>(A, withPrecision, format);
    } break;
    case NLS_DCOMPLEX: {
        if (A.allReal()) {
            ArrayOf B = RealPart(A);
            res = NumberToStringHelperReal<double>(B, withPrecision, format);
        } else {
            res = NumberToStringHelperComplex<double>(A, withPrecision, format);
        }
    } break;
    case NLS_SINGLE: {
        res = NumberToStringHelperReal<single>(A, withPrecision, format);
    } break;
    case NLS_SCOMPLEX: {
        if (A.allReal()) {
            ArrayOf B = RealPart(A);
            res = NumberToStringHelperReal<single>(B, withPrecision, format);
        } else {
            res = NumberToStringHelperComplex<single>(A, withPrecision, format);
        }
    } break;
    case NLS_LOGICAL: {
        res = NumberToStringHelperReal<logical>(A, withPrecision, format);
    } break;
    case NLS_UINT8: {
        res = NumberToStringHelperReal<uint8>(A, withPrecision, format);
    } break;
    case NLS_INT8: {
        res = NumberToStringHelperReal<int8>(A, withPrecision, format);
    } break;
    case NLS_UINT16: {
        res = NumberToStringHelperReal<uint16>(A, withPrecision, format);
    } break;
    case NLS_INT16: {
        res = NumberToStringHelperReal<int16>(A, withPrecision, format);
    } break;
    case NLS_UINT32: {
        res = NumberToStringHelperReal<uint32>(A, withPrecision, format);
    } break;
    case NLS_INT32: {
        res = NumberToStringHelperReal<int32>(A, withPrecision, format);
    } break;
    case NLS_UINT64: {
        res = NumberToStringHelperReal<uint64>(A, withPrecision, format);
    } break;
    case NLS_INT64: {
        res = NumberToStringHelperReal<int64>(A, withPrecision, format);
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
    std::wstring format = L"%%.5g";
    return NumberToString(A, format, needToOverload);
}
//=============================================================================
ArrayOf
NumberToString(const ArrayOf& A, const std::wstring& format, bool& needToOverload)
{
    return NumberToString(A, false, format, needToOverload);
}
//=============================================================================
ArrayOf
NumberToString(const ArrayOf& A, double N, bool& needToOverload)
{
    std::wstring format
        = L"%%" + std::to_wstring((int)(N + 7)) + L"." + std::to_wstring((int)N) + L"g";
    return NumberToString(A, true, format, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
