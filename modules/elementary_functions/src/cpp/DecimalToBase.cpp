//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <cstdlib>
#include <algorithm>
#include <unordered_map>
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "StringHelpers.hpp"
#include "DecimalToBase.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifndef _MAX_U64TOSTR_BASE2_COUNT
#define _MAX_U64TOSTR_BASE2_COUNT (64 + 1)
#endif
//=============================================================================
static wchar_t digit[] = L"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
//=============================================================================
static std::unordered_map<uint64, std::wstring> baseConverted;
static size_t lastBase = 10;
static size_t lastDigits = 0;
//=============================================================================
static std::wstring
ullToBaseGeneric(unsigned long long v, size_t b, size_t len)
{
    std::wstring res;
    int d = 1;
    auto m = static_cast<unsigned long long>(b);
    while (m <= v) {
        unsigned long long tmp = m * b;
        d++;
        if (tmp / b != m) {
            break;
        }
        m = tmp;
    }

    int p = (int)(len - d);
    p = p < 0 ? 0 : p;
    int k = d + p;
    res.reserve(k + 1);
    while (--k >= p) {
        res.push_back(digit[v % b]);
        v /= b;
    }
    std::reverse(res.begin(), res.end());
    return res;
}
//=============================================================================
static std::wstring
ullToBase(unsigned long long v, size_t b, size_t len)
{
    std::wstring res;
    switch (b) {
    case 2:
    case 8:
    case 10:
    case 16: {
#ifdef _MSC_VER
        wchar_t buffer[_MAX_U64TOSTR_BASE2_COUNT];
        res = _ui64tow(v, buffer, (int)b);
        StringHelpers::to_upper(res);
#else
        res = ullToBaseGeneric(v, b, len);
#endif
    } break;
    default: {
        res = ullToBaseGeneric(v, b, len);
    } break;
    }
    if (res.size() < len) {
        std::wstring zeros = std::wstring(len - res.size(), L'0');
        zeros.append(res);
        res.clear();
        res.append(zeros);
    }
    return res;
}
//=============================================================================
template <class T>
wstringVector
dec2base(T* values, size_t len, size_t base, size_t ndigits, bool isSigned, size_t& commonSize)
{
    T maxValue = 0;
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> mat(values, 1, len);
    if (isSigned) {
        if (!(mat.array() >= (T)0.).all()) {
            Error(_W("An array of integers values, 0 <= D <= flintmax expected."));
        }
    }
    maxValue = mat.maxCoeff();
    std::wstring maxStr = ullToBase((unsigned long long)maxValue, base, ndigits);
    size_t lenMax = maxStr.size() > ndigits ? maxStr.size() : ndigits;
    baseConverted[(uint64)maxValue] = maxStr;
    wstringVector res;
    res.reserve(len);
    std::wstring s;
    s.reserve(lenMax);
    for (size_t k = 0; k < len; k++) {
        if (baseConverted.count((uint64)values[k])) {
            s = baseConverted[(uint64)values[k]];
        } else {
            s = ullToBase((unsigned long long)values[k], base, ndigits);
            if (baseConverted.size() > baseConverted.max_size() - 1000) {
                baseConverted.clear();
            }
            baseConverted[(uint64)values[k]] = s;
        }
        if (s.size() < lenMax) {
            std::wstring zeros = std::wstring(lenMax - s.size(), L'0');
            zeros.append(s);
            s.assign(zeros);
        }
        res.push_back(s);
    }
    return res;
}
//=============================================================================
ArrayOf
DecimalToBase(ArrayOf& A, ArrayOf& Base, ArrayOf& Ndigits, bool& needToOverload)
{
    needToOverload = false;
    double dbase = Base.getContentAsDoubleScalar();
    double fbase = std::floor(dbase);
    if (dbase != fbase) {
        Error(_W("The base must be an integer value between 2 and 36."));
    }
    auto ibase = static_cast<size_t>(fbase);
    if (ibase < 2 || ibase > 36) {
        Error(_W("The base must be an integer value between 2 and 36."));
    }
    double ddigits = Ndigits.getContentAsDoubleScalar();
    double fdigits = std::floor(ddigits);
    if ((ddigits != fdigits) || (ddigits < 0)) {
        Error(_W("#3 parameter: positive integer value expected."));
    }
    auto ndigits = static_cast<size_t>(fdigits);
    if (ibase != lastBase) {
        baseConverted.clear();
        lastBase = ibase;
    }
    if (A.isEmpty(false)) {
        baseConverted.clear();
        return ArrayOf::characterArrayConstructor(L"");
    }
    Dimensions dimsA = A.getDimensions();
    size_t commonSize = 0;
    wstringVector vs;
    switch (A.getDataClass()) {
    case NLS_CHAR:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        A.promoteType(NLS_UINT64);
        auto* ptr
            = static_cast<uint64*>(const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
        vs = dec2base<uint64>(
            ptr, (size_t)dimsA.getElementCount(), ibase, ndigits, false, commonSize);
    } break;
    case NLS_INT8: {
        auto* ptr
            = static_cast<int8*>(const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
        vs = dec2base<int8>(ptr, (size_t)dimsA.getElementCount(), ibase, ndigits, true, commonSize);
    } break;
    case NLS_INT16: {
        auto* ptr
            = static_cast<int16*>(const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
        vs = dec2base<int16>(
            ptr, (size_t)dimsA.getElementCount(), ibase, ndigits, true, commonSize);
    } break;
    case NLS_INT32: {
        auto* ptr
            = static_cast<int32*>(const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
        vs = dec2base<int32>(
            ptr, (size_t)dimsA.getElementCount(), ibase, ndigits, true, commonSize);
    } break;
    case NLS_INT64: {
        auto* ptr
            = static_cast<int64*>(const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
        vs = dec2base<int64>(
            ptr, (size_t)dimsA.getElementCount(), ibase, ndigits, true, commonSize);
    } break;
    case NLS_SINGLE: {
        auto* ptr
            = static_cast<single*>(const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
        if (!A.isIntegerValue() || !A.isPositive()) {
            Error(_W("An array of integers values, 0 <= D <= flintmax expected."));
        }
        vs = dec2base<single>(
            ptr, (size_t)dimsA.getElementCount(), ibase, ndigits, true, commonSize);
    } break;
    case NLS_DOUBLE: {
        auto* ptr
            = static_cast<double*>(const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
        if (!A.isIntegerValue() || !A.isPositive()) {
            Error(_W("An array of integers values, 0 <= D <= flintmax expected."));
        }
        vs = dec2base<double>(
            ptr, (size_t)dimsA.getElementCount(), ibase, ndigits, true, commonSize);
    } break;
    case NLS_SCOMPLEX: {
        Error(_W("An array of integers values, 0 <= D <= flintmax expected."));
    } break;
    case NLS_DCOMPLEX: {
        Error(_W("An array of integers values, 0 <= D <= flintmax expected."));
    } break;
    case NLS_LOGICAL: {
        Error(_W("An array of integers values, 0 <= D <= flintmax expected."));
    } break;
    default: {
        needToOverload = true;
        return {};
    } break;
    }
    return ArrayOf::characterVectorToCharacterArray(vs);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
