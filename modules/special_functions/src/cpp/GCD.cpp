//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <numeric>
#include <type_traits>
#include <cstdint>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GCD.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static double
basic_gcd(double a, double b)
{
    double _a = fabs(a);
    double _b = fabs(b);

    while (_b != 0) {
        double _t = fmod(_a, _b);
        _a = _b;
        _b = _t;
    }
    return _a;
}
//=============================================================================
// wrapper to compute gcd for various integer types using std::gcd
// signed version
template <class T, typename std::enable_if_t<std::is_signed<T>::value, int> = 0>
inline T
gcd_wrapper(T a, T b) noexcept
{
    long long aa = static_cast<long long>(a);
    long long bb = static_cast<long long>(b);
    if (aa < 0) {
        aa = -aa;
    }
    if (bb < 0) {
        bb = -bb;
    }
    long long g = std::gcd(aa, bb);
    return static_cast<T>(g);
}
// unsigned version
template <class T, typename std::enable_if_t<!std::is_signed<T>::value, int> = 0>
inline T
gcd_wrapper(T a, T b) noexcept
{
    unsigned long long aa = static_cast<unsigned long long>(a);
    unsigned long long bb = static_cast<unsigned long long>(b);
    unsigned long long g = std::gcd(aa, bb);
    return static_cast<T>(g);
}
//=============================================================================
template <class T>
static ArrayOf
realGCD(NelsonType destinationClass, T* ptrA, T* ptrB, const Dimensions& commonDims)
{
    indexType N = commonDims.getElementCount();
    T* ptrRes = (T*)ArrayOf::allocateArrayOf(destinationClass, N);
    ArrayOf res = ArrayOf(destinationClass, commonDims, ptrRes);
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = (T)basic_gcd((double)ptrA[k], (double)ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
realGCD(NelsonType destinationClass, T* ptrA, T b, const Dimensions& commonDims)
{
    indexType N = commonDims.getElementCount();
    T* ptrRes = (T*)ArrayOf::allocateArrayOf(destinationClass, N);
    ArrayOf res = ArrayOf(destinationClass, commonDims, ptrRes);
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = (T)basic_gcd((double)ptrA[k], (double)b);
    }
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
realGCD(NelsonType destinationClass, T a, T* ptrB, const Dimensions& commonDims)
{
    indexType N = commonDims.getElementCount();
    T* ptrRes = (T*)ArrayOf::allocateArrayOf(destinationClass, N);
    ArrayOf res = ArrayOf(destinationClass, commonDims, ptrRes);
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = (T)basic_gcd((double)a, (double)ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
integerGCD(NelsonType destinationClass, T* ptrA, T* ptrB, const Dimensions& commonDims)
{
    indexType N = commonDims.getElementCount();
    T* ptrRes = (T*)ArrayOf::allocateArrayOf(destinationClass, N);
    ArrayOf res = ArrayOf(destinationClass, commonDims, ptrRes);
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = gcd_wrapper(ptrA[k], ptrB[k]);
    }
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
integerGCD(NelsonType destinationClass, T* ptrA, T b, const Dimensions& commonDims)
{
    indexType N = commonDims.getElementCount();
    T* ptrRes = (T*)ArrayOf::allocateArrayOf(destinationClass, N);
    ArrayOf res = ArrayOf(destinationClass, commonDims, ptrRes);
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = gcd_wrapper(ptrA[k], b);
    }
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
integerGCD(NelsonType destinationClass, T a, T* ptrB, const Dimensions& commonDims)
{
    indexType N = commonDims.getElementCount();
    T* ptrRes = (T*)ArrayOf::allocateArrayOf(destinationClass, N);
    ArrayOf res = ArrayOf(destinationClass, commonDims, ptrRes);
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = gcd_wrapper(a, ptrB[k]);
    }
    return res;
}
//=============================================================================
ArrayOf
GCD(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return res;
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    bool supportedDimensions = dimsA.equals(dimsB) || dimsA.isScalar() || dimsB.isScalar();
    if (!supportedDimensions) {
        Error(_W("Inputs must be the same size."));
    }
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        auto* ptrA = (logical*)A.getDataPointer();
        auto* ptrB = (logical*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<logical>(NLS_LOGICAL, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<logical>(NLS_LOGICAL, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<logical>(NLS_LOGICAL, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_UINT8: {
        auto* ptrA = (uint8*)A.getDataPointer();
        auto* ptrB = (uint8*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<uint8>(NLS_UINT8, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<uint8>(NLS_UINT8, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<uint8>(NLS_UINT8, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_INT8: {
        int8* ptrA = (int8*)A.getDataPointer();
        int8* ptrB = (int8*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<int8>(NLS_INT8, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<int8>(NLS_INT8, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<int8>(NLS_INT8, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_UINT16: {
        auto* ptrA = (uint16*)A.getDataPointer();
        auto* ptrB = (uint16*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<uint16>(NLS_UINT16, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<uint16>(NLS_UINT16, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<uint16>(NLS_UINT16, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_INT16: {
        auto* ptrA = (int16*)A.getDataPointer();
        auto* ptrB = (int16*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<int16>(NLS_INT16, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<int16>(NLS_INT16, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<int16>(NLS_INT16, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_UINT32: {
        auto* ptrA = (uint32*)A.getDataPointer();
        auto* ptrB = (uint32*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<uint32>(NLS_UINT32, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<uint32>(NLS_UINT32, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<uint32>(NLS_UINT32, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_INT32: {
        auto* ptrA = (int32*)A.getDataPointer();
        auto* ptrB = (int32*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<int32>(NLS_INT32, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<int32>(NLS_INT32, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<int32>(NLS_INT32, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_UINT64: {
        auto* ptrA = (uint64*)A.getDataPointer();
        auto* ptrB = (uint64*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<uint64>(NLS_UINT64, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<uint64>(NLS_UINT64, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<uint64>(NLS_UINT64, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_INT64: {
        auto* ptrA = (int64*)A.getDataPointer();
        auto* ptrB = (int64*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<int64>(NLS_INT64, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<int64>(NLS_INT64, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<int64>(NLS_INT64, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_CHAR: {
        auto* ptrA = (charType*)A.getDataPointer();
        auto* ptrB = (charType*)B.getDataPointer();
        if (dimsA.equals(dimsB)) {
            res = integerGCD<charType>(NLS_CHAR, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = integerGCD<charType>(NLS_CHAR, ptrA[0], ptrB, dimsB);
            } else {
                res = integerGCD<charType>(NLS_CHAR, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_SINGLE: {
        auto* ptrA = (single*)A.getDataPointer();
        auto* ptrB = (single*)B.getDataPointer();
        bool isIntegerA = IsIntegerForm(ptrA, dimsA.getElementCount()) || A.isEmpty();
        bool isIntegerB = IsIntegerForm(ptrB, dimsB.getElementCount()) || B.isEmpty();
        if (!isIntegerA || !isIntegerB) {
            Error(_W("Inputs must be real integers."));
        }
        if (dimsA.equals(dimsB)) {
            res = realGCD<single>(NLS_SINGLE, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = realGCD<single>(NLS_SINGLE, ptrA[0], ptrB, dimsB);
            } else {
                res = realGCD<single>(NLS_SINGLE, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_DOUBLE: {
        auto* ptrA = (double*)A.getDataPointer();
        auto* ptrB = (double*)B.getDataPointer();
        bool isIntegerA = IsIntegerForm(ptrA, dimsA.getElementCount()) || A.isEmpty();
        bool isIntegerB = IsIntegerForm(ptrB, dimsB.getElementCount()) || B.isEmpty();
        if (!isIntegerA || !isIntegerB) {
            Error(_W("Inputs must be real integers."));
        }
        if (dimsA.equals(dimsB)) {
            res = realGCD<double>(NLS_DOUBLE, ptrA, ptrB, dimsA);
        } else {
            if (A.isScalar()) {
                res = realGCD<double>(NLS_DOUBLE, ptrA[0], ptrB, dimsB);
            } else {
                res = realGCD<double>(NLS_DOUBLE, ptrA, ptrB[0], dimsA);
            }
        }
    } break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX: {
        Error(_W("Inputs must be real integers."));
    } break;
    default: {
        needToOverload = true;
        return res;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
