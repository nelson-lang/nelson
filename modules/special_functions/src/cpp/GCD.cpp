//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/integer/common_factor_ct.hpp>
#include <boost/integer/common_factor_rt.hpp>
#include "nlsBuildConfig.h"
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
template <class T>
static ArrayOf
realGCD(NelsonType destinationClass, T* ptrA, T* ptrB, const Dimensions& commonDims)
{
    indexType N = commonDims.getElementCount();
    T* ptrRes = (T*)ArrayOf::allocateArrayOf(destinationClass, N);
    ArrayOf res = ArrayOf(destinationClass, commonDims, ptrRes);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = boost::integer::gcd(ptrA[k], ptrB[k]);
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = boost::integer::gcd(ptrA[k], b);
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)N; ++k) {
        ptrRes[k] = boost::integer::gcd(a, ptrB[k]);
    }
    return res;
}
//=============================================================================
ArrayOf
GCD(const ArrayOf& A, const ArrayOf& B)
{
    if (A.getDataClass() != B.getDataClass()) {
        Error(_W("Inputs must be same type."));
    }
    ArrayOf res;
    ArrayOf _A(A);
    ArrayOf _B(B);
    bool asSparse = false;
    if (_A.isSparse()) {
        _A.makeDense();
        asSparse = true;
    }
    if (_B.isSparse()) {
        _B.makeDense();
        asSparse = true;
    }
    Dimensions dimsA = _A.getDimensions();
    Dimensions dimsB = _B.getDimensions();
    bool supportedDimensions = dimsA.equals(dimsB) || dimsA.isScalar() || dimsB.isScalar();
    if (!supportedDimensions) {
        Error(_W("Inputs must be the same size."));
    }
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        logical* ptrA = reinterpret_cast<logical*>(const_cast<void*>(_A.getDataPointer()));
        logical* ptrB = reinterpret_cast<logical*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<uint8*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<uint8*>(const_cast<void*>(_B.getDataPointer()));
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
        int8* ptrA = reinterpret_cast<int8*>(const_cast<void*>(_A.getDataPointer()));
        int8* ptrB = reinterpret_cast<int8*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<uint16*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<uint16*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<int16*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<int16*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<uint32*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<uint32*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<int32*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<int32*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<uint64*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<uint64*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<int64*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<int64*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<charType*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<charType*>(const_cast<void*>(_B.getDataPointer()));
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
        auto* ptrA = reinterpret_cast<single*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<single*>(const_cast<void*>(_B.getDataPointer()));
        bool isIntegerA = IsIntegerForm(ptrA, dimsA.getElementCount()) || _A.isEmpty();
        bool isIntegerB = IsIntegerForm(ptrB, dimsB.getElementCount()) || _B.isEmpty();
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
        auto* ptrA = reinterpret_cast<double*>(const_cast<void*>(_A.getDataPointer()));
        auto* ptrB = reinterpret_cast<double*>(const_cast<void*>(_B.getDataPointer()));
        bool isIntegerA = IsIntegerForm(ptrA, dimsA.getElementCount()) || _A.isEmpty();
        bool isIntegerB = IsIntegerForm(ptrB, dimsB.getElementCount()) || _B.isEmpty();
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
        Error(_W("Inputs type not managed."));
    } break;
    }
    if (asSparse) {
        res.makeSparse();
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
