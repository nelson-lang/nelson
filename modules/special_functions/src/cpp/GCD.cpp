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
#include <boost/integer/common_factor_ct.hpp>
#include <boost/integer/common_factor_rt.hpp>
#include "nlsConfig.h"
#include "GCD.hpp"
#include "Error.hpp"
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
realGCD(Class destinationClass, T* ptrA, T* ptrB, const Dimensions commonDims)
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
realGCD(Class destinationClass, T* ptrA, T b, const Dimensions commonDims)
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
realGCD(Class destinationClass, T a, T* ptrB, const Dimensions commonDims)
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
integerGCD(Class destinationClass, T* ptrA, T* ptrB, const Dimensions commonDims)
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
integerGCD(Class destinationClass, T* ptrA, T b, const Dimensions commonDims)
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
integerGCD(Class destinationClass, T a, T* ptrB, const Dimensions commonDims)
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
    if (A.getDataClass() == B.getDataClass()) {
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
    } else {
        Class commonClass;
        if (A.isIntegerType()) {
            bool isCompatible = (B.getDataClass() == NLS_DOUBLE) && B.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            commonClass = A.getDataClass();

        } else if (B.isIntegerType()) {
            bool isCompatible = (A.getDataClass() == NLS_DOUBLE) && A.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            commonClass = B.getDataClass();
        } else {
            bool asComplex = A.isComplex() || B.isComplex();
            bool asSingle = A.isSingleClass() || B.isSingleClass();
            if (asComplex) {
                if (asSingle) {
                    commonClass = NLS_SCOMPLEX;
                } else {
                    commonClass = NLS_DCOMPLEX;
                }
            } else {
                if (asSingle) {
                    commonClass = NLS_SINGLE;
                } else {
                    commonClass = NLS_DOUBLE;
                }
            }
        }
        ArrayOf AA(A);
        ArrayOf BB(B);
        AA.promoteType(commonClass);
        BB.promoteType(commonClass);
        res = GCD(AA, BB, needToOverload);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
