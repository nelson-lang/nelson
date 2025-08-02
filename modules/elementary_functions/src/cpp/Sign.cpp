//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Sign.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
SignUnsignedInteger(NelsonType outputClass, const T* ptrA, const Dimensions& dimsA)
{
    ompIndexType nbElements = dimsA.getElementCount();
    T* ptrB = (T*)ArrayOf::allocateArrayOf(outputClass, nbElements);
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        if (ptrA[k] == (T)0) {
            ptrB[k] = (T)(0);
        } else {
            ptrB[k] = (T)(1);
        }
    }
    return ArrayOf(outputClass, dimsA, ptrB);
}
//=============================================================================
template <class T>
ArrayOf
SignSignedInteger(NelsonType outputClass, const T* ptrA, const Dimensions& dimsA)
{
    ompIndexType nbElements = dimsA.getElementCount();
    T* ptrB = (T*)ArrayOf::allocateArrayOf(outputClass, nbElements);
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        if (ptrA[k] < (T)0) {
            ptrB[k] = (T)(-1);
        } else if (ptrA[k] == (T)0) {
            ptrB[k] = (T)(0);
        } else {
            ptrB[k] = (T)(1);
        }
    }
    return ArrayOf(outputClass, dimsA, ptrB);
}
//=============================================================================

template <class T>
ArrayOf
SignReal(NelsonType outputClass, const T* ptrA, const Dimensions& dimsA)
{
    ompIndexType nbElements = dimsA.getElementCount();
    T* ptrB = (T*)ArrayOf::allocateArrayOf(outputClass, nbElements);
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        if (std::isnan(ptrA[k])) {
            ptrB[k] = (T)std::nan("NaN");
        } else if (ptrA[k] < (T)0) {
            ptrB[k] = (T)(-1);
        } else if (ptrA[k] == (T)0) {
            ptrB[k] = (T)(0);
        } else {
            ptrB[k] = (T)(1);
        }
    }
    return ArrayOf(outputClass, dimsA, ptrB);
}
//=============================================================================
template <class T>
ArrayOf
SignComplex(NelsonType outputClass, const T* ptrA, const Dimensions& dimsA)
{
    ompIndexType nbElements = dimsA.getElementCount();
    T* ptrB = (T*)ArrayOf::allocateArrayOf(outputClass, nbElements);
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements * 2; k = k + 2) {
        std::complex<T> x(ptrA[k], ptrA[k + 1]);
        std::complex<T> r = x / std::abs(x);
        ptrB[k] = r.real();
        ptrB[k + 1] = r.imag();
    }

    return decomplexify(ArrayOf(outputClass, dimsA, ptrB));
}
//=============================================================================
ArrayOf
Sign(const ArrayOf& A, bool& needToOverload)
{
    if (A.isSparse()) {
        needToOverload = true;
        return {};
    }
    needToOverload = false;
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        Dimensions dimsA = A.getDimensions();
        return SignUnsignedInteger<logical>(NLS_LOGICAL, (logical*)A.getDataPointer(), dimsA);
    } break;
    case NLS_UINT8: {
        Dimensions dimsA = A.getDimensions();
        return SignUnsignedInteger<uint8>(NLS_UINT8, (uint8*)A.getDataPointer(), dimsA);
    } break;
    case NLS_INT8: {
        Dimensions dimsA = A.getDimensions();
        return SignSignedInteger<int8>(NLS_INT8, (int8*)A.getDataPointer(), dimsA);
    } break;
    case NLS_UINT16: {
        Dimensions dimsA = A.getDimensions();
        return SignUnsignedInteger<uint16>(NLS_UINT16, (uint16*)A.getDataPointer(), dimsA);
    } break;
    case NLS_INT16: {
        Dimensions dimsA = A.getDimensions();
        return SignSignedInteger<int16>(NLS_INT16, (int16*)A.getDataPointer(), dimsA);
    } break;
    case NLS_UINT32: {
        Dimensions dimsA = A.getDimensions();
        return SignUnsignedInteger<uint32>(NLS_UINT32, (uint32*)A.getDataPointer(), dimsA);
    } break;
    case NLS_INT32: {
        Dimensions dimsA = A.getDimensions();
        return SignSignedInteger<int32>(NLS_INT32, (int32*)A.getDataPointer(), dimsA);
    } break;
    case NLS_UINT64: {
        Dimensions dimsA = A.getDimensions();
        return SignUnsignedInteger<uint64>(NLS_UINT64, (uint64*)A.getDataPointer(), dimsA);
    } break;
    case NLS_INT64: {
        Dimensions dimsA = A.getDimensions();
        return SignSignedInteger<int64>(NLS_INT64, (int64*)A.getDataPointer(), dimsA);
    } break;
    case NLS_SINGLE: {
        Dimensions dimsA = A.getDimensions();
        return SignReal<single>(NLS_SINGLE, (single*)A.getDataPointer(), dimsA);
    } break;
    case NLS_DOUBLE: {
        Dimensions dimsA = A.getDimensions();
        return SignReal<double>(NLS_DOUBLE, (double*)A.getDataPointer(), dimsA);
    } break;
    case NLS_SCOMPLEX: {
        Dimensions dimsA = A.getDimensions();
        return SignComplex<single>(NLS_SCOMPLEX, (single*)A.getDataPointer(), dimsA);
    } break;
    case NLS_DCOMPLEX: {
        Dimensions dimsA = A.getDimensions();
        return SignComplex<double>(NLS_DCOMPLEX, (double*)A.getDataPointer(), dimsA);
    } break;
    case NLS_CHAR: {
        Dimensions dimsA = A.getDimensions();
        return SignUnsignedInteger<charType>(NLS_CHAR, (charType*)A.getDataPointer(), dimsA);
    } break;
    case NLS_UNKNOWN:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
}
//=============================================================================
