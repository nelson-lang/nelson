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
#include "ToDouble.hpp"
#include "StringToDoubleComplex.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
ToDouble(const ArrayOf& A)
{
    double* pDouble
        = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, A.elementCount(), stringVector(), false);
    ArrayOf r = ArrayOf(NLS_DOUBLE, A.dimensions(), pDouble, A.isSparse());
    T* ptrA = (T*)A.dataPointer();
    ompIndexType N = (ompIndexType)A.elementCount();
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < N; ++i) {
        pDouble[i] = (double)ptrA[i];
    }
    return r;
}
//=============================================================================
ArrayOf
ToDouble(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isComplex() && A.isEmpty()) {
        // an empty complex array converts to an empty real array
        ArrayOf r = decomplexify(A);
        r.promoteType(NLS_DOUBLE);
        return r;
    }
    if (A.isSparse()) {
        ArrayOf r(A);
        r.promoteType(A.isComplex() ? NLS_DCOMPLEX : NLS_DOUBLE);
        return r;
    }
    switch (A.dataClass()) {
    case NLS_MISSING_ARRAY: {
        Dimensions dimsA = A.dimensions();
        indexType nbElements = dimsA.elementCount();
        double* ptr
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nbElements, stringVector(), false);
        ArrayOf R = ArrayOf(NLS_DOUBLE, dimsA, ptr);
        for (indexType k = 0; k < nbElements; k = k + 1) {
            ptr[k] = std::nan("");
        }
        return R;
    } break;
    case NLS_GO_HANDLE:
    case NLS_HANDLE: {
        needToOverload = true;
        return {};
    } break;
    case NLS_STRING_ARRAY: {
        Dimensions dimsA = A.dimensions();
        indexType nbElements = dimsA.elementCount();
        double* ptrComplex
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbElements, stringVector(), false);
        auto* strElements = (ArrayOf*)A.dataPointer();
        indexType q = 0;
        for (indexType k = 0; k < nbElements; k = k + 1) {
            ArrayOf element = strElements[k];
            if (element.dataClass() == NLS_CHAR) {
                std::wstring str = element.asWideString();
                bool wasConverted = false;
                doublecomplex asComplex = stringToDoubleComplex(str, wasConverted);
                if (wasConverted) {
                    ptrComplex[q] = asComplex.real();
                    ptrComplex[q + 1] = asComplex.imag();
                } else {
                    ptrComplex[q] = std::nan("");
                    ptrComplex[q + 1] = 0;
                }
            } else {
                ptrComplex[q] = std::nan("");
                ptrComplex[q + 1] = 0;
            }
            q = q + 2;
        }
        ArrayOf R = ArrayOf(NLS_DCOMPLEX, dimsA, ptrComplex);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_CELL_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY: {
        needToOverload = true;
        return {};
    } break;
    case NLS_LOGICAL: {
        return ToDouble<logical>(A);
    } break;
    case NLS_UINT8: {
        return ToDouble<uint8>(A);
    } break;
    case NLS_INT8: {
        return ToDouble<int8>(A);
    } break;
    case NLS_UINT16: {
        return ToDouble<uint16>(A);
    } break;
    case NLS_INT16: {
        return ToDouble<int16>(A);
    } break;
    case NLS_UINT32: {
        return ToDouble<uint32>(A);
    } break;
    case NLS_INT32: {
        return ToDouble<int32>(A);
    } break;
    case NLS_UINT64: {
        return ToDouble<uint64>(A);
    } break;
    case NLS_INT64: {
        return ToDouble<int64>(A);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        ArrayOf r(A);
        r.ensureSingleOwner();
        return r;
    } break;
    case NLS_SCOMPLEX: {
        ompIndexType nbElements = A.elementCount();
        double* pDouble
            = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbElements, stringVector(), false);
        ArrayOf r = ArrayOf(NLS_DCOMPLEX, A.dimensions(), pDouble, false);
        auto* pSingle
            = static_cast<single*>(const_cast<void*>(static_cast<const void*>(A.dataPointer())));
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < (ompIndexType)(nbElements * 2); k++) {
            pDouble[k] = static_cast<double>(pSingle[k]);
        }
        return r;
    } break;
    case NLS_SINGLE: {
        return ToDouble<single>(A);
    } break;
    case NLS_CHAR: {
        return ToDouble<charType>(A);
    } break;
    default: {
        needToOverload = true;
        return {};
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
