//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <complex>
#include "Logarithm10.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
log10Complex(NelsonType destinationClass, T* values, bool allReal, Dimensions& dims)
{
    ompIndexType elementCount = dims.getElementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(destinationClass, elementCount);
    std::complex<T>* outZ = reinterpret_cast<std::complex<T>*>(ptrOut);
    if (allReal) {
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            if (values[k] >= 0) {
                outZ[k].real(std::log10(values[k]));
                outZ[k].imag(0);
            } else {
                std::complex<T> current(values[k], (T)0);
                outZ[k] = std::log10(current);
                if (std::isnan(outZ[k].imag())) {
                    outZ[k].imag((T)0);
                }
            }
        }
    } else {
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(values);
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            outZ[k] = std::log10(Az[k]);
        }
    }
    return ArrayOf(destinationClass, dims, ptrOut);
}
//=============================================================================
ArrayOf
Logarithm10(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    needToOverload = false;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        Dimensions dimsA = A.getDimensions();
        res = log10Complex<double>(NLS_DCOMPLEX, (double*)A.getDataPointer(), true, dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        Dimensions dimsA = A.getDimensions();
        res = log10Complex<double>(NLS_DCOMPLEX, (double*)A.getDataPointer(), false, dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SINGLE: {
        Dimensions dimsA = A.getDimensions();
        res = log10Complex<single>(NLS_SCOMPLEX, (single*)A.getDataPointer(), true, dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        Dimensions dimsA = A.getDimensions();
        res = log10Complex<single>(NLS_SCOMPLEX, (single*)res.getDataPointer(), false, dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
