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
#include <cmath>
#include <complex>
#include "Logarithm1p.hpp"
#include "complex_abs.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
log1pComplex(NelsonType destinationClass, T* values, bool allReal, const Dimensions& dims)
{
    ompIndexType nbElements = dims.elementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(destinationClass, nbElements);
    std::complex<T>* outZ = reinterpret_cast<std::complex<T>*>(ptrOut);
    if (allReal) {
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < nbElements; ++k) {
            T v = values[k];
            if (v >= -1 || std::isnan(v)) {
                outZ[k].real(std::log1p(v));
                outZ[k].imag(0);
            } else {
                outZ[k].real(log(complex_abs<T>(v + 1, 0)));
                outZ[k].imag(atan2((T)0, v + 1));
            }
        }
    } else {
        std::complex<T>* inZ = reinterpret_cast<std::complex<T>*>(values);
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < nbElements; ++k) {
            outZ[k].real(log(complex_abs<T>(inZ[k].real() + 1, inZ[k].imag())));
            outZ[k].imag(atan2(inZ[k].imag(), inZ[k].real() + 1));
        }
    }
    return ArrayOf(destinationClass, dims, ptrOut);
}
//=============================================================================
ArrayOf
Logarithm1p(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        res = log1pComplex<double>(NLS_DCOMPLEX, (double*)A.dataPointer(), true, A.dimensions());
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        res = log1pComplex<double>(NLS_DCOMPLEX, (double*)A.dataPointer(), false, A.dimensions());
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SINGLE: {
        res = log1pComplex<single>(NLS_SCOMPLEX, (single*)A.dataPointer(), true, A.dimensions());
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        res = log1pComplex<single>(NLS_SCOMPLEX, (single*)A.dataPointer(), false, A.dimensions());
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
