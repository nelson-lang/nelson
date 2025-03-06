//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
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
    ompIndexType nbElements = dims.getElementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(destinationClass, nbElements);
    std::complex<T>* outZ = reinterpret_cast<std::complex<T>*>(ptrOut);
    if (allReal) {
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < nbElements; ++k) {
            outZ[k].real(log(complex_abs<T>(values[k] + 1, 0)));
            T imag = atan2((T)0, values[k] + 1);
            if (std::isnan(outZ[k].real()) && std::isnan(imag)) {
                outZ[k].imag(0);
            } else {
                outZ[k].imag(imag);
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
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        res = log1pComplex<double>(
            NLS_DCOMPLEX, (double*)A.getDataPointer(), true, A.getDimensions());
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        res = log1pComplex<double>(
            NLS_DCOMPLEX, (double*)A.getDataPointer(), false, A.getDimensions());
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SINGLE: {
        res = log1pComplex<single>(
            NLS_SCOMPLEX, (single*)A.getDataPointer(), true, A.getDimensions());
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        res = log1pComplex<single>(
            NLS_SCOMPLEX, (single*)A.getDataPointer(), false, A.getDimensions());
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
