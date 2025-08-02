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
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Logarithm2.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Logarithm2(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    Dimensions dimsA = A.getDimensions();
    ompIndexType elementCount = dimsA.getElementCount();
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        double* ptrA = (double*)A.getDataPointer();
        double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, elementCount);
        std::complex<double>* ptrCplxC = reinterpret_cast<std::complex<double>*>((double*)ptrC);
        res = ArrayOf(NLS_DCOMPLEX, dimsA, ptrC);
        bool allReal = true;
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            std::complex<double> C(ptrA[k], 0);
            ptrCplxC[k] = std::log(C) / std::log(2);
            if (!((ptrCplxC[k].imag() == 0.)
                    || (std::isnan(ptrCplxC[k].real()) && std::isnan(ptrCplxC[k].imag())))) {
                allReal = false;
            }
        }
        if (allReal) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SINGLE: {
        single* ptrA = (single*)A.getDataPointer();
        single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, elementCount);
        std::complex<single>* ptrCplxC = reinterpret_cast<std::complex<single>*>((single*)ptrC);
        res = ArrayOf(NLS_SCOMPLEX, dimsA, ptrC);
        bool allReal = true;
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            std::complex<single> C(ptrA[k], 0);
            ptrCplxC[k] = std::log(C) / std::log(2.f);
            if (!((ptrCplxC[k].imag() == 0.)
                    || (std::isnan(ptrCplxC[k].real()) && std::isnan(ptrCplxC[k].imag())))) {
                allReal = false;
            }
        }
        if (allReal) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptrA = (double*)A.getDataPointer();
        std::complex<double>* ptrCplxA = reinterpret_cast<std::complex<double>*>((double*)ptrA);
        double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, elementCount);
        std::complex<double>* ptrCplxC = reinterpret_cast<std::complex<double>*>((double*)ptrC);
        res = ArrayOf(NLS_DCOMPLEX, dimsA, ptrC);
        bool allReal = true;
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            ptrCplxC[k] = std::log(ptrCplxA[k]) / std::log(2);
            if (!((ptrCplxC[k].imag() == 0.)
                    || (std::isnan(ptrCplxC[k].real()) && std::isnan(ptrCplxC[k].imag())))) {
                allReal = false;
            }
        }
        if (allReal) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        single* ptrA = (single*)A.getDataPointer();
        std::complex<single>* ptrCplxA = reinterpret_cast<std::complex<single>*>((single*)ptrA);
        single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, elementCount);
        std::complex<single>* ptrCplxC = reinterpret_cast<std::complex<single>*>((single*)ptrC);
        res = ArrayOf(NLS_SCOMPLEX, dimsA, ptrC);
        bool allReal = true;
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            ptrCplxC[k] = std::log(ptrCplxA[k]) / std::log(2.f);
            if (!((ptrCplxC[k].imag() == 0.)
                    || (std::isnan(ptrCplxC[k].real()) && std::isnan(ptrCplxC[k].imag())))) {
                allReal = false;
            }
        }
        if (allReal) {
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
ArrayOfVector
Frexp(const ArrayOf& A, bool& needToOverload)
{
    ArrayOfVector res;
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    Dimensions dimsA = A.getDimensions();
    ompIndexType elementCount = dimsA.getElementCount();
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        double* ptrA = (double*)A.getDataPointer();
        double* ptrM = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount);
        double* ptrE = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount);
        ArrayOf M = ArrayOf(NLS_DOUBLE, dimsA, ptrM);
        ArrayOf E = ArrayOf(NLS_DOUBLE, dimsA, ptrE);
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            int e;
            double m = frexp(ptrA[k], &e);
            ptrM[k] = m;
            if (std::isnan(m) || std::isinf(m)) {
                ptrE[k] = (double)0;
            } else {
                ptrE[k] = (double)e;
            }
        }
        res.push_back(M);
        res.push_back(E);
    } break;
    case NLS_SINGLE: {
        single* ptrA = (single*)A.getDataPointer();
        single* ptrM = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        single* ptrE = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        ArrayOf M = ArrayOf(NLS_SINGLE, dimsA, ptrM);
        ArrayOf E = ArrayOf(NLS_SINGLE, dimsA, ptrE);
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            int e;
            single m = frexpf(ptrA[k], &e);
            ptrM[k] = m;
            if (std::isnan(m) || std::isinf(m)) {
                ptrE[k] = (single)0.;
            } else {
                ptrE[k] = (single)e;
            }
        }
        res.push_back(M);
        res.push_back(E);
    } break;
    case NLS_DCOMPLEX: {
        double* ptrA = (double*)A.getDataPointer();
        std::complex<double>* ptrCplxA = reinterpret_cast<std::complex<double>*>((double*)ptrA);
        double* ptrM = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount);
        double* ptrE = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount);
        ArrayOf M = ArrayOf(NLS_DOUBLE, dimsA, ptrM);
        ArrayOf E = ArrayOf(NLS_DOUBLE, dimsA, ptrE);
        Warning(WARNING_IMAGINARY_PART_IGNORED, _W("Imaginary part ignored."));
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            int e;
            double m = frexp(ptrCplxA[k].real(), &e);
            ptrM[k] = m;
            if (std::isnan(m) || std::isinf(m)) {
                ptrE[k] = (double)0;
            } else {
                ptrE[k] = (double)e;
            }
        }
        res.push_back(M);
        res.push_back(E);
    } break;
    case NLS_SCOMPLEX: {
        single* ptrA = (single*)A.getDataPointer();
        std::complex<single>* ptrCplxA = reinterpret_cast<std::complex<single>*>((single*)ptrA);
        single* ptrM = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        single* ptrE = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        ArrayOf M = ArrayOf(NLS_SINGLE, dimsA, ptrM);
        ArrayOf E = ArrayOf(NLS_SINGLE, dimsA, ptrE);
        Warning(WARNING_IMAGINARY_PART_IGNORED, _W("Imaginary part ignored."));
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; ++k) {
            int e;
            single m = frexp(ptrCplxA[k].real(), &e);
            ptrM[k] = m;
            if (std::isnan(m) || std::isinf(m)) {
                ptrE[k] = (single)0;
            } else {
                ptrE[k] = (single)e;
            }
        }
        res.push_back(M);
        res.push_back(E);
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
