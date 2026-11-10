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
#include "LogarithmKernels.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Logarithm2(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return {};
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX:
        return logarithmDense<double, LogarithmBase::Two>(A, NLS_DOUBLE, NLS_DCOMPLEX, true);
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
        return logarithmDense<single, LogarithmBase::Two>(A, NLS_SINGLE, NLS_SCOMPLEX, true);
    default:
        needToOverload = true;
        break;
    }
    return {};
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
    Dimensions dimsA = A.dimensions();
    ompIndexType elementCount = dimsA.elementCount();
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        double* ptrA = (double*)A.dataPointer();
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
        single* ptrA = (single*)A.dataPointer();
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
        double* ptrA = (double*)A.dataPointer();
        std::complex<double>* ptrCplxA = reinterpret_cast<std::complex<double>*>((double*)ptrA);
        double* ptrM = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount);
        double* ptrE = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount);
        ArrayOf M = ArrayOf(NLS_DOUBLE, dimsA, ptrM);
        ArrayOf E = ArrayOf(NLS_DOUBLE, dimsA, ptrE);
        RaiseWarning(WARNING_IMAGINARY_PART_IGNORED, _W("Imaginary part ignored."));
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
        single* ptrA = (single*)A.dataPointer();
        std::complex<single>* ptrCplxA = reinterpret_cast<std::complex<single>*>((single*)ptrA);
        single* ptrM = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        single* ptrE = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        ArrayOf M = ArrayOf(NLS_SINGLE, dimsA, ptrM);
        ArrayOf E = ArrayOf(NLS_SINGLE, dimsA, ptrE);
        RaiseWarning(WARNING_IMAGINARY_PART_IGNORED, _W("Imaginary part ignored."));
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
