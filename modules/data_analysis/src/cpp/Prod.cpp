//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Prod.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
RealProdT(const T* sp, T* dp, indexType elementCount, bool withnan)
{
    T accum = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(* : accum)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)elementCount; k++) {
        T val = sp[k];
        if (!withnan) {
            if (!std::isnan(val)) {
                accum *= val;
            }
        } else {
            accum *= val;
        }
    }
    dp[0] = accum;
}
//=============================================================================
template <class T>
void
RealProdT(
    const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize, bool withnan)
{
    ompIndexType totalElements = planes * planesize;
    OMP_PARALLEL_FOR_LOOP(totalElements)
    for (ompIndexType idx = 0; idx < totalElements; idx++) {
        // Convert linear index back to i, j components
        indexType i = idx / planesize;
        indexType j = idx % planesize;

        T accum = 1;
        for (indexType k = 0; k < linesize; k++) {
            T val = sp[i * planesize * linesize + j + k * planesize];
            if (!withnan) {
                if (!std::isnan(val)) {
                    accum *= val;
                }
            } else {
                accum *= val;
            }
        }
        dp[i * planesize + j] = accum;
    }
}
//=============================================================================
template <class T>
void
ComplexProdT(const T* sp, T* dp, indexType elementCount, bool withnan)
{
    // For complex numbers, use a sequential approach to avoid OpenMP reduction issues
    T accum_r = 1;
    T accum_i = 0;

    for (indexType k = 0; k < elementCount; k++) {
        T vr = sp[2 * k];
        T vi = sp[(2 * k) + 1];
        if (!withnan) {
            if (!std::isnan(vr) && !std::isnan(vi)) {
                T t1 = accum_r * vr - accum_i * vi;
                T t2 = accum_r * vi + accum_i * vr;
                accum_r = t1;
                accum_i = t2;
            }
        } else {
            T t1 = accum_r * vr - accum_i * vi;
            T t2 = accum_r * vi + accum_i * vr;
            accum_r = t1;
            accum_i = t2;
        }
    }
    dp[0] = accum_r;
    dp[1] = accum_i;
}
//=============================================================================
template <class T>
void
ComplexProdT(
    const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize, bool withnan)
{
    ompIndexType totalElements = planes * planesize;
    OMP_PARALLEL_FOR_LOOP(totalElements)
    for (ompIndexType idx = 0; idx < totalElements; idx++) {
        // Convert linear index back to i, j components
        indexType i = idx / planesize;
        indexType j = idx % planesize;

        T accum_r = 1;
        T accum_i = 0;

        for (indexType k = 0; k < linesize; k++) {
            T vr = sp[2 * (i * planesize * linesize + j + k * planesize)];
            T vi = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];

            if (!withnan) {
                if (!std::isnan(vr) && !std::isnan(vi)) {
                    T t1 = accum_r * vr - accum_i * vi;
                    T t2 = accum_r * vi + accum_i * vr;
                    accum_r = t1;
                    accum_i = t2;
                }
            } else {
                T t1 = accum_r * vr - accum_i * vi;
                T t2 = accum_r * vi + accum_i * vr;
                accum_r = t1;
                accum_i = t2;
            }
        }

        dp[2 * (i * planesize + j)] = accum_r;
        dp[2 * (i * planesize + j) + 1] = accum_i;
    }
}
//=============================================================================
ArrayOf
Prod(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan)
{
    ArrayOf res;
    NelsonType classA = A.getDataClass();
    if (classA > NLS_LOGICAL || A.isSparse()) {
        std::wstring classname;
        ClassName(A, classname);
        std::wstring msg = _W("function") + L" " + classname + L"_prod" + L" " + _W("undefined.");
        Error(msg);
    }
    if (A.isEmpty(true) && A.is2D()) {
        res = ArrayOf::doubleConstructor(1);
    } else if (A.isScalar()) {
        res = A;
    } else {
        Dimensions dimsA = A.getDimensions();
        indexType workDim;
        if (d == 0) {
            indexType l = 0;
            while (dimsA.getAt(l) == 1) {
                l++;
            }
            workDim = l;
        } else {
            workDim = d - 1;
        }
        Dimensions dimsRes = dimsA;
        dimsRes.setDimensionLength(workDim, 1);
        dimsRes.simplify();
        indexType planecount;
        indexType planesize = 1;
        indexType linesize = dimsA[workDim];
        for (indexType l = 0; l < workDim; l++) {
            planesize *= dimsA[l];
        }
        planecount = 1;
        for (indexType l = workDim + 1; l < dimsA.getLength(); l++) {
            planecount *= dimsA[l];
        }
        switch (classA) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64: {
            A.promoteType(NLS_DOUBLE);
            double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(
                NLS_DOUBLE, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                RealProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), false);
            } else {
                RealProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                    planesize, linesize, false);
            }
            res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
            if (classA != NLS_LOGICAL) {
                res.promoteType(classA);
            }
        } break;
        case NLS_SINGLE: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                RealProdT<single>(static_cast<const single*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), withnan);
            } else {
                RealProdT<single>(static_cast<const single*>(A.getDataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DOUBLE: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                RealProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), withnan);
            } else {
                RealProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_SCOMPLEX: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                ComplexProdT<single>(static_cast<const single*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), withnan);
            } else {
                ComplexProdT<single>(static_cast<const single*>(A.getDataPointer()), ptr,
                    planecount, planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DCOMPLEX: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                ComplexProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), withnan);
            } else {
                ComplexProdT<double>(static_cast<const double*>(A.getDataPointer()), ptr,
                    planecount, planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        default: {
            std::wstring classname;
            ClassName(A, classname);
            std::wstring msg
                = _W("function") + L" " + classname + L"_prod" + L" " + _W("undefined.");
            Error(msg);
        } break;
        }
    }
    NelsonType outputClass = classA;
    if (strtype == L"default") {
        if (classA == NLS_DOUBLE || classA == NLS_SINGLE || classA == NLS_DCOMPLEX
            || classA == NLS_SCOMPLEX) {
            outputClass = classA;
        } else {
            outputClass = NLS_DOUBLE;
        }
    } else if (strtype == L"native") {
        outputClass = classA;
    } else if (strtype == L"double") {
        if (res.getDataClass() == NLS_SCOMPLEX || res.getDataClass() == NLS_DCOMPLEX) {
            outputClass = NLS_DCOMPLEX;
        } else {
            outputClass = NLS_DOUBLE;
        }
    }
    res.promoteType(outputClass);
    switch (outputClass) {
    case NLS_SCOMPLEX: {
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    default: {
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
