//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LinearInterpolation1D.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
int
interv(const T* xt, int lxt, T x, int* left, int* mflag);
//=============================================================================
template <class T>
void
LinearInterpolationReal(
    const T* x1, const T* y1, indexType x1count, const T* xi, indexType xicount, T* yi);
//=============================================================================
template <class T>
void
LinearInterpolationComplex(
    const T* x1, const T* y1, indexType x1count, const T* xi, indexType xicount, T* yi);
//=============================================================================
ArrayOf
LinearInterpolation1D(const ArrayOf& V, const ArrayOf& XQ)
{
    ArrayOf X;
    indexType len = 0;
    NelsonType destinationClass = NLS_DOUBLE;
    switch (V.getDataClass()) {
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        destinationClass = NLS_DOUBLE;
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        destinationClass = NLS_SINGLE;
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    if (V.isVector()) {
        len = V.getElementCount();
    } else {
        len = V.getRows();
    }
    Dimensions dims(1, len);
    switch (destinationClass) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(destinationClass, len);
        X = ArrayOf(destinationClass, dims, ptr);
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
            ptr[k] = (double)(k + 1);
        }
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(destinationClass, len);
        X = ArrayOf(destinationClass, dims, ptr);
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
            ptr[k] = (single)(k + 1);
        }
    } break;
    default: {
    } break;
    }
    return LinearInterpolation1D(X, V, XQ);
}
//=============================================================================
ArrayOf
LinearInterpolation1D(const ArrayOf& X, const ArrayOf& V, const ArrayOf& XQ)
{
    ArrayOf res;
    bool isSupportedType = X.isSingleType() || X.isDoubleType();
    if (!isSupportedType) {
        Error(_W("'double' or 'single' type for all input arguments expected."));
    }
    bool isSparse = X.isSparse() || V.isSparse() || XQ.isSparse();
    if (isSparse) {
        Error(_W("dense type for all input arguments expected."));
    }
    if (X.isComplex() || XQ.isComplex()) {
        Error(_W("Inputs arguments must be real."));
    }
    if (X.getElementCount() != V.getElementCount()) {
        Error(_W("X and V must be of the same length."));
    }
    ArrayOf x1(X);
    ArrayOf y1(V);
    ArrayOf xi(XQ);
    if (X.getDataClass() == NLS_SINGLE || V.getDataClass() == NLS_SINGLE
        || XQ.getDataClass() == NLS_SINGLE) {
        x1.promoteType(NLS_SINGLE);
        if (V.isComplex()) {
            y1.promoteType(NLS_SCOMPLEX);
        } else {
            y1.promoteType(NLS_SINGLE);
        }
        xi.promoteType(NLS_SINGLE);
    }
    switch (y1.getDataClass()) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, xi.getElementCount());
        LinearInterpolationReal<double>((const double*)x1.getDataPointer(),
            (const double*)y1.getDataPointer(), x1.getElementCount(),
            (const double*)xi.getDataPointer(), xi.getElementCount(), ptr);
        res = ArrayOf(NLS_DOUBLE, xi.getDimensions(), ptr);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, xi.getElementCount());
        LinearInterpolationReal<single>((const single*)x1.getDataPointer(),
            (const single*)y1.getDataPointer(), x1.getElementCount(),
            (const single*)xi.getDataPointer(), xi.getElementCount(), ptr);
        res = ArrayOf(NLS_SINGLE, xi.getDimensions(), ptr);
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, xi.getElementCount());
        LinearInterpolationComplex<double>((const double*)x1.getDataPointer(),
            (const double*)y1.getDataPointer(), x1.getElementCount(),
            (const double*)xi.getDataPointer(), xi.getElementCount(), (double*)ptr);
        res = ArrayOf(NLS_DCOMPLEX, xi.getDimensions(), ptr);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, xi.getElementCount());
        LinearInterpolationComplex<single>((const single*)x1.getDataPointer(),
            (const single*)y1.getDataPointer(), x1.getElementCount(),
            (const single*)xi.getDataPointer(), xi.getElementCount(), (single*)ptr);
        res = ArrayOf(NLS_SCOMPLEX, xi.getDimensions(), ptr);
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return res;
}
//=============================================================================
template <class T>
void
LinearInterpolationComplex(
    const T* x1, const T* y1, indexType x1count, const T* xi, indexType xicount, T* yi)
{
    for (ompIndexType k = 0; k < (ompIndexType)xicount; k++) {
        int left = 0;
        int mflag = 0;
        interv<T>(x1, (int)x1count, xi[k], &left, &mflag);
        if (mflag == 0) {
            T frac = (xi[k] - x1[left - 1]) / (x1[left] - x1[left - 1]);
            yi[2 * k] = y1[2 * (left - 1)] + frac * (y1[2 * left] - y1[2 * (left - 1)]);
            yi[2 * k + 1]
                = y1[2 * (left - 1) + 1] + frac * (y1[2 * left + 1] - y1[2 * (left - 1) + 1]);
        } else {
            yi[2 * k] = (T)std::nan("NaN");
            yi[2 * k + 1] = (T)std::nan("NaN");
        }
    }
}
//=============================================================================
template <class T>
void
LinearInterpolationReal(
    const T* x1, const T* y1, indexType x1count, const T* xi, indexType xicount, T* yi)
{
    for (ompIndexType k = 0; k < (ompIndexType)xicount; k++) {
        int left = 0;
        int mflag = 0;
        interv<T>(x1, (int)x1count, xi[k], &left, &mflag);
        if (mflag == 0) {
            T frac = (xi[k] - x1[left - 1]) / (x1[left] - x1[left - 1]);
            yi[k] = y1[left - 1] + frac * (y1[left] - y1[left - 1]);
        } else {
            yi[k] = (T)std::nan("NaN");
        }
    }
}
//=============================================================================
// https://netlib.org/pppack/interv.f
template <class T>
int
interv(const T* xt, int lxt, T x, int* left, int* mflag)
{
    static int ilo = 1;
    static int ihi, istep, middle;
    --xt;
    ihi = ilo + 1;
    if (ihi < lxt) {
        goto L20;
    }
    if (x >= xt[lxt]) {
        goto L110;
    }
    if (lxt <= 1) {
        goto L90;
    }
    ilo = lxt - 1;
    ihi = lxt;

L20:
    if (x >= xt[ihi]) {
        goto L40;
    }
    if (x >= xt[ilo]) {
        goto L100;
    }
    istep = 1;
L31:
    ihi = ilo;
    ilo = ihi - istep;
    if (ilo <= 1) {
        goto L35;
    }
    if (x >= xt[ilo]) {
        goto L50;
    }
    istep <<= 1;
    goto L31;
L35:
    ilo = 1;
    if (x < xt[1]) {
        goto L90;
    }
    goto L50;
L40:
    istep = 1;
L41:
    ilo = ihi;
    ihi = ilo + istep;
    if (ihi >= lxt) {
        goto L45;
    }
    if (x < xt[ihi]) {
        goto L50;
    }
    istep <<= 1;
    goto L41;
L45:
    if (x >= xt[lxt]) {
        goto L110;
    }
    ihi = lxt;
L50:
    middle = (ilo + ihi) / 2;
    if (middle == ilo) {
        goto L100;
    }
    if (x < xt[middle]) {
        goto L53;
    }
    ilo = middle;
    goto L50;
L53:
    ihi = middle;
    goto L50;
L90:
    *mflag = -1;
    *left = 1;
    return 0;
L100:
    *mflag = 0;
    *left = ilo;
    return 0;
L110:
    *mflag = 1;
    if (x == xt[lxt]) {
        *mflag = 0;
    }
    *left = lxt;
L111:
    if (*left == 1) {
        return 0;
    }
    --(*left);
    if (xt[*left] < xt[lxt]) {
        return 0;
    }
    goto L111;
}
//=============================================================================
}
//=============================================================================
