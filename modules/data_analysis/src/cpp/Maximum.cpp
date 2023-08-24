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
#include "Maximum.hpp"
#include "complex_abs.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
TMaxLessInteger(
    const T* spx, const T* spy, T* dp, indexType count, indexType stridex, indexType stridey)
{
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        T x = spx[stridex * i];
        T y = spy[stridey * i];
        dp[i] = (x > y) ? x : y;
    }
}
//=============================================================================
template <class T>
void
TMaxLessReal(bool omitnan, const T* spx, const T* spy, T* dp, indexType count, indexType stridex,
    indexType stridey)
{
    if (omitnan) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            T x = spx[stridex * i];
            T y = spy[stridey * i];
            if (std::isnan(x) && std::isnan(y)) {
                dp[i] = (T)std::nan("NaN");
            } else if (std::isnan(x) && !std::isnan(y)) {
                dp[i] = y;
            } else if (std::isnan(y) && !std::isnan(x)) {
                dp[i] = x;
            } else {
                dp[i] = (x > y) ? x : y;
            }
        }
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            T x = spx[stridex * i];
            T y = spy[stridey * i];
            if (std::isnan(x) || std::isnan(y)) {
                dp[i] = (T)std::nan("NaN");
            } else {
                dp[i] = (x > y) ? x : y;
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMaxLessComplex(bool omitnan, const T* spx, const T* spy, T* dp, indexType count, indexType stridex,
    indexType stridey)
{
    if (omitnan) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            T xre = spx[2 * stridex * i];
            T xim = spx[2 * stridex * i + 1];
            T yre = spy[2 * stridey * i];
            T yim = spy[2 * stridey * i + 1];

            T xmag = complex_abs(xre, xim);
            T ymag = complex_abs(yre, yim);
            if (std::isnan(xmag) && std::isnan(ymag)) {
                dp[2 * i] = xre;
                dp[2 * i + 1] = xim;
            } else if (std::isnan(xmag)) {
                dp[2 * i] = yre;
                dp[2 * i + 1] = yim;
            } else if (std::isnan(ymag)) {
                dp[2 * i] = xre;
                dp[2 * i + 1] = xim;
            } else {
                if (xmag > ymag) {
                    dp[2 * i] = xre;
                    dp[2 * i + 1] = xim;
                } else {
                    dp[2 * i] = yre;
                    dp[2 * i + 1] = yim;
                }
            }
        }
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            T xre = spx[2 * stridex * i];
            T xim = spx[2 * stridex * i + 1];
            T yre = spy[2 * stridey * i];
            T yim = spy[2 * stridey * i + 1];
            T xmag = complex_abs(xre, xim);
            T ymag = complex_abs(yre, yim);
            if (std::isnan(xmag) && std::isnan(ymag)) {
                dp[2 * i] = xre;
                dp[2 * i + 1] = xim;
            } else if (std::isnan(xmag)) {
                dp[2 * i] = xre;
                dp[2 * i + 1] = xim;
            } else if (std::isnan(ymag)) {
                dp[2 * i] = yre;
                dp[2 * i + 1] = yim;
            } else {
                if (xmag > ymag) {
                    dp[2 * i] = xre;
                    dp[2 * i + 1] = xim;
                } else {
                    dp[2 * i] = yre;
                    dp[2 * i + 1] = yim;
                }
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMaxAllInteger(const T* sp, T* dp, indexType elementCount)
{
    T maxval = sp[0];
    for (ompIndexType k = 1; k < (ompIndexType)elementCount; ++k) {
        if (sp[k] > maxval) {
            maxval = sp[k];
        }
    }
    dp[0] = maxval;
}
//=============================================================================
template <class T>
void
TMaxInteger(
    const T* sp, T* dp, double* iptr, indexType planes, indexType planesize, indexType linesize)
{
    T maxval;
    double maxdex;
    for (indexType i = 0; i < planes; i++) {
        for (indexType j = 0; j < planesize; j++) {
            maxval = sp[i * planesize * linesize + j];
            maxdex = 0;
            for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                T val = sp[i * planesize * linesize + j + k * planesize];
                if (val > maxval) {
                    maxval = val;
                    maxdex = (double)k;
                }
            }
            dp[i * planesize + j] = maxval;
            if (iptr != nullptr) {
                iptr[i * planesize + j] = maxdex + 1;
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMaxAllReal(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    if (omitnan) {
        T maxval;
        bool init = false;
        for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
            if (!std::isnan(sp[k])) {
                maxval = sp[k];
                init = true;
                break;
            }
        }
        if (!init) {
            maxval = sp[0];
        }
        for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
            T val = sp[k];
            if (!std::isnan(val)) {
                if (val > maxval) {
                    maxval = val;
                }
            }
        }
        if (init) {
            dp[0] = maxval;
        } else {
            dp[0] = (T)std::nan("NaN");
        }
    } else {
        T maxval;
        maxval = sp[0];
        for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
            T val = sp[k];
            if (val > maxval || std::isnan(val)) {
                maxval = val;
            }
        }
        dp[0] = maxval;
    }
}
//=============================================================================
template <class T>
void
TMaxReal(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    if (omitnan) {
        T maxval;
        for (ompIndexType i = 0; i < (ompIndexType)planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                bool init = false;
                double maxdex = 0;
                for (indexType k = 0; k < linesize; k++) {
                    T val = sp[i * planesize * linesize + j + k * planesize];
                    if (!std::isnan(val)) {
                        maxval = val;
                        maxdex = (double)k;
                        init = true;
                        break;
                    }
                }
                if (init) {
                    for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                        T val = sp[i * planesize * linesize + j + k * planesize];
                        if (!std::isnan(val)) {
                            if (val > maxval) {
                                maxval = val;
                                maxdex = (double)k;
                            }
                        }
                    }
                }
                if (init) {
                    dp[i * planesize + j] = maxval;
                    if (iptr != nullptr) {
                        iptr[i * planesize + j] = maxdex + 1;
                    }
                } else {
                    dp[i * planesize + j] = (T)std::nan("NaN");
                    if (iptr != nullptr) {
                        iptr[i * planesize + j] = 0;
                    }
                }
            }
        }
    } else {
        T maxval;
        for (ompIndexType i = 0; i < (ompIndexType)planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                double maxdex = 0;
                maxval = sp[i * planesize * linesize + j + 0 * planesize];
                maxdex = (double)0;
                for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                    T val = sp[i * planesize * linesize + j + k * planesize];
                    if (val > maxval || std::isnan(val)) {

                        maxval = val;
                        maxdex = (double)k;
                    }
                }
                dp[i * planesize + j] = maxval;
                if (iptr != nullptr) {
                    iptr[i * planesize + j] = maxdex + 1;
                }
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMaxAllComplex(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    if (omitnan) {
        T maxval = (T)0;
        T maxval_r = (T)0;
        T maxval_i = (T)0;
        T tstval;
        bool init = false;
        for (indexType k = 0; k < elementCount; ++k) {
            T val_re = sp[2 * k];
            T val_im = sp[(2 * k) + 1];
            if ((!std::isnan(val_re)) && (!std::isnan(val_im))) {
                tstval = complex_abs(val_re, val_im);
                init = true;
                break;
            }
        }

        if (init) {
            for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
                T val_re = sp[2 * k];
                T val_im = sp[(2 * k) + 1];
                if ((!std::isnan(val_re)) && (!std::isnan(val_im))) {
                    tstval = complex_abs(val_re, val_im);
                    if (tstval > maxval) {
                        maxval = tstval;
                        maxval_r = val_re;
                        maxval_i = val_im;
                    }
                }
            }
        }

        if (init) {
            dp[0] = maxval_r;
            dp[1] = maxval_i;
        } else {
            dp[0] = (T)std::nan("nan");
            dp[1] = (T)std::nan("nan");
        }

    } else {
        T maxval = (T)0;
        T maxval_r = (T)0;
        T maxval_i = (T)0;
        T tstval;
        tstval = complex_abs(sp[0], sp[1]);
        for (ompIndexType k = 1; k < (ompIndexType)elementCount; ++k) {
            T val_re = sp[2 * k];
            T val_im = sp[(2 * k) + 1];
            tstval = complex_abs(val_re, val_im);
            if (tstval > maxval || std::isnan(tstval)) {
                maxval = tstval;
                maxval_r = val_re;
                maxval_i = val_im;
            }
        }
        dp[0] = maxval_r;
        dp[1] = maxval_i;
    }
}
//=============================================================================
template <class T>
void
TMaxComplex(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    if (omitnan) {
        T maxval = (T)0;
        T maxval_r = (T)0;
        T maxval_i = (T)0;
        T tstval;
        double maxdex;
        bool init = false;
        for (ompIndexType i = 0; i < (ompIndexType)planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                init = false;
                maxdex = 0;
                for (indexType k = 0; k < linesize; k++) {
                    T val_re = sp[2 * (i * planesize * linesize + j + k * planesize)];
                    T val_im = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                    if ((!std::isnan(val_re)) && (!std::isnan(val_im))) {
                        tstval = complex_abs(val_re, val_im);
                        init = true;
                        maxval = tstval;
                        maxdex = (double)k;
                        maxval_r = val_re;
                        maxval_i = val_im;
                        break;
                    }
                }
                if (init) {
                    for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                        T val_re = sp[2 * (i * planesize * linesize + j + k * planesize)];
                        T val_im = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                        if ((!std::isnan(val_re)) && (!std::isnan(val_im))) {
                            tstval = complex_abs(val_re, val_im);
                            if (tstval > maxval) {
                                maxval = tstval;
                                maxdex = (double)k;
                                maxval_r = val_re;
                                maxval_i = val_im;
                            }
                        }
                    }
                }
                if (init) {
                    dp[2 * (i * planesize + j)] = maxval_r;
                    dp[2 * (i * planesize + j) + 1] = maxval_i;
                    if (iptr != nullptr) {
                        iptr[i * planesize + j] = maxdex + 1;
                    }
                } else {
                    dp[2 * (i * planesize + j)] = (T)std::nan("NaN");
                    dp[2 * (i * planesize + j) + 1] = (T)std::nan("NaN");
                    if (iptr != nullptr) {
                        iptr[i * planesize + j] = 0;
                    }
                }
            }
        }

    } else {
        T maxval = (T)0;
        T maxval_r = (T)0;
        T maxval_i = (T)0;
        T tstval;
        double maxdex;
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                maxval_r = sp[2 * (i * planesize * linesize + j + 0 * planesize)];
                maxval_i = sp[2 * (i * planesize * linesize + j + 0 * planesize) + 1];
                maxval = complex_abs(maxval_r, maxval_i);
                maxdex = 0;

                for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                    T val_re = sp[2 * (i * planesize * linesize + j + k * planesize)];
                    T val_im = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                    tstval = complex_abs(val_re, val_im);
                    if (tstval > maxval || std::isnan(tstval)) {
                        maxval = tstval;
                        maxdex = (double)k;
                        maxval_r = val_re;
                        maxval_i = val_im;
                    }
                }
                dp[2 * (i * planesize + j)] = maxval_r;
                dp[2 * (i * planesize + j) + 1] = maxval_i;
                if (iptr != nullptr) {
                    iptr[i * planesize + j] = maxdex + 1;
                }
            }
        }
    }
}
//=============================================================================
// C = max(A, B)
ArrayOf
Maximum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isEmpty()) {
        return B;
    }
    if (B.isEmpty()) {
        return A;
    }
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return {};
    }
    Dimensions ASize(A.getDimensions());
    Dimensions BSize(B.getDimensions());
    Dimensions outDim;
    ASize.simplify();
    BSize.simplify();
    indexType AStride = 1;
    indexType BStride = 1;
    if (ASize.isScalar() || BSize.isScalar() || ASize.equals(BSize)) {
        if (ASize.isScalar()) {
            outDim = BSize;
            AStride = 0;
            BStride = 1;
        } else if (BSize.isScalar()) {
            outDim = ASize;
            AStride = 1;
            BStride = 0;
        } else {
            outDim = ASize;
            AStride = 1;
            BStride = 1;
        }
    } else {
        Error(_W("Input Arguments must have same size."));
    }
    ArrayOf res;
    NelsonType commonType = A.getDataClass();
    switch (A.getDataClass()) {
    case NLS_LOGICAL: {
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<logical>((const logical*)A.getDataPointer(),
            (const logical*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<uint8>((const uint8*)A.getDataPointer(), (const uint8*)B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<int8>((const int8*)A.getDataPointer(), (const int8*)B.getDataPointer(), ptr,
            outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<uint16>((const uint16*)A.getDataPointer(),
            (const uint16*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<int16>((const int16*)A.getDataPointer(), (const int16*)B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<uint32>((const uint32*)A.getDataPointer(),
            (const uint32*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<int32>((const int32*)A.getDataPointer(), (const int32*)B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<uint64>((const uint64*)A.getDataPointer(),
            (const uint64*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<int64>((const int64*)A.getDataPointer(), (const int64*)B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessReal<single>(omitNaN, (const single*)A.getDataPointer(),
            (const single*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessReal<double>(omitNaN, (const double*)A.getDataPointer(),
            (const double*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessComplex<single>(omitNaN, (const single*)A.getDataPointer(),
            (const single*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessComplex<double>(omitNaN, (const double*)A.getDataPointer(),
            (const double*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMaxLessInteger<charType>((const charType*)A.getDataPointer(),
            (const charType*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
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
    return res;
}
//=============================================================================
// [M, i] = max(A)
ArrayOfVector
Maximum(bool omitNaN, const ArrayOf& A, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval(1);
    needToOverload = false;
    if (A.isEmpty()) {
        retval << A;
        if (nLhs > 1) {
            retval << ArrayOf::emptyConstructor();
        }
    } else if (A.isScalar()) {
        retval << A;
        if (nLhs > 1) {
            retval << ArrayOf::doubleConstructor(1);
        }
    } else {
        Dimensions dimA(A.getDimensions());
        indexType d = 0;
        while (dimA[d] == 1) {
            d++;
        }
        indexType dim = d + 1;
        retval = Maximum(omitNaN, A, dim, nLhs, needToOverload);
    }
    return retval;
}
//=============================================================================
// [M, i] = max(A, [], dim)
ArrayOfVector
Maximum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval;
    needToOverload = false;
    if (dim < 1) {
        Error(_W("Dimension argument must be a positive integer scalar."));
    }
    indexType workDim = dim - 1;
    if (A.isSparse()) {
        needToOverload = true;
        return retval;
    }
    if (A.isEmpty()) {
        retval << A;
        if (nLhs > 1) {
            retval << ArrayOf::emptyConstructor();
        }
        return retval;
    }
    if (A.isScalar()) {
        retval << A;
        if (nLhs > 1) {
            retval << ArrayOf::doubleConstructor(1);
        }
        return retval;
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions outDim(dimsA);
    outDim[workDim] = 1;
    indexType planecount;
    indexType planesize;
    indexType linesize = dimsA[workDim];
    planesize = 1;
    for (indexType d = 0; d < workDim; d++) {
        planesize *= dimsA[d];
    }
    planecount = 1;
    for (indexType d = workDim + 1; d < dimsA.getLength(); d++) {
        planecount *= dimsA[d];
    }
    ArrayOf index;
    double* iptr = nullptr;
    ArrayOf res;

    if (dimsA.equals(outDim)) {
        outDim.simplify();
        retval << A;
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)outDim.getElementCount(); ++k) {
                iptr[k] = 1;
            }
            retval << index;
        }
        return retval;
    }

    switch (A.getDataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, outDim.getElementCount());
        TMaxInteger<logical>((const logical*)A.getDataPointer(), (logical*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_LOGICAL, outDim, ptr);
    } break;
    case NLS_UINT8: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, outDim.getElementCount());
        TMaxInteger<uint8>(
            (const uint8*)A.getDataPointer(), (uint8*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT8, outDim, ptr);
    } break;
    case NLS_INT8: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, outDim.getElementCount());
        TMaxInteger<int8>(
            (const int8*)A.getDataPointer(), (int8*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT8, outDim, ptr);
    } break;
    case NLS_UINT16: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, outDim.getElementCount());
        TMaxInteger<uint16>(
            (const uint16*)A.getDataPointer(), (uint16*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT16, outDim, ptr);
    } break;
    case NLS_INT16: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, outDim.getElementCount());
        TMaxInteger<int16>(
            (const int16*)A.getDataPointer(), (int16*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT16, outDim, ptr);
    } break;
    case NLS_UINT32: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, outDim.getElementCount());
        TMaxInteger<uint32>(
            (const uint32*)A.getDataPointer(), (uint32*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT32, outDim, ptr);
    } break;
    case NLS_INT32: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, outDim.getElementCount());
        TMaxInteger<int32>(
            (const int32*)A.getDataPointer(), (int32*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT32, outDim, ptr);
    } break;
    case NLS_UINT64: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, outDim.getElementCount());
        TMaxInteger<uint64>(
            (const uint64*)A.getDataPointer(), (uint64*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT64, outDim, ptr);
    } break;
    case NLS_INT64: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, outDim.getElementCount());
        TMaxInteger<int64>(
            (const int64*)A.getDataPointer(), (int64*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT64, outDim, ptr);
    } break;
    case NLS_SINGLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.getElementCount());
        TMaxReal<single>(omitNaN, (const single*)A.getDataPointer(), (single*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        TMaxReal<double>(omitNaN, (const double*)A.getDataPointer(), (double*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.getElementCount());
        TMaxComplex<single>(omitNaN, (const single*)A.getDataPointer(), (single*)ptr, iptr,
            planecount, planesize, linesize);
        res = ArrayOf(NLS_SCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.getElementCount());
        TMaxComplex<double>(omitNaN, (const double*)A.getDataPointer(), (double*)ptr, iptr,
            planecount, planesize, linesize);
        res = ArrayOf(NLS_DCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        ArrayOf _A(A);
        _A.promoteType(NLS_DOUBLE);
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        TMaxInteger<double>((const double*)_A.getDataPointer(), (double*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    }
    retval << res;
    if (nLhs > 1) {
        retval << index;
    }
    return retval;
}
//=============================================================================
ArrayOf
MaximumAll(bool omitNaN, const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    if (A.isEmpty()) {
        return A;
    }
    if (A.isScalar()) {
        return A;
    }
    Dimensions outDim(1, 1);
    indexType elementCount = A.getElementCount();
    switch (A.getDataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;

    case NLS_LOGICAL: {
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, outDim.getElementCount());
        TMaxAllInteger<logical>((const logical*)A.getDataPointer(), (logical*)ptr, elementCount);
        res = ArrayOf(NLS_LOGICAL, outDim, ptr);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, outDim.getElementCount());
        TMaxAllInteger<uint8>((const uint8*)A.getDataPointer(), (uint8*)ptr, elementCount);
        res = ArrayOf(NLS_UINT8, outDim, ptr);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, outDim.getElementCount());
        TMaxAllInteger<int8>((const int8*)A.getDataPointer(), (int8*)ptr, elementCount);
        res = ArrayOf(NLS_INT8, outDim, ptr);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, outDim.getElementCount());
        TMaxAllInteger<uint16>((const uint16*)A.getDataPointer(), (uint16*)ptr, elementCount);
        res = ArrayOf(NLS_UINT16, outDim, ptr);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, outDim.getElementCount());
        TMaxAllInteger<int16>((const int16*)A.getDataPointer(), (int16*)ptr, elementCount);
        res = ArrayOf(NLS_INT16, outDim, ptr);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, outDim.getElementCount());
        TMaxAllInteger<uint32>((const uint32*)A.getDataPointer(), (uint32*)ptr, elementCount);
        res = ArrayOf(NLS_UINT32, outDim, ptr);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, outDim.getElementCount());
        TMaxAllInteger<int32>((const int32*)A.getDataPointer(), (int32*)ptr, elementCount);
        res = ArrayOf(NLS_INT32, outDim, ptr);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, outDim.getElementCount());
        TMaxAllInteger<uint64>((const uint64*)A.getDataPointer(), (uint64*)ptr, elementCount);
        res = ArrayOf(NLS_UINT64, outDim, ptr);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, outDim.getElementCount());
        TMaxAllInteger<int64>((const int64*)A.getDataPointer(), (int64*)ptr, elementCount);
        res = ArrayOf(NLS_INT64, outDim, ptr);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.getElementCount());
        TMaxAllReal<single>(omitNaN, (const single*)A.getDataPointer(), (single*)ptr, elementCount);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        TMaxAllReal<double>(omitNaN, (const double*)A.getDataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.getElementCount());
        TMaxAllComplex<single>(
            omitNaN, (const single*)A.getDataPointer(), (single*)ptr, elementCount);
        res = ArrayOf(NLS_SCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.getElementCount());
        TMaxAllComplex<double>(
            omitNaN, (const double*)A.getDataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        ArrayOf _A(A);
        _A.promoteType(NLS_DOUBLE);
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        TMaxAllInteger<double>((const double*)_A.getDataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
