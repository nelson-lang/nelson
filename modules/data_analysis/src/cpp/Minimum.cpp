//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "omp_for_loop.hpp"
#include "Minimum.hpp"
#include "complex_abs.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
TMinLessInteger(
    const T* spx, const T* spy, T* dp, indexType count, indexType stridex, indexType stridey)
{
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        T x = spx[stridex * i];
        T y = spy[stridey * i];
        dp[i] = (x < y) ? x : y;
    }
}
//=============================================================================
template <class T>
void
TMinLessReal(bool omitnan, const T* spx, const T* spy, T* dp, indexType count, indexType stridex,
    indexType stridey)
{
    if (omitnan) {
        OMP_PARALLEL_FOR_LOOP(count)
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            T x = spx[stridex * i];
            T y = spy[stridey * i];
            if (std::isnan(x) && std::isnan(y)) {
                dp[i] = (T)std::nan("");
            } else if (std::isnan(x) && !std::isnan(y)) {
                dp[i] = y;
            } else if (std::isnan(y) && !std::isnan(x)) {
                dp[i] = x;
            } else {
                dp[i] = (x < y) ? x : y;
            }
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(count)
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            T x = spx[stridex * i];
            T y = spy[stridey * i];
            if (std::isnan(x) || std::isnan(y)) {
                dp[i] = (T)std::nan("");
            } else {
                dp[i] = (x < y) ? x : y;
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMinLessComplex(bool omitnan, const T* spx, const T* spy, T* dp, indexType count, indexType stridex,
    indexType stridey)
{
    if (omitnan) {
        OMP_PARALLEL_FOR_LOOP(count)
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
                if (xmag < ymag) {
                    dp[2 * i] = xre;
                    dp[2 * i + 1] = xim;
                } else {
                    dp[2 * i] = yre;
                    dp[2 * i + 1] = yim;
                }
            }
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(count)
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
                if (xmag < ymag) {
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
TMinAllInteger(const T* sp, T* dp, indexType elementCount)
{
    T minval = sp[0];
    for (ompIndexType k = 1; k < (ompIndexType)elementCount; ++k) {
        if (sp[k] < minval) {
            minval = sp[k];
        }
    }
    dp[0] = minval;
}
//=============================================================================
template <class T>
void
TMinInteger(
    const T* sp, T* dp, double* iptr, indexType planes, indexType planesize, indexType linesize)
{
    T minval;
    double mindex;
    for (indexType i = 0; i < planes; i++) {
        for (indexType j = 0; j < planesize; j++) {
            minval = sp[i * planesize * linesize + j];
            mindex = 0;
            for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                T val = sp[i * planesize * linesize + j + k * planesize];
                if (val < minval) {
                    minval = val;
                    mindex = (double)k;
                }
            }
            dp[i * planesize + j] = minval;
            if (iptr != nullptr) {
                iptr[i * planesize + j] = mindex + 1;
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMinAllReal(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    if (omitnan) {
        T minval;
        bool init = false;
        for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
            if (!std::isnan(sp[k])) {
                minval = sp[k];
                init = true;
                break;
            }
        }
        if (!init) {
            minval = sp[0];
        }
        for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
            T val = sp[k];
            if (!std::isnan(val)) {
                if (val < minval) {
                    minval = val;
                }
            }
        }
        if (init) {
            dp[0] = minval;
        } else {
            dp[0] = (T)std::nan("");
        }
    } else {
        T minval;
        minval = sp[0];
        for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
            T val = sp[k];
            if (val < minval || std::isnan(val)) {
                minval = val;
            }
        }
        dp[0] = minval;
    }
}
//=============================================================================
template <class T>
void
TMinReal(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    if (omitnan) {
        T minval;
        for (ompIndexType i = 0; i < (ompIndexType)planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                bool init = false;
                double mindex = 0;
                for (indexType k = 0; k < linesize; k++) {
                    T val = sp[i * planesize * linesize + j + k * planesize];
                    if (!std::isnan(val)) {
                        minval = val;
                        mindex = (double)k;
                        init = true;
                        break;
                    }
                }
                if (init) {
                    for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                        T val = sp[i * planesize * linesize + j + k * planesize];
                        if (!std::isnan(val)) {
                            if (val < minval) {
                                minval = val;
                                mindex = (double)k;
                            }
                        }
                    }
                }
                if (init) {
                    dp[i * planesize + j] = minval;
                    if (iptr != nullptr) {
                        iptr[i * planesize + j] = mindex + 1;
                    }
                } else {
                    dp[i * planesize + j] = (T)std::nan("");
                    if (iptr != nullptr) {
                        iptr[i * planesize + j] = 0;
                    }
                }
            }
        }
    } else {
        T minval;
        for (ompIndexType i = 0; i < (ompIndexType)planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                double mindex = 0;
                minval = sp[i * planesize * linesize + j + 0 * planesize];
                mindex = (double)0;
                for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                    T val = sp[i * planesize * linesize + j + k * planesize];
                    if (val < minval || std::isnan(val)) {

                        minval = val;
                        mindex = (double)k;
                    }
                }
                dp[i * planesize + j] = minval;
                if (iptr != nullptr) {
                    iptr[i * planesize + j] = mindex + 1;
                }
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMinAllComplex(bool omitnan, const T* sp, T* dp, indexType elementCount)
{
    if (omitnan) {
        T minval = (T)0;
        T minval_r = (T)0;
        T minval_i = (T)0;
        T tstval;
        bool init = false;
        for (indexType k = 0; k < elementCount; ++k) {
            T val_re = sp[2 * k];
            T val_im = sp[(2 * k) + 1];
            if ((!std::isnan(val_re)) && (!std::isnan(val_im))) {
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
                    if (tstval < minval) {
                        minval = tstval;
                        minval_r = val_re;
                        minval_i = val_im;
                    }
                }
            }
        }

        if (init) {
            dp[0] = minval_r;
            dp[1] = minval_i;
        } else {
            dp[0] = (T)std::nan("");
            dp[1] = (T)std::nan("");
        }

    } else {
        T minval = (T)0;
        T minval_r = (T)0;
        T minval_i = (T)0;
        T tstval;
        for (ompIndexType k = 1; k < (ompIndexType)elementCount; ++k) {
            T val_re = sp[2 * k];
            T val_im = sp[(2 * k) + 1];
            tstval = complex_abs(val_re, val_im);
            if (tstval < minval || std::isnan(tstval)) {
                minval = tstval;
                minval_r = val_re;
                minval_i = val_im;
            }
        }
        dp[0] = minval_r;
        dp[1] = minval_i;
    }
}
//=============================================================================
template <class T>
void
TMinComplex(bool omitnan, const T* sp, T* dp, double* iptr, indexType planes, indexType planesize,
    indexType linesize)
{
    if (omitnan) {
        T minval;
        T minval_r;
        T minval_i;
        T tstval;
        double mindex;
        bool init = false;
        for (ompIndexType i = 0; i < (ompIndexType)planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                init = false;
                mindex = 0;
                for (indexType k = 0; k < linesize; k++) {
                    T val_re = sp[2 * (i * planesize * linesize + j + k * planesize)];
                    T val_im = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                    if ((!std::isnan(val_re)) && (!std::isnan(val_im))) {
                        tstval = complex_abs(val_re, val_im);
                        init = true;
                        minval = tstval;
                        mindex = (double)k;
                        minval_r = val_re;
                        minval_i = val_im;
                        break;
                    }
                }
                if (init) {
                    for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                        T val_re = sp[2 * (i * planesize * linesize + j + k * planesize)];
                        T val_im = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                        if ((!std::isnan(val_re)) && (!std::isnan(val_im))) {
                            tstval = complex_abs(val_re, val_im);
                            if (tstval < minval) {
                                minval = tstval;
                                mindex = (double)k;
                                minval_r = val_re;
                                minval_i = val_im;
                            }
                        }
                    }
                }
                if (init) {
                    dp[2 * (i * planesize + j)] = minval_r;
                    dp[2 * (i * planesize + j) + 1] = minval_i;
                    if (iptr != nullptr) {
                        iptr[i * planesize + j] = mindex + 1;
                    }
                } else {
                    dp[2 * (i * planesize + j)] = (T)std::nan("");
                    dp[2 * (i * planesize + j) + 1] = (T)std::nan("");
                    if (iptr != nullptr) {
                        iptr[i * planesize + j] = 0;
                    }
                }
            }
        }

    } else {
        T minval;
        T minval_r;
        T minval_i;
        T tstval;
        double mindex;
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                minval_r = sp[2 * (i * planesize * linesize + j + 0 * planesize)];
                minval_i = sp[2 * (i * planesize * linesize + j + 0 * planesize) + 1];
                minval = complex_abs(minval_r, minval_i);
                mindex = 0;

                for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                    T val_re = sp[2 * (i * planesize * linesize + j + k * planesize)];
                    T val_im = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                    tstval = complex_abs(val_re, val_im);
                    if (tstval < minval || std::isnan(tstval)) {
                        minval = tstval;
                        mindex = (double)k;
                        minval_r = val_re;
                        minval_i = val_im;
                    }
                }
                dp[2 * (i * planesize + j)] = minval_r;
                dp[2 * (i * planesize + j) + 1] = minval_i;
                if (iptr != nullptr) {
                    iptr[i * planesize + j] = mindex + 1;
                }
            }
        }
    }
}
//=============================================================================
// C = min(A, B)
ArrayOf
Minimum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
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
        TMinLessInteger<logical>((const logical*)A.getDataPointer(),
            (const logical*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<uint8>((const uint8*)A.getDataPointer(), (const uint8*)B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<int8>((const int8*)A.getDataPointer(), (const int8*)B.getDataPointer(), ptr,
            outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<uint16>((const uint16*)A.getDataPointer(),
            (const uint16*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<int16>((const int16*)A.getDataPointer(), (const int16*)B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<uint32>((const uint32*)A.getDataPointer(),
            (const uint32*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<int32>((const int32*)A.getDataPointer(), (const int32*)B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<uint64>((const uint64*)A.getDataPointer(),
            (const uint64*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<int64>((const int64*)A.getDataPointer(), (const int64*)B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessReal<single>(omitNaN, (const single*)A.getDataPointer(),
            (const single*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessReal<double>(omitNaN, (const double*)A.getDataPointer(),
            (const double*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessComplex<single>(omitNaN, (const single*)A.getDataPointer(),
            (const single*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessComplex<double>(omitNaN, (const double*)A.getDataPointer(),
            (const double*)B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_CHAR: {
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(commonType, outDim.getElementCount());
        res = ArrayOf(commonType, outDim, ptr);
        TMinLessInteger<charType>((const charType*)A.getDataPointer(),
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
// [M, i] = min(A)
ArrayOfVector
Minimum(bool omitNaN, const ArrayOf& A, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval;
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
        retval = Minimum(omitNaN, A, dim, nLhs, needToOverload);
    }
    return retval;
}
//=============================================================================
// [M, i] = min(A, [], dim)
ArrayOfVector
Minimum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs, bool& needToOverload)
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
            ompIndexType elementCount = outDim.getElementCount();
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount);
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < elementCount; ++k) {
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
        TMinInteger<logical>((const logical*)A.getDataPointer(), (logical*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_LOGICAL, outDim, ptr);
    } break;
    case NLS_UINT8: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, outDim.getElementCount());
        TMinInteger<uint8>(
            (const uint8*)A.getDataPointer(), (uint8*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT8, outDim, ptr);
    } break;
    case NLS_INT8: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, outDim.getElementCount());
        TMinInteger<int8>(
            (const int8*)A.getDataPointer(), (int8*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT8, outDim, ptr);
    } break;
    case NLS_UINT16: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, outDim.getElementCount());
        TMinInteger<uint16>(
            (const uint16*)A.getDataPointer(), (uint16*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT16, outDim, ptr);
    } break;
    case NLS_INT16: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, outDim.getElementCount());
        TMinInteger<int16>(
            (const int16*)A.getDataPointer(), (int16*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT16, outDim, ptr);
    } break;
    case NLS_UINT32: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, outDim.getElementCount());
        TMinInteger<uint32>(
            (const uint32*)A.getDataPointer(), (uint32*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT32, outDim, ptr);
    } break;
    case NLS_INT32: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, outDim.getElementCount());
        TMinInteger<int32>(
            (const int32*)A.getDataPointer(), (int32*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT32, outDim, ptr);
    } break;
    case NLS_UINT64: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, outDim.getElementCount());
        TMinInteger<uint64>(
            (const uint64*)A.getDataPointer(), (uint64*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_UINT64, outDim, ptr);
    } break;
    case NLS_INT64: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, outDim.getElementCount());
        TMinInteger<int64>(
            (const int64*)A.getDataPointer(), (int64*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_INT64, outDim, ptr);
    } break;
    case NLS_SINGLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.getElementCount());
        TMinReal<single>(omitNaN, (const single*)A.getDataPointer(), (single*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        TMinReal<double>(omitNaN, (const double*)A.getDataPointer(), (double*)ptr, iptr, planecount,
            planesize, linesize);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.getElementCount());
        TMinComplex<single>(omitNaN, (const single*)A.getDataPointer(), (single*)ptr, iptr,
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
        TMinComplex<double>(omitNaN, (const double*)A.getDataPointer(), (double*)ptr, iptr,
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
        TMinInteger<double>((const double*)_A.getDataPointer(), (double*)ptr, iptr, planecount,
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
MinimumAll(bool omitNaN, const ArrayOf& A, bool& needToOverload)
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
        TMinAllInteger<logical>((const logical*)A.getDataPointer(), (logical*)ptr, elementCount);
        res = ArrayOf(NLS_LOGICAL, outDim, ptr);
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, outDim.getElementCount());
        TMinAllInteger<uint8>((const uint8*)A.getDataPointer(), (uint8*)ptr, elementCount);
        res = ArrayOf(NLS_UINT8, outDim, ptr);
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, outDim.getElementCount());
        TMinAllInteger<int8>((const int8*)A.getDataPointer(), (int8*)ptr, elementCount);
        res = ArrayOf(NLS_INT8, outDim, ptr);
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, outDim.getElementCount());
        TMinAllInteger<uint16>((const uint16*)A.getDataPointer(), (uint16*)ptr, elementCount);
        res = ArrayOf(NLS_UINT16, outDim, ptr);
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, outDim.getElementCount());
        TMinAllInteger<int16>((const int16*)A.getDataPointer(), (int16*)ptr, elementCount);
        res = ArrayOf(NLS_INT16, outDim, ptr);
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, outDim.getElementCount());
        TMinAllInteger<uint32>((const uint32*)A.getDataPointer(), (uint32*)ptr, elementCount);
        res = ArrayOf(NLS_UINT32, outDim, ptr);
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, outDim.getElementCount());
        TMinAllInteger<int32>((const int32*)A.getDataPointer(), (int32*)ptr, elementCount);
        res = ArrayOf(NLS_INT32, outDim, ptr);
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, outDim.getElementCount());
        TMinAllInteger<uint64>((const uint64*)A.getDataPointer(), (uint64*)ptr, elementCount);
        res = ArrayOf(NLS_UINT64, outDim, ptr);
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, outDim.getElementCount());
        TMinAllInteger<int64>((const int64*)A.getDataPointer(), (int64*)ptr, elementCount);
        res = ArrayOf(NLS_INT64, outDim, ptr);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.getElementCount());
        TMinAllReal<single>(omitNaN, (const single*)A.getDataPointer(), (single*)ptr, elementCount);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        TMinAllReal<double>(omitNaN, (const double*)A.getDataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.getElementCount());
        TMinAllComplex<single>(
            omitNaN, (const single*)A.getDataPointer(), (single*)ptr, elementCount);
        res = ArrayOf(NLS_SCOMPLEX, outDim, ptr);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.getElementCount());
        TMinAllComplex<double>(
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
        TMinAllInteger<double>((const double*)_A.getDataPointer(), (double*)ptr, elementCount);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
