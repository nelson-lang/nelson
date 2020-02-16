//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsConfig.h"
#include "Maximum.hpp"
#include "complex_abs.hpp"
#include "Error.hpp"
#include "FindCommonClass.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
TMaxGreaterReal(
    const T* spx, const T* spy, T* dp, indexType count, indexType stridex, indexType stridey)
{
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dp[i] = (spx[stridex * i] > spy[stridey * i]) ? spx[stridex * i] : spy[stridey * i];
    }
}
//=============================================================================
template <class T>
void
TMaxGreaterComplex(
    const T* spx, const T* spy, T* dp, indexType count, indexType stridex, indexType stridey)
{
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        T xmag = complex_abs(spx[2 * stridex * i], spx[2 * stridex * i + 1]);
        T ymag = complex_abs(spy[2 * stridey * i], spy[2 * stridey * i + 1]);
        if (xmag > ymag) {
            dp[2 * i] = spx[2 * stridex * i];
            dp[2 * i + 1] = spx[2 * stridex * i + 1];
        } else {
            dp[2 * i] = spy[2 * stridey * i];
            dp[2 * i + 1] = spy[2 * stridey * i + 1];
        }
    }
}
//=============================================================================
template <class T>
void
TMaxInteger(
    const T* sp, T* dp, double* iptr, indexType planes, indexType planesize, indexType linesize)
{
    T maxval;
    double maxdex;
    if (planes == 1 && planesize == 1) {
        maxval = sp[0];
        maxdex = 0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for ordered
#endif
        for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
            if (sp[k] > maxval) {
                maxval = sp[k];
                maxdex = (double)k;
            }
        }
        dp[0] = maxval;
        if (iptr != nullptr) {
            iptr[0] = maxdex + 1;
        }
    } else {
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                maxval = sp[i * planesize * linesize + j];
                maxdex = 0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for ordered
#endif
                for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                    if (sp[i * planesize * linesize + j + k * planesize] > maxval) {
                        maxval = sp[i * planesize * linesize + j + k * planesize];
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
TMaxReal(
    const T* sp, T* dp, double* iptr, indexType planes, indexType planesize, indexType linesize)
{
    T maxval;
    double maxdex = 0;
    bool init = false;

    for (indexType i = 0; i < planes; i++) {
        for (indexType j = 0; j < planesize; j++) {
            init = false;
            maxdex = 0;
            for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                if (!std::isnan(sp[i * planesize * linesize + j + k * planesize])) {
                    if (!init) {
                        init = true;
                        maxval = sp[i * planesize * linesize + j + k * planesize];
                        maxdex = (double)k;
                    } else if (sp[i * planesize * linesize + j + k * planesize] > maxval) {
                        maxval = sp[i * planesize * linesize + j + k * planesize];
                        maxdex = (double)k;
                    }
                }
            }
            if (init) {
                dp[i * planesize + j] = maxval;
                if (iptr != nullptr) {
                    iptr[i * planesize + j] = maxdex + 1;
                }
            } else {
                dp[i * planesize + j] = (T)atof("nan");
                if (iptr != nullptr) {
                    iptr[i * planesize + j] = 0;
                }
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMaxComplex(
    const T* sp, T* dp, double* iptr, indexType planes, indexType planesize, indexType linesize)
{
    T maxval;
    T maxval_r;
    T maxval_i;
    T tstval;
    double maxdex;
    bool init = false;

    for (indexType i = 0; i < planes; i++) {
        for (indexType j = 0; j < planesize; j++) {
            init = false;
            maxdex = 0;
            for (ompIndexType k = 0; k < (ompIndexType)linesize; k++) {
                if ((!std::isnan(sp[2 * (i * planesize * linesize + j + k * planesize)]))
                    && (!std::isnan(sp[2 * (i * planesize * linesize + j + k * planesize) + 1]))) {
                    tstval = complex_abs(sp[2 * (i * planesize * linesize + j + k * planesize)],
                        sp[2 * (i * planesize * linesize + j + k * planesize) + 1]);
                    if (!init) {
                        init = true;
                        maxval = tstval;
                        maxdex = (double)k;
                        maxval_r = sp[2 * (i * planesize * linesize + j + k * planesize)];
                        maxval_i = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                    } else if (tstval > maxval) {
                        maxval = tstval;
                        maxdex = (double)k;
                        maxval_r = sp[2 * (i * planesize * linesize + j + k * planesize)];
                        maxval_i = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
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
                dp[2 * (i * planesize + j)] = (T)atof("nan");
                dp[2 * (i * planesize + j) + 1] = (T)atof("nan");
                if (iptr != nullptr) {
                    iptr[i * planesize + j] = 0;
                }
            }
        }
    }
}
//=============================================================================
// C = max(A, B)
ArrayOf
Maximum(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
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
        return ArrayOf();
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
    Class outType;
    if (A.getDataClass() != B.getDataClass()) {
        outType = FindCommonClass(A, B, needToOverload);
    } else {
        if (A.getDataClass() == NLS_CHAR) {
            outType = NLS_DOUBLE;
        } else {
            outType = A.getDataClass();
        }
    }
    ArrayOf res;
    if (needToOverload) {
        return res;
    }
    ArrayOf _A(A);
    ArrayOf _B(B);
    switch (outType) {
    case NLS_LOGICAL: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<logical>((const logical*)_A.getDataPointer(),
            (const logical*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT8: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<uint8>((const uint8*)_A.getDataPointer(), (const uint8*)_B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT8: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        int8* ptr = (int8*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<int8>((const int8*)_A.getDataPointer(), (const int8*)_B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT16: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        uint16* ptr = (uint16*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<uint16>((const uint16*)_A.getDataPointer(),
            (const uint16*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT16: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        int16* ptr = (int16*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<int16>((const int16*)_A.getDataPointer(), (const int16*)_B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT32: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        uint32* ptr = (uint32*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<uint32>((const uint32*)_A.getDataPointer(),
            (const uint32*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT32: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        int32* ptr = (int32*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<int32>((const int32*)_A.getDataPointer(), (const int32*)_B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_UINT64: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        uint64* ptr = (uint64*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<uint64>((const uint64*)_A.getDataPointer(),
            (const uint64*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_INT64: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        int64* ptr = (int64*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<int64>((const int64*)_A.getDataPointer(), (const int64*)_B.getDataPointer(),
            ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_SINGLE: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<single>((const single*)_A.getDataPointer(),
            (const single*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_DOUBLE: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<double>((const double*)_A.getDataPointer(),
            (const double*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_SCOMPLEX: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        single* ptr = (single*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterComplex<single>((const single*)_A.getDataPointer(),
            (const single*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_DCOMPLEX: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        double* ptr = (double*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterComplex<double>((const double*)_A.getDataPointer(),
            (const double*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_CHAR: {
        _A.promoteType(outType);
        _B.promoteType(outType);
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(outType, outDim.getElementCount());
        res = ArrayOf(outType, outDim, ptr);
        TMaxGreaterReal<charType>((const charType*)_A.getDataPointer(),
            (const charType*)_B.getDataPointer(), ptr, outDim.getElementCount(), AStride, BStride);
    } break;
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
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
Maximum(const ArrayOf& A, int nLhs, bool& needToOverload)
{
    ArrayOfVector retval;
    needToOverload = false;
    if (A.isEmpty()) {
        retval.push_back(A);
        if (nLhs > 1) {
            retval.push_back(ArrayOf::emptyConstructor());
        }
    } else if (A.isScalar()) {
        retval.push_back(A);
        if (nLhs > 1) {
            retval.push_back(ArrayOf::doubleConstructor(1));
        }
    } else {
        Dimensions dimA(A.getDimensions());
        indexType d = 0;
        while (dimA[d] == 1) {
            d++;
        }
        indexType dim = d + 1;
        retval = Maximum(A, dim, nLhs, needToOverload);
    }
    return retval;
}
//=============================================================================
// [M, i] = max(A, [], dim)
ArrayOfVector
Maximum(const ArrayOf& A, indexType dim, int nLhs, bool& needToOverload)
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
        retval.push_back(A);
        if (nLhs > 1) {
            retval.push_back(ArrayOf::emptyConstructor());
        }
        return retval;
    }
    if (A.isScalar()) {
        retval.push_back(A);
        if (nLhs > 1) {
            retval.push_back(ArrayOf::doubleConstructor(1));
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
    switch (A.getDataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
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
        TMaxReal<single>(
            (const single*)A.getDataPointer(), (single*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
    } break;
    case NLS_DOUBLE: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
        TMaxReal<double>(
            (const double*)A.getDataPointer(), (double*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
    } break;
    case NLS_SCOMPLEX: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.getElementCount());
        TMaxComplex<single>(
            (const single*)A.getDataPointer(), (single*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_SCOMPLEX, outDim, ptr);
    } break;
    case NLS_DCOMPLEX: {
        if (nLhs > 1) {
            iptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
            index = ArrayOf(NLS_DOUBLE, outDim, iptr);
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.getElementCount());
        TMaxComplex<double>(
            (const double*)A.getDataPointer(), (double*)ptr, iptr, planecount, planesize, linesize);
        res = ArrayOf(NLS_DCOMPLEX, outDim, ptr);
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
    retval.push_back(res);
    if (nLhs > 1) {
        retval.push_back(index);
    }
    return retval;
}
//=============================================================================
}
//=============================================================================
