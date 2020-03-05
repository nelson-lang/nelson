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
#include <algorithm>
#include "Sum.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
signedIntegerSumAsDouble(const int64* sp, double* dp, indexType elementCount)
{
    if (dp == nullptr) {
        return;
    }
    double sum = 0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for reduction(+ : sum)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; ++i) {
        sum += (double)sp[i];
    }
    dp[0] = sum;
}
//=============================================================================
static void
signedIntegerSumAsDouble(
    const int64* sp, double* dp, size_t planes, size_t planesize, size_t linesize)
{
    for (size_t i = 0; i < planes; i++) {
        for (size_t j = 0; j < planesize; j++) {
            double accum = 0;
            for (size_t k = 0; k < linesize; k++) {
                int64 val = sp[i * planesize * linesize + j + k * planesize];
                accum += val;
            }
            dp[i * planesize + j] = accum;
        }
    }
}
//=============================================================================
static void
unsignedIntegerSumAsDouble(const uint64* sp, double* dp, indexType elementCount)
{
    if (dp == nullptr) {
        return;
    }
    double sum = 0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for reduction(+ : sum)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; ++i) {
        sum += (double)sp[i];
    }
    dp[0] = sum;
}
//=============================================================================
static void
unsignedIntegerSumAsDouble(
    const uint64* sp, double* dp, size_t planes, size_t planesize, size_t linesize)
{
    for (size_t i = 0; i < planes; i++) {
        for (size_t j = 0; j < planesize; j++) {
            double accum = 0;
            for (size_t k = 0; k < linesize; k++) {
                uint64 val = sp[i * planesize * linesize + j + k * planesize];
                accum += val;
            }
            dp[i * planesize + j] = accum;
        }
    }
}
//=============================================================================
template <class T>
void
RealSumT(const T* sp, T* dp, indexType elementCount, bool withnan)
{
    if (dp == nullptr) {
        return;
    }
    T sum = (T)0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for reduction(+ : sum)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; i++) {
        T val = sp[i];
        if (!withnan) {
            if (!std::isnan(val)) {
                sum += val;
            }
        } else {
            sum += val;
        }
    }
    dp[0] = sum;
}
//=============================================================================
template <class T>
void
RealSumT(const T* sp, T* dp, size_t planes, size_t planesize, size_t linesize, bool withnan)
{
    for (size_t i = 0; i < planes; i++) {
        for (size_t j = 0; j < planesize; j++) {
            T accum = 0;
            for (size_t k = 0; k < linesize; k++) {
                T val = sp[i * planesize * linesize + j + k * planesize];
                if (!withnan) {
                    if (!std::isnan(val)) {
                        accum += val;
                    }
                } else {
                    accum += val;
                }
            }
            dp[i * planesize + j] = accum;
        }
    }
}
//=============================================================================
template <class T>
void
ComplexSumT(const T* sp, T* dp, indexType elementCount, bool withnan)
{
    if (dp == nullptr) {
        return;
    }
    T sum_r = (T)0;
    T sum_i = (T)0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for reduction(+ : sum_r, sum_i)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; i++) {
        T vr = sp[i];
        T vi = sp[i + 1];
        if (!withnan) {
            if (!std::isnan(vr) && !std::isnan(vi)) {
                sum_r += vr;
                sum_i += vi;
            }
        } else {
            sum_r += vr;
            sum_i += vi;
        }
    }
    dp[0] = sum_r;
    dp[1] = sum_i;
}
//=============================================================================
template <class T>
void
ComplexSumT(const T* sp, T* dp, size_t planes, size_t planesize, size_t linesize, bool withnan)
{
    for (size_t i = 0; i < planes; i++) {
        for (size_t j = 0; j < planesize; j++) {
            T accum_r = 0;
            T accum_i = 0;
            for (size_t k = 0; k < linesize; k++) {
                T vr = sp[2 * (i * planesize * linesize + j + k * planesize)];
                T vi = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                if (!withnan) {
                    if (!std::isnan(vr) && !std::isnan(vi)) {
                        accum_r += vr;
                        accum_i += vi;
                    }
                } else {
                    accum_r += vr;
                    accum_i += vi;
                }
            }
            dp[2 * (i * planesize + j)] = accum_r;
            dp[2 * (i * planesize + j) + 1] = accum_i;
        }
    }
}
//=============================================================================
ArrayOf
Sum(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan)
{
    ArrayOf res;
    Class classA = A.getDataClass();
    if (classA < NLS_LOGICAL || A.isSparse() || classA == NLS_CHAR) {
        std::wstring classname;
        ClassName(A, classname);
        std::wstring msg = _W("function") + L" " + classname + L"_sum" + L" " + _W("undefined.");
        Error(msg);
    }
    if (A.isEmpty(true) && A.is2D()) {
        res = ArrayOf::doubleConstructor(0);
    } else if (A.isScalar()) {
        res = A;
    } else {
        Dimensions dimsA = A.getDimensions();
        indexType workDim;
        if (d == 0) {
            size_t l = 0;
            while (dimsA[l] == 1) {
                l++;
            }
            workDim = l;
        } else {
            workDim = d - 1;
        }
        Dimensions dimsRes = dimsA;
        dimsRes.setDimensionLength(workDim, 1);
        dimsRes.simplify();
        size_t planecount;
        size_t planesize = 1;
        size_t linesize = dimsA[workDim];
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
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_UINT64: {
            A.promoteType(NLS_UINT64);
            double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(
                NLS_DOUBLE, dimsRes.getElementCount(), stringVector(), true));
            res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
            if (A.isVector()) {
                unsignedIntegerSumAsDouble(
                    static_cast<const uint64*>(A.getDataPointer()), ptr, dimsA.getElementCount());
            } else {
                unsignedIntegerSumAsDouble(static_cast<const uint64*>(A.getDataPointer()), ptr,
                    planecount, planesize, linesize);
            }
        } break;
        case NLS_INT8:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_INT64: {
            A.promoteType(NLS_INT64);
            double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(
                NLS_DOUBLE, dimsRes.getElementCount(), stringVector(), true));
            res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
            if (A.isVector()) {
                signedIntegerSumAsDouble(
                    static_cast<const int64*>(A.getDataPointer()), ptr, dimsA.getElementCount());
            } else {
                signedIntegerSumAsDouble(static_cast<const int64*>(A.getDataPointer()), ptr,
                    planecount, planesize, linesize);
            }
        } break;
        case NLS_SINGLE: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                RealSumT<single>(static_cast<const single*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), withnan);
            } else {
                RealSumT<single>(static_cast<const single*>(A.getDataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DOUBLE: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                RealSumT<double>(static_cast<const double*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), withnan);
            } else {
                RealSumT<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_SCOMPLEX: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                ComplexSumT<single>(static_cast<const single*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), withnan);
            } else {
                ComplexSumT<single>(static_cast<const single*>(A.getDataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DCOMPLEX: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.getElementCount(), stringVector(), true));
            if (A.isVector()) {
                ComplexSumT<double>(static_cast<const double*>(A.getDataPointer()), ptr,
                    dimsA.getElementCount(), withnan);
            } else {
                ComplexSumT<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        default: {
            std::wstring classname;
            ClassName(A, classname);
            std::wstring msg
                = _W("function") + L" " + classname + L"_sum" + L" " + _W("undefined.");
            Error(msg);
        } break;
        }
    }
    if (strtype == L"default") {
        if (classA == NLS_DOUBLE || classA == NLS_SINGLE || classA == NLS_DCOMPLEX
            || classA == NLS_SCOMPLEX) {
            res.promoteType(classA);
        } else {
            res.promoteType(NLS_DOUBLE);
        }
    } else if (strtype == L"native") {
        res.promoteType(classA);
    } else if (strtype == L"double") {
        if (res.getDataClass() == NLS_SCOMPLEX || res.getDataClass() == NLS_DCOMPLEX) {
            res.promoteType(NLS_DCOMPLEX);
        } else {
            res.promoteType(NLS_DOUBLE);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
