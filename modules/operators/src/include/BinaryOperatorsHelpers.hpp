//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <array>
#include <cmath>
#include <complex>
#include <limits>
#include <string>
#include "ArrayOf.hpp"
#include "MatrixCheck.hpp"
#include "nlsOperators_exports.h"
//=============================================================================
#if defined(__clang__)
#define NLS_QUOTIENT_NO_FMA_ATTRIBUTE
#define NLS_QUOTIENT_NO_FMA_BODY _Pragma("clang fp contract(off)")
#elif defined(__GNUC__)
#define NLS_QUOTIENT_NO_FMA_ATTRIBUTE __attribute__((optimize("fp-contract=off")))
#define NLS_QUOTIENT_NO_FMA_BODY
#else
#define NLS_QUOTIENT_NO_FMA_ATTRIBUTE
#define NLS_QUOTIENT_NO_FMA_BODY
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
struct BinaryBroadcast
{
    Dimensions dimensions;
    std::array<indexType, MAXDIMS> outputLengths {};
    std::array<indexType, MAXDIMS> aStrides {};
    std::array<indexType, MAXDIMS> bStrides {};
    indexType nDims { 0 };
};
//=============================================================================
NLSOPERATORS_IMPEXP ArrayOf
binaryOperatorEmptyMatrixEmptyMatrix(
    const ArrayOf& A, const ArrayOf& B, NelsonType commonClass, const std::string& operatorName);
//=============================================================================
NLSOPERATORS_IMPEXP BinaryBroadcast
binaryOperatorBroadcast(const ArrayOf& A, const ArrayOf& B, const std::string& operatorName);
//=============================================================================
inline indexType
binaryOperatorBroadcastIndex(const BinaryBroadcast& info, indexType linearIndex,
    const std::array<indexType, MAXDIMS>& strides)
{
    indexType index = 0;
    indexType remaining = linearIndex;
    for (indexType k = 0; k < info.nDims; ++k) {
        indexType coord = remaining % info.outputLengths[k];
        remaining /= info.outputLengths[k];
        index += coord * strides[k];
    }
    return index;
}
//=============================================================================
template <class T, class Operation>
ArrayOf
binaryOperatorBroadcastNumeric(const ArrayOf& A, const ArrayOf& B, NelsonType destinationClass,
    const std::string& operatorName, Operation operation)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, operatorName);
    indexType elementCount = info.dimensions.elementCount();
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(destinationClass, elementCount));
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    for (indexType k = 0; k < elementCount; ++k) {
        indexType indexA = binaryOperatorBroadcastIndex(info, k, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, k, info.bStrides);
        ptrC[k] = operation(ptrA[indexA], ptrB[indexB]);
    }
    return ArrayOf(destinationClass, info.dimensions, ptrC, false);
}
//=============================================================================
template <class T, class Operation>
ArrayOf
binaryOperatorBroadcastComplex(const ArrayOf& A, const ArrayOf& B, NelsonType destinationClass,
    const std::string& operatorName, Operation operation)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, operatorName);
    indexType elementCount = info.dimensions.elementCount();
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(destinationClass, elementCount));
    const auto* ptrA = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    const auto* ptrB = reinterpret_cast<const std::complex<T>*>(B.dataPointer());
    auto* ptrZC = reinterpret_cast<std::complex<T>*>(ptrC);
    for (indexType k = 0; k < elementCount; ++k) {
        indexType indexA = binaryOperatorBroadcastIndex(info, k, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, k, info.bStrides);
        ptrZC[k] = operation(ptrA[indexA], ptrB[indexB]);
    }
    return ArrayOf(destinationClass, info.dimensions, ptrC, false);
}
//=============================================================================
using BinaryOperatorKernel = ArrayOf (*)(NelsonType, const ArrayOf&, const ArrayOf&);
//=============================================================================
template <class Fallback>
ArrayOf
binaryOperator2DDispatch(NelsonType destinationClass, const ArrayOf& A, const ArrayOf& B,
    BinaryOperatorKernel scalarScalar, BinaryOperatorKernel scalarMatrix,
    BinaryOperatorKernel matrixScalar, BinaryOperatorKernel matrixMatrix,
    BinaryOperatorKernel rowColumn, BinaryOperatorKernel columnRow, BinaryOperatorKernel rowMatrix,
    BinaryOperatorKernel matrixRow, BinaryOperatorKernel columnMatrix,
    BinaryOperatorKernel matrixColumn, Fallback fallback)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        if (A.isScalar()) {
            return scalarScalar(destinationClass, A, B);
        }
        return matrixMatrix(destinationClass, A, B);
    }
    if (A.isScalar() || B.isScalar()) {
        if (A.isScalar()) {
            return scalarMatrix(destinationClass, A, B);
        }
        return matrixScalar(destinationClass, A, B);
    }
    if (A.isVector() || B.isVector()) {
        if ((A.isRowVector() && B.isRowVector()) || (A.isColumnVector() && B.isColumnVector())) {
            return fallback(A, B, destinationClass);
        }
        if (A.isRowVector() && B.isColumnVector()) {
            return rowColumn(destinationClass, A, B);
        }
        if (A.isColumnVector() && B.isRowVector()) {
            return columnRow(destinationClass, A, B);
        }
        if (A.rows() == B.rows()) {
            if (A.isVector()) {
                if (!B.is2D()) {
                    return fallback(A, B, destinationClass);
                }
                return rowMatrix(destinationClass, A, B);
            }
            if (!A.is2D()) {
                return fallback(A, B, destinationClass);
            }
            return matrixRow(destinationClass, A, B);
        }
        if (A.columns() == B.columns()) {
            if (A.isVector()) {
                if (!B.is2D()) {
                    return fallback(A, B, destinationClass);
                }
                return columnMatrix(destinationClass, A, B);
            }
            if (!A.is2D()) {
                return fallback(A, B, destinationClass);
            }
            return matrixColumn(destinationClass, A, B);
        }
        return fallback(A, B, destinationClass);
    }
    return fallback(A, B, destinationClass);
}
//=============================================================================
// Complex by real (finite, nonzero) products and quotients are computed part by part.
template <class T>
inline bool
realFactorsAreFiniteNonZero(const T* values, indexType count)
{
    for (indexType k = 0; k < count; ++k) {
        if (!std::isfinite(values[k]) || values[k] == T(0)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
// Complex times real c (finite, nonzero): part by part; a NaN spreads to both parts when
// both input parts are nonzero.
template <class T>
inline std::complex<T>
complexTimesRealScalar(std::complex<T> z, T c)
{
    T re = z.real() * c;
    T im = z.imag() * c;
    if ((std::isnan(re) || std::isnan(im)) && z.real() != 0 && z.imag() != 0) {
        re = std::numeric_limits<T>::quiet_NaN();
        im = re;
    }
    return std::complex<T>(re, im);
}
//=============================================================================
// Quotients needing the special rules (real or imaginary divisor, Inf or NaN part), branch-free;
// an overflowing sum or underflowing product only sends a finite case to the exact path.
template <class T>
inline bool
complexQuotientIsSpecial(std::complex<T> n, std::complex<T> z)
{
    T sum = n.real() + n.imag() + z.real() + z.imag();
    return !std::isfinite(sum) | (z.real() * z.imag() == 0);
}
//=============================================================================
// Smith's method rounded step by step (no fused multiply-add), as the LU kernels divide.
template <class T>
NLS_QUOTIENT_NO_FMA_ATTRIBUTE inline std::complex<T>
complexQuotientSmith(std::complex<T> n, std::complex<T> z)
{
    NLS_QUOTIENT_NO_FMA_BODY
    T a = n.real();
    T b = n.imag();
    T c = z.real();
    T d = z.imag();
    if (std::fabs(c) >= std::fabs(d)) {
        T r = d / c;
        T den = c + d * r;
        return std::complex<T>((a + b * r) / den, (b - a * r) / den);
    }
    T r = c / d;
    T den = c * r + d;
    return std::complex<T>((a * r + b) / den, (b * r - a) / den);
}
//=============================================================================
// Real or imaginary divisor: part by part, an exact zero part paired with a nonzero one stays
// 0. Otherwise a NaN gives NaN + NaNi and an infinite divisor of a finite value gives 0.
template <class T>
inline std::complex<T>
complexQuotientSpecial(std::complex<T> n, std::complex<T> z)
{
    T a = n.real();
    T b = n.imag();
    T c = z.real();
    T d = z.imag();
    if (d == 0) {
        return std::complex<T>((a == 0 && b != 0) ? (T)0 : a / c, b == 0 ? (T)0 : b / c);
    }
    if (c == 0) {
        return std::complex<T>((b == 0 && a != 0) ? (T)0 : b / d, a == 0 ? (T)0 : -a / d);
    }
    if (std::isnan(a) || std::isnan(b) || std::isnan(c) || std::isnan(d)) {
        T nan = std::numeric_limits<T>::quiet_NaN();
        return std::complex<T>(nan, nan);
    }
    if (std::isinf(c) || std::isinf(d)) {
        if (std::isinf(a) || std::isinf(b)) {
            T nan = std::numeric_limits<T>::quiet_NaN();
            return std::complex<T>(nan, nan);
        }
        return std::complex<T>((T)0, (T)0);
    }
    // infinite numerator, finite divisor
    return complexQuotientSmith<T>(n, z);
}
//=============================================================================
template <class T>
inline std::complex<T>
complexQuotient(std::complex<T> n, std::complex<T> z)
{
    return complexQuotientIsSpecial<T>(n, z) ? complexQuotientSpecial<T>(n, z)
                                              : complexQuotientSmith<T>(n, z);
}
//=============================================================================
// Element quotient for real or complex values (complex ones follow complexQuotient).
template <class T>
inline T
divideValues(T n, T d)
{
    return n / d;
}
template <class T>
inline std::complex<T>
divideValues(std::complex<T> n, std::complex<T> d)
{
    return complexQuotient<T>(n, d);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
