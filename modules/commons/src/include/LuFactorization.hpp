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
#include <algorithm>
#include <cmath>
#include <complex>
#include <utility>
#include "LapackPrototypes.hpp"
//=============================================================================
// Products and sums below must round one by one: no fused multiply-add.
#if defined(__clang__)
#define NLS_LU_NO_FMA_ATTRIBUTE
#define NLS_LU_NO_FMA_BODY _Pragma("clang fp contract(off)")
#elif defined(__GNUC__)
#define NLS_LU_NO_FMA_ATTRIBUTE __attribute__((optimize("fp-contract=off")))
#define NLS_LU_NO_FMA_BODY
#else
#define NLS_LU_NO_FMA_ATTRIBUTE
#define NLS_LU_NO_FMA_BODY
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
// Largest row or column count handled by the unblocked kernels.
template <class T>
constexpr lapack_int
smallLuLimit()
{
    return 16;
}
//=============================================================================
template <>
constexpr lapack_int
smallLuLimit<std::complex<double>>()
{
    return 8;
}
//=============================================================================
template <>
constexpr lapack_int
smallLuLimit<std::complex<float>>()
{
    return 8;
}
//=============================================================================
template <class T>
inline bool
isSmallLu(lapack_int m, lapack_int n)
{
    return m <= smallLuLimit<T>() && n <= smallLuLimit<T>();
}
//=============================================================================
template <class T>
inline T
luModulus(const T& a)
{
    return std::abs(a);
}
//=============================================================================
template <class T>
inline T
luModulus(const std::complex<T>& a)
{
    return std::abs(a.real()) + std::abs(a.imag());
}
//=============================================================================
template <class T>
inline bool
luIsZero(const T& a)
{
    return a == T(0);
}
//=============================================================================
template <class T>
inline bool
luIsZero(const std::complex<T>& a)
{
    return a.real() == T(0) && a.imag() == T(0);
}
//=============================================================================
template <class T>
inline T
luConj(const T& a)
{
    return a;
}
//=============================================================================
template <class T>
inline std::complex<T>
luConj(const std::complex<T>& a)
{
    return std::conj(a);
}
//=============================================================================
// c - a * b
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline T
luMulSub(const T& c, const T& a, const T& b)
{
    NLS_LU_NO_FMA_BODY
    T product = a * b;
    return c - product;
}
//=============================================================================
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline std::complex<T>
luMulSub(const std::complex<T>& c, const std::complex<T>& a, const std::complex<T>& b)
{
    NLS_LU_NO_FMA_BODY
    T productRe = a.real() * b.real() - a.imag() * b.imag();
    T productIm = a.real() * b.imag() + a.imag() * b.real();
    return std::complex<T>(c.real() - productRe, c.imag() - productIm);
}
//=============================================================================
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline T
luMul(const T& a, const T& b)
{
    NLS_LU_NO_FMA_BODY
    return a * b;
}
//=============================================================================
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline std::complex<T>
luMul(const std::complex<T>& a, const std::complex<T>& b)
{
    NLS_LU_NO_FMA_BODY
    T productRe = a.real() * b.real() - a.imag() * b.imag();
    T productIm = a.real() * b.imag() + a.imag() * b.real();
    return std::complex<T>(productRe, productIm);
}
//=============================================================================
template <class T>
inline T
luDivide(const T& a, const T& b)
{
    return a / b;
}
//=============================================================================
// Part of a complex quotient by an exact zero: signed Inf, or NaN for a zero or NaN part.
template <class T>
inline T
luOverZero(T part)
{
    return (part == T(0) || std::isnan(part)) ? std::numeric_limits<T>::quiet_NaN()
                                              : std::copysign(std::numeric_limits<T>::infinity(), part);
}
//=============================================================================
// Complex division by Smith's method.
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline std::complex<T>
luDivide(const std::complex<T>& a, const std::complex<T>& b)
{
    NLS_LU_NO_FMA_BODY
    T ar = a.real();
    T ai = a.imag();
    T br = b.real();
    T bi = b.imag();
    if (br == T(0) && bi == T(0)) {
        return std::complex<T>(luOverZero(ar), luOverZero(ai));
    }
    if (std::abs(br) >= std::abs(bi)) {
        T ratio = bi / br;
        T scale = br + bi * ratio;
        return std::complex<T>((ar + ai * ratio) / scale, (ai - ar * ratio) / scale);
    }
    T ratio = br / bi;
    T scale = br * ratio + bi;
    return std::complex<T>((ar * ratio + ai) / scale, (ai * ratio - ar) / scale);
}
//=============================================================================
// Unblocked LU with partial pivoting like ?getf2: pivot division, no step is skipped.
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline void
smallLuFactor(lapack_int m, lapack_int n, T* a, lapack_int lda, lapack_int* ipiv, lapack_int* info)
{
    *info = 0;
    lapack_int steps = std::min(m, n);
    for (lapack_int k = 0; k < steps; ++k) {
        T* colK = a + (size_t)k * (size_t)lda;
        lapack_int p = k;
        auto best = luModulus(colK[k]);
        for (lapack_int i = k + 1; i < m; ++i) {
            auto value = luModulus(colK[i]);
            if (value > best) {
                best = value;
                p = i;
            }
        }
        ipiv[k] = p + 1;
        if (!luIsZero(colK[p])) {
            if (p != k) {
                for (lapack_int j = 0; j < n; ++j) {
                    std::swap(a[k + (size_t)j * lda], a[p + (size_t)j * lda]);
                }
            }
            T pivot = colK[k];
            for (lapack_int i = k + 1; i < m; ++i) {
                colK[i] = luDivide(colK[i], pivot);
            }
        } else if (*info == 0) {
            *info = k + 1;
        }
        for (lapack_int j = k + 1; j < n; ++j) {
            T* colJ = a + (size_t)j * (size_t)lda;
            T ukj = colJ[k];
            for (lapack_int i = k + 1; i < m; ++i) {
                colJ[i] = luMulSub(colJ[i], colK[i], ukj);
            }
        }
    }
}
//=============================================================================
// Solves A * x = b in place with a small factor of order n (b holds nrhs columns).
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline void
smallLuSolve(const T* lu, lapack_int n, const lapack_int* ipiv, T* x, lapack_int nrhs)
{
    for (lapack_int col = 0; col < nrhs; ++col) {
        T* b = x + (size_t)col * (size_t)n;
        for (lapack_int i = 0; i < n; ++i) {
            lapack_int p = ipiv[i] - 1;
            if (p != i) {
                std::swap(b[i], b[p]);
            }
        }
        for (lapack_int k = 0; k < n; ++k) {
            for (lapack_int i = k + 1; i < n; ++i) {
                b[i] = luMulSub(b[i], b[k], lu[i + (size_t)k * n]);
            }
        }
        for (lapack_int k = n; k-- > 0;) {
            b[k] = luDivide(b[k], lu[k + (size_t)k * n]);
            for (lapack_int i = 0; i < k; ++i) {
                b[i] = luMulSub(b[i], b[k], lu[i + (size_t)k * n]);
            }
        }
    }
}
//=============================================================================
// Solves y * A = c in place for each row c, stored conjugated as the columns of x.
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline void
smallLuSolveRows(const T* lu, lapack_int n, const lapack_int* ipiv, T* x, lapack_int nrhs)
{
    for (lapack_int col = 0; col < nrhs; ++col) {
        T* z = x + (size_t)col * (size_t)n;
        for (lapack_int j = 0; j < n; ++j) {
            z[j] = luConj(z[j]);
        }
        for (lapack_int j = 0; j < n; ++j) {
            T t = z[j];
            for (lapack_int k = 0; k < j; ++k) {
                t = luMulSub(t, z[k], lu[k + (size_t)j * n]);
            }
            z[j] = luDivide(t, lu[j + (size_t)j * n]);
        }
        for (lapack_int j = n; j-- > 0;) {
            T t = z[j];
            for (lapack_int k = j + 1; k < n; ++k) {
                t = luMulSub(t, z[k], lu[k + (size_t)j * n]);
            }
            z[j] = t;
        }
        for (lapack_int k = n; k-- > 0;) {
            lapack_int p = ipiv[k] - 1;
            if (p != k) {
                std::swap(z[k], z[p]);
            }
        }
        for (lapack_int j = 0; j < n; ++j) {
            z[j] = luConj(z[j]);
        }
    }
}
//=============================================================================
// Solves T * x = b in place for a small triangular T with a nonzero diagonal.
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline void
smallTriangularSolve(const T* t, lapack_int n, bool upper, T* x, lapack_int nrhs)
{
    for (lapack_int col = 0; col < nrhs; ++col) {
        T* b = x + (size_t)col * (size_t)n;
        for (lapack_int step = 0; step < n; ++step) {
            lapack_int k = upper ? n - 1 - step : step;
            b[k] = luDivide(b[k], t[k + (size_t)k * n]);
            lapack_int first = upper ? 0 : k + 1;
            lapack_int last = upper ? k : n;
            for (lapack_int i = first; i < last; ++i) {
                b[i] = luMulSub(b[i], b[k], t[i + (size_t)k * n]);
            }
        }
    }
}
//=============================================================================
// Solves y * T = c in place for each row c, stored conjugated as the columns of x.
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline void
smallTriangularSolveRows(const T* t, lapack_int n, bool upper, T* x, lapack_int nrhs)
{
    for (lapack_int col = 0; col < nrhs; ++col) {
        T* z = x + (size_t)col * (size_t)n;
        for (lapack_int j = 0; j < n; ++j) {
            z[j] = luConj(z[j]);
        }
        for (lapack_int step = 0; step < n; ++step) {
            lapack_int j = upper ? step : n - 1 - step;
            lapack_int first = upper ? 0 : j + 1;
            lapack_int last = upper ? j : n;
            T s = z[j];
            for (lapack_int k = first; k < last; ++k) {
                s = luMulSub(s, z[k], t[k + (size_t)j * n]);
            }
            z[j] = luDivide(s, t[j + (size_t)j * n]);
        }
        for (lapack_int j = 0; j < n; ++j) {
            z[j] = luConj(z[j]);
        }
    }
}
//=============================================================================
// Drop-in ?getrf: small matrices use the unblocked kernel, others LAPACK.
inline void
luFactor(const lapack_int* m, const lapack_int* n, double* a, const lapack_int* lda,
    lapack_int* ipiv, lapack_int* info)
{
    if (isSmallLu<double>(*m, *n)) {
        smallLuFactor<double>(*m, *n, a, *lda, ipiv, info);
    } else {
        LAPACK_dgetrf(m, n, a, lda, ipiv, info);
    }
}
//=============================================================================
inline void
luFactor(const lapack_int* m, const lapack_int* n, float* a, const lapack_int* lda,
    lapack_int* ipiv, lapack_int* info)
{
    if (isSmallLu<float>(*m, *n)) {
        smallLuFactor<float>(*m, *n, a, *lda, ipiv, info);
    } else {
        LAPACK_sgetrf(m, n, a, lda, ipiv, info);
    }
}
//=============================================================================
inline void
luFactor(const lapack_int* m, const lapack_int* n, std::complex<double>* a, const lapack_int* lda,
    lapack_int* ipiv, lapack_int* info)
{
    if (isSmallLu<std::complex<double>>(*m, *n)) {
        smallLuFactor<std::complex<double>>(*m, *n, a, *lda, ipiv, info);
    } else {
        LAPACK_zgetrf(m, n, a, lda, ipiv, info);
    }
}
//=============================================================================
inline void
luFactor(const lapack_int* m, const lapack_int* n, std::complex<float>* a, const lapack_int* lda,
    lapack_int* ipiv, lapack_int* info)
{
    if (isSmallLu<std::complex<float>>(*m, *n)) {
        smallLuFactor<std::complex<float>>(*m, *n, a, *lda, ipiv, info);
    } else {
        LAPACK_cgetrf(m, n, a, lda, ipiv, info);
    }
}
//=============================================================================
// Determinant from a ?getrf factor: diagonal product first to last, then the swap sign.
template <class T>
NLS_LU_NO_FMA_ATTRIBUTE inline T
luDeterminant(const T* factor, lapack_int n, const lapack_int* ipiv)
{
    T product = n > 0 ? factor[0] : T(1);
    bool negative = n > 0 && ipiv[0] != 1;
    for (lapack_int k = 1; k < n; ++k) {
        product = luMul(product, factor[k + (size_t)k * n]);
        negative = negative != (ipiv[k] != k + 1);
    }
    return negative ? -product : product;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
#undef NLS_LU_NO_FMA_ATTRIBUTE
#undef NLS_LU_NO_FMA_BODY
//=============================================================================
