//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4101)
#endif
//=============================================================================
#include "LUMatrixFactorization.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if WITH_OPENMP
#include <omp.h>
#endif
#include "LapackPrototypes.hpp"
#include <Eigen/Dense>
#include <algorithm>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Eye.hpp"
#include "LinearAlgebraHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#if WITH_OPENMP
class ScopedOpenMPThreads
{
    int previousThreadCount = 1;
    bool changed = false;

public:
    explicit ScopedOpenMPThreads(int requestedThreadCount)
    {
        previousThreadCount = omp_get_max_threads();
        if (requestedThreadCount > 0 && requestedThreadCount < previousThreadCount) {
            omp_set_num_threads(requestedThreadCount);
            changed = true;
        }
    }

    ~ScopedOpenMPThreads()
    {
        if (changed) {
            omp_set_num_threads(previousThreadCount);
        }
    }
};
//=============================================================================
static int
luThreadCount(int nrows, int ncols)
{
    int currentThreadCount = omp_get_max_threads();
    if ((size_t)nrows * (size_t)ncols <= 1024 * 1024 && currentThreadCount > 1) {
        return 1;
    }
    return currentThreadCount;
}
#else
class ScopedOpenMPThreads
{
public:
    explicit ScopedOpenMPThreads(int) { }
};
//=============================================================================
static int
luThreadCount(int, int)
{
    return 1;
}
#endif
//=============================================================================
/**
 * Builds the row permutation (nrows entries) from the LAPACK row-swap list 'piv'
 * (1-based, length p). The caller frees the result with delete[].
 */
int*
updatePivotVector(int nrows, int* piv, int p)
{
    int* fullpivot = nullptr;
    try {
        fullpivot = new int[nrows];
    } catch (std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    OMP_PARALLEL_FOR_LOOP(nrows)
    for (ompIndexType i = 0; i < (ompIndexType)nrows; i++) {
        fullpivot[i] = (int)i;
    }

    for (ompIndexType i = 0; i < (ompIndexType)p; i++) {
        if (piv[i] != (i + 1)) {
            std::swap(fullpivot[i], fullpivot[piv[i] - 1]);
        }
    }
    return fullpivot;
}
//=============================================================================
static bool
hasPivotInterchanges(const std::vector<lapack_int>& piv)
{
    for (size_t k = 0; k < piv.size(); ++k) {
        if (piv[k] != static_cast<lapack_int>(k + 1)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
template <class T>
void
RealLUWithoutPivot(int nrows, int ncols, T* l, T* u, const T* a)
{
    if (nrows > ncols) {
        ompIndexType lrows = nrows;
        ompIndexType lcols = ncols;
        ompIndexType urows = ncols;
        ompIndexType ucols = ncols;
        OMP_PARALLEL_FOR_LOOP(lcols)
        for (ompIndexType col = 0; col < lcols; col++) {
            l[col + col * lrows] = 1.0;
            ompIndexType firstRow = col + 1;
            if (firstRow < lrows) {
                std::copy_n(
                    a + firstRow + col * nrows, lrows - firstRow, l + firstRow + col * lrows);
            }
        }
#if WITH_OPENMP
#pragma omp parallel for if (urows > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType col = 0; col < ucols; col++) {
            ompIndexType lastRow = std::min(col, urows - 1);
            std::copy_n(a + col * nrows, lastRow + 1, u + col * urows);
        }
    } else {
        ompIndexType lrows = nrows;
        ompIndexType lcols = nrows;
        ompIndexType urows = nrows;
#if WITH_OPENMP
#pragma omp parallel for if (lrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType col = 0; col < lcols; col++) {
            l[col + col * lrows] = 1.0;
            ompIndexType firstRow = col + 1;
            if (firstRow < lrows) {
                std::copy_n(
                    a + firstRow + col * nrows, lrows - firstRow, l + firstRow + col * lrows);
            }
        }
#if WITH_OPENMP
#pragma omp parallel for if (nrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType col = 0; col < ncols; col++) {
            ompIndexType lastRow = std::min(col, urows - 1);
            std::copy_n(a + col * nrows, lastRow + 1, u + col * urows);
        }
    }
}
//=============================================================================
/**
 * Fills l, u and the permutation matrix pmat from a LAPACK LU of complex 'a'.
 * 'a' is overwritten.
 */
template <class T>
void
ComplexLUP(int nrows, int ncols, T* l, T* u, T* pmat, std::complex<T>* a,
    void (*lapack_fn)(const lapack_int* m, const lapack_int* n, std::complex<T>* a,
        const lapack_int* lda, lapack_int* ipiv, lapack_int* info))
{
    ompIndexType i = 0;
    ompIndexType j = 0;
    int p = std::min(nrows, ncols);
    std::vector<lapack_int> piv(p);
    ScopedOpenMPThreads scopedThreads(luThreadCount(nrows, ncols));
    lapack_int m = nrows;
    lapack_int n = ncols;
    lapack_int lda = nrows;
    lapack_int info = 0;
    lapack_fn(&m, &n, a, &lda, piv.data(), &info);

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);

    OMP_PARALLEL_FOR_LOOP(nrows)
    for (i = 0; i < (ompIndexType)nrows; i++) {
        pmat[i + fullpivot[i] * nrows] = 1;
    }
    delete[] fullpivot;
    fullpivot = nullptr;

    if (nrows > ncols) {
        auto* ptrZl = reinterpret_cast<std::complex<T>*>((T*)l);
        auto* ptrZu = reinterpret_cast<std::complex<T>*>((T*)u);

        ompIndexType lrows = nrows;
        ompIndexType lcols = ncols;
        ompIndexType urows = ncols;
        ompIndexType ucols = ncols;
        OMP_PARALLEL_FOR_LOOP(lcols)
        for (i = 0; i < lcols; i++) {
            ptrZl[(i + i * lrows)].real(1.0);
            ptrZl[(i + i * lrows)].imag(0.0);
        }
#if WITH_OPENMP
#pragma omp parallel for private(i, j) if (lrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < std::min(i, lcols); j++) {
                ptrZl[(i + j * lrows)] = a[(i + j * nrows)];
            }
        }
#if WITH_OPENMP
#pragma omp parallel for private(i, j) if (urows > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 0; i < urows; i++) {
            for (j = i; j < ucols; j++) {
                ptrZu[(i + j * urows)] = a[(i + j * nrows)];
            }
        }
    } else {
        ompIndexType lrows = nrows;
        ompIndexType lcols = nrows;
        ompIndexType urows = nrows;
        auto* ptrZl = reinterpret_cast<std::complex<T>*>((T*)l);
        auto* ptrZu = reinterpret_cast<std::complex<T>*>((T*)u);
#if WITH_OPENMP
#pragma omp parallel for if (lcols > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 0; i < lcols; i++) {
            ptrZl[(i + i * lrows)].real(1.0);
            ptrZl[(i + i * lrows)].imag(0.0);
        }
#if WITH_OPENMP
#pragma omp parallel for private(i, j) if (lrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < i; j++) {
                ptrZl[(i + j * lrows)] = a[(i + j * nrows)];
            }
        }
#if WITH_OPENMP
#pragma omp parallel for private(i, j) if (nrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 0; i < nrows; i++) {
            for (j = i; j < ncols; j++) {
                ptrZu[(i + j * urows)] = a[(i + j * nrows)];
            }
        }
    }
}
//=============================================================================
/**
 * Fills l (nrows x min(nrows, ncols)) and u (min(nrows, ncols) x ncols) from a
 * LAPACK LU of complex 'a'. 'a' is overwritten.
 */
template <class T>
void
ComplexLU(int nrows, int ncols, T* l, T* u, std::complex<T>* a,
    void (*lapack_fn)(const lapack_int* m, const lapack_int* n, std::complex<T>* a,
        const lapack_int* lda, lapack_int* ipiv, lapack_int* info))
{
    ompIndexType i = 0;
    ompIndexType j = 0;
    int p = std::min(nrows, ncols);
    std::vector<lapack_int> piv(p);
    ScopedOpenMPThreads scopedThreads(luThreadCount(nrows, ncols));
    lapack_int m = nrows;
    lapack_int n = ncols;
    lapack_int lda = nrows;
    lapack_int info = 0;
    lapack_fn(&m, &n, a, &lda, piv.data(), &info);

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);
    if (nrows > ncols) {
        auto* ptrZl = reinterpret_cast<std::complex<T>*>((T*)l);
        auto* ptrZu = reinterpret_cast<std::complex<T>*>((T*)u);

        ompIndexType lrows = nrows;
        ompIndexType lcols = ncols;
        ompIndexType urows = ncols;
        ompIndexType ucols = ncols;

#if WITH_OPENMP
#pragma omp parallel for if (lcols > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 0; i < lcols; i++) {
            ptrZl[(fullpivot[i] + i * lrows)].real(1.0);
            ptrZl[(fullpivot[i] + i * lrows)].imag(0.0);
        }
#if WITH_OPENMP
#pragma omp parallel for private(i, j) if (lrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < std::min(i, lcols); j++) {
                ptrZl[(fullpivot[i] + j * lrows)] = a[(i + j * nrows)];
            }
        }
#if WITH_OPENMP
#pragma omp parallel for private(i, j) if (urows > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 0; i < urows; i++) {
            for (j = i; j < ucols; j++) {
                ptrZu[(i + j * urows)] = a[(i + j * nrows)];
            }
        }
    } else {
        auto* ptrZl = reinterpret_cast<std::complex<T>*>((T*)l);
        auto* ptrZu = reinterpret_cast<std::complex<T>*>((T*)u);

        ompIndexType lrows = nrows;
        ompIndexType lcols = nrows;
        ompIndexType urows = nrows;
        OMP_PARALLEL_FOR_LOOP(lcols)
        for (i = 0; i < lcols; i++) {
            ptrZl[(fullpivot[i] + i * lrows)].real(1.0);
            ptrZl[(fullpivot[i] + i * lrows)].imag(0.0);
        }
#if WITH_OPENMP
#pragma omp parallel for private(i, j) if (lrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < i; j++) {
                ptrZl[(fullpivot[i] + j * lrows)] = a[(i + j * nrows)];
            }
        }
#if WITH_OPENMP
#pragma omp parallel for private(i, j) if (nrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (i = 0; i < nrows; i++) {
            for (j = i; j < ncols; j++) {
                ptrZu[(i + j * urows)] = a[(i + j * nrows)];
            }
        }
    }
    if (fullpivot) {
        delete[] fullpivot;
        fullpivot = nullptr;
    }
}
//=============================================================================
/**
 * Fills l, u and the permutation matrix pmat from a LAPACK LU of real 'a'.
 * 'a' is overwritten.
 */
template <class T>
void
RealLUP(int nrows, int ncols, T* l, T* u, T* pmat, T* a,
    void (*lapack_fn)(const lapack_int* m, const lapack_int* n, T* a, const lapack_int* lda,
        lapack_int* ipiv, lapack_int* info))
{
    int p = std::min(nrows, ncols);
    std::vector<lapack_int> piv(p);

    ScopedOpenMPThreads scopedThreads(luThreadCount(nrows, ncols));
    lapack_int m = nrows;
    lapack_int n = ncols;
    lapack_int lda = nrows;
    lapack_int info = 0;
    lapack_fn(&m, &n, a, &lda, piv.data(), &info);

    if (!hasPivotInterchanges(piv)) {
        OMP_PARALLEL_FOR_LOOP(nrows)
        for (ompIndexType i = 0; i < (ompIndexType)nrows; i++) {
            pmat[i + i * nrows] = 1;
        }
        RealLUWithoutPivot(nrows, ncols, l, u, a);
        return;
    }

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);

    OMP_PARALLEL_FOR_LOOP(nrows)
    for (ompIndexType i = 0; i < (ompIndexType)nrows; i++) {
        pmat[i + fullpivot[i] * nrows] = 1;
    }
    if (nrows > ncols) {
        ompIndexType lrows = nrows;
        ompIndexType lcols = ncols;
        ompIndexType urows = ncols;
        ompIndexType ucols = ncols;

        OMP_PARALLEL_FOR_LOOP(lcols)
        for (ompIndexType col = 0; col < lcols; col++) {
            l[col + col * lrows] = 1.0;
            for (ompIndexType row = col + 1; row < lrows; row++) {
                l[row + col * lrows] = a[row + col * nrows];
            }
        }

#if WITH_OPENMP
#pragma omp parallel for if (urows > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType col = 0; col < ucols; col++) {
            ompIndexType lastRow = std::min(col, urows - 1);
            for (ompIndexType row = 0; row <= lastRow; row++) {
                u[row + col * urows] = a[row + col * nrows];
            }
        }
    } else {
        ompIndexType lrows = nrows;
        ompIndexType lcols = nrows;
        ompIndexType urows = nrows;

        OMP_PARALLEL_FOR_LOOP(lcols)
        for (ompIndexType col = 0; col < lcols; col++) {
            l[col + col * lrows] = 1.0;
            for (ompIndexType row = col + 1; row < lrows; row++) {
                l[row + col * lrows] = a[row + col * nrows];
            }
        }
#if WITH_OPENMP
#pragma omp parallel for if (nrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType col = 0; col < ncols; col++) {
            ompIndexType lastRow = std::min(col, urows - 1);
            for (ompIndexType row = 0; row <= lastRow; row++) {
                u[row + col * urows] = a[row + col * nrows];
            }
        }
    }
    if (fullpivot) {
        delete[] fullpivot;
        fullpivot = nullptr;
    }
}
//=============================================================================
/**
 * Fills l and u from a LAPACK LU of real 'a'. 'a' is overwritten.
 */
template <class T>
void
RealLU(int nrows, int ncols, T* l, T* u, T* a,
    void (*lapack_fn)(const lapack_int* m, const lapack_int* n, T* a, const lapack_int* lda,
        lapack_int* ipiv, lapack_int* info))
{
    ompIndexType i = 0;
    ompIndexType j = 0;
    int p = std::min(nrows, ncols);
    std::vector<lapack_int> piv(p);

    ScopedOpenMPThreads scopedThreads(luThreadCount(nrows, ncols));
    lapack_int m = nrows;
    lapack_int n = ncols;
    lapack_int lda = nrows;
    lapack_int info = 0;
    lapack_fn(&m, &n, a, &lda, piv.data(), &info);

    if (!hasPivotInterchanges(piv)) {
        RealLUWithoutPivot(nrows, ncols, l, u, a);
        return;
    }

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);

    if (nrows > ncols) {
        ompIndexType lrows = nrows;
        ompIndexType lcols = ncols;
        ompIndexType urows = ncols;
        ompIndexType ucols = ncols;
        OMP_PARALLEL_FOR_LOOP(lcols)
        for (ompIndexType col = 0; col < lcols; col++) {
            l[fullpivot[col] + col * lrows] = 1.0;
            for (ompIndexType row = col + 1; row < lrows; row++) {
                l[fullpivot[row] + col * lrows] = a[row + col * nrows];
            }
        }
#if WITH_OPENMP
#pragma omp parallel for if (urows > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType col = 0; col < ucols; col++) {
            ompIndexType lastRow = std::min(col, urows - 1);
            for (ompIndexType row = 0; row <= lastRow; row++) {
                u[row + col * urows] = a[row + col * nrows];
            }
        }
    } else {
        ompIndexType lrows = nrows;
        ompIndexType lcols = nrows;
        ompIndexType urows = nrows;
#if WITH_OPENMP
#pragma omp parallel for if (lrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType col = 0; col < lcols; col++) {
            l[fullpivot[col] + col * lrows] = 1.0;
            for (ompIndexType row = col + 1; row < lrows; row++) {
                l[fullpivot[row] + col * lrows] = a[row + col * nrows];
            }
        }
#if WITH_OPENMP
#pragma omp parallel for if (nrows > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType col = 0; col < ncols; col++) {
            ompIndexType lastRow = std::min(col, urows - 1);
            for (ompIndexType row = 0; row <= lastRow; row++) {
                u[row + col * urows] = a[row + col * nrows];
            }
        }
    }
    if (fullpivot) {
        delete[] fullpivot;
        fullpivot = nullptr;
    }
}
//=============================================================================
/**
 * LU factorization of A. nLhs <= 1 returns the factored matrix, nLhs == 2 returns L and U,
 * nLhs == 3 also returns the nrows x nrows permutation matrix. NaN or Inf in A is an error.
 */
ArrayOfVector
LUMatrixFactorizationDoubleReal(const ArrayOf& A, int nLhs)
{
    ArrayOfVector retval;

    if (!isAllFinite<double>(A)) {
        Error("Nelson:lu:matrixWithNaNInf", "%s must not contain NaN or Inf.", "Input to LU");
    }
    int nrows = (int)A.rows();
    int ncols = (int)A.columns();
    int p = std::min(nrows, ncols);

    if (A.isEmpty()) {
        if (nLhs <= 1) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, ncols), nullptr);
            return retval;
        }
        if (nLhs <= 2) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), nullptr);

        } else if (nLhs == 3) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), nullptr);
            retval << Eye(nrows, nrows, NLS_DOUBLE, false);
        }
        return retval;
    }

    if (nLhs <= 1) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        lapack_int m = nrows;
        lapack_int n = ncols;
        lapack_int lda = nrows;
        lapack_int info = 0;
        std::vector<lapack_int> piv((size_t)p);
        ScopedOpenMPThreads scopedThreads(luThreadCount(nrows, ncols));
        LAPACK_dgetrf(&m, &n, (double*)AA.dataPointer(), &lda, piv.data(), &info);
        retval << AA;
        return retval;
    }

    double* l = (double*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * p, stringVector(), true);
    double* u = (double*)ArrayOf::allocateArrayOf(A.dataClass(), p * ncols, stringVector(), true);
    if (nLhs == 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLU<double>(nrows, ncols, l, u, (double*)AA.dataPointer(), LAPACK_dgetrf);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        indexType length = (indexType)(nrows * nrows);
        double* piv
            = (double*)ArrayOf::allocateArrayOf(A.dataClass(), length, stringVector(), true);

        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLUP<double>(nrows, ncols, l, u, piv, (double*)AA.dataPointer(), LAPACK_dgetrf);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, nrows), piv);
    }
    return retval;
}
//=============================================================================
/**
 * LU factorization of A. nLhs <= 1 returns the factored matrix, nLhs == 2 returns L and U,
 * nLhs == 3 also returns the nrows x nrows permutation matrix. NaN or Inf in A is an error.
 */
ArrayOfVector
LUMatrixFactorizationSingleReal(const ArrayOf& A, int nLhs)
{
    if (!isAllFinite<single>(A)) {
        Error("Nelson:lu:matrixWithNaNInf", "%s must not contain NaN or Inf.", "Input to LU");
    }
    ArrayOfVector retval;

    int nrows = (int)A.rows();
    int ncols = (int)A.columns();
    int p = std::min(nrows, ncols);

    if (A.isEmpty()) {
        if (nLhs <= 1) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, ncols), nullptr);
            return retval;
        }
        if (nLhs <= 2) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), nullptr);

        } else if (nLhs == 3) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), nullptr);
            retval << Eye(nrows, nrows, NLS_SINGLE, false);
        }
        return retval;
    }

    if (nLhs <= 1) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        lapack_int m = nrows;
        lapack_int n = ncols;
        lapack_int lda = nrows;
        lapack_int info = 0;
        std::vector<lapack_int> piv((size_t)p);
        ScopedOpenMPThreads scopedThreads(luThreadCount(nrows, ncols));
        LAPACK_sgetrf(&m, &n, (single*)AA.dataPointer(), &lda, piv.data(), &info);
        retval << AA;
        return retval;
    }

    single* l = (single*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * p, stringVector(), true);
    single* u = (single*)ArrayOf::allocateArrayOf(A.dataClass(), p * ncols, stringVector(), true);
    if (nLhs == 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLU<single>(nrows, ncols, l, u, (single*)AA.dataPointer(), LAPACK_sgetrf);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        single* piv
            = (single*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * nrows, stringVector(), true);

        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLUP<single>(nrows, ncols, l, u, piv, (single*)AA.dataPointer(), LAPACK_sgetrf);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, nrows), piv);
    }
    return retval;
}
//=============================================================================
/**
 * LU factorization of A. nLhs <= 1 returns the factored matrix, nLhs == 2 returns L and U,
 * nLhs == 3 also returns the nrows x nrows permutation matrix. NaN or Inf in A is an error.
 */
ArrayOfVector
LUMatrixFactorizationDoubleComplex(const ArrayOf& A, int nLhs)
{
    ArrayOfVector retval;
    if (!isAllFinite<double>(A)) {
        Error("Nelson:lu:matrixWithNaNInf", "%s must not contain NaN or Inf.", "Input to LU");
    }
    int nrows = (int)A.rows();
    int ncols = (int)A.columns();
    int p = std::min(nrows, ncols);

    if (A.isEmpty()) {
        if (nLhs <= 1) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, ncols), nullptr);
            return retval;
        }
        if (nLhs <= 2) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), nullptr);

        } else if (nLhs == 3) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), nullptr);
            retval << Eye(nrows, nrows, NLS_DOUBLE, false);
        }
        return retval;
    }

    if (nLhs <= 1) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<double>*>((double*)AA.dataPointer());
        lapack_int m = nrows;
        lapack_int n = ncols;
        lapack_int lda = nrows;
        lapack_int info = 0;
        std::vector<lapack_int> piv((size_t)p);
        ScopedOpenMPThreads scopedThreads(luThreadCount(nrows, ncols));
        LAPACK_zgetrf(&m, &n, ptrZa, &lda, piv.data(), &info);
        retval << AA;
        return retval;
    }

    double* l = (double*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * p, stringVector(), true);

    double* u = (double*)ArrayOf::allocateArrayOf(A.dataClass(), p * ncols, stringVector(), true);
    if (nLhs == 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<double>*>((double*)AA.dataPointer());

        ComplexLU<double>(nrows, ncols, l, u, ptrZa, LAPACK_zgetrf);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        double* piv
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nrows * nrows, stringVector(), true);
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<double>*>((double*)AA.dataPointer());

        ComplexLUP<double>(nrows, ncols, l, u, piv, ptrZa, LAPACK_zgetrf);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
        retval << ArrayOf(NLS_DOUBLE, Dimensions(nrows, nrows), piv);
    }
    return retval;
}
//=============================================================================
/**
 * LU factorization of A. nLhs <= 1 returns the factored matrix, nLhs == 2 returns L and U,
 * nLhs == 3 also returns the nrows x nrows permutation matrix. NaN or Inf in A is an error.
 */
ArrayOfVector
LUMatrixFactorizationSingleComplex(const ArrayOf& A, int nLhs)
{
    ArrayOfVector retval;
    if (!isAllFinite<single>(A)) {
        Error("Nelson:lu:matrixWithNaNInf", "%s must not contain NaN or Inf.", "Input to LU");
    }
    int nrows = (int)A.rows();
    int ncols = (int)A.columns();
    int p = std::min(nrows, ncols);

    if (A.isEmpty()) {
        if (nLhs <= 1) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, ncols), nullptr);
            return retval;
        }
        if (nLhs <= 2) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), nullptr);

        } else if (nLhs == 3) {
            retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), nullptr);
            retval << Eye(nrows, nrows, NLS_SINGLE, false);
        }
        return retval;
    }

    if (nLhs <= 1) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<single>*>((single*)AA.dataPointer());
        lapack_int m = nrows;
        lapack_int n = ncols;
        lapack_int lda = nrows;
        lapack_int info = 0;
        std::vector<lapack_int> piv((size_t)p);
        ScopedOpenMPThreads scopedThreads(luThreadCount(nrows, ncols));
        LAPACK_cgetrf(&m, &n, ptrZa, &lda, piv.data(), &info);
        retval << AA;
        return retval;
    }

    single* l = (single*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * p, stringVector(), true);

    single* u = (single*)ArrayOf::allocateArrayOf(A.dataClass(), p * ncols, stringVector(), true);
    if (nLhs == 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<single>*>((single*)AA.dataPointer());

        ComplexLU<single>(nrows, ncols, l, u, ptrZa, LAPACK_cgetrf);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        single* piv
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, nrows * nrows, stringVector(), true);
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<single>*>((single*)AA.dataPointer());

        ComplexLUP<single>(nrows, ncols, l, u, piv, ptrZa, LAPACK_cgetrf);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
        retval << ArrayOf(NLS_SINGLE, Dimensions(nrows, nrows), piv);
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
