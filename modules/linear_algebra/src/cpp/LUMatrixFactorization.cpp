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
#include "LuFactorization.hpp"
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
// Writes the row permutation as a matrix (pmat) or as a 1-based row vector (pvec).
template <class T>
static void
fillPermutation(int nrows, const int* fullpivot, T* pmat, double* pvec)
{
    if (pvec != nullptr) {
        OMP_PARALLEL_FOR_LOOP(nrows)
        for (ompIndexType i = 0; i < (ompIndexType)nrows; i++) {
            pvec[i] = fullpivot ? (double)(fullpivot[i] + 1) : (double)(i + 1);
        }
        return;
    }
    OMP_PARALLEL_FOR_LOOP(nrows)
    for (ompIndexType i = 0; i < (ompIndexType)nrows; i++) {
        pmat[i + (fullpivot ? fullpivot[i] : (int)i) * nrows] = 1;
    }
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
ComplexLUP(int nrows, int ncols, T* l, T* u, T* pmat, double* pvec, std::complex<T>* a,
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
    fillPermutation<T>(nrows, fullpivot, pmat, pvec);
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
RealLUP(int nrows, int ncols, T* l, T* u, T* pmat, double* pvec, T* a,
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
        fillPermutation<T>(nrows, nullptr, pmat, pvec);
        RealLUWithoutPivot(nrows, ncols, l, u, a);
        return;
    }

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);
    fillPermutation<T>(nrows, fullpivot, pmat, pvec);
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
// Third LU output: a permutation matrix of class 'cls' or a double row vector.
template <class T> class PermutationOutput
{
    NelsonType cls;
    int nrows;
    bool asVector;

public:
    T* pmat = nullptr;
    double* pvec = nullptr;
    PermutationOutput(NelsonType _cls, int _nrows, bool _asVector)
        : cls(_cls), nrows(_nrows), asVector(_asVector)
    {
        if (asVector) {
            pvec = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nrows, stringVector(), false);
        } else {
            pmat = (T*)ArrayOf::allocateArrayOf(
                cls, (indexType)nrows * (indexType)nrows, stringVector(), true);
        }
    }
    ArrayOf
    get() const
    {
        if (asVector) {
            return ArrayOf(NLS_DOUBLE, Dimensions(1, nrows), pvec);
        }
        return ArrayOf(cls, Dimensions(nrows, nrows), pmat);
    }
};
//=============================================================================
static ArrayOf
identityPermutation(int nrows, NelsonType cls, bool asVector)
{
    if (!asVector) {
        return Eye(nrows, nrows, cls, false);
    }
    double* pvec = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nrows, stringVector(), false);
    fillPermutation<double>(nrows, nullptr, nullptr, pvec);
    return ArrayOf(NLS_DOUBLE, Dimensions(1, nrows), pvec);
}
//=============================================================================
/**
 * LU factorization of A. nLhs <= 1 returns the factored matrix, nLhs == 2 returns L and U,
 * nLhs == 3 also returns the permutation (matrix, or row vector if asVector). NaN or Inf in A is an error.
 */
ArrayOfVector
LUMatrixFactorizationDoubleReal(const ArrayOf& A, int nLhs, bool asVector)
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
            retval << identityPermutation(nrows, NLS_DOUBLE, asVector);
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
        luFactor(&m, &n, (double*)AA.dataPointer(), &lda, piv.data(), &info);
        retval << AA;
        return retval;
    }

    double* l = (double*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * p, stringVector(), true);
    double* u = (double*)ArrayOf::allocateArrayOf(A.dataClass(), p * ncols, stringVector(), true);
    if (nLhs == 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLU<double>(nrows, ncols, l, u, (double*)AA.dataPointer(), luFactor);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        PermutationOutput<double> perm(A.dataClass(), nrows, asVector);
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLUP<double>(
            nrows, ncols, l, u, perm.pmat, perm.pvec, (double*)AA.dataPointer(), luFactor);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
        retval << perm.get();
    }
    return retval;
}
//=============================================================================
/**
 * LU factorization of A. nLhs <= 1 returns the factored matrix, nLhs == 2 returns L and U,
 * nLhs == 3 also returns the permutation (matrix, or row vector if asVector). NaN or Inf in A is an error.
 */
ArrayOfVector
LUMatrixFactorizationSingleReal(const ArrayOf& A, int nLhs, bool asVector)
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
            retval << identityPermutation(nrows, NLS_SINGLE, asVector);
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
        luFactor(&m, &n, (single*)AA.dataPointer(), &lda, piv.data(), &info);
        retval << AA;
        return retval;
    }

    single* l = (single*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * p, stringVector(), true);
    single* u = (single*)ArrayOf::allocateArrayOf(A.dataClass(), p * ncols, stringVector(), true);
    if (nLhs == 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLU<single>(nrows, ncols, l, u, (single*)AA.dataPointer(), luFactor);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        PermutationOutput<single> perm(A.dataClass(), nrows, asVector);
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLUP<single>(
            nrows, ncols, l, u, perm.pmat, perm.pvec, (single*)AA.dataPointer(), luFactor);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
        retval << perm.get();
    }
    return retval;
}
//=============================================================================
/**
 * LU factorization of A. nLhs <= 1 returns the factored matrix, nLhs == 2 returns L and U,
 * nLhs == 3 also returns the permutation (matrix, or row vector if asVector). NaN or Inf in A is an error.
 */
ArrayOfVector
LUMatrixFactorizationDoubleComplex(const ArrayOf& A, int nLhs, bool asVector)
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
            retval << identityPermutation(nrows, NLS_DOUBLE, asVector);
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
        luFactor(&m, &n, ptrZa, &lda, piv.data(), &info);
        retval << AA;
        return retval;
    }

    double* l = (double*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * p, stringVector(), true);

    double* u = (double*)ArrayOf::allocateArrayOf(A.dataClass(), p * ncols, stringVector(), true);
    if (nLhs == 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<double>*>((double*)AA.dataPointer());

        ComplexLU<double>(nrows, ncols, l, u, ptrZa, luFactor);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        PermutationOutput<double> perm(NLS_DOUBLE, nrows, asVector);
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<double>*>((double*)AA.dataPointer());

        ComplexLUP<double>(nrows, ncols, l, u, perm.pmat, perm.pvec, ptrZa, luFactor);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
        retval << perm.get();
    }
    return retval;
}
//=============================================================================
/**
 * LU factorization of A. nLhs <= 1 returns the factored matrix, nLhs == 2 returns L and U,
 * nLhs == 3 also returns the permutation (matrix, or row vector if asVector). NaN or Inf in A is an error.
 */
ArrayOfVector
LUMatrixFactorizationSingleComplex(const ArrayOf& A, int nLhs, bool asVector)
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
            retval << identityPermutation(nrows, NLS_SINGLE, asVector);
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
        luFactor(&m, &n, ptrZa, &lda, piv.data(), &info);
        retval << AA;
        return retval;
    }

    single* l = (single*)ArrayOf::allocateArrayOf(A.dataClass(), nrows * p, stringVector(), true);

    single* u = (single*)ArrayOf::allocateArrayOf(A.dataClass(), p * ncols, stringVector(), true);
    if (nLhs == 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<single>*>((single*)AA.dataPointer());

        ComplexLU<single>(nrows, ncols, l, u, ptrZa, luFactor);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        PermutationOutput<single> perm(NLS_SINGLE, nrows, asVector);
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<single>*>((single*)AA.dataPointer());

        ComplexLUP<single>(nrows, ncols, l, u, perm.pmat, perm.pvec, ptrZa, luFactor);
        retval << ArrayOf(A.dataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.dataClass(), Dimensions(p, ncols), u);
        retval << perm.get();
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
