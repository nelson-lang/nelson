//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LUMatrixFactorization.hpp"
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/src/misc/lapacke.h>
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Eye.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * @brief Check if all elements in the given array are finite.
 *
 * This function checks if all elements in the input array are finite. The function
 * uses Eigen's `allFinite()` method to perform the check.
 *
 * @tparam T The data type of the elements in the array.
 * @param A The input array to check for finiteness.
 * @return True if all elements are finite, false otherwise.
 */
template <class T>
bool
isAllFinite(const ArrayOf& A)
{
    if (A.isComplex()) {
        auto* pzA = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
            pzA, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        return matArrayIn.allFinite();

    } else {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matArrayIn(
            (T*)A.getDataPointer(), (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        return matArrayIn.allFinite();
    }
    return false;
}
//=============================================================================
/**
 * @brief Update a pivot vector based on a given permutation array.
 *
 * This function initializes a pivot vector and updates it based on a permutation array.
 * The permutation array 'piv' is expected to contain the desired order of elements.
 * The function uses OpenMP for parallelization if the _NLS_WITH_OPENMP macro is defined.
 *
 * @param nrows The number of rows in the pivot vector.
 * @param piv   The permutation array specifying the desired order.
 * @param p     The size of the permutation array.
 * @return      The updated pivot vector.
 */
int*
updatePivotVector(int nrows, int* piv, int p)
{
    int* fullpivot = nullptr;
    try {
        fullpivot = new int[nrows];
    } catch (std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
/**
 * @brief Perform Complex LUP decomposition on a given matrix.
 *
 * This function performs a Complex LUP decomposition on the input matrix 'a'. The result
 * is decomposed into lower triangular matrix 'l', upper triangular matrix 'u', and a
 * permutation matrix 'pmat'. The function uses LAPACK functions for the decomposition.
 * The decomposition is parallelized using OpenMP if the _NLS_WITH_OPENMP macro is defined.
 *
 * @tparam T             The data type of the matrix elements.
 * @param nrows         The number of rows in the matrix.
 * @param ncols         The number of columns in the matrix.
 * @param l             The output lower triangular matrix.
 * @param u             The output upper triangular matrix.
 * @param pmat          The output permutation matrix.
 * @param a             The input matrix to be decomposed.
 * @param lapacke_fn    LAPACK function for complex LUP decomposition.
 */
template <class T>
void
ComplexLUP(int nrows, int ncols, T* l, T* u, T* pmat, std::complex<T>* a,
    lapack_int (*lapacke_fn)(int matrix_layout, lapack_int m, lapack_int n, std::complex<T>* a,
        lapack_int lda, lapack_int* ipiv))
{
    ompIndexType i = 0;
    ompIndexType j = 0;
    int p = std::min(nrows, ncols);
    std::vector<lapack_int> piv(p);
    lapack_int info = (lapacke_fn)(LAPACK_COL_MAJOR, nrows, ncols, a, nrows, piv.data());

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);

#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < lcols; i++) {
            ptrZl[(i + i * lrows)].real(1.0);
            ptrZl[(i + i * lrows)].imag(0.0);
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < std::min(i, lcols); j++) {
                ptrZl[(i + j * lrows)] = a[(i + j * nrows)];
            }
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < lcols; i++) {
            ptrZl[(i + i * lrows)].real(1.0);
            ptrZl[(i + i * lrows)].imag(0.0);
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < i; j++) {
                ptrZl[(i + j * lrows)] = a[(i + j * nrows)];
            }
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
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
 * @brief Perform LU decomposition on a complex matrix A, factorizing it into lower (L) and upper
 * (U) triangular matrices.
 *
 * This function computes the LU decomposition of a complex matrix A such that A = P * L * U, where
 * P is a permutation matrix, L is a lower triangular matrix, and U is an upper triangular matrix.
 *
 * @tparam T Data type of the matrix elements.
 * @param nrows Number of rows in the matrix A.
 * @param ncols Number of columns in the matrix A.
 * @param l Pointer to the array representing the lower triangular matrix L. Should have dimensions
 * nrows x std::min(nrows, ncols).
 * @param u Pointer to the array representing the upper triangular matrix U. Should have dimensions
 * std::min(nrows, ncols) x ncols.
 * @param a Pointer to the complex matrix A with dimensions nrows x ncols.
 * @param lapacke_fn Function pointer to LAPACK routine for LU decomposition of complex matrices.
 * @return Lapack_info Lapack error code indicating success or failure of the LU decomposition.
 *
 * @note The LAPACK routine specified by lapacke_fn should have the following signature:
 *       lapack_int lapack_fn(int matrix_layout, lapack_int m, lapack_int n, std::complex<T>* a,
 * lapack_int lda, lapack_int* ipiv)
 *
 * @details The function updates the lower and upper triangular matrices (l and u) using the
 * provided LAPACK routine. It also updates the pivot vector and constructs the permutation matrix
 * to be applied to the original matrix A.
 *
 * @warning The input matrix A is modified during the decomposition.
 * @warning It is the responsibility of the caller to free the memory allocated for the fullpivot
 * array.
 *
 * @see LAPACK documentation for details on LAPACK routines.
 */
template <class T>
void
ComplexLU(int nrows, int ncols, T* l, T* u, std::complex<T>* a,
    lapack_int (*lapacke_fn)(int matrix_layout, lapack_int m, lapack_int n, std::complex<T>* a,
        lapack_int lda, lapack_int* ipiv))
{
    ompIndexType i = 0;
    ompIndexType j = 0;
    int p = std::min(nrows, ncols);
    std::vector<lapack_int> piv(p);
    lapack_int info = (lapacke_fn)(LAPACK_COL_MAJOR, nrows, ncols, a, nrows, piv.data());

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);
    if (nrows > ncols) {
        auto* ptrZl = reinterpret_cast<std::complex<T>*>((T*)l);
        auto* ptrZu = reinterpret_cast<std::complex<T>*>((T*)u);

        ompIndexType lrows = nrows;
        ompIndexType lcols = ncols;
        ompIndexType urows = ncols;
        ompIndexType ucols = ncols;

#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < lcols; i++) {
            ptrZl[(fullpivot[i] + i * lrows)].real(1.0);
            ptrZl[(fullpivot[i] + i * lrows)].imag(0.0);
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < std::min(i, lcols); j++) {
                ptrZl[(fullpivot[i] + j * lrows)] = a[(i + j * nrows)];
            }
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
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
        ompIndexType ucols = ncols;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < lcols; i++) {
            ptrZl[(fullpivot[i] + i * lrows)].real(1.0);
            ptrZl[(fullpivot[i] + i * lrows)].imag(0.0);
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < i; j++) {
                ptrZl[(fullpivot[i] + j * lrows)] = a[(i + j * nrows)];
            }
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
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
 * @brief Perform Complex LU decomposition on a given matrix.
 *
 * This function performs a Complex LU decomposition on the input matrix 'a'. The result
 * is decomposed into lower triangular matrix 'l' and upper triangular matrix 'u'.
 * The function uses LAPACK functions for the decomposition. The decomposition is
 * parallelized using OpenMP if the _NLS_WITH_OPENMP macro is defined.
 *
 * @tparam T             The data type of the matrix elements.
 * @param nrows         The number of rows in the matrix.
 * @param ncols         The number of columns in the matrix.
 * @param l             The output lower triangular matrix.
 * @param u             The output upper triangular matrix.
 * @param a             The input matrix to be decomposed.
 * @param lapacke_fn    LAPACK function for complex LU decomposition.
 */
template <class T>
void
RealLUP(int nrows, int ncols, T* l, T* u, T* pmat, T* a,
    lapack_int (*lapacke_fn)(
        int matrix_order, lapack_int m, lapack_int n, T* a, lapack_int lda, lapack_int* ipiv))
{
    int p = std::min(nrows, ncols);
    std::vector<lapack_int> piv(p);

    lapack_int info = (lapacke_fn)(LAPACK_COL_MAJOR, nrows, ncols, a, nrows, piv.data());

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);

#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)nrows; i++) {
        pmat[i + fullpivot[i] * nrows] = 1;
    }
    if (fullpivot) {
        delete[] fullpivot;
        fullpivot = nullptr;
    }
    if (nrows > ncols) {
        ompIndexType lrows = nrows;
        ompIndexType lcols = ncols;
        ompIndexType urows = ncols;
        ompIndexType ucols = ncols;
        ompIndexType i = 0;
        ompIndexType j = 0;

#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < lcols; i++) {
            l[i + i * lrows] = 1.0;
        }

#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < std::min(i, lcols); j++) {
                l[i + j * lrows] = a[i + j * nrows];
            }
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 0; i < urows; i++) {
            for (j = i; j < ucols; j++) {
                u[i + j * urows] = a[i + j * nrows];
            }
        }
    } else {
        ompIndexType lrows = nrows;
        ompIndexType lcols = nrows;
        ompIndexType urows = nrows;
        ompIndexType ucols = ncols;
        ompIndexType i = 0;
        ompIndexType j = 0;

#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < lcols; i++) {
            l[i + i * lrows] = 1.0;
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < i; j++) {
                l[i + j * lrows] = a[i + j * nrows];
            }
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 0; i < nrows; i++) {
            for (j = i; j < ncols; j++) {
                u[i + j * urows] = a[i + j * nrows];
            }
        }
    }
}
//=============================================================================
/**
 * @brief Perform Real LU decomposition on a given matrix.
 *
 * This function performs a Real LU decomposition on the input matrix 'a'. The result
 * is decomposed into lower triangular matrix 'l' and upper triangular matrix 'u'.
 * The function uses LAPACK functions for the decomposition. The decomposition is
 * parallelized using OpenMP if the _NLS_WITH_OPENMP macro is defined.
 *
 * @tparam T             The data type of the matrix elements.
 * @param nrows         The number of rows in the matrix.
 * @param ncols         The number of columns in the matrix.
 * @param l             The output lower triangular matrix.
 * @param u             The output upper triangular matrix.
 * @param a             The input matrix to be decomposed.
 * @param lapacke_fn    LAPACK function for real LU decomposition.
 */
template <class T>
void
RealLU(int nrows, int ncols, T* l, T* u, T* a,
    lapack_int (*lapacke_fn)(
        int matrix_order, lapack_int m, lapack_int n, T* a, lapack_int lda, lapack_int* ipiv))
{
    ompIndexType i = 0;
    ompIndexType j = 0;
    int p = std::min(nrows, ncols);
    std::vector<int> piv(p);

    lapack_int info = (lapacke_fn)(LAPACK_COL_MAJOR, nrows, ncols, a, nrows, piv.data());

    int* fullpivot = updatePivotVector(nrows, piv.data(), p);

    if (nrows > ncols) {
        ompIndexType lrows = nrows;
        ompIndexType lcols = ncols;
        ompIndexType urows = ncols;
        ompIndexType ucols = ncols;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (i = 0; i < lcols; i++) {
            l[fullpivot[i] + i * lrows] = 1.0;
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < std::min(i, lcols); j++) {
                l[fullpivot[i] + j * lrows] = a[i + j * nrows];
            }
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 0; i < urows; i++)
            for (j = i; j < ucols; j++)
                u[i + j * urows] = a[i + j * nrows];
    } else {
        ompIndexType lrows = nrows;
        ompIndexType lcols = nrows;
        ompIndexType urows = nrows;
        ompIndexType ucols = ncols;
        for (i = 0; i < lcols; i++) {
            l[fullpivot[i] + i * lrows] = 1.0;
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 1; i < lrows; i++) {
            for (j = 0; j < i; j++) {
                l[fullpivot[i] + j * lrows] = a[i + j * nrows];
            }
        }
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(i, j)
#endif
        for (i = 0; i < nrows; i++)
            for (j = i; j < ncols; j++)
                u[i + j * urows] = a[i + j * nrows];
    }
    if (fullpivot) {
        delete[] fullpivot;
        fullpivot = nullptr;
    }
}
//=============================================================================
/**
 * @brief Perform LU matrix factorization on a given double-precision real matrix.
 *
 * This function performs LU matrix factorization on the input matrix 'A'. The result
 * is decomposed into lower triangular matrix 'l' and upper triangular matrix 'u'.
 * The function uses LAPACK functions for the decomposition. The number of left-hand side
 * matrices to be returned is determined by the parameter 'nLhs'. If 'nLhs' is less than or
 * equal to 2, only 'l' and 'u' are returned. If 'nLhs' is 3, the permutation matrix 'piv' is also
 * returned. The decomposition is parallelized using OpenMP if the _NLS_WITH_OPENMP macro is
 * defined.
 *
 * @param A      The input matrix to be factorized.
 * @param nLhs   The number of left-hand side matrices to be returned.
 * @return       An ArrayOfVector containing the factorized matrices.
 */
ArrayOfVector
LUMatrixFactorizationDoubleReal(const ArrayOf& A, int nLhs)
{
    ArrayOfVector retval;

    if (!isAllFinite<double>(A)) {
        Error(_("Input to LU must not contain NaN or Inf."), "Nelson:lu:matrixWithNaNInf");
    }
    int nrows = (int)A.getRows();
    int ncols = (int)A.getColumns();
    int p = std::min(nrows, ncols);

    if (A.isEmpty()) {
        if (nLhs <= 2) {
            retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), nullptr);

        } else if (nLhs == 3) {
            retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), nullptr);
            retval << Eye(nrows, nrows, NLS_DOUBLE, false);
        }
        return retval;
    }

    double* l
        = (double*)ArrayOf::allocateArrayOf(A.getDataClass(), nrows * p, stringVector(), true);
    double* u
        = (double*)ArrayOf::allocateArrayOf(A.getDataClass(), p * ncols, stringVector(), true);
    if (nLhs <= 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLU<double>(nrows, ncols, l, u, (double*)AA.getDataPointer(), LAPACKE_dgetrf);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        indexType length = (indexType)(nrows * nrows);
        double* piv
            = (double*)ArrayOf::allocateArrayOf(A.getDataClass(), length, stringVector(), true);

        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLUP<double>(nrows, ncols, l, u, piv, (double*)AA.getDataPointer(), LAPACKE_dgetrf);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), u);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, nrows), piv);
    }
    return retval;
}
//=============================================================================
/**
 * @brief Perform LU matrix factorization on a single-precision real matrix A.
 *
 * This function computes the LU factorization of a single-precision real matrix A such that A = P *
 * L * U, where P is a permutation matrix, L is a lower triangular matrix, and U is an upper
 * triangular matrix.
 *
 * @param A An array of single-precision real values representing the input matrix.
 * @param nLhs Number of left-hand side matrices to return. It determines the number of matrices in
 * the output ArrayOfVector. If nLhs is 1 or 2, only the factorized matrices L and U are returned.
 *             If nLhs is 3, the factorized matrices L, U, and the pivot vector Piv are returned.
 * @return An ArrayOfVector containing the factorized matrices. The order of matrices depends on the
 * value of nLhs:
 *         - If nLhs is 1 or 2, the returned ArrayOfVector contains two matrices: L (Dimensions:
 * nrows x p) and U (Dimensions: p x ncols).
 *         - If nLhs is 3, the returned ArrayOfVector contains three matrices: L (Dimensions: nrows
 * x p), U (Dimensions: p x ncols), and Piv (Dimensions: nrows x nrows).
 *
 * @note The input matrix A is modified during the factorization.
 * @warning The function checks for the presence of NaN or Inf in the input matrix A and raises an
 * error if found. It is the responsibility of the caller to handle errors appropriately.
 * @see LAPACK documentation for details on LAPACK routine LAPACKE_sgetrf.
 */
ArrayOfVector
LUMatrixFactorizationSingleReal(const ArrayOf& A, int nLhs)
{
    if (!isAllFinite<single>(A)) {
        Error(_("Input to LU must not contain NaN or Inf."), "Nelson:lu:matrixWithNaNInf");
    }
    ArrayOfVector retval;

    if (!isAllFinite<single>(A)) {
        Error(_("Input to LU must not contain NaN or Inf."), "Nelson:lu:matrixWithNaNInf");
    }
    int nrows = (int)A.getRows();
    int ncols = (int)A.getColumns();
    int p = std::min(nrows, ncols);

    if (A.isEmpty()) {
        if (nLhs <= 2) {
            retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), nullptr);

        } else if (nLhs == 3) {
            retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), nullptr);
            retval << Eye(nrows, nrows, NLS_SINGLE, false);
        }
        return retval;
    }

    single* l
        = (single*)ArrayOf::allocateArrayOf(A.getDataClass(), nrows * p, stringVector(), true);
    single* u
        = (single*)ArrayOf::allocateArrayOf(A.getDataClass(), p * ncols, stringVector(), true);
    if (nLhs <= 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLU<single>(nrows, ncols, l, u, (single*)AA.getDataPointer(), LAPACKE_sgetrf);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        single* piv = (single*)ArrayOf::allocateArrayOf(
            A.getDataClass(), nrows * nrows, stringVector(), true);

        ArrayOf AA(A);
        AA.ensureSingleOwner();
        RealLUP<single>(nrows, ncols, l, u, piv, (single*)AA.getDataPointer(), LAPACKE_sgetrf);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), u);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, nrows), piv);
    }
    return retval;
}
//=============================================================================
/**
 * @brief Perform LU matrix factorization on a given single-precision real matrix.
 *
 * This function performs LU matrix factorization on the input matrix 'A'. The result
 * is decomposed into lower triangular matrix 'l' and upper triangular matrix 'u'.
 * The function uses LAPACK functions for the decomposition. The number of left-hand side
 * matrices to be returned is determined by the parameter 'nLhs'. If 'nLhs' is less than or
 * equal to 2, only 'l' and 'u' are returned. If 'nLhs' is 3, the permutation matrix 'piv' is also
 * returned. The decomposition is parallelized using OpenMP if the _NLS_WITH_OPENMP macro is
 * defined.
 *
 * @param A      The input matrix to be factorized.
 * @param nLhs   The number of left-hand side matrices to be returned.
 * @return       An ArrayOfVector containing the factorized matrices.
 */
ArrayOfVector
LUMatrixFactorizationDoubleComplex(const ArrayOf& A, int nLhs)
{
    ArrayOfVector retval;
    if (!isAllFinite<double>(A)) {
        Error(_("Input to LU must not contain NaN or Inf."), "Nelson:lu:matrixWithNaNInf");
    }
    int nrows = (int)A.getRows();
    int ncols = (int)A.getColumns();
    int p = std::min(nrows, ncols);

    if (A.isEmpty()) {
        if (nLhs <= 2) {
            retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), nullptr);

        } else if (nLhs == 3) {
            retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), nullptr);
            retval << Eye(nrows, nrows, NLS_DOUBLE, false);
        }
        return retval;
    }

    double* l
        = (double*)ArrayOf::allocateArrayOf(A.getDataClass(), nrows * p, stringVector(), true);

    double* u
        = (double*)ArrayOf::allocateArrayOf(A.getDataClass(), p * ncols, stringVector(), true);
    if (nLhs <= 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<double>*>((double*)AA.getDataPointer());

        ComplexLU<double>(nrows, ncols, l, u, ptrZa, LAPACKE_zgetrf);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        double* piv
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nrows * nrows, stringVector(), true);
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<double>*>((double*)AA.getDataPointer());

        ComplexLUP<double>(nrows, ncols, l, u, piv, ptrZa, LAPACKE_zgetrf);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), u);
        retval << ArrayOf(NLS_DOUBLE, Dimensions(nrows, nrows), piv);
    }
    return retval;
}
//=============================================================================
/**
 * @brief Perform LU matrix factorization on a given single-precision complex matrix.
 *
 * This function performs LU matrix factorization on the input matrix 'A'. The result
 * is decomposed into lower triangular matrix 'l' and upper triangular matrix 'u'.
 * The function uses LAPACK functions for the decomposition. The number of left-hand side
 * matrices to be returned is determined by the parameter 'nLhs'. If 'nLhs' is less than or
 * equal to 2, only 'l' and 'u' are returned. If 'nLhs' is 3, the permutation matrix 'piv' is also
 * returned. The decomposition is parallelized using OpenMP if the _NLS_WITH_OPENMP macro is
 * defined.
 *
 * @param A      The input matrix to be factorized.
 * @param nLhs   The number of left-hand side matrices to be returned.
 * @return       An ArrayOfVector containing the factorized matrices.
 */
ArrayOfVector
LUMatrixFactorizationSingleComplex(const ArrayOf& A, int nLhs)
{
    ArrayOfVector retval;
    if (!isAllFinite<single>(A)) {
        Error(_("Input to LU must not contain NaN or Inf."), "Nelson:lu:matrixWithNaNInf");
    }
    int nrows = (int)A.getRows();
    int ncols = (int)A.getColumns();
    int p = std::min(nrows, ncols);

    if (A.isEmpty()) {
        if (nLhs <= 2) {
            retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), nullptr);

        } else if (nLhs == 3) {
            retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), nullptr);
            retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), nullptr);
            retval << Eye(nrows, nrows, NLS_SINGLE, false);
        }
        return retval;
    }

    single* l
        = (single*)ArrayOf::allocateArrayOf(A.getDataClass(), nrows * p, stringVector(), true);

    single* u
        = (single*)ArrayOf::allocateArrayOf(A.getDataClass(), p * ncols, stringVector(), true);
    if (nLhs <= 2) {
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<single>*>((single*)AA.getDataPointer());

        ComplexLU<single>(nrows, ncols, l, u, ptrZa, LAPACKE_cgetrf);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), u);
    } else if (nLhs == 3) {
        single* piv
            = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, nrows * nrows, stringVector(), true);
        ArrayOf AA(A);
        AA.ensureSingleOwner();
        auto* ptrZa = reinterpret_cast<std::complex<single>*>((single*)AA.getDataPointer());

        ComplexLUP<single>(nrows, ncols, l, u, piv, ptrZa, LAPACKE_cgetrf);
        retval << ArrayOf(A.getDataClass(), Dimensions(nrows, p), l);
        retval << ArrayOf(A.getDataClass(), Dimensions(p, ncols), u);
        retval << ArrayOf(NLS_SINGLE, Dimensions(nrows, nrows), piv);
    }
    return retval;
}
//=============================================================================
}
//=============================================================================
