//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/IterativeLinearSolvers>
#include <Eigen/SparseCholesky>
#include <Eigen/SparseLU>
#include <Eigen/SparseQR>
#include <cmath>
#include <limits>
#include <string>
#include <vector>
#include <algorithm>
#include <cstdio>
#include "LeftDivide.hpp"
#include "MatrixCheck.hpp"
#include "DotLeftDivide.hpp"
#include "LinearEquationSolver.hpp"
#include "LeastSquareSolver.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IntegerCompatibilityHelpers.hpp"
#include "SparseOperatorsHelpers.hpp"
#include "Decomplexify.hpp"
#include "LapackPrototypes.hpp"
#include "WarningIds.hpp"
#include "DiagonalDivide.hpp"
//=============================================================================
#if !defined(_MKL_LAPACK_H_)
extern "C"
{
    void LAPACK_GLOBAL(sgbtrf, SGBTRF)(const lapack_int*, const lapack_int*, const lapack_int*,
        const lapack_int*, float*, const lapack_int*, lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(dgbtrf, DGBTRF)(const lapack_int*, const lapack_int*, const lapack_int*,
        const lapack_int*, double*, const lapack_int*, lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(cgbtrf, CGBTRF)(const lapack_int*, const lapack_int*, const lapack_int*,
        const lapack_int*, std::complex<float>*, const lapack_int*, lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(zgbtrf, ZGBTRF)(const lapack_int*, const lapack_int*, const lapack_int*,
        const lapack_int*, std::complex<double>*, const lapack_int*, lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(sgbtrs, SGBTRS)(const char*, const lapack_int*, const lapack_int*,
        const lapack_int*, const lapack_int*, const float*, const lapack_int*, const lapack_int*,
        float*, const lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(dgbtrs, DGBTRS)(const char*, const lapack_int*, const lapack_int*,
        const lapack_int*, const lapack_int*, const double*, const lapack_int*, const lapack_int*,
        double*, const lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(cgbtrs, CGBTRS)(const char*, const lapack_int*, const lapack_int*,
        const lapack_int*, const lapack_int*, const std::complex<float>*, const lapack_int*,
        const lapack_int*, std::complex<float>*, const lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(zgbtrs, ZGBTRS)(const char*, const lapack_int*, const lapack_int*,
        const lapack_int*, const lapack_int*, const std::complex<double>*, const lapack_int*,
        const lapack_int*, std::complex<double>*, const lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(sgbcon, SGBCON)(const char*, const lapack_int*, const lapack_int*,
        const lapack_int*, const float*, const lapack_int*, const lapack_int*, const float*,
        float*, float*, lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(dgbcon, DGBCON)(const char*, const lapack_int*, const lapack_int*,
        const lapack_int*, const double*, const lapack_int*, const lapack_int*, const double*,
        double*, double*, lapack_int*, lapack_int*);
    void LAPACK_GLOBAL(cgbcon, CGBCON)(const char*, const lapack_int*, const lapack_int*,
        const lapack_int*, const std::complex<float>*, const lapack_int*, const lapack_int*,
        const float*, float*, std::complex<float>*, float*, lapack_int*);
    void LAPACK_GLOBAL(zgbcon, ZGBCON)(const char*, const lapack_int*, const lapack_int*,
        const lapack_int*, const std::complex<double>*, const lapack_int*, const lapack_int*,
        const double*, double*, std::complex<double>*, double*, lapack_int*);
}
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
// Band LU routines per scalar type, used for sparse divisors with a dense band.
template <class T> struct SparseBandSolver;
//=============================================================================
template <> struct SparseBandSolver<double>
{
    using Real = double;
    static constexpr char transposeSolve = 'T';
    static void
    gbtrf(lapack_int n, lapack_int kl, lapack_int ku, double* ab, lapack_int ldab,
        lapack_int* ipiv, lapack_int* info)
    {
        LAPACK_GLOBAL(dgbtrf, DGBTRF)(&n, &n, &kl, &ku, ab, &ldab, ipiv, info);
    }
    static void
    gbtrs(char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs,
        const double* ab, lapack_int ldab, const lapack_int* ipiv, double* b, lapack_int* info)
    {
        LAPACK_GLOBAL(dgbtrs, DGBTRS)
        (&trans, &n, &kl, &ku, &nrhs, ab, &ldab, ipiv, b, &n, info);
    }
    static void
    gbcon(lapack_int n, lapack_int kl, lapack_int ku, const double* ab, lapack_int ldab,
        const lapack_int* ipiv, double anorm, double* rcond, lapack_int* info)
    {
        char norm = '1';
        std::vector<double> work((size_t)3 * (size_t)n);
        std::vector<lapack_int> iwork((size_t)n);
        LAPACK_GLOBAL(dgbcon, DGBCON)
        (&norm, &n, &kl, &ku, ab, &ldab, ipiv, &anorm, rcond, work.data(), iwork.data(), info);
    }
};
//=============================================================================
template <> struct SparseBandSolver<single>
{
    using Real = single;
    static constexpr char transposeSolve = 'T';
    static void
    gbtrf(lapack_int n, lapack_int kl, lapack_int ku, single* ab, lapack_int ldab,
        lapack_int* ipiv, lapack_int* info)
    {
        LAPACK_GLOBAL(sgbtrf, SGBTRF)(&n, &n, &kl, &ku, ab, &ldab, ipiv, info);
    }
    static void
    gbtrs(char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs,
        const single* ab, lapack_int ldab, const lapack_int* ipiv, single* b, lapack_int* info)
    {
        LAPACK_GLOBAL(sgbtrs, SGBTRS)
        (&trans, &n, &kl, &ku, &nrhs, ab, &ldab, ipiv, b, &n, info);
    }
    static void
    gbcon(lapack_int n, lapack_int kl, lapack_int ku, const single* ab, lapack_int ldab,
        const lapack_int* ipiv, single anorm, single* rcond, lapack_int* info)
    {
        char norm = '1';
        std::vector<single> work((size_t)3 * (size_t)n);
        std::vector<lapack_int> iwork((size_t)n);
        LAPACK_GLOBAL(sgbcon, SGBCON)
        (&norm, &n, &kl, &ku, ab, &ldab, ipiv, &anorm, rcond, work.data(), iwork.data(), info);
    }
};
//=============================================================================
template <> struct SparseBandSolver<doublecomplex>
{
    using Real = double;
    static constexpr char transposeSolve = 'C';
    static void
    gbtrf(lapack_int n, lapack_int kl, lapack_int ku, doublecomplex* ab, lapack_int ldab,
        lapack_int* ipiv, lapack_int* info)
    {
        LAPACK_GLOBAL(zgbtrf, ZGBTRF)(&n, &n, &kl, &ku, ab, &ldab, ipiv, info);
    }
    static void
    gbtrs(char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs,
        const doublecomplex* ab, lapack_int ldab, const lapack_int* ipiv, doublecomplex* b,
        lapack_int* info)
    {
        LAPACK_GLOBAL(zgbtrs, ZGBTRS)
        (&trans, &n, &kl, &ku, &nrhs, ab, &ldab, ipiv, b, &n, info);
    }
    static void
    gbcon(lapack_int n, lapack_int kl, lapack_int ku, const doublecomplex* ab, lapack_int ldab,
        const lapack_int* ipiv, double anorm, double* rcond, lapack_int* info)
    {
        char norm = '1';
        std::vector<doublecomplex> work((size_t)2 * (size_t)n);
        std::vector<double> rwork((size_t)n);
        LAPACK_GLOBAL(zgbcon, ZGBCON)
        (&norm, &n, &kl, &ku, ab, &ldab, ipiv, &anorm, rcond, work.data(), rwork.data(), info);
    }
};
//=============================================================================
template <> struct SparseBandSolver<singlecomplex>
{
    using Real = single;
    static constexpr char transposeSolve = 'C';
    static void
    gbtrf(lapack_int n, lapack_int kl, lapack_int ku, singlecomplex* ab, lapack_int ldab,
        lapack_int* ipiv, lapack_int* info)
    {
        LAPACK_GLOBAL(cgbtrf, CGBTRF)(&n, &n, &kl, &ku, ab, &ldab, ipiv, info);
    }
    static void
    gbtrs(char trans, lapack_int n, lapack_int kl, lapack_int ku, lapack_int nrhs,
        const singlecomplex* ab, lapack_int ldab, const lapack_int* ipiv, singlecomplex* b,
        lapack_int* info)
    {
        LAPACK_GLOBAL(cgbtrs, CGBTRS)
        (&trans, &n, &kl, &ku, &nrhs, ab, &ldab, ipiv, b, &n, info);
    }
    static void
    gbcon(lapack_int n, lapack_int kl, lapack_int ku, const singlecomplex* ab, lapack_int ldab,
        const lapack_int* ipiv, single anorm, single* rcond, lapack_int* info)
    {
        char norm = '1';
        std::vector<singlecomplex> work((size_t)2 * (size_t)n);
        std::vector<single> rwork((size_t)n);
        LAPACK_GLOBAL(cgbcon, CGBCON)
        (&norm, &n, &kl, &ku, ab, &ldab, ipiv, &anorm, rcond, work.data(), rwork.data(), info);
    }
};
//=============================================================================
template <class T>
static Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>
arrayToDenseEigenMatrix(const ArrayOf& A)
{
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> dense(A.rows(), A.columns());
    if (A.isSparse()) {
        dense.setZero();
        const auto* sparseA = getSparseMatrixPointer<T>(A);
        for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
            for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
                dense(it.row(), it.col()) = it.value();
            }
        }
    } else {
        const T* ptrA = static_cast<const T*>(A.dataPointer());
        for (indexType col = 0; col < A.columns(); ++col) {
            for (indexType row = 0; row < A.rows(); ++row) {
                dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                    = ptrA[row + col * A.rows()];
            }
        }
    }
    return dense;
}
//=============================================================================
static NelsonType
emptyDivideResultClass(const ArrayOf& A, const ArrayOf& B)
{
    // the result is empty or all zeros: it is real
    if (A.isSingleClass() || B.isSingleClass()) {
        return NLS_SINGLE;
    }
    if (A.dataClass() == B.dataClass() && !A.isComplex()) {
        return A.dataClass();
    }
    return NLS_DOUBLE;
}
//=============================================================================
static void
promoteSparseDivideOperand(ArrayOf& value, NelsonType dataClass)
{
    if (value.dataClass() != dataClass) {
        value.promoteType(dataClass);
    }
}
//=============================================================================
static bool
isSparseDivideSingleClass(NelsonType dataClass)
{
    return dataClass == NLS_SINGLE || dataClass == NLS_SCOMPLEX;
}
//=============================================================================
static bool
isSparseDivideDoubleClass(NelsonType dataClass)
{
    return dataClass == NLS_DOUBLE || dataClass == NLS_DCOMPLEX;
}
//=============================================================================
static ArrayOf
zeroLeftDivideResult(NelsonType dataClass, const Dimensions& dimsC)
{
    if (dimsC.elementCount() == 0) {
        ArrayOf res = ArrayOf::empty(dimsC);
        res.promoteType(dataClass);
        return res;
    }
    void* ptrC = ArrayOf::allocateArrayOf(dataClass, dimsC.elementCount(), stringVector(), true);
    return ArrayOf(dataClass, dimsC, ptrC, false);
}
//=============================================================================
template <class T>
static ArrayOf
zeroSparseLeftDivideResult(NelsonType dataClass, const Dimensions& dimsC)
{
    auto* sparseResult = new NelsonSparseMatrix<T>(dimsC.rows(), dimsC.columns());
    return makeSparseOperatorResult<T>(dataClass, dimsC, sparseResult);
}
//=============================================================================
static ArrayOf
zeroSparseLeftDivideResult(NelsonType dataClass, const Dimensions& dimsC)
{
    switch (dataClass) {
    case NLS_SCOMPLEX:
        return zeroSparseLeftDivideResult<singlecomplex>(dataClass, dimsC);
    case NLS_DCOMPLEX:
        return zeroSparseLeftDivideResult<doublecomplex>(dataClass, dimsC);
    case NLS_SINGLE:
        return zeroSparseLeftDivideResult<single>(dataClass, dimsC);
    case NLS_DOUBLE:
    default:
        return zeroSparseLeftDivideResult<double>(NLS_DOUBLE, dimsC);
    }
}
//=============================================================================
template <class T>
static ArrayOf
sparseArrayToDenseArrayOf(NelsonType dataClass, const ArrayOf& A)
{
    Dimensions dims = A.dimensions();
    T* ptr = static_cast<T*>(
        ArrayOf::allocateArrayOf(dataClass, dims.elementCount(), stringVector(), true));
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
            ptr[it.row() + it.col() * dims.rows()] = it.value();
        }
    }
    return ArrayOf(dataClass, dims, ptr, false);
}
//=============================================================================
template <class T>
static ArrayOf
eigenDenseMatrixToArrayOf(
    NelsonType dataClass, const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& dense)
{
    Dimensions dims(dense.rows(), dense.cols());
    T* ptr = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, dims.elementCount()));
    for (indexType col = 0; col < dims.columns(); ++col) {
        for (indexType row = 0; row < dims.rows(); ++row) {
            ptr[row + col * dims.rows()]
                = dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
        }
    }
    return ArrayOf(dataClass, dims, ptr, false);
}
//=============================================================================
template <class T>
static ArrayOf
eigenDenseMatrixToSparseArrayOf(
    NelsonType dataClass, const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& dense)
{
    auto* sparseResult = new NelsonSparseMatrix<T>(dense.sparseView());
    return makeSparseOperatorResult<T>(
        dataClass, Dimensions(dense.rows(), dense.cols()), sparseResult);
}
//=============================================================================
template <class T>
static bool
isSparseDiagonal(const NelsonSparseMatrix<T>& A)
{
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            if (it.row() != it.col() && it.value() != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
isSparseHermitian(const NelsonSparseMatrix<T>& A)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;
    RealScalar scale = RealScalar(1);
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            scale = std::max(scale, static_cast<RealScalar>(std::abs(it.value())));
        }
    }
    const RealScalar tolerance = Eigen::NumTraits<RealScalar>::dummy_precision() * scale;
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            T transposed = Eigen::numext::conj(A.coeff(it.col(), it.row()));
            if (std::abs(it.value() - transposed) > tolerance) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
isSparseZero(const NelsonSparseMatrix<T>& A)
{
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            if (it.value() != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
isSparseDiagonalSingular(const NelsonSparseMatrix<T>& A)
{
    if (!isSparseDiagonal<T>(A)) {
        return false;
    }
    for (signedIndexType k = 0; k < A.rows(); ++k) {
        if (A.coeff(k, k) == T(0)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static void
warnSparseRankDeficient(indexType rows, indexType cols, signedIndexType rank)
{
    signedIndexType expectedRank = static_cast<signedIndexType>(rows < cols ? rows : cols);
    if (rank < expectedRank) {
        if (rows == cols) {
            Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
            return;
        }
        RaiseWarning("Nelson:rankDeficientMatrix",
            resolveMessageTemplate("Nelson:operators:matrixRankDeficientMachine",
                "Matrix is rank deficient to machine precision:")
                + " RANK = " + std::to_string(rank));
    }
}
//=============================================================================
static double
zeroPivotQuotient(double value)
{
    if (value == 0.) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return std::copysign(std::numeric_limits<double>::infinity(), value);
}
//=============================================================================
static single
zeroPivotQuotient(single value)
{
    if (value == single(0)) {
        return std::numeric_limits<single>::quiet_NaN();
    }
    return std::copysign(std::numeric_limits<single>::infinity(), value);
}
//=============================================================================
static doublecomplex
zeroPivotQuotient(const doublecomplex& value)
{
    const double inf = std::numeric_limits<double>::infinity();
    const double nan = std::numeric_limits<double>::quiet_NaN();
    const double re = value.real();
    const double im = value.imag();
    if (re == 0. && im == 0.) {
        return { nan, 0. };
    }
    return { re == 0. ? 0. : std::copysign(inf, re), im == 0. ? 0. : std::copysign(inf, im) };
}
//=============================================================================
static singlecomplex
zeroPivotQuotient(const singlecomplex& value)
{
    const single inf = std::numeric_limits<single>::infinity();
    const single nan = std::numeric_limits<single>::quiet_NaN();
    const single re = value.real();
    const single im = value.imag();
    if (re == single(0) && im == single(0)) {
        return { nan, single(0) };
    }
    return { re == single(0) ? single(0) : std::copysign(inf, re),
        im == single(0) ? single(0) : std::copysign(inf, im) };
}
//=============================================================================
template <class T>
static ArrayOf
sparseSingularDiagonalLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result(rhs.rows(), rhs.cols());
    for (signedIndexType row = 0; row < rhs.rows(); ++row) {
        T pivot = sparseA->coeff(row, row);
        for (signedIndexType col = 0; col < rhs.cols(); ++col) {
            if (pivot == T(0)) {
                result(row, col) = zeroPivotQuotient(rhs(row, col));
            } else {
                result(row, col) = rhs(row, col) / pivot;
            }
        }
    }
    Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseRegularDiagonalLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result(rhs.rows(), rhs.cols());
    for (signedIndexType row = 0; row < rhs.rows(); ++row) {
        T pivot = sparseA->coeff(row, row);
        for (signedIndexType col = 0; col < rhs.cols(); ++col) {
            result(row, col) = rhs(row, col) / pivot;
        }
    }
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseZeroSquareLeftDivide(NelsonType dataClass, const ArrayOf& B)
{
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = arrayToDenseEigenMatrix<T>(B);
    for (signedIndexType col = 0; col < result.cols(); ++col) {
        for (signedIndexType row = 0; row < result.rows(); ++row) {
            result(row, col) = zeroPivotQuotient(result(row, col));
        }
    }
    Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseSingularSquareLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> lu = arrayToDenseEigenMatrix<T>(A);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    const signedIndexType n = lu.rows();
    std::vector<signedIndexType> pivots(static_cast<size_t>(n));
    for (signedIndexType k = 0; k < n; ++k) {
        signedIndexType pivot = k;
        auto best = std::abs(lu(k, k));
        for (signedIndexType row = k + 1; row < n; ++row) {
            auto candidate = std::abs(lu(row, k));
            if (candidate > best) {
                best = candidate;
                pivot = row;
            }
        }
        pivots[static_cast<size_t>(k)] = pivot;
        if (pivot != k) {
            lu.row(k).swap(lu.row(pivot));
        }
        if (lu(k, k) == T(0)) {
            continue;
        }
        for (signedIndexType row = k + 1; row < n; ++row) {
            lu(row, k) /= lu(k, k);
            for (signedIndexType col = k + 1; col < n; ++col) {
                lu(row, col) -= lu(row, k) * lu(k, col);
            }
        }
    }

    for (signedIndexType k = 0; k < n; ++k) {
        signedIndexType pivot = pivots[static_cast<size_t>(k)];
        if (pivot != k) {
            rhs.row(k).swap(rhs.row(pivot));
        }
    }
    for (signedIndexType k = 0; k < n; ++k) {
        for (signedIndexType row = k + 1; row < n; ++row) {
            rhs.row(row) -= lu(row, k) * rhs.row(k);
        }
    }

    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result(rhs.rows(), rhs.cols());
    result.setZero();
    for (signedIndexType k = n - 1; k >= 0; --k) {
        for (signedIndexType col = 0; col < rhs.cols(); ++col) {
            T value = rhs(k, col);
            for (signedIndexType row = k + 1; row < n; ++row) {
                value -= lu(k, row) * result(row, col);
            }
            result(k, col) = value / lu(k, k);
        }
    }
    Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseSimplicialLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    solved = false;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    if (isSparseDiagonal<T>(*sparseA) || !isSparseHermitian<T>(*sparseA)) {
        return {};
    }

    Eigen::SimplicialLLT<NelsonSparseMatrix<T>, Eigen::Lower, Eigen::AMDOrdering<signedIndexType>>
        lltSolver;
    lltSolver.compute(*sparseA);
    if (lltSolver.info() == Eigen::Success) {
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = lltSolver.solve(rhs);
        if (lltSolver.info() == Eigen::Success) {
            solved = true;
            if (B.isSparse()) {
                return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
            }
            return eigenDenseMatrixToArrayOf<T>(dataClass, result);
        }
    }

    Eigen::SimplicialLDLT<NelsonSparseMatrix<T>, Eigen::Lower, Eigen::AMDOrdering<signedIndexType>>
        ldltSolver;
    ldltSolver.compute(*sparseA);
    if (ldltSolver.info() != Eigen::Success) {
        return {};
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = ldltSolver.solve(rhs);
    if (ldltSolver.info() != Eigen::Success) {
        return {};
    }
    solved = true;
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseLeastSquaresIterativeLeftDivide(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;
    solved = false;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::LeastSquaresConjugateGradient<NelsonSparseMatrix<T>> solver;
    RealScalar tolerance
        = sizeof(RealScalar) == sizeof(single) ? RealScalar(1e-6f) : RealScalar(1e-12);
    solver.setTolerance(tolerance);
    solver.setMaxIterations(static_cast<int>(std::max<indexType>(20, A.columns() * 4)));
    solver.compute(*sparseA);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = solver.solve(rhs);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> residual = (*sparseA) * result - rhs;
    RealScalar rhsNorm = rhs.norm();
    if (rhsNorm == RealScalar(0)) {
        rhsNorm = RealScalar(1);
    }
    RealScalar relativeResidual = residual.norm() / rhsNorm;
    RealScalar acceptedResidual
        = sizeof(RealScalar) == sizeof(single) ? RealScalar(1e-4f) : RealScalar(1e-7);
    if (!std::isfinite(static_cast<double>(relativeResidual))
        || relativeResidual > acceptedResidual) {
        return {};
    }
    solved = true;
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
static bool
preferIterativeRectangularSparsePath(const ArrayOf& A, const ArrayOf& B)
{
    if (A.rows() == A.columns()) {
        return false;
    }
    if (B.columns() > 4) {
        return false;
    }
    indexType largeDimension = std::max(A.rows(), A.columns());
    indexType smallDimension = std::min(A.rows(), A.columns());
    return largeDimension >= 1200 && smallDimension >= 400;
}
//=============================================================================
template <class T>
static ArrayOf
sparseQrLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    solved = false;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::SparseQR<NelsonSparseMatrix<T>, Eigen::COLAMDOrdering<signedIndexType>> solver;
    solver.compute(*sparseA);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    if (B.isSparse()) {
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = solver.solve(rhs);
        if (solver.info() != Eigen::Success) {
            return {};
        }
        warnSparseRankDeficient(A.rows(), A.columns(), solver.rank());
        solved = true;
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = solver.solve(rhs);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    warnSparseRankDeficient(A.rows(), A.columns(), solver.rank());
    solved = true;
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseLuLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    solved = false;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::SparseLU<NelsonSparseMatrix<T>, Eigen::COLAMDOrdering<signedIndexType>> solver;
    solver.compute(*sparseA);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    if (B.isSparse()) {
        const auto* sparseB = getSparseMatrixPointer<T>(B);
        auto* sparseResult = new NelsonSparseMatrix<T>(solver.solve(*sparseB));
        if (solver.info() != Eigen::Success) {
            delete sparseResult;
            return {};
        }
        solved = true;
        return makeSparseOperatorResult<T>(
            dataClass, Dimensions(A.columns(), B.columns()), sparseResult);
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = solver.solve(rhs);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    solved = true;
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
// True when nonzeros fill more than half of the band and A is neither tri-diagonal nor triangular.
template <class T>
static bool
useSparseBandSolver(const NelsonSparseMatrix<T>& A, signedIndexType& lower, signedIndexType& upper)
{
    lower = 0;
    upper = 0;
    double nonZeros = 0;
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            if (it.value() == T(0)) {
                continue;
            }
            nonZeros += 1;
            signedIndexType offset = it.row() - col;
            if (offset > lower) {
                lower = offset;
            } else if (-offset > upper) {
                upper = -offset;
            }
        }
    }
    if (lower == 0 || upper == 0 || (lower <= 1 && upper <= 1)) {
        return false;
    }
    const double n = static_cast<double>(A.rows());
    const double kl = static_cast<double>(lower);
    const double ku = static_cast<double>(upper);
    const double bandEntries = n * (kl + ku + 1) - (kl * (kl + 1) + ku * (ku + 1)) / 2;
    return nonZeros > 0.5 * bandEntries;
}
//=============================================================================
template <class R>
static void
warnSparseBandCondition(R rcond, bool zeroPivot)
{
    if (zeroPivot || rcond == R(0)) {
        RaiseWarning("Nelson:singularMatrix",
            resolveMessageTemplate(
                "Nelson:singularMatrix", "Matrix is singular to working precision."));
    } else if (std::isnan(rcond)) {
        RaiseWarning("Nelson:illConditionedMatrix",
            resolveMessageTemplate("Nelson:operators:matrixIllConditioned",
                "Matrix is singular, nearly singular or badly scaled, results may be inaccurate. "
                "RCOND = NaN."));
    } else if (rcond < std::numeric_limits<R>::epsilon()) {
        std::string format = resolveMessageTemplate("Nelson:operators:matrixNearlySingular",
            "Matrix is nearly singular or badly scaled, results may be inaccurate. RCOND = %e.");
        char buffer[1024];
        snprintf(buffer, sizeof(buffer), format.c_str(), static_cast<double>(rcond));
        std::wstring id = WARNING_NEARLY_SINGULAR_MATRIX;
        RaiseWarning(std::string(id.begin(), id.end()), buffer);
    }
}
//=============================================================================
// Band LU solve; a transposed solve gets the conjugate transpose and factors the original matrix.
template <class T>
static ArrayOf
sparseBandLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B,
    signedIndexType lower, signedIndexType upper, bool transposed)
{
    using Solver = SparseBandSolver<T>;
    using Real = typename Solver::Real;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const lapack_int n = static_cast<lapack_int>(A.rows());
    const lapack_int kl = static_cast<lapack_int>(transposed ? upper : lower);
    const lapack_int ku = static_cast<lapack_int>(transposed ? lower : upper);
    const lapack_int ldab = 2 * kl + ku + 1;
    std::vector<T> ab((size_t)ldab * (size_t)n, T(0));
    std::vector<Real> columnSums((size_t)n, Real(0));
    for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
            signedIndexType row = it.row();
            signedIndexType column = col;
            T value = it.value();
            if (transposed) {
                std::swap(row, column);
                value = Eigen::numext::conj(value);
            }
            ab[(size_t)(kl + ku + row - column) + (size_t)column * (size_t)ldab] = value;
            columnSums[(size_t)column] += static_cast<Real>(std::abs(value));
        }
    }
    Real anorm = 0;
    for (Real sum : columnSums) {
        if (std::isnan(sum)) {
            anorm = sum;
            break;
        }
        anorm = std::max(anorm, sum);
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = arrayToDenseEigenMatrix<T>(B);
    const lapack_int nrhs = static_cast<lapack_int>(result.cols());
    std::vector<lapack_int> ipiv((size_t)n);
    lapack_int info = 0;
    Solver::gbtrf(n, kl, ku, ab.data(), ldab, ipiv.data(), &info);
    bool zeroPivot = info > 0;
    Real rcond = 0;
    if (!zeroPivot) {
        if (std::isnan(anorm)) {
            rcond = std::numeric_limits<Real>::quiet_NaN();
        } else if (!std::isinf(anorm)) {
            lapack_int conInfo = 0;
            Solver::gbcon(n, kl, ku, ab.data(), ldab, ipiv.data(), anorm, &rcond, &conInfo);
        }
    }
    // a zero pivot gives Inf or NaN entries, as plain substitution does
    lapack_int solveInfo = 0;
    Solver::gbtrs(transposed ? Solver::transposeSolve : 'N', n, kl, ku, nrhs, ab.data(), ldab,
        ipiv.data(), result.data(), &solveInfo);
    warnSparseBandCondition<Real>(rcond, zeroPivot);
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
static ArrayOf
leftDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload, bool transposed);
//=============================================================================
template <class T>
static ArrayOf
sparseLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& needToOverload,
    bool transposed)
{
    needToOverload = false;
    if (A.dataClass() != B.dataClass()) {
        needToOverload = true;
        return {};
    }
    if (!A.isSparse() && B.isSparse()) {
        // keep the transposed flag: '/' must factor the original dense divisor
        ArrayOf denseB = sparseArrayToDenseArrayOf<T>(dataClass, B);
        return leftDivide(A, denseB, needToOverload, transposed);
    }
    if (!A.isSparse()) {
        needToOverload = true;
        return {};
    }
    if (A.rows() != B.rows()) {
        Error(L"Nelson:dimagree:requestedDivideOperationRequiresArgumentsCorrect",
            L"Requested divide operation requires arguments to have correct dimensions.");
    }

    if (A.rows() == A.columns()) {
        const auto* sparseA = getSparseMatrixPointer<T>(A);
        if (isSparseZero<T>(*sparseA)) {
            return sparseZeroSquareLeftDivide<T>(dataClass, B);
        }
        if (isSparseDiagonalSingular<T>(*sparseA)) {
            return sparseSingularDiagonalLeftDivide<T>(dataClass, A, B);
        }
        if (isSparseDiagonal<T>(*sparseA)) {
            return sparseRegularDiagonalLeftDivide<T>(dataClass, A, B);
        }
        signedIndexType lower = 0;
        signedIndexType upper = 0;
        if (useSparseBandSolver<T>(*sparseA, lower, upper)) {
            return sparseBandLeftDivide<T>(dataClass, A, B, lower, upper, transposed);
        }
        bool solved = false;
        ArrayOf result = sparseSimplicialLeftDivide<T>(dataClass, A, B, solved);
        if (solved) {
            return result;
        }
        result = sparseLuLeftDivide<T>(dataClass, A, B, solved);
        if (solved) {
            return result;
        }
        return sparseSingularSquareLeftDivide<T>(dataClass, A, B);
    } else {
        bool solved = false;
        if (preferIterativeRectangularSparsePath(A, B)) {
            ArrayOf result = sparseLeastSquaresIterativeLeftDivide<T>(dataClass, A, B, solved);
            if (solved) {
                return result;
            }
        }
        ArrayOf result = sparseQrLeftDivide<T>(dataClass, A, B, solved);
        if (solved) {
            return result;
        }
        if (!preferIterativeRectangularSparsePath(A, B)) {
            result = sparseLeastSquaresIterativeLeftDivide<T>(dataClass, A, B, solved);
            if (solved) {
                return result;
            }
        }
    }
    bool solved = false;
    ArrayOf result = sparseQrLeftDivide<T>(dataClass, A, B, solved);
    if (solved) {
        return result;
    }
    Error(L"Nelson:operators:sparseSolverFailed", L"Sparse linear equation solver failed.");
    return {};
}
//=============================================================================
static ArrayOf
sparseLeftDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload, bool transposed)
{
    switch (A.dataClass()) {
    case NLS_SINGLE:
        return sparseLeftDivide<single>(NLS_SINGLE, A, B, needToOverload, transposed);
    case NLS_SCOMPLEX:
        return sparseLeftDivide<singlecomplex>(NLS_SCOMPLEX, A, B, needToOverload, transposed);
    case NLS_DOUBLE:
        return sparseLeftDivide<double>(NLS_DOUBLE, A, B, needToOverload, transposed);
    case NLS_DCOMPLEX:
        return sparseLeftDivide<doublecomplex>(NLS_DCOMPLEX, A, B, needToOverload, transposed);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
static ArrayOf
promotedSparseLeftDivide(const ArrayOf& A, const ArrayOf& B, NelsonType dataClass,
    bool& needToOverload, bool transposed)
{
    ArrayOf AA = A;
    ArrayOf BB = B;
    promoteSparseDivideOperand(AA, dataClass);
    promoteSparseDivideOperand(BB, dataClass);
    return sparseLeftDivide(AA, BB, needToOverload, transposed);
}
//=============================================================================
static ArrayOf
leftDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload, bool transposed)
{
    needToOverload = false;
    if (A.isScalar()) {
        return DotLeftDivide(A, B, needToOverload);
    }
    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dimsA = A.dimensions();
        Dimensions dimsB = B.dimensions();
        dimsA.simplify();
        dimsB.simplify();
        if (dimsA.length() > 2 || dimsB.length() > 2) {
            Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
        }
        if (dimsA[0] != dimsB[0]) {
            Error(L"Nelson:dimagree:requestedDivideOperationRequiresArgumentsCorrect",
                L"Requested divide operation requires arguments to have correct dimensions.");
        }
        Dimensions dimsC(dimsA[1], dimsB[1]);
        NelsonType dataClass = emptyDivideResultClass(A, B);
        if (A.isSparse() && B.isSparse()) {
            return zeroSparseLeftDivideResult(dataClass, dimsC);
        }
        return zeroLeftDivideResult(dataClass, dimsC);
    }

    if (A.isSparse() || B.isSparse()) {
        if (A.dataClass() != B.dataClass()) {
            bool isComplex = A.isComplex() || B.isComplex();
            NelsonType dataClassA = A.dataClass();
            NelsonType dataClassB = B.dataClass();
            if ((isSparseDivideDoubleClass(dataClassA) && isSparseDivideSingleClass(dataClassB))
                || (isSparseDivideSingleClass(dataClassA)
                    && isSparseDivideDoubleClass(dataClassB))) {
                return promotedSparseLeftDivide(
                    A, B, isComplex ? NLS_SCOMPLEX : NLS_SINGLE, needToOverload, transposed);
            }
            if (isSparseDivideDoubleClass(dataClassA) && isSparseDivideDoubleClass(dataClassB)
                && isComplex) {
                return promotedSparseLeftDivide(A, B, NLS_DCOMPLEX, needToOverload, transposed);
            }
            if (isSparseDivideSingleClass(dataClassA) && isSparseDivideSingleClass(dataClassB)
                && isComplex) {
                return promotedSparseLeftDivide(A, B, NLS_SCOMPLEX, needToOverload, transposed);
            }
        }
        return sparseLeftDivide(A, B, needToOverload, transposed);
    }

    if (!A.is2D() || !B.is2D()) {
        Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
    }
    if (A.dimensionLength(0) != B.dimensionLength(0)) {
        Error(L"Nelson:dimagree:requestedDivideOperationRequiresArgumentsCorrect",
            L"Requested divide operation requires arguments to have correct dimensions.");
    }

    if (A.dataClass() != B.dataClass()) {
        bool isComplex = A.isComplex() || B.isComplex();
        if ((A.isDoubleClass() && B.isSingleClass()) || (A.isSingleClass() && B.isDoubleClass())) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            if (isComplex) {
                AA.promoteType(NLS_SCOMPLEX);
                BB.promoteType(NLS_SCOMPLEX);
            } else {
                AA.promoteType(NLS_SINGLE);
                BB.promoteType(NLS_SINGLE);
            }
            return leftDivide(AA, BB, needToOverload, transposed);
        }
        if (A.isDoubleType() && B.isDoubleType() && isComplex) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            AA.promoteType(NLS_DCOMPLEX);
            BB.promoteType(NLS_DCOMPLEX);
            return leftDivide(AA, BB, needToOverload, transposed);
        }
        if (A.isSingleType() && B.isSingleType() && isComplex) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            AA.promoteType(NLS_SCOMPLEX);
            BB.promoteType(NLS_SCOMPLEX);
            return leftDivide(AA, BB, needToOverload, transposed);
        }

        if (A.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(B);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (B.dataClass() == NLS_CHAR) {
                ArrayOf BB = B;
                BB.promoteType(A.dataClass());
                return leftDivide(A, BB, needToOverload, transposed);
            }
            if (B.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf AA = A;
            AA.promoteType(B.dataClass());
            ArrayOf res = leftDivide(AA, B, needToOverload, transposed);
            if (!needToOverload) {
                res.promoteType(A.dataClass());
            }
            return res;
        } else if (B.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(A);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (A.dataClass() == NLS_CHAR) {
                ArrayOf AA = A;
                AA.promoteType(B.dataClass());
                return leftDivide(AA, B, needToOverload, transposed);
            }
            if (A.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf BB = B;
            BB.promoteType(A.dataClass());
            ArrayOf res = leftDivide(A, BB, needToOverload, transposed);
            if (!needToOverload) {
                res.promoteType(B.dataClass());
            }
            return res;
        }
    }

    bool diagonalSolved = false;
    ArrayOf diagonalResult = denseDiagonalDivide<false>(A, B, diagonalSolved);
    if (diagonalSolved) {
        return diagonalResult;
    }

    std::wstring warningId;
    std::string warningMessage;
    ArrayOf res;

    bool isSquare = A.dimensionLength(0) == A.dimensionLength(1);

    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        res = isSquare ? solveLinearEquationDouble(A, B, warningId, warningMessage, transposed)
                       : solveLeastSquareDouble(A, B, warningId, warningMessage);
    } break;
    case NLS_SINGLE: {
        res = isSquare ? solveLinearEquationSingle(A, B, warningId, warningMessage, transposed)
                       : solveLeastSquareSingle(A, B, warningId, warningMessage);
    } break;
    case NLS_DCOMPLEX: {
        res = isSquare
            ? solveLinearEquationDoubleComplex(A, B, warningId, warningMessage, transposed)
            : solveLeastSquareDoubleComplex(A, B, warningId, warningMessage);
    } break;
    case NLS_SCOMPLEX: {
        res = isSquare
            ? solveLinearEquationSingleComplex(A, B, warningId, warningMessage, transposed)
            : solveLeastSquareSingleComplex(A, B, warningId, warningMessage);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    if (!warningMessage.empty()) {
        RaiseWarning(std::string(warningId.begin(), warningId.end()), warningMessage);
    }
    return res;
}
//=============================================================================
static ArrayOf
leftDivideRealIfNoImaginaryPart(
    const ArrayOf& A, const ArrayOf& B, bool& needToOverload, bool transposed)
{
    // a complex quotient whose imaginary parts are all zero is returned as real
    ArrayOf res = leftDivide(A, B, needToOverload, transposed);
    if (!needToOverload && res.isComplex()) {
        res = decomplexify(res);
    }
    return res;
}
//=============================================================================
ArrayOf
LeftDivide(ArrayOf A, ArrayOf B, bool& needToOverload)
{
    return leftDivideRealIfNoImaginaryPart(A, B, needToOverload, false);
}
//=============================================================================
ArrayOf
LeftDivideTransposed(ArrayOf A, ArrayOf B, bool& needToOverload)
{
    return leftDivideRealIfNoImaginaryPart(A, B, needToOverload, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
