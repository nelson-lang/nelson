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
#include "lapack_eigen_config.hpp"
#include "LapackPrototypes.hpp"
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <complex>
#include <limits>
#include <string>
#include <type_traits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T> struct LapackLeastSquaresTraits;
//=============================================================================
template <> struct LapackLeastSquaresTraits<double>
{
    using Scalar = double;
    using Real = double;
    static constexpr NelsonType nelsonClass = NLS_DOUBLE;
    static void
    gelsy(const lapack_int* m, const lapack_int* n, const lapack_int* nrhs, Scalar* a,
        const lapack_int* lda, Scalar* b, const lapack_int* ldb, lapack_int* jpvt,
        const Real* rcond, lapack_int* rank, Scalar* work, const lapack_int* lwork, Real*,
        lapack_int* info)
    {
        LAPACK_dgelsy(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, info);
    }
    static void
    geqp3(lapack_int* m, lapack_int* n, Scalar* a, lapack_int* lda, lapack_int* jpvt, Scalar* tau,
        Scalar* work, lapack_int* lwork, Real*, lapack_int* info)
    {
        LAPACK_dgeqp3(m, n, a, lda, jpvt, tau, work, lwork, info);
    }
    static void
    geqrf(lapack_int* m, lapack_int* n, Scalar* a, lapack_int* lda, Scalar* tau, Scalar* work,
        lapack_int* lwork, lapack_int* info)
    {
        LAPACK_dgeqrf(m, n, a, lda, tau, work, lwork, info);
    }
    static void
    ormqr(char* side, char* trans, lapack_int* m, lapack_int* n, lapack_int* k, const Scalar* a,
        lapack_int* lda, const Scalar* tau, Scalar* c, lapack_int* ldc, Scalar* work,
        lapack_int* lwork, lapack_int* info)
    {
        LAPACK_dormqr(side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork, info);
    }
    static void
    gelss(const lapack_int* m, const lapack_int* n, const lapack_int* nrhs, Scalar* a,
        const lapack_int* lda, Scalar* b, const lapack_int* ldb, Real* s, const Real* rcond,
        lapack_int* rank, Scalar* work, const lapack_int* lwork, Real*, lapack_int* info)
    {
        LAPACK_dgelss(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, lwork, info);
    }
};
//=============================================================================
template <> struct LapackLeastSquaresTraits<single>
{
    using Scalar = single;
    using Real = single;
    static constexpr NelsonType nelsonClass = NLS_SINGLE;
    static void
    gelsy(const lapack_int* m, const lapack_int* n, const lapack_int* nrhs, Scalar* a,
        const lapack_int* lda, Scalar* b, const lapack_int* ldb, lapack_int* jpvt,
        const Real* rcond, lapack_int* rank, Scalar* work, const lapack_int* lwork, Real*,
        lapack_int* info)
    {
        LAPACK_sgelsy(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, info);
    }
    static void
    geqp3(lapack_int* m, lapack_int* n, Scalar* a, lapack_int* lda, lapack_int* jpvt, Scalar* tau,
        Scalar* work, lapack_int* lwork, Real*, lapack_int* info)
    {
        LAPACK_sgeqp3(m, n, a, lda, jpvt, tau, work, lwork, info);
    }
    static void
    geqrf(lapack_int* m, lapack_int* n, Scalar* a, lapack_int* lda, Scalar* tau, Scalar* work,
        lapack_int* lwork, lapack_int* info)
    {
        LAPACK_sgeqrf(m, n, a, lda, tau, work, lwork, info);
    }
    static void
    ormqr(char* side, char* trans, lapack_int* m, lapack_int* n, lapack_int* k, const Scalar* a,
        lapack_int* lda, const Scalar* tau, Scalar* c, lapack_int* ldc, Scalar* work,
        lapack_int* lwork, lapack_int* info)
    {
        LAPACK_sormqr(side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork, info);
    }
    static void
    gelss(const lapack_int* m, const lapack_int* n, const lapack_int* nrhs, Scalar* a,
        const lapack_int* lda, Scalar* b, const lapack_int* ldb, Real* s, const Real* rcond,
        lapack_int* rank, Scalar* work, const lapack_int* lwork, Real*, lapack_int* info)
    {
        LAPACK_sgelss(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, lwork, info);
    }
};
//=============================================================================
template <> struct LapackLeastSquaresTraits<doublecomplex>
{
    using Scalar = doublecomplex;
    using Real = double;
    static constexpr NelsonType nelsonClass = NLS_DCOMPLEX;
    static void
    gelsy(const lapack_int* m, const lapack_int* n, const lapack_int* nrhs, Scalar* a,
        const lapack_int* lda, Scalar* b, const lapack_int* ldb, lapack_int* jpvt,
        const Real* rcond, lapack_int* rank, Scalar* work, const lapack_int* lwork, Real* rwork,
        lapack_int* info)
    {
        LAPACK_zgelsy(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, rwork, info);
    }
    static void
    geqp3(lapack_int* m, lapack_int* n, Scalar* a, lapack_int* lda, lapack_int* jpvt, Scalar* tau,
        Scalar* work, lapack_int* lwork, Real* rwork, lapack_int* info)
    {
        LAPACK_zgeqp3(m, n, a, lda, jpvt, tau, work, lwork, rwork, info);
    }
    static void
    geqrf(lapack_int* m, lapack_int* n, Scalar* a, lapack_int* lda, Scalar* tau, Scalar* work,
        lapack_int* lwork, lapack_int* info)
    {
        LAPACK_zgeqrf(m, n, a, lda, tau, work, lwork, info);
    }
    static void
    ormqr(char* side, char* trans, lapack_int* m, lapack_int* n, lapack_int* k, const Scalar* a,
        lapack_int* lda, const Scalar* tau, Scalar* c, lapack_int* ldc, Scalar* work,
        lapack_int* lwork, lapack_int* info)
    {
        LAPACK_zunmqr(side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork, info);
    }
    static void
    gelss(const lapack_int* m, const lapack_int* n, const lapack_int* nrhs, Scalar* a,
        const lapack_int* lda, Scalar* b, const lapack_int* ldb, Real* s, const Real* rcond,
        lapack_int* rank, Scalar* work, const lapack_int* lwork, Real* rwork, lapack_int* info)
    {
        LAPACK_zgelss(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, lwork, rwork, info);
    }
};
//=============================================================================
template <> struct LapackLeastSquaresTraits<singlecomplex>
{
    using Scalar = singlecomplex;
    using Real = single;
    static constexpr NelsonType nelsonClass = NLS_SCOMPLEX;
    static void
    gelsy(const lapack_int* m, const lapack_int* n, const lapack_int* nrhs, Scalar* a,
        const lapack_int* lda, Scalar* b, const lapack_int* ldb, lapack_int* jpvt,
        const Real* rcond, lapack_int* rank, Scalar* work, const lapack_int* lwork, Real* rwork,
        lapack_int* info)
    {
        LAPACK_cgelsy(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, rwork, info);
    }
    static void
    geqp3(lapack_int* m, lapack_int* n, Scalar* a, lapack_int* lda, lapack_int* jpvt, Scalar* tau,
        Scalar* work, lapack_int* lwork, Real* rwork, lapack_int* info)
    {
        LAPACK_cgeqp3(m, n, a, lda, jpvt, tau, work, lwork, rwork, info);
    }
    static void
    geqrf(lapack_int* m, lapack_int* n, Scalar* a, lapack_int* lda, Scalar* tau, Scalar* work,
        lapack_int* lwork, lapack_int* info)
    {
        LAPACK_cgeqrf(m, n, a, lda, tau, work, lwork, info);
    }
    static void
    ormqr(char* side, char* trans, lapack_int* m, lapack_int* n, lapack_int* k, const Scalar* a,
        lapack_int* lda, const Scalar* tau, Scalar* c, lapack_int* ldc, Scalar* work,
        lapack_int* lwork, lapack_int* info)
    {
        LAPACK_cunmqr(side, trans, m, n, k, a, lda, tau, c, ldc, work, lwork, info);
    }
    static void
    gelss(const lapack_int* m, const lapack_int* n, const lapack_int* nrhs, Scalar* a,
        const lapack_int* lda, Scalar* b, const lapack_int* ldb, Real* s, const Real* rcond,
        lapack_int* rank, Scalar* work, const lapack_int* lwork, Real* rwork, lapack_int* info)
    {
        LAPACK_cgelss(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, lwork, rwork, info);
    }
};
//=============================================================================
template <typename T>
static void
copyArrayToLapackBuffer(const ArrayOf& source, std::vector<T>& destination, lapack_int rows)
{
    const T* src = reinterpret_cast<const T*>(source.dataPointer());
    for (indexType col = 0; col < source.columns(); ++col) {
        for (indexType row = 0; row < source.rows(); ++row) {
            destination[row + col * rows] = src[row + col * source.rows()];
        }
    }
}
//=============================================================================
template <typename T>
static ArrayOf
copyLapackSolutionToArray(NelsonType destinationClass, const std::vector<T>& source,
    lapack_int rows, indexType n, indexType nrhs)
{
    T* ptr = reinterpret_cast<T*>(ArrayOf::allocateArrayOf(destinationClass, n * nrhs));
    for (indexType col = 0; col < nrhs; ++col) {
        for (indexType row = 0; row < n; ++row) {
            ptr[row + col * n] = source[row + col * rows];
        }
    }
    return ArrayOf(destinationClass, Dimensions(n, nrhs), ptr, false);
}
//=============================================================================
template <typename T>
static typename LapackLeastSquaresTraits<T>::Real
lapackWorkspaceValue(const T& value)
{
    return static_cast<typename LapackLeastSquaresTraits<T>::Real>(std::real(value));
}
//=============================================================================
template <typename T>
static lapack_int
estimateQrRank(const std::vector<T>& qr, lapack_int lda, lapack_int m, lapack_int n)
{
    using Real = typename LapackLeastSquaresTraits<T>::Real;
    lapack_int diagonalSize = std::min(m, n);
    Real scale = Real(0);
    for (lapack_int k = 0; k < diagonalSize; ++k) {
        scale = std::max(scale, static_cast<Real>(std::abs(qr[k + k * lda])));
    }
    if (scale == Real(0)) {
        return 0;
    }
    Real tolerance
        = static_cast<Real>(std::max(m, n)) * std::numeric_limits<Real>::epsilon() * scale;
    lapack_int rank = 0;
    for (lapack_int k = 0; k < diagonalSize; ++k) {
        if (static_cast<Real>(std::abs(qr[k + k * lda])) > tolerance) {
            ++rank;
        }
    }
    return rank;
}
//=============================================================================
template <typename T>
static void
solvePivotedUpperTriangular(const std::vector<T>& qr, const std::vector<T>& qtb,
    std::vector<T>& solution, const std::vector<lapack_int>& jpvt, lapack_int lda, lapack_int ldb,
    lapack_int rank, lapack_int n, lapack_int nrhs)
{
    std::vector<T> pivoted(static_cast<size_t>(std::max<lapack_int>(1, rank)));
    for (lapack_int col = 0; col < nrhs; ++col) {
        std::fill(pivoted.begin(), pivoted.end(), T {});
        for (lapack_int row = rank - 1; row >= 0; --row) {
            T value = qtb[row + col * ldb];
            for (lapack_int k = row + 1; k < rank; ++k) {
                value -= qr[row + k * lda] * pivoted[k];
            }
            pivoted[row] = value / qr[row + row * lda];
        }
        for (lapack_int row = 0; row < rank; ++row) {
            lapack_int originalColumn = jpvt[static_cast<size_t>(row)] - 1;
            solution[static_cast<size_t>(originalColumn) + static_cast<size_t>(col) * n]
                = pivoted[row];
        }
    }
}
//=============================================================================
template <typename T>
static ArrayOf
solveLapackFullRankQrLeastSquares(NelsonType destinationClass, const ArrayOf& matA,
    const ArrayOf& matB, lapack_int& rank, lapack_int& info)
{
    using Traits = LapackLeastSquaresTraits<T>;
    lapack_int m = static_cast<lapack_int>(matA.rows());
    lapack_int n = static_cast<lapack_int>(matA.columns());
    lapack_int nrhs = static_cast<lapack_int>(matB.columns());
    lapack_int lda = std::max<lapack_int>(1, m);
    lapack_int ldb = std::max<lapack_int>(1, m);
    lapack_int reflectors = std::min(m, n);
    std::vector<T> qr(static_cast<size_t>(lda) * static_cast<size_t>(std::max<lapack_int>(1, n)));
    std::vector<T> qtb(
        static_cast<size_t>(ldb) * static_cast<size_t>(std::max<lapack_int>(1, nrhs)));
    copyArrayToLapackBuffer<T>(matA, qr, lda);
    copyArrayToLapackBuffer<T>(matB, qtb, ldb);
    std::vector<T> tau(static_cast<size_t>(std::max<lapack_int>(1, reflectors)));
    T workQuery {};
    lapack_int lwork = -1;
    Traits::geqrf(&m, &n, qr.data(), &lda, tau.data(), &workQuery, &lwork, &info);
    if (info != 0) {
        return {};
    }
    lwork = std::max<lapack_int>(1, static_cast<lapack_int>(lapackWorkspaceValue<T>(workQuery)));
    std::vector<T> work(static_cast<size_t>(lwork));
    Traits::geqrf(&m, &n, qr.data(), &lda, tau.data(), work.data(), &lwork, &info);
    if (info != 0) {
        return {};
    }
    rank = estimateQrRank<T>(qr, lda, m, n);
    if (rank < n) {
        return {};
    }
    char side = 'L';
    char trans = std::is_same<T, double>::value || std::is_same<T, single>::value ? 'T' : 'C';
    lwork = -1;
    Traits::ormqr(&side, &trans, &m, &nrhs, &reflectors, qr.data(), &lda, tau.data(), qtb.data(),
        &ldb, &workQuery, &lwork, &info);
    if (info != 0) {
        return {};
    }
    lwork = std::max<lapack_int>(1, static_cast<lapack_int>(lapackWorkspaceValue<T>(workQuery)));
    work.assign(static_cast<size_t>(lwork), T {});
    Traits::ormqr(&side, &trans, &m, &nrhs, &reflectors, qr.data(), &lda, tau.data(), qtb.data(),
        &ldb, work.data(), &lwork, &info);
    if (info != 0) {
        return {};
    }
    std::vector<T> solution(
        static_cast<size_t>(std::max<lapack_int>(1, n)) * static_cast<size_t>(nrhs));
    std::vector<lapack_int> jpvt(static_cast<size_t>(n));
    for (lapack_int k = 0; k < n; ++k) {
        jpvt[static_cast<size_t>(k)] = k + 1;
    }
    solvePivotedUpperTriangular<T>(qr, qtb, solution, jpvt, lda, ldb, n, n, nrhs);
    return copyLapackSolutionToArray<T>(
        destinationClass, solution, n, matA.columns(), matB.columns());
}
//=============================================================================
template <typename T>
static ArrayOf
solveLapackQrLeastSquares(NelsonType destinationClass, const ArrayOf& matA, const ArrayOf& matB,
    lapack_int& rank, lapack_int& info)
{
    if (matA.rows() >= matA.columns()) {
        ArrayOf fullRankResult
            = solveLapackFullRankQrLeastSquares<T>(destinationClass, matA, matB, rank, info);
        if (info != 0) {
            return {};
        }
        if (!fullRankResult.isEmpty()) {
            return fullRankResult;
        }
    }
    using Traits = LapackLeastSquaresTraits<T>;
    using Real = typename Traits::Real;
    lapack_int m = static_cast<lapack_int>(matA.rows());
    lapack_int n = static_cast<lapack_int>(matA.columns());
    lapack_int nrhs = static_cast<lapack_int>(matB.columns());
    lapack_int lda = std::max<lapack_int>(1, m);
    lapack_int ldb = std::max<lapack_int>(1, std::max(m, n));
    lapack_int reflectors = std::min(m, n);
    std::vector<T> qr(static_cast<size_t>(lda) * static_cast<size_t>(std::max<lapack_int>(1, n)));
    std::vector<T> qtb(
        static_cast<size_t>(ldb) * static_cast<size_t>(std::max<lapack_int>(1, nrhs)));
    copyArrayToLapackBuffer<T>(matA, qr, lda);
    copyArrayToLapackBuffer<T>(matB, qtb, ldb);
    std::vector<lapack_int> jpvt(static_cast<size_t>(std::max<lapack_int>(1, n)), 0);
    std::vector<T> tau(static_cast<size_t>(std::max<lapack_int>(1, reflectors)));
    std::vector<Real> rwork(static_cast<size_t>(std::max<lapack_int>(1, 2 * n)));
    T workQuery {};
    lapack_int lwork = -1;
    Traits::geqp3(
        &m, &n, qr.data(), &lda, jpvt.data(), tau.data(), &workQuery, &lwork, rwork.data(), &info);
    if (info != 0) {
        return {};
    }
    lwork = std::max<lapack_int>(1, static_cast<lapack_int>(lapackWorkspaceValue<T>(workQuery)));
    std::vector<T> work(static_cast<size_t>(lwork));
    Traits::geqp3(
        &m, &n, qr.data(), &lda, jpvt.data(), tau.data(), work.data(), &lwork, rwork.data(), &info);
    if (info != 0) {
        return {};
    }
    char side = 'L';
    char trans = std::is_same<T, double>::value || std::is_same<T, single>::value ? 'T' : 'C';
    lwork = -1;
    Traits::ormqr(&side, &trans, &m, &nrhs, &reflectors, qr.data(), &lda, tau.data(), qtb.data(),
        &ldb, &workQuery, &lwork, &info);
    if (info != 0) {
        return {};
    }
    lwork = std::max<lapack_int>(1, static_cast<lapack_int>(lapackWorkspaceValue<T>(workQuery)));
    work.assign(static_cast<size_t>(lwork), T {});
    Traits::ormqr(&side, &trans, &m, &nrhs, &reflectors, qr.data(), &lda, tau.data(), qtb.data(),
        &ldb, work.data(), &lwork, &info);
    if (info != 0) {
        return {};
    }
    rank = estimateQrRank<T>(qr, lda, m, n);
    std::vector<T> solution(static_cast<size_t>(std::max<lapack_int>(1, n))
        * static_cast<size_t>(std::max<lapack_int>(1, nrhs)));
    solvePivotedUpperTriangular<T>(qr, qtb, solution, jpvt, lda, ldb, rank, n, nrhs);
    return copyLapackSolutionToArray<T>(
        destinationClass, solution, n, matA.columns(), matB.columns());
}
//=============================================================================
template <typename T>
static lapack_int
lapackLeastSquaresWorkspace(bool svdDriver, lapack_int m, lapack_int n, lapack_int nrhs,
    std::vector<T>& a, lapack_int lda, std::vector<T>& b, lapack_int ldb,
    std::vector<lapack_int>& jpvt, std::vector<typename LapackLeastSquaresTraits<T>::Real>& s,
    typename LapackLeastSquaresTraits<T>::Real rcond, lapack_int& rank,
    std::vector<typename LapackLeastSquaresTraits<T>::Real>& rwork, lapack_int& info)
{
    using Traits = LapackLeastSquaresTraits<T>;
    using Real = typename Traits::Real;
    T workQuery {};
    lapack_int lwork = -1;
    if (svdDriver) {
        Traits::gelss(&m, &n, &nrhs, a.data(), &lda, b.data(), &ldb, s.data(), &rcond, &rank,
            &workQuery, &lwork, rwork.data(), &info);
    } else {
        Traits::gelsy(&m, &n, &nrhs, a.data(), &lda, b.data(), &ldb, jpvt.data(), &rcond, &rank,
            &workQuery, &lwork, rwork.data(), &info);
    }
    if (info != 0) {
        return 0;
    }
    Real query = static_cast<Real>(std::real(workQuery));
    return std::max<lapack_int>(1, static_cast<lapack_int>(query));
}
//=============================================================================
template <typename T>
static ArrayOf
solveLapackLeastSquares(NelsonType destinationClass, const ArrayOf& matA, const ArrayOf& matB,
    bool svdDriver, lapack_int& rank, lapack_int& info)
{
    if (!svdDriver) {
        return solveLapackQrLeastSquares<T>(destinationClass, matA, matB, rank, info);
    }
    using Traits = LapackLeastSquaresTraits<T>;
    using Real = typename Traits::Real;
    lapack_int m = static_cast<lapack_int>(matA.rows());
    lapack_int n = static_cast<lapack_int>(matA.columns());
    lapack_int nrhs = static_cast<lapack_int>(matB.columns());
    lapack_int lda = std::max<lapack_int>(1, m);
    lapack_int ldb = std::max<lapack_int>(1, std::max(m, n));
    std::vector<T> a(static_cast<size_t>(lda) * static_cast<size_t>(std::max<lapack_int>(1, n)));
    std::vector<T> b(static_cast<size_t>(ldb) * static_cast<size_t>(std::max<lapack_int>(1, nrhs)));
    copyArrayToLapackBuffer<T>(matA, a, lda);
    copyArrayToLapackBuffer<T>(matB, b, ldb);
    std::vector<lapack_int> jpvt(static_cast<size_t>(std::max<lapack_int>(1, n)), 0);
    std::vector<Real> s(static_cast<size_t>(std::max<lapack_int>(1, std::min(m, n))));
    lapack_int rworkSize = std::max<lapack_int>(1, 5 * std::min(m, n));
    rworkSize = std::max<lapack_int>(rworkSize, 2 * n);
    std::vector<Real> rwork(static_cast<size_t>(rworkSize));
    Real rcond = static_cast<Real>(-1);
    rank = 0;
    info = 0;
    lapack_int lwork = lapackLeastSquaresWorkspace<T>(
        svdDriver, m, n, nrhs, a, lda, b, ldb, jpvt, s, rcond, rank, rwork, info);
    if (info != 0) {
        return {};
    }
    std::vector<T> work(static_cast<size_t>(lwork));
    if (svdDriver) {
        Traits::gelss(&m, &n, &nrhs, a.data(), &lda, b.data(), &ldb, s.data(), &rcond, &rank,
            work.data(), &lwork, rwork.data(), &info);
    } else {
        Traits::gelsy(&m, &n, &nrhs, a.data(), &lda, b.data(), &ldb, jpvt.data(), &rcond, &rank,
            work.data(), &lwork, rwork.data(), &info);
    }
    if (info != 0) {
        return {};
    }
    return copyLapackSolutionToArray<T>(destinationClass, b, ldb, matA.columns(), matB.columns());
}
//=============================================================================
template <typename T>
static bool
isRankDeficient(indexType rows, indexType columns, lapack_int rank)
{
    lapack_int expectedRank = static_cast<lapack_int>(std::min(rows, columns));
    return rank < expectedRank;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
