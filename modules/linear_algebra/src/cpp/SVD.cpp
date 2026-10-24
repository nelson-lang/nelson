//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "i18n.hpp"
#include "BlasThreadScope.hpp"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include "SVD.hpp"
#include "ClassName.hpp"
#include <algorithm>
#include <cmath>
#include "LapackPrototypes.hpp"
#include <Eigen/Dense>
#include <Eigen/SVD>
#include <limits>
#include <vector>
#include "NewWithException.hpp"
#include "NelsonConfiguration.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Helper: in-place transpose for square column-major matrices. Falls back to Eigen
// transpose when leading dimension != n.
template <typename T>
static void
inplace_transpose_colmajor(T* data, int n, int ld)
{
    if (n <= 1) {
        return;
    }
    if (ld == n) {
        for (int i = 0; i < n; ++i) {
            for (int j = i + 1; j < n; ++j) {
                std::swap(data[i + j * (size_t)ld], data[j + i * (size_t)ld]);
            }
        }
    } else {
        // fallback to Eigen-based transpose (allocates temporary)
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> mat(data, n, n);
        mat = mat.transpose().eval();
    }
}
//=============================================================================
template <typename T>
static void
inplace_adjoint_colmajor(std::complex<T>* data, int n, int ld)
{
    if (n <= 1) {
        if (n == 1) {
            data[0] = std::conj(data[0]);
        }
        return;
    }
    if (ld == n) {
        for (int i = 0; i < n; ++i) {
            data[i + i * (size_t)ld] = std::conj(data[i + i * (size_t)ld]);
            for (int j = i + 1; j < n; ++j) {
                auto upper = std::conj(data[i + j * (size_t)ld]);
                auto lower = std::conj(data[j + i * (size_t)ld]);
                data[i + j * (size_t)ld] = lower;
                data[j + i * (size_t)ld] = upper;
            }
        }
    } else {
        Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> mat(data, n, n);
        mat = mat.adjoint().eval();
    }
}
//=============================================================================
static bool
use_gesdd_for_size(int m, int n)
{
    uint64_t elems = (uint64_t)m * (uint64_t)n;
#if WITH_OPENMP
    return elems >= NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold();
#else
    // default: use gesdd for matrices with >= 1e6 elements
    return elems >= 1000000ULL;
#endif
}
//=============================================================================
static int
doubleSvdThreadLimit(char jobz, lapack_int m, lapack_int n)
{
    if (jobz != 'N') {
        return useSingleThreadBlasForSmallMatrix(m, n) ? 1 : 0;
    }

    uint64_t elements = static_cast<uint64_t>(m) * static_cast<uint64_t>(n);
    if (elements <= 589824ULL) {
        return 1;
    }
    return 0;
}
//=============================================================================
static lapack_int
lapackDgesdd(char jobz, lapack_int m, lapack_int n, double* a, lapack_int lda, double* s, double* u,
    lapack_int ldu, double* vt, lapack_int ldvt)
{
    int threadLimit = doubleSvdThreadLimit(jobz, m, n);
    BlasThreadScope blasThreadScope(threadLimit > 0, threadLimit);
    lapack_int info = 0;
    lapack_int lwork = -1;
    lapack_int minMN = std::min(m, n);
    lapack_int iworkSize = std::max<lapack_int>(1, 8 * minMN);
    std::vector<lapack_int> iwork((size_t)iworkSize);
    double workQuery = 0.;
    LAPACK_dgesdd(
        &jobz, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &workQuery, &lwork, iwork.data(), &info);
    if (info != 0) {
        return info;
    }
    lwork = std::max<lapack_int>(1, (lapack_int)workQuery);
    std::vector<double> work((size_t)lwork);
    LAPACK_dgesdd(
        &jobz, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work.data(), &lwork, iwork.data(), &info);
    return info;
}
//=============================================================================
static lapack_int
lapackSgesdd(char jobz, lapack_int m, lapack_int n, single* a, lapack_int lda, single* s, single* u,
    lapack_int ldu, single* vt, lapack_int ldvt)
{
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n));
    lapack_int info = 0;
    lapack_int lwork = -1;
    lapack_int minMN = std::min(m, n);
    lapack_int iworkSize = std::max<lapack_int>(1, 8 * minMN);
    std::vector<lapack_int> iwork((size_t)iworkSize);
    single workQuery = 0.;
    LAPACK_sgesdd(
        &jobz, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &workQuery, &lwork, iwork.data(), &info);
    if (info != 0) {
        return info;
    }
    lwork = std::max<lapack_int>(1, (lapack_int)workQuery);
    std::vector<single> work((size_t)lwork);
    LAPACK_sgesdd(
        &jobz, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work.data(), &lwork, iwork.data(), &info);
    return info;
}
//=============================================================================
static lapack_int
complexGesddRworkSize(char jobz, lapack_int m, lapack_int n)
{
    lapack_int minMN = std::min(m, n);
    lapack_int maxMN = std::max(m, n);
    if (jobz == 'N') {
        return std::max<lapack_int>(1, 5 * minMN);
    }
    return std::max<lapack_int>(1, minMN * std::max(5 * minMN + 7, 2 * maxMN + 2 * minMN + 1));
}
//=============================================================================
static lapack_int
complexGesddWorkSize(char jobz, lapack_int m, lapack_int n)
{
    if (jobz != 'N') {
        return -1;
    }
    lapack_int minMN = std::min(m, n);
    lapack_int maxMN = std::max(m, n);
    return std::max<lapack_int>(1, 2 * minMN + maxMN);
}
//=============================================================================
static lapack_int
lapackZgesdd(char jobz, lapack_int m, lapack_int n, doublecomplex* a, lapack_int lda, double* s,
    doublecomplex* u, lapack_int ldu, doublecomplex* vt, lapack_int ldvt)
{
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n));
    lapack_int info = 0;
    lapack_int lwork = complexGesddWorkSize(jobz, m, n);
    lapack_int minMN = std::min(m, n);
    std::vector<double> rwork((size_t)complexGesddRworkSize(jobz, m, n));
    std::vector<lapack_int> iwork((size_t)std::max<lapack_int>(1, 8 * minMN));
    if (lwork < 0) {
        doublecomplex workQuery(0., 0.);
        LAPACK_zgesdd(&jobz, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &workQuery, &lwork,
            rwork.data(), iwork.data(), &info);
        if (info != 0) {
            return info;
        }
        lwork = std::max<lapack_int>(1, (lapack_int)workQuery.real());
    }
    std::vector<doublecomplex> work((size_t)lwork);
    LAPACK_zgesdd(&jobz, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work.data(), &lwork, rwork.data(),
        iwork.data(), &info);
    return info;
}
//=============================================================================
static lapack_int
lapackCgesdd(char jobz, lapack_int m, lapack_int n, singlecomplex* a, lapack_int lda, single* s,
    singlecomplex* u, lapack_int ldu, singlecomplex* vt, lapack_int ldvt)
{
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n));
    lapack_int info = 0;
    lapack_int lwork = complexGesddWorkSize(jobz, m, n);
    lapack_int minMN = std::min(m, n);
    std::vector<single> rwork((size_t)complexGesddRworkSize(jobz, m, n));
    std::vector<lapack_int> iwork((size_t)std::max<lapack_int>(1, 8 * minMN));
    if (lwork < 0) {
        singlecomplex workQuery(0., 0.);
        LAPACK_cgesdd(&jobz, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &workQuery, &lwork,
            rwork.data(), iwork.data(), &info);
        if (info != 0) {
            return info;
        }
        lwork = std::max<lapack_int>(1, (lapack_int)workQuery.real());
    }
    std::vector<singlecomplex> work((size_t)lwork);
    LAPACK_cgesdd(&jobz, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work.data(), &lwork, rwork.data(),
        iwork.data(), &info);
    return info;
}
//=============================================================================
static lapack_int
lapackDgesvd(char jobu, char jobvt, lapack_int m, lapack_int n, double* a, lapack_int lda,
    double* s, double* u, lapack_int ldu, double* vt, lapack_int ldvt, double*)
{
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n));
    lapack_int info = 0;
    lapack_int lwork = -1;
    double workQuery = 0.;
    LAPACK_dgesvd(&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &workQuery, &lwork, &info);
    if (info != 0) {
        return info;
    }
    lwork = std::max<lapack_int>(1, (lapack_int)workQuery);
    std::vector<double> work((size_t)lwork);
    LAPACK_dgesvd(
        &jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackSgesvd(char jobu, char jobvt, lapack_int m, lapack_int n, single* a, lapack_int lda,
    single* s, single* u, lapack_int ldu, single* vt, lapack_int ldvt, single*)
{
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n));
    lapack_int info = 0;
    lapack_int lwork = -1;
    single workQuery = 0.;
    LAPACK_sgesvd(&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &workQuery, &lwork, &info);
    if (info != 0) {
        return info;
    }
    lwork = std::max<lapack_int>(1, (lapack_int)workQuery);
    std::vector<single> work((size_t)lwork);
    LAPACK_sgesvd(
        &jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work.data(), &lwork, &info);
    return info;
}
//=============================================================================
static lapack_int
lapackZgesvd(char jobu, char jobvt, lapack_int m, lapack_int n, doublecomplex* a, lapack_int lda,
    double* s, doublecomplex* u, lapack_int ldu, doublecomplex* vt, lapack_int ldvt, double*)
{
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n));
    lapack_int info = 0;
    lapack_int lwork = -1;
    lapack_int minMN = std::min(m, n);
    std::vector<double> rwork((size_t)std::max<lapack_int>(1, 5 * minMN));
    doublecomplex workQuery(0., 0.);
    LAPACK_zgesvd(&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &workQuery, &lwork,
        rwork.data(), &info);
    if (info != 0) {
        return info;
    }
    lwork = std::max<lapack_int>(1, (lapack_int)workQuery.real());
    std::vector<doublecomplex> work((size_t)lwork);
    LAPACK_zgesvd(&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work.data(), &lwork,
        rwork.data(), &info);
    return info;
}
//=============================================================================
static lapack_int
lapackCgesvd(char jobu, char jobvt, lapack_int m, lapack_int n, singlecomplex* a, lapack_int lda,
    single* s, singlecomplex* u, lapack_int ldu, singlecomplex* vt, lapack_int ldvt, single*)
{
    BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n));
    lapack_int info = 0;
    lapack_int lwork = -1;
    lapack_int minMN = std::min(m, n);
    std::vector<single> rwork((size_t)std::max<lapack_int>(1, 5 * minMN));
    singlecomplex workQuery(0., 0.);
    LAPACK_cgesvd(&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &workQuery, &lwork,
        rwork.data(), &info);
    if (info != 0) {
        return info;
    }
    lwork = std::max<lapack_int>(1, (lapack_int)workQuery.real());
    std::vector<singlecomplex> work((size_t)lwork);
    LAPACK_cgesvd(&jobu, &jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work.data(), &lwork,
        rwork.data(), &info);
    return info;
}
//=============================================================================
static bool
tryLowRankSVDValuesDouble(const double* A, int m, int n, double* singularValues)
{
    constexpr int MAX_FAST_RANK = 32;
    int minMN = std::min(m, n);
    if (minMN < 64) {
        return false;
    }
    std::vector<double> basis((size_t)MAX_FAST_RANK * (size_t)m);
    std::vector<double> column(m);
    double referenceNorm = 0.;
    int rank = 0;
    double tolerance = 0.;

    for (int col = 0; col < n; ++col) {
        if (rank == MAX_FAST_RANK) {
            return false;
        }

        double columnNormSquared = 0.;
        const double* sourceColumn = A + (size_t)col * (size_t)m;
        for (int row = 0; row < m; ++row) {
            double value = sourceColumn[row];
            column[row] = value;
            columnNormSquared += value * value;
        }

        double columnNorm = std::sqrt(columnNormSquared);
        if (columnNorm > referenceNorm) {
            referenceNorm = columnNorm;
            tolerance = referenceNorm * std::numeric_limits<double>::epsilon()
                * (double)std::max(m, n) * 16.;
        }
        if (columnNorm <= tolerance) {
            continue;
        }

        for (int k = 0; k < rank; ++k) {
            const double* q = basis.data() + (size_t)k * (size_t)m;
            double projection = 0.;
            for (int row = 0; row < m; ++row) {
                projection += q[row] * column[row];
            }
            for (int row = 0; row < m; ++row) {
                column[row] -= projection * q[row];
            }
        }

        double residualNormSquared = 0.;
        for (int row = 0; row < m; ++row) {
            residualNormSquared += column[row] * column[row];
        }
        double residualNorm = std::sqrt(residualNormSquared);
        if (residualNorm <= tolerance) {
            continue;
        }

        double* q = basis.data() + (size_t)rank * (size_t)m;
        double inverseNorm = 1. / residualNorm;
        for (int row = 0; row < m; ++row) {
            q[row] = column[row] * inverseNorm;
        }
        ++rank;
    }

    std::fill(singularValues, singularValues + minMN, 0.);
    if (rank == 0) {
        return true;
    }

    std::vector<double> projected((size_t)rank * (size_t)n);
    for (int col = 0; col < n; ++col) {
        const double* sourceColumn = A + (size_t)col * (size_t)m;
        for (int k = 0; k < rank; ++k) {
            const double* q = basis.data() + (size_t)k * (size_t)m;
            double value = 0.;
            for (int row = 0; row < m; ++row) {
                value += q[row] * sourceColumn[row];
            }
            projected[(size_t)k + (size_t)col * (size_t)rank] = value;
        }
    }

    std::vector<double> compactSingularValues(rank);
    int info = lapackDgesdd('N', rank, n, projected.data(), rank, compactSingularValues.data(),
        nullptr, rank, nullptr, n);
    if (info != 0) {
        return false;
    }
    std::copy(compactSingularValues.begin(), compactSingularValues.end(), singularValues);
    return true;
}
//=============================================================================
static void
SVD_double(const ArrayOf& A, ArrayOf& s)
{
    char JOBU = 'N';
    char JOBVT = 'N';
    Dimensions dimsA = A.dimensions();
    int m = static_cast<int>(dimsA.rows());
    int n = static_cast<int>(dimsA.columns());
    int ldu = m;
    int ldvt = n;
    int lda = m;
    double* ds = new_with_exception<double>(std::min(m, n), false);
    double* u = nullptr;
    double* vt = nullptr;
    Eigen::Map<Eigen::MatrixXd> matA((double*)A.dataPointer(), (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        // NaN or Inf input gives NaN singular values
        std::fill(ds, ds + std::min(m, n), std::numeric_limits<double>::quiet_NaN());
        Dimensions dimsS(std::min(m, n), 1);
        s = ArrayOf(NLS_DOUBLE, dimsS, ds);
        return;
    }
    if (tryLowRankSVDValuesDouble((double*)A.dataPointer(), m, n, ds)) {
        Dimensions dimsS(std::min(m, n), 1);
        s = ArrayOf(NLS_DOUBLE, dimsS, ds);
        return;
    }
    char JOBZ = 'N';
    if (JOBU == 'A' && JOBVT == 'A') {
        JOBZ = 'A';
    } else if (JOBU == 'S' && JOBVT == 'S') {
        JOBZ = 'S';
    } else if (JOBU == 'N' && JOBVT == 'N') {
        JOBZ = 'N';
    } else {
        JOBZ = 'S';
    }
    int info = lapackDgesdd(JOBZ, m, n, (double*)A.dataPointer(), lda, ds, u, ldu, vt, ldvt);
    if (info != 0) {
        delete[] ds;
        Error("Nelson:svd:lapackError", "LAPACK_%s error.", "dgesdd");
    }
    Dimensions dimsS(std::min(m, n), 1);
    s = ArrayOf(NLS_DOUBLE, dimsS, ds);
}
//=============================================================================
static void
SVD_doublecomplex(const ArrayOf& A, ArrayOf& s)
{
    char JOBU = 'N';
    char JOBVT = 'N';
    Dimensions dimsA = A.dimensions();
    int m = static_cast<int>(dimsA.rows());
    int n = static_cast<int>(dimsA.columns());
    int ldu = m;
    int ldvt = n;
    int lda = m;
    double* ds = new_with_exception<double>(std::min(m, n), false);
    auto* Rz = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
    doublecomplex* uz = nullptr;
    doublecomplex* vtz = nullptr;
    Eigen::Map<Eigen::MatrixXcd> matA(Rz, (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        // NaN or Inf input gives NaN singular values
        std::fill(ds, ds + std::min(m, n), std::numeric_limits<double>::quiet_NaN());
        Dimensions dimsS(std::min(m, n), 1);
        s = ArrayOf(NLS_DOUBLE, dimsS, ds);
        return;
    }
    char JOBZ = 'N';
    if (JOBU == 'A' && JOBVT == 'A') {
        JOBZ = 'A';
    } else if (JOBU == 'S' && JOBVT == 'S') {
        JOBZ = 'S';
    } else if (JOBU == 'N' && JOBVT == 'N') {
        JOBZ = 'N';
    } else {
        JOBZ = 'S';
    }
    int info = lapackZgesdd(JOBZ, m, n, Rz, lda, ds, uz, ldu, vtz, ldvt);
    if (info > 0) {
        delete[] ds;
        Error("Nelson:svd:lapackError", "LAPACK_%s error.", "zgesdd");
    }
    Dimensions dimsS(std::min(m, n), 1);
    s = ArrayOf(NLS_DOUBLE, dimsS, ds);
}
//=============================================================================
static void
SVD_single(const ArrayOf& A, ArrayOf& s)
{
    char JOBU = 'N';
    char JOBVT = 'N';
    Dimensions dimsA = A.dimensions();
    int m = static_cast<int>(dimsA.rows());
    int n = static_cast<int>(dimsA.columns());
    int ldu = m;
    int ldvt = n;
    int lda = m;
    single* ds = new_with_exception<single>(std::min(m, n), false);
    single* u = nullptr;
    single* vt = nullptr;
    Eigen::Map<Eigen::MatrixXf> matA((single*)A.dataPointer(), (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        // NaN or Inf input gives NaN singular values
        std::fill(ds, ds + std::min(m, n), std::numeric_limits<single>::quiet_NaN());
        Dimensions dimsS(std::min(m, n), 1);
        s = ArrayOf(NLS_SINGLE, dimsS, ds);
        return;
    }
    char JOBZ = 'N';
    if (JOBU == 'A' && JOBVT == 'A') {
        JOBZ = 'A';
    } else if (JOBU == 'S' && JOBVT == 'S') {
        JOBZ = 'S';
    } else if (JOBU == 'N' && JOBVT == 'N') {
        JOBZ = 'N';
    } else {
        JOBZ = 'S';
    }
    int info = 0;
    if (use_gesdd_for_size(m, n)) {
        info = lapackSgesdd(JOBZ, m, n, (single*)A.dataPointer(), lda, ds, u, ldu, vt, ldvt);
        if (info > 0) {
            delete[] ds;
            Error("Nelson:svd:lapackError", "LAPACK_%s error.", "sgesdd");
        }
    } else {
        info = lapackSgesvd(
            JOBU, JOBVT, m, n, (single*)A.dataPointer(), lda, ds, u, ldu, vt, ldvt, nullptr);
        if (info > 0) {
            delete[] ds;
            Error("Nelson:svd:lapackError", "LAPACK_%s error.", "sgesvd");
        }
    }
    Dimensions dimsS(std::min(m, n), 1);
    s = ArrayOf(NLS_SINGLE, dimsS, ds);
}
//=============================================================================
static void
SVD_singlecomplex(const ArrayOf& A, ArrayOf& s)
{
    char JOBU = 'N';
    char JOBVT = 'N';
    Dimensions dimsA = A.dimensions();
    int m = static_cast<int>(dimsA.rows());
    int n = static_cast<int>(dimsA.columns());
    int ldu = m;
    int ldvt = n;
    int lda = m;
    single* ds = new_with_exception<single>(std::min(m, n), false);
    auto* Rz = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
    singlecomplex* uz = nullptr;
    singlecomplex* vtz = nullptr;
    Eigen::Map<Eigen::MatrixXcf> matA(Rz, (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        // NaN or Inf input gives NaN singular values
        std::fill(ds, ds + std::min(m, n), std::numeric_limits<single>::quiet_NaN());
        Dimensions dimsS(std::min(m, n), 1);
        s = ArrayOf(NLS_SINGLE, dimsS, ds);
        return;
    }
    char JOBZ = 'N';
    if (JOBU == 'A' && JOBVT == 'A') {
        JOBZ = 'A';
    } else if (JOBU == 'S' && JOBVT == 'S') {
        JOBZ = 'S';
    } else if (JOBU == 'N' && JOBVT == 'N') {
        JOBZ = 'N';
    } else {
        JOBZ = 'S';
    }
    int info = lapackCgesdd(JOBZ, m, n, Rz, lda, ds, uz, ldu, vtz, ldvt);
    if (info > 0) {
        delete[] ds;
        Error("Nelson:svd:lapackError", "LAPACK_%s error.", "cgesdd");
    }
    Dimensions dimsS(std::min(m, n), 1);
    s = ArrayOf(NLS_SINGLE, dimsS, ds);
}
//=============================================================================
static void
SVD_doublecomplex(const ArrayOf& A, SVD_FLAG flag, ArrayOf& U, ArrayOf& S, ArrayOf& V, bool withV)
{
    Dimensions dimsA = A.dimensions();
    int m = static_cast<int>(dimsA.rows());
    int n = static_cast<int>(dimsA.columns());
    auto* Rz = reinterpret_cast<doublecomplex*>((double*)A.dataPointer());
    Eigen::Map<Eigen::MatrixXcd> matA(Rz, (Eigen::Index)m, (Eigen::Index)n);
    // NaN or Inf input gives NaN outputs
    bool asNaN = !matA.allFinite();
    if (flag == SVD_FLAG::SVD_DEFAULT) {
        char JOBU = 'A';
        char JOBVT = 'A';
        if (!withV) {
            JOBVT = 'N';
        }
        int ldu = m;
        int ldvt = n;
        int lda = m;
        int minMN = std::min(m, n);
        double* dstemp = new_with_exception<double>(minMN, false);
        double* u = new_with_exception<double>(((size_t)ldu * (size_t)m * (size_t)2), false);
        auto* uz = reinterpret_cast<doublecomplex*>(u);
        double* vt = nullptr;
        doublecomplex* vtz = nullptr;
        if (withV) {
            vt = new_with_exception<double>((size_t)ldvt * (size_t)n * (size_t)2, false);
            vtz = reinterpret_cast<doublecomplex*>(vt);
        }
        char JOBZ = 'N';
        if (JOBU == 'A' && JOBVT == 'A') {
            JOBZ = 'A';
        } else if (JOBU == 'S' && JOBVT == 'S') {
            JOBZ = 'S';
        } else if (JOBU == 'N' && JOBVT == 'N') {
            JOBZ = 'N';
        } else {
            JOBZ = 'S';
        }
        int info = 0;
        if (asNaN) {
            double nanValue = std::numeric_limits<double>::quiet_NaN();
            std::fill(dstemp, dstemp + minMN, nanValue);
            std::fill(u, u + (size_t)ldu * (size_t)m * (size_t)2, nanValue);
            if (withV && vt != nullptr) {
                std::fill(vt, vt + (size_t)ldvt * (size_t)n * (size_t)2, nanValue);
            }
        } else if (use_gesdd_for_size(m, n)) {
            info = lapackZgesdd(JOBZ, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "zgesdd");
            }
        } else {
            info = lapackZgesvd(JOBU, JOBVT, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt, nullptr);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "zgesvd");
            }
        }
        Dimensions dimsU(m, m);
        U = ArrayOf(NLS_DCOMPLEX, dimsU, u);
        double* ds = new_with_exception<double>((size_t)m * (size_t)n, true);
        Dimensions dimsS(m, n);
        Eigen::Map<Eigen::VectorXd> matStmp(dstemp, minMN);
        Eigen::Map<Eigen::MatrixXd> matS(ds, m, n);
        OMP_PARALLEL_FOR_LOOP(minMN)
        for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
            matS(k, k) = matStmp(k);
        }
        delete[] dstemp;
        dstemp = nullptr;
        S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        if (withV) {
            Dimensions dimsV(n, n);
            inplace_adjoint_colmajor(vtz, n, n);
            V = ArrayOf(NLS_DCOMPLEX, dimsV, vt);
        }
    } else if (flag == SVD_ECON) {
        int mU;
        int nU;
        int mS;
        int nS;
        int mV;
        int nV;
        if (m > n) {
            mU = m;
            nU = n;
            mS = n;
            nS = n;
            mV = n;
            nV = n;
        } else {
            if (m == n) {
                mU = m;
                nU = m;
                mS = m;
                nS = m;
                mV = m;
                nV = m;
            } else // m < n
            {
                mS = m;
                nS = m;
                mU = m;
                nU = m;
                mV = n;
                nV = m;
            }
        }
        Dimensions dimsU(mU, nU);
        Dimensions dimsS(mS, nS);
        Dimensions dimsV(mV, nV);
        char JOBU = 'S';
        char JOBVT = 'S';
        if (!withV) {
            JOBVT = 'N';
        }
        int ldu = m;
        int ldvt = n;
        int lda = m;
        int minMN = std::min(m, n);
        int maxMN = std::max(m, n);
        double* dstemp = new_with_exception<double>(minMN, false);
        double* u = new_with_exception<double>((size_t)ldu * (size_t)m * (size_t)2, false);
        auto* uz = reinterpret_cast<doublecomplex*>(u);
        double* vt = nullptr;
        doublecomplex* vtz = nullptr;
        if (withV) {
            vt = new_with_exception<double>((size_t)ldvt * (size_t)n * (size_t)2, false);
            vtz = reinterpret_cast<doublecomplex*>(vt);
        }
        char JOBZ = 'N';
        if (JOBU == 'A' && JOBVT == 'A') {
            JOBZ = 'A';
        } else if (JOBU == 'S' && JOBVT == 'S') {
            JOBZ = 'S';
        } else if (JOBU == 'N' && JOBVT == 'N') {
            JOBZ = 'N';
        } else {
            JOBZ = 'S';
        }
        int info = 0;
        if (asNaN) {
            double nanValue = std::numeric_limits<double>::quiet_NaN();
            std::fill(dstemp, dstemp + minMN, nanValue);
            std::fill(u, u + (size_t)ldu * (size_t)m * (size_t)2, nanValue);
            if (withV && vt != nullptr) {
                std::fill(vt, vt + (size_t)ldvt * (size_t)n * (size_t)2, nanValue);
            }
        } else if (use_gesdd_for_size(m, n)) {
            info = lapackZgesdd(JOBZ, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "zgesdd");
            }
        } else {
            info = lapackZgesvd(JOBU, JOBVT, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt, nullptr);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "zgesvd");
            }
        }
        U = ArrayOf(NLS_DCOMPLEX, dimsU, u);
        Eigen::Map<Eigen::VectorXd> matStmp(dstemp, minMN);
        if (m > n) {
            double* ds = new_with_exception<double>((size_t)n * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXd> matS(ds, n, n);
            OMP_PARALLEL_FOR_LOOP(minMN)
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        } else {
            double* ds = new_with_exception<double>((size_t)minMN * (size_t)maxMN, true);
            Eigen::Map<Eigen::MatrixXd> matS(ds, m, m);
            OMP_PARALLEL_FOR_LOOP(minMN)
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        }
        delete[] dstemp;
        dstemp = nullptr;
        if (withV) {
            Eigen::Map<Eigen::MatrixXcd> matV(vtz, ldvt, n);
            double* vt2 = new_with_exception<double>((size_t)ldvt * (size_t)n * (size_t)2, false);
            auto* vt2z = reinterpret_cast<doublecomplex*>(vt2);
            // manual transpose copy: vt2z[a + b*n] = vtz[b + a*ldvt]
            for (int a = 0; a < n; ++a) {
                for (int b = 0; b < ldvt; ++b) {
                    vt2z[a + b * (size_t)n] = std::conj(vtz[b + a * (size_t)ldvt]);
                }
            }
            delete[] vt;
            vt = nullptr;
            V = ArrayOf(NLS_DCOMPLEX, dimsV, vt2);
        }
    }
}
//=============================================================================
static void
SVD_single(const ArrayOf& A, SVD_FLAG flag, ArrayOf& U, ArrayOf& S, ArrayOf& V, bool withV)
{
    Dimensions dimsA = A.dimensions();
    int m = static_cast<int>(dimsA.rows());
    int n = static_cast<int>(dimsA.columns());
    Eigen::Map<Eigen::MatrixXf> matA((single*)A.dataPointer(), (Eigen::Index)m, (Eigen::Index)n);
    // NaN or Inf input gives NaN outputs
    bool asNaN = !matA.allFinite();
    if (flag == SVD_FLAG::SVD_DEFAULT) {
        char JOBU = 'A';
        char JOBVT = 'A';
        if (!withV) {
            JOBVT = 'N';
        }
        int ldu = m;
        int ldvt = n;
        int lda = m;
        int minMN = std::min(m, n);
        single* dstemp = new_with_exception<single>(minMN, false);
        single* u = new_with_exception<single>((size_t)(ldu) * (size_t)(m), false);
        single* vt = nullptr;
        if (withV) {
            vt = new_with_exception<single>((size_t)ldvt * (size_t)n, false);
        }
        char JOBZ = 'N';
        if (JOBU == 'A' && JOBVT == 'A') {
            JOBZ = 'A';
        } else if (JOBU == 'S' && JOBVT == 'S') {
            JOBZ = 'S';
        } else if (JOBU == 'N' && JOBVT == 'N') {
            JOBZ = 'N';
        } else {
            JOBZ = 'S';
        }
        int info = 0;
        if (asNaN) {
            single nanValue = std::numeric_limits<single>::quiet_NaN();
            std::fill(dstemp, dstemp + minMN, nanValue);
            std::fill(u, u + (size_t)ldu * (size_t)m, nanValue);
            if (withV && vt != nullptr) {
                std::fill(vt, vt + (size_t)ldvt * (size_t)n, nanValue);
            }
        } else if (use_gesdd_for_size(m, n)) {
            info
                = lapackSgesdd(JOBZ, m, n, (single*)A.dataPointer(), lda, dstemp, u, ldu, vt, ldvt);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "sgesdd");
            }
        } else {
            info = lapackSgesvd(JOBU, JOBVT, m, n, (single*)A.dataPointer(), lda, dstemp, u, ldu,
                vt, ldvt, nullptr);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "sgesvd");
            }
        }
        Dimensions dimsU(m, m);
        U = ArrayOf(NLS_SINGLE, dimsU, u);
        single* ds = new_with_exception<single>((size_t)m * (size_t)n, true);
        Dimensions dimsS(m, n);
        Eigen::Map<Eigen::VectorXf> matStmp(dstemp, minMN);
        Eigen::Map<Eigen::MatrixXf> matS(ds, m, n);
        OMP_PARALLEL_FOR_LOOP(minMN)
        for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
            matS(k, k) = matStmp(k);
        }
        delete[] dstemp;
        dstemp = nullptr;
        S = ArrayOf(NLS_SINGLE, dimsS, ds);
        if (withV) {
            Dimensions dimsV(n, n);
            inplace_transpose_colmajor(vt, n, n);
            V = ArrayOf(NLS_SINGLE, dimsV, vt);
        }
    } else if (flag == SVD_ECON) {
        int mU;
        int nU;
        int mS;
        int nS;
        int mV;
        int nV;
        if (m > n) {
            mU = m;
            nU = n;
            mS = n;
            nS = n;
            mV = n;
            nV = n;
        } else {
            if (m == n) {
                mU = m;
                nU = m;
                mS = m;
                nS = m;
                mV = m;
                nV = m;
            } else // m < n
            {
                mS = m;
                nS = m;
                mU = m;
                nU = m;
                mV = n;
                nV = m;
            }
        }
        Dimensions dimsU(mU, nU);
        Dimensions dimsS(mS, nS);
        Dimensions dimsV(mV, nV);
        char JOBU = 'S';
        char JOBVT = 'S';
        int ldu = m;
        int ldvt = n;
        int lda = m;
        int minMN = std::min(m, n);
        int maxMN = std::max(m, n);
        single* dstemp = new_with_exception<single>(minMN, false);
        single* u = new_with_exception<single>((size_t)ldu * (size_t)m, false);
        single* vt = nullptr;
        if (withV) {
            vt = new_with_exception<single>((size_t)ldvt * (size_t)n, false);
        }
        char JOBZ = 'N';
        if (JOBU == 'A' && JOBVT == 'A') {
            JOBZ = 'A';
        } else if (JOBU == 'S' && JOBVT == 'S') {
            JOBZ = 'S';
        } else if (JOBU == 'N' && JOBVT == 'N') {
            JOBZ = 'N';
        } else {
            JOBZ = 'S';
        }
        int info = 0;
        if (asNaN) {
            single nanValue = std::numeric_limits<single>::quiet_NaN();
            std::fill(dstemp, dstemp + minMN, nanValue);
            std::fill(u, u + (size_t)ldu * (size_t)m, nanValue);
            if (withV && vt != nullptr) {
                std::fill(vt, vt + (size_t)ldvt * (size_t)n, nanValue);
            }
        } else if (use_gesdd_for_size(m, n)) {
            info
                = lapackSgesdd(JOBZ, m, n, (single*)A.dataPointer(), lda, dstemp, u, ldu, vt, ldvt);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "sgesdd");
            }
        } else {
            info = lapackSgesvd(JOBU, JOBVT, m, n, (single*)A.dataPointer(), lda, dstemp, u, ldu,
                vt, ldvt, nullptr);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "sgesvd");
            }
        }
        U = ArrayOf(NLS_SINGLE, dimsU, u);
        Eigen::Map<Eigen::VectorXf> matStmp(dstemp, minMN);
        if (m > n) {
            single* ds = new_with_exception<single>((size_t)n * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXf> matS(ds, n, n);
            OMP_PARALLEL_FOR_LOOP(minMN)
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_SINGLE, dimsS, ds);
        } else {
            single* ds = new_with_exception<single>((size_t)minMN * (size_t)maxMN, true);
            Eigen::Map<Eigen::MatrixXf> matS(ds, m, m);
            OMP_PARALLEL_FOR_LOOP(minMN)
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_SINGLE, dimsS, ds);
        }
        delete[] dstemp;
        dstemp = nullptr;
        if (withV) {
            Eigen::Map<Eigen::MatrixXf> matV(vt, ldvt, n);
            single* vt2 = new_with_exception<single>((size_t)ldvt * (size_t)n, false);
            // manual transpose copy: vt2[a + b*n] = vt[b + a*ldvt]
            for (int a = 0; a < n; ++a) {
                for (int b = 0; b < ldvt; ++b) {
                    vt2[a + b * (size_t)n] = vt[b + a * (size_t)ldvt];
                }
            }
            delete[] vt;
            vt = nullptr;
            V = ArrayOf(NLS_SINGLE, dimsV, vt2);
        }
    }
}
//=============================================================================
static void
SVD_double(const ArrayOf& A, SVD_FLAG flag, ArrayOf& U, ArrayOf& S, ArrayOf& V, bool withV)
{
    Dimensions dimsA = A.dimensions();
    int m = static_cast<int>(dimsA.rows());
    int n = static_cast<int>(dimsA.columns());
    Eigen::Map<Eigen::MatrixXd> matA((double*)A.dataPointer(), (Eigen::Index)m, (Eigen::Index)n);
    // NaN or Inf input gives NaN outputs
    bool asNaN = !matA.allFinite();
    if (flag == SVD_FLAG::SVD_DEFAULT) {
        char JOBU = 'A';
        char JOBVT = 'A';
        if (!withV) {
            JOBVT = 'N';
        }
        int ldu = m;
        int ldvt = n;
        int lda = m;
        int minMN = std::min(m, n);
        double* dstemp = new_with_exception<double>(minMN, false);
        double* u = new_with_exception<double>((size_t)ldu * (size_t)m, false);
        double* vt = nullptr;
        if (withV) {
            vt = new_with_exception<double>((size_t)ldvt * (size_t)n, false);
        }
        char JOBZ = 'N';
        if (JOBU == 'A' && JOBVT == 'A') {
            JOBZ = 'A';
        } else if (JOBU == 'S' && JOBVT == 'S') {
            JOBZ = 'S';
        } else if (JOBU == 'N' && JOBVT == 'N') {
            JOBZ = 'N';
        } else {
            JOBZ = 'S';
        }
        int info = 0;
        if (asNaN) {
            double nanValue = std::numeric_limits<double>::quiet_NaN();
            std::fill(dstemp, dstemp + minMN, nanValue);
            std::fill(u, u + (size_t)ldu * (size_t)m, nanValue);
            if (withV && vt != nullptr) {
                std::fill(vt, vt + (size_t)ldvt * (size_t)n, nanValue);
            }
        } else if (use_gesdd_for_size(m, n)) {
            info
                = lapackDgesdd(JOBZ, m, n, (double*)A.dataPointer(), lda, dstemp, u, ldu, vt, ldvt);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "dgesdd");
            }
        } else {
            info = lapackDgesvd(JOBU, JOBVT, m, n, (double*)A.dataPointer(), lda, dstemp, u, ldu,
                vt, ldvt, nullptr);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "dgesvd");
            }
        }
        Dimensions dimsU(m, m);
        U = ArrayOf(NLS_DOUBLE, dimsU, u);
        double* ds = new_with_exception<double>((size_t)m * (size_t)n, true);
        Dimensions dimsS(m, n);
        Eigen::Map<Eigen::VectorXd> matStmp(dstemp, minMN);
        Eigen::Map<Eigen::MatrixXd> matS(ds, m, n);
        OMP_PARALLEL_FOR_LOOP(minMN)
        for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
            matS(k, k) = matStmp(k);
        }
        delete[] dstemp;
        dstemp = nullptr;
        S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        if (withV) {
            Dimensions dimsV(n, n);
            inplace_transpose_colmajor(vt, n, n);
            V = ArrayOf(NLS_DOUBLE, dimsV, vt);
        }
    } else if (flag == SVD_ECON) {
        int mU;
        int nU;
        int mS;
        int nS;
        int mV;
        int nV;
        if (m > n) {
            mU = m;
            nU = n;
            mS = n;
            nS = n;
            mV = n;
            nV = n;
        } else {
            if (m == n) {
                mU = m;
                nU = m;
                mS = m;
                nS = m;
                mV = m;
                nV = m;
            } else // m < n
            {
                mS = m;
                nS = m;
                mU = m;
                nU = m;
                mV = n;
                nV = m;
            }
        }
        Dimensions dimsU(mU, nU);
        Dimensions dimsS(mS, nS);
        Dimensions dimsV(mV, nV);
        char JOBU = 'S';
        char JOBVT = 'S';
        if (!withV) {
            JOBVT = 'N';
        }
        int ldu = m;
        int ldvt = n;
        int lda = m;
        int minMN = std::min(m, n);
        int maxMN = std::max(m, n);
        double* dstemp = new_with_exception<double>(minMN, false);
        double* u = new_with_exception<double>((size_t)ldu * (size_t)m, false);
        double* vt = nullptr;
        if (withV) {
            vt = new_with_exception<double>((size_t)ldvt * (size_t)n, false);
        }
        char JOBZ = 'N';
        if (JOBU == 'A' && JOBVT == 'A') {
            JOBZ = 'A';
        } else if (JOBU == 'S' && JOBVT == 'S') {
            JOBZ = 'S';
        } else if (JOBU == 'N' && JOBVT == 'N') {
            JOBZ = 'N';
        } else {
            JOBZ = 'S';
        }
        int info = 0;
        if (asNaN) {
            double nanValue = std::numeric_limits<double>::quiet_NaN();
            std::fill(dstemp, dstemp + minMN, nanValue);
            std::fill(u, u + (size_t)ldu * (size_t)m, nanValue);
            if (withV && vt != nullptr) {
                std::fill(vt, vt + (size_t)ldvt * (size_t)n, nanValue);
            }
        } else if (use_gesdd_for_size(m, n)) {
            info
                = lapackDgesdd(JOBZ, m, n, (double*)A.dataPointer(), lda, dstemp, u, ldu, vt, ldvt);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "dgesdd");
            }
        } else {
            info = lapackDgesvd(JOBU, JOBVT, m, n, (double*)A.dataPointer(), lda, dstemp, u, ldu,
                vt, ldvt, nullptr);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "dgesvd");
            }
        }
        U = ArrayOf(NLS_DOUBLE, dimsU, u);
        Eigen::Map<Eigen::VectorXd> matStmp(dstemp, minMN);
        if (m > n) {
            double* ds = new_with_exception<double>((size_t)n * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXd> matS(ds, n, n);
            OMP_PARALLEL_FOR_LOOP(minMN)
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        } else {
            double* ds = new_with_exception<double>((size_t)minMN * (size_t)maxMN, true);
            Eigen::Map<Eigen::MatrixXd> matS(ds, m, m);
            OMP_PARALLEL_FOR_LOOP(minMN)
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        }
        delete[] dstemp;
        dstemp = nullptr;
        if (withV) {
            Eigen::Map<Eigen::MatrixXd> matV(vt, ldvt, n);
            double* vt2 = new_with_exception<double>((size_t)ldvt * (size_t)n, false);
            // manual transpose copy: vt2[a + b*n] = vt[b + a*ldvt]
            for (int a = 0; a < n; ++a) {
                for (int b = 0; b < ldvt; ++b) {
                    vt2[a + b * (size_t)n] = vt[b + a * (size_t)ldvt];
                }
            }
            delete[] vt;
            vt = nullptr;
            V = ArrayOf(NLS_DOUBLE, dimsV, vt2);
        }
    }
}
//=============================================================================
static void
SVD_singlecomplex(const ArrayOf& A, SVD_FLAG flag, ArrayOf& U, ArrayOf& S, ArrayOf& V, bool withV)
{
    Dimensions dimsA = A.dimensions();
    int m = static_cast<int>(dimsA.rows());
    int n = static_cast<int>(dimsA.columns());
    auto* Rz = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(Rz, (Eigen::Index)m, (Eigen::Index)n);
    // NaN or Inf input gives NaN outputs
    bool asNaN = !matA.allFinite();
    if (flag == SVD_FLAG::SVD_DEFAULT) {
        char JOBU = 'A';
        char JOBVT = 'A';
        if (!withV) {
            JOBVT = 'N';
        }
        int ldu = m;
        int ldvt = n;
        int lda = m;
        int minMN = std::min(m, n);
        single* dstemp = new_with_exception<single>(minMN, false);
        single* u = new_with_exception<single>((size_t)ldu * (size_t)m * (size_t)2, false);
        single* vt = nullptr;
        singlecomplex* vtz = nullptr;
        if (withV) {
            vt = new_with_exception<single>((size_t)ldvt * (size_t)n * (size_t)2, false);
            vtz = reinterpret_cast<singlecomplex*>(vt);
        }
        auto* uz = reinterpret_cast<singlecomplex*>(u);
        char JOBZ = 'N';
        if (JOBU == 'A' && JOBVT == 'A') {
            JOBZ = 'A';
        } else if (JOBU == 'S' && JOBVT == 'S') {
            JOBZ = 'S';
        } else if (JOBU == 'N' && JOBVT == 'N') {
            JOBZ = 'N';
        } else {
            JOBZ = 'S';
        }
        int info = 0;
        if (asNaN) {
            single nanValue = std::numeric_limits<single>::quiet_NaN();
            std::fill(dstemp, dstemp + minMN, nanValue);
            std::fill(u, u + (size_t)ldu * (size_t)m * (size_t)2, nanValue);
            if (withV && vt != nullptr) {
                std::fill(vt, vt + (size_t)ldvt * (size_t)n * (size_t)2, nanValue);
            }
        } else if (use_gesdd_for_size(m, n)) {
            info = lapackCgesdd(JOBZ, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "cgesdd");
            }
        } else {
            info = lapackCgesvd(JOBU, JOBVT, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt, nullptr);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "cgesvd");
            }
        }
        Dimensions dimsU(m, m);
        U = ArrayOf(NLS_SCOMPLEX, dimsU, u);
        single* ds = new_with_exception<single>((size_t)m * (size_t)n, true);
        Dimensions dimsS(m, n);
        Eigen::Map<Eigen::VectorXf> matStmp(dstemp, minMN);
        Eigen::Map<Eigen::MatrixXf> matS(ds, m, n);
        OMP_PARALLEL_FOR_LOOP(minMN)
        for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
            matS(k, k) = matStmp(k);
        }
        delete[] dstemp;
        dstemp = nullptr;
        S = ArrayOf(NLS_SINGLE, dimsS, ds);
        if (withV) {
            Dimensions dimsV(n, n);
            inplace_adjoint_colmajor(vtz, n, n);
            V = ArrayOf(NLS_SCOMPLEX, dimsV, vt);
        }
    } else if (flag == SVD_ECON) {
        int mU;
        int nU;
        int mS;
        int nS;
        int mV;
        int nV;
        if (m > n) {
            mU = m;
            nU = n;
            mS = n;
            nS = n;
            mV = n;
            nV = n;
        } else {
            if (m == n) {
                mU = m;
                nU = m;
                mS = m;
                nS = m;
                mV = m;
                nV = m;
            } else // m < n
            {
                mS = m;
                nS = m;
                mU = m;
                nU = m;
                mV = n;
                nV = m;
            }
        }
        Dimensions dimsU(mU, nU);
        Dimensions dimsS(mS, nS);
        Dimensions dimsV(mV, nV);
        char JOBU = 'S';
        char JOBVT = 'S';
        if (!withV) {
            JOBVT = 'N';
        }
        int ldu = m;
        int ldvt = n;
        int lda = m;
        int minMN = std::min(m, n);
        int maxMN = std::max(m, n);
        single* dstemp = new_with_exception<single>(minMN, false);
        single* u = new_with_exception<single>((size_t)ldu * (size_t)m * (size_t)2, false);
        auto* uz = reinterpret_cast<singlecomplex*>(u);
        single* vt = nullptr;
        singlecomplex* vtz = nullptr;
        if (withV) {
            vt = new_with_exception<single>((size_t)ldvt * (size_t)n * (size_t)2, false);
            vtz = reinterpret_cast<singlecomplex*>(vt);
        }
        char JOBZ = 'N';
        if (JOBU == 'A' && JOBVT == 'A') {
            JOBZ = 'A';
        } else if (JOBU == 'S' && JOBVT == 'S') {
            JOBZ = 'S';
        } else if (JOBU == 'N' && JOBVT == 'N') {
            JOBZ = 'N';
        } else {
            JOBZ = 'S';
        }
        int info = 0;
        if (asNaN) {
            single nanValue = std::numeric_limits<single>::quiet_NaN();
            std::fill(dstemp, dstemp + minMN, nanValue);
            std::fill(u, u + (size_t)ldu * (size_t)m * (size_t)2, nanValue);
            if (withV && vt != nullptr) {
                std::fill(vt, vt + (size_t)ldvt * (size_t)n * (size_t)2, nanValue);
            }
        } else if (use_gesdd_for_size(m, n)) {
            info = lapackCgesdd(JOBZ, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "cgesdd");
            }
        } else {
            info = lapackCgesvd(JOBU, JOBVT, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt, nullptr);
            if (info > 0) {
                delete[] dstemp;
                delete[] u;
                delete[] vt;
                Error("Nelson:svd:lapackError", "LAPACK_%s error.", "cgesvd");
            }
        }
        U = ArrayOf(NLS_SCOMPLEX, dimsU, u);
        Eigen::Map<Eigen::VectorXf> matStmp(dstemp, minMN);
        if (m > n) {
            single* ds = new_with_exception<single>((size_t)n * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXf> matS(ds, n, n);
            OMP_PARALLEL_FOR_LOOP(minMN)
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_SINGLE, dimsS, ds);
        } else {
            single* ds = new_with_exception<single>((size_t)minMN * (size_t)maxMN, true);
            Eigen::Map<Eigen::MatrixXf> matS(ds, m, m);
            OMP_PARALLEL_FOR_LOOP(minMN)
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_SINGLE, dimsS, ds);
        }
        delete[] dstemp;
        dstemp = nullptr;
        if (withV) {
            Eigen::Map<Eigen::MatrixXcf> matV(vtz, ldvt, n);
            single* vt2 = new_with_exception<single>((size_t)ldvt * (size_t)n * (size_t)2, false);
            auto* vt2z = reinterpret_cast<singlecomplex*>(vt2);
            // manual transpose copy: vt2z[a + b*n] = vtz[b + a*ldvt]
            for (int a = 0; a < n; ++a) {
                for (int b = 0; b < ldvt; ++b) {
                    vt2z[a + b * (size_t)n] = std::conj(vtz[b + a * (size_t)ldvt]);
                }
            }
            delete[] vt;
            vt = nullptr;
            V = ArrayOf(NLS_SCOMPLEX, dimsV, vt2);
        }
    }
}
//=============================================================================
static void
SVD(const ArrayOf& A, SVD_FLAG flag, ArrayOf& U, ArrayOf& S, ArrayOf& V, bool withV)
{
    bool isSupportedTypes = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE
                                || A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "svd", ClassName(A));
    }
    Dimensions DimsA = A.dimensions();
    indexType m = DimsA.rows();
    indexType n = DimsA.columns();
    if (A.isEmpty()) {
        S = ArrayOf::empty(DimsA);
        if (m == 0) {
            U = ArrayOf::empty(m, m);
            if (withV) {
                indexType n = DimsA.columns();
                double* mat = static_cast<double*>(
                    ArrayOf::allocateArrayOf(NLS_DOUBLE, n * n, stringVector(), false));
                Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matEye(mat, n, n);
                matEye.setIdentity();
                Dimensions dimsV(n, n);
                V = ArrayOf(NLS_DOUBLE, dimsV, (void*)mat);
            }
        } else {
            n = m;
            double* mat = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, n * m, stringVector(), false));
            Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matEye(mat, n, m);
            matEye.setIdentity();
            Dimensions dimsU(m, n);
            U = ArrayOf(NLS_DOUBLE, dimsU, (void*)mat);
            if (withV) {
                V = ArrayOf::empty(DimsA.columns(), DimsA.columns());
            }
        }
        if (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX) {
            S.promoteType(NLS_SINGLE);
            U.promoteType(NLS_SINGLE);
            if (withV) {
                V.promoteType(NLS_SINGLE);
            }
        }
    } else {
        SVD_FLAG svdFlag = SVD_FLAG::SVD_DEFAULT;
        if (flag != SVD_FLAG::SVD_DEFAULT) {
            if (flag == SVD_FLAG::SVD_ECON) {
                if (m == n) {
                    svdFlag = SVD_FLAG::SVD_DEFAULT;
                } else {
                    svdFlag = SVD_FLAG::SVD_ECON;
                }
            } else // SVD_0
            {
                if (m > n) {
                    svdFlag = SVD_FLAG::SVD_ECON;
                } else {
                    svdFlag = SVD_FLAG::SVD_DEFAULT;
                }
            }
        }
        ArrayOf COPY_A(A);
        COPY_A.ensureSingleOwner();
        if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
            if (A.dataClass() == NLS_DOUBLE) {
                SVD_double(COPY_A, svdFlag, U, S, V, withV);
            } else {
                SVD_doublecomplex(COPY_A, svdFlag, U, S, V, withV);
            }
        } else {
            if (A.dataClass() == NLS_SINGLE) {
                SVD_single(COPY_A, svdFlag, U, S, V, withV);
            } else {
                SVD_singlecomplex(COPY_A, svdFlag, U, S, V, withV);
            }
        }
    }
}
//=============================================================================
void
SVD(const ArrayOf& A, SVD_FLAG flag, ArrayOf& U, ArrayOf& S, ArrayOf& V)
{
    SVD(A, flag, U, S, V, true);
}
//=============================================================================
void
SVD(const ArrayOf& A, SVD_FLAG flag, ArrayOf& U, ArrayOf& S)
{
    ArrayOf V;
    SVD(A, flag, U, S, V);
}
//=============================================================================
void
SVD(const ArrayOf& A, ArrayOf& s)
{
    bool isSupportedTypes = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE
                                || A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "svd", ClassName(A));
    }
    if (A.isEmpty()) {
        Dimensions dimsS(0, 1);
        s = ArrayOf::empty(dimsS);
        if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
            s.promoteType(NLS_DOUBLE);
        } else {
            s.promoteType(NLS_SINGLE);
        }
    } else {
        ArrayOf COPY_A(A);
        COPY_A.ensureSingleOwner();
        if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
            if (A.dataClass() == NLS_DOUBLE) {
                SVD_double(COPY_A, s);
            } else {
                SVD_doublecomplex(COPY_A, s);
            }
        } else {
            if (A.dataClass() == NLS_SINGLE) {
                SVD_single(COPY_A, s);
            } else {
                SVD_singlecomplex(COPY_A, s);
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
