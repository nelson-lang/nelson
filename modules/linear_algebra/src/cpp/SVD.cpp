//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include "SVD.hpp"
#include "ClassName.hpp"
#include <Eigen/Dense>
#include <Eigen/SVD>
#include <Eigen/src/misc/lapacke.h>
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// https://software.intel.com/sites/products/documentation/doclib/mkl_sa/11/mkl_lapack_examples/lapacke_dgesvd_col.c.htm
//=============================================================================
static void
SVD_double(const ArrayOf& A, ArrayOf& s)
{
    char JOBU = 'N';
    char JOBVT = 'N';
    Dimensions dimsA = A.getDimensions();
    int m = static_cast<int>(dimsA.getRows());
    int n = static_cast<int>(dimsA.getColumns());
    int ldu = m;
    int ldvt = n;
    int lda = m;
    double* superb = new_with_exception<double>(std::min(m, n) - 1, true);
    double* ds = new_with_exception<double>(std::min(m, n), true);
    double* u = nullptr;
    double* vt = nullptr;
    Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        Error(_("svd: cannot take svd of matrix containing Inf or NaN values."));
    }
    int info = LAPACKE_dgesvd(LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, (double*)A.getDataPointer(), lda,
        ds, u, ldu, vt, ldvt, superb);
    if (info > 0) {
        Error(_("LAPACKE_dgesvd error."));
    }
    delete[] superb;
    superb = nullptr;
    Dimensions dimsS(std::min(m, n), 1);
    s = ArrayOf(NLS_DOUBLE, dimsS, ds);
}
//=============================================================================
static void
SVD_doublecomplex(const ArrayOf& A, ArrayOf& s)
{
    char JOBU = 'N';
    char JOBVT = 'N';
    Dimensions dimsA = A.getDimensions();
    int m = static_cast<int>(dimsA.getRows());
    int n = static_cast<int>(dimsA.getColumns());
    int ldu = m;
    int ldvt = n;
    int lda = m;
    double* superb = new_with_exception<double>((std::min(m, n) - 1), true);
    double* ds = new_with_exception<double>(std::min(m, n), true);
    auto* Rz = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
    doublecomplex* uz = nullptr;
    doublecomplex* vtz = nullptr;
    Eigen::Map<Eigen::MatrixXcd> matA(Rz, (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        Error(_("svd: cannot take svd of matrix containing Inf or NaN values."));
    }
    int info = LAPACKE_zgesvd(
        LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, Rz, lda, ds, uz, ldu, vtz, ldvt, superb);
    if (info > 0) {
        Error(_("LAPACKE_zgesvd error."));
    }
    delete[] superb;
    superb = nullptr;
    Dimensions dimsS(std::min(m, n), 1);
    s = ArrayOf(NLS_DOUBLE, dimsS, ds);
}
//=============================================================================
static void
SVD_single(const ArrayOf& A, ArrayOf& s)
{
    char JOBU = 'N';
    char JOBVT = 'N';
    Dimensions dimsA = A.getDimensions();
    int m = static_cast<int>(dimsA.getRows());
    int n = static_cast<int>(dimsA.getColumns());
    int ldu = m;
    int ldvt = n;
    int lda = m;
    single* superb = new_with_exception<single>(std::min(m, n) - 1, true);
    single* ds = new_with_exception<single>(std::min(m, n), true);
    single* u = nullptr;
    single* vt = nullptr;
    Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        Error(_("svd: cannot take svd of matrix containing Inf or NaN values."));
    }
    int info = LAPACKE_sgesvd(LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, (single*)A.getDataPointer(), lda,
        ds, u, ldu, vt, ldvt, superb);
    if (info > 0) {
        Error(_("LAPACKE_sgesvd error."));
    }
    delete[] superb;
    superb = nullptr;
    Dimensions dimsS(std::min(m, n), 1);
    s = ArrayOf(NLS_SINGLE, dimsS, ds);
}
//=============================================================================
static void
SVD_singlecomplex(const ArrayOf& A, ArrayOf& s)
{
    char JOBU = 'N';
    char JOBVT = 'N';
    Dimensions dimsA = A.getDimensions();
    int m = static_cast<int>(dimsA.getRows());
    int n = static_cast<int>(dimsA.getColumns());
    int ldu = m;
    int ldvt = n;
    int lda = m;
    single* superb = new_with_exception<single>((std::min(m, n) - 1), true);
    single* ds = new_with_exception<single>(std::min(m, n), true);
    auto* Rz = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
    singlecomplex* uz = nullptr;
    singlecomplex* vtz = nullptr;
    Eigen::Map<Eigen::MatrixXcf> matA(Rz, (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        Error(_("svd: cannot take svd of matrix containing Inf or NaN values."));
    }
    int info = LAPACKE_cgesvd(
        LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, Rz, lda, ds, uz, ldu, vtz, ldvt, superb);
    if (info > 0) {
        Error(_("LAPACKE_cgesvd error."));
    }
    delete[] superb;
    superb = nullptr;
    Dimensions dimsS(std::min(m, n), 1);
    s = ArrayOf(NLS_SINGLE, dimsS, ds);
}
//=============================================================================
static void
SVD_doublecomplex(const ArrayOf& A, SVD_FLAG flag, ArrayOf& U, ArrayOf& S, ArrayOf& V, bool withV)
{
    Dimensions dimsA = A.getDimensions();
    int m = static_cast<int>(dimsA.getRows());
    int n = static_cast<int>(dimsA.getColumns());
    auto* Rz = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
    Eigen::Map<Eigen::MatrixXcd> matA(Rz, (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        Error(_("svd: cannot take svd of matrix containing Inf or NaN values."));
    }
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
        int maxMN = std::max(m, n);
        double* superb = new_with_exception<double>(minMN - 1, true);
        double* dstemp = new_with_exception<double>(minMN, true);
        double* u = new_with_exception<double>(((size_t)ldu * (size_t)m * (size_t)2), true);
        auto* uz = reinterpret_cast<doublecomplex*>(u);
        double* vt = nullptr;
        doublecomplex* vtz = nullptr;
        if (withV) {
            vt = new_with_exception<double>((size_t)ldvt * (size_t)n * (size_t)2, true);
            vtz = reinterpret_cast<doublecomplex*>(vt);
        }
        int info = LAPACKE_zgesvd(
            LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt, superb);
        if (info > 0) {
            Error(_("LAPACKE_zgesvd error."));
        }
        delete[] superb;
        superb = nullptr;
        Dimensions dimsU(maxMN, maxMN);
        U = ArrayOf(NLS_DCOMPLEX, dimsU, u);
        double* ds = new_with_exception<double>((size_t)minMN * (size_t)maxMN, true);
        Dimensions dimsS(m, n);
        Eigen::Map<Eigen::VectorXd> matStmp(dstemp, minMN);
        Eigen::Map<Eigen::MatrixXd> matS(ds, maxMN, minMN);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
            matS(k, k) = matStmp(k);
        }
        delete[] dstemp;
        dstemp = nullptr;
        S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        if (withV) {
            Dimensions dimsV(minMN, minMN);
            Eigen::Map<Eigen::MatrixXcd> matV(vtz, minMN, minMN);
            matV = matV.transpose().eval();
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
        double* superb = new_with_exception<double>(minMN - 1, true);
        double* dstemp = new_with_exception<double>(minMN, true);
        double* u = new_with_exception<double>((size_t)ldu * (size_t)m * (size_t)2, true);
        auto* uz = reinterpret_cast<doublecomplex*>(u);
        double* vt = nullptr;
        doublecomplex* vtz = nullptr;
        if (withV) {
            vt = new_with_exception<double>((size_t)ldvt * (size_t)n * (size_t)2, true);
            vtz = reinterpret_cast<doublecomplex*>(vt);
        }
        int info = LAPACKE_zgesvd(
            LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt, superb);
        if (info > 0) {
            Error(_("LAPACKE_zgesvd error."));
        }
        delete[] superb;
        superb = nullptr;
        U = ArrayOf(NLS_DCOMPLEX, dimsU, u);
        Eigen::Map<Eigen::VectorXd> matStmp(dstemp, minMN);
        if (m > n) {
            double* ds = new_with_exception<double>((size_t)n * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXd> matS(ds, n, n);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        } else {
            double* ds = new_with_exception<double>((size_t)minMN * (size_t)maxMN, true);
            Eigen::Map<Eigen::MatrixXd> matS(ds, m, m);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        }
        delete[] dstemp;
        dstemp = nullptr;
        if (withV) {
            Eigen::Map<Eigen::MatrixXcd> matV(vtz, ldvt, n);
            double* vt2 = new_with_exception<double>((size_t)ldvt * (size_t)n * (size_t)2, true);
            auto* vt2z = reinterpret_cast<doublecomplex*>(vt2);
            Eigen::Map<Eigen::MatrixXcd> matV2(vt2z, n, ldvt);
            matV2 = matV.transpose().eval();
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
    Dimensions dimsA = A.getDimensions();
    int m = static_cast<int>(dimsA.getRows());
    int n = static_cast<int>(dimsA.getColumns());
    Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        Error(_("svd: cannot take svd of matrix containing Inf or NaN values."));
    }
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
        int maxMN = std::max(m, n);
        single* superb = new_with_exception<single>(minMN - 1, true);
        single* dstemp = new_with_exception<single>(minMN, true);
        single* u = new_with_exception<single>((size_t)(ldu) * (size_t)(m), true);
        single* vt = nullptr;
        if (withV) {
            vt = new_with_exception<single>((size_t)ldvt * (size_t)n, true);
        }
        int info = LAPACKE_sgesvd(LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, (single*)A.getDataPointer(),
            lda, dstemp, u, ldu, vt, ldvt, superb);
        if (info > 0) {
            Error(_("LAPACKE_sgesvd error."));
        }
        delete[] superb;
        superb = nullptr;
        Dimensions dimsU(maxMN, maxMN);
        U = ArrayOf(NLS_SINGLE, dimsU, u);
        single* ds = new_with_exception<single>((size_t)minMN * (size_t)maxMN, true);
        Dimensions dimsS(m, n);
        Eigen::Map<Eigen::VectorXf> matStmp(dstemp, minMN);
        Eigen::Map<Eigen::MatrixXf> matS(ds, maxMN, minMN);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
            matS(k, k) = matStmp(k);
        }
        delete[] dstemp;
        dstemp = nullptr;
        S = ArrayOf(NLS_SINGLE, dimsS, ds);
        if (withV) {
            Dimensions dimsV(minMN, minMN);
            Eigen::Map<Eigen::MatrixXf> matV(vt, minMN, minMN);
            matV = matV.transpose().eval();
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
        single* superb = new_with_exception<single>(minMN - 1, true);
        single* dstemp = new_with_exception<single>(minMN, true);
        single* u = new_with_exception<single>((size_t)ldu * (size_t)m, true);
        single* vt = nullptr;
        if (withV) {
            vt = new_with_exception<single>((size_t)ldvt * (size_t)n, true);
        }
        int info = LAPACKE_sgesvd(LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, (single*)A.getDataPointer(),
            lda, dstemp, u, ldu, vt, ldvt, superb);
        if (info > 0) {
            Error(_("LAPACKE_sgesvd error."));
        }
        delete[] superb;
        superb = nullptr;
        U = ArrayOf(NLS_SINGLE, dimsU, u);
        Eigen::Map<Eigen::VectorXf> matStmp(dstemp, minMN);
        if (m > n) {
            single* ds = new_with_exception<single>((size_t)n * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXf> matS(ds, n, n);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_SINGLE, dimsS, ds);
        } else {
            single* ds = new_with_exception<single>((size_t)minMN * (size_t)maxMN, true);
            Eigen::Map<Eigen::MatrixXf> matS(ds, m, m);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_SINGLE, dimsS, ds);
        }
        delete[] dstemp;
        dstemp = nullptr;
        if (withV) {
            Eigen::Map<Eigen::MatrixXf> matV(vt, ldvt, n);
            single* vt2 = new_with_exception<single>((size_t)ldvt * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXf> matV2(vt2, n, ldvt);
            matV2 = matV.transpose().eval();
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
    Dimensions dimsA = A.getDimensions();
    int m = static_cast<int>(dimsA.getRows());
    int n = static_cast<int>(dimsA.getColumns());
    Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        Error(_("svd: cannot take svd of matrix containing Inf or NaN values."));
    }
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
        int maxMN = std::max(m, n);
        double* superb = new_with_exception<double>(minMN - 1, true);
        double* dstemp = new_with_exception<double>(minMN, true);
        double* u = new_with_exception<double>((size_t)ldu * (size_t)m, true);
        double* vt = nullptr;
        if (withV) {
            vt = new_with_exception<double>((size_t)ldvt * (size_t)n, true);
        }
        int info = LAPACKE_dgesvd(LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, (double*)A.getDataPointer(),
            lda, dstemp, u, ldu, vt, ldvt, superb);
        if (info > 0) {
            Error(_("LAPACKE_dgesvd error."));
        }
        delete[] superb;
        superb = nullptr;
        Dimensions dimsU(maxMN, maxMN);
        U = ArrayOf(NLS_DOUBLE, dimsU, u);
        double* ds = new_with_exception<double>((size_t)minMN * (size_t)maxMN, true);
        Dimensions dimsS(m, n);
        Eigen::Map<Eigen::VectorXd> matStmp(dstemp, minMN);
        Eigen::Map<Eigen::MatrixXd> matS(ds, maxMN, minMN);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
            matS(k, k) = matStmp(k);
        }
        delete[] dstemp;
        dstemp = nullptr;
        S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        if (withV) {
            Dimensions dimsV(minMN, minMN);
            Eigen::Map<Eigen::MatrixXd> matV(vt, minMN, minMN);
            matV = matV.transpose().eval();
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
        double* superb = new_with_exception<double>(minMN - 1, true);
        double* dstemp = new_with_exception<double>(minMN, true);
        double* u = new_with_exception<double>((size_t)ldu * (size_t)m, true);
        double* vt = nullptr;
        if (withV) {
            vt = new_with_exception<double>((size_t)ldvt * (size_t)n, true);
        }
        int info = LAPACKE_dgesvd(LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, (double*)A.getDataPointer(),
            lda, dstemp, u, ldu, vt, ldvt, superb);
        if (info > 0) {
            Error(_("LAPACKE_dgesvd error."));
        }
        delete[] superb;
        superb = nullptr;
        U = ArrayOf(NLS_DOUBLE, dimsU, u);
        Eigen::Map<Eigen::VectorXd> matStmp(dstemp, minMN);
        if (m > n) {
            double* ds = new_with_exception<double>((size_t)n * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXd> matS(ds, n, n);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        } else {
            double* ds = new_with_exception<double>((size_t)minMN * (size_t)maxMN, true);
            Eigen::Map<Eigen::MatrixXd> matS(ds, m, m);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_DOUBLE, dimsS, ds);
        }
        delete[] dstemp;
        dstemp = nullptr;
        if (withV) {
            Eigen::Map<Eigen::MatrixXd> matV(vt, ldvt, n);
            double* vt2 = new_with_exception<double>((size_t)ldvt * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXd> matV2(vt2, n, ldvt);
            matV2 = matV.transpose().eval();
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
    Dimensions dimsA = A.getDimensions();
    int m = static_cast<int>(dimsA.getRows());
    int n = static_cast<int>(dimsA.getColumns());
    auto* Rz = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(Rz, (Eigen::Index)m, (Eigen::Index)n);
    if (!matA.allFinite()) {
        Error(_("svd: cannot take svd of matrix containing Inf or NaN values."));
    }
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
        int maxMN = std::max(m, n);
        single* superb = new_with_exception<single>(minMN - 1, true);
        single* dstemp = new_with_exception<single>(minMN, true);
        single* u = new_with_exception<single>((size_t)ldu * (size_t)m * (size_t)2, true);
        single* vt = nullptr;
        singlecomplex* vtz = nullptr;
        if (withV) {
            vt = new_with_exception<single>((size_t)ldvt * (size_t)n * (size_t)2, true);
            vtz = reinterpret_cast<singlecomplex*>(vt);
        }
        auto* uz = reinterpret_cast<singlecomplex*>(u);
        int info = LAPACKE_cgesvd(
            LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt, superb);
        if (info > 0) {
            Error(_("LAPACKE_cgesvd error."));
        }
        delete[] superb;
        superb = nullptr;
        Dimensions dimsU(maxMN, maxMN);
        U = ArrayOf(NLS_SCOMPLEX, dimsU, u);
        single* ds = new_with_exception<single>((size_t)minMN * (size_t)maxMN, true);
        Dimensions dimsS(m, n);
        Eigen::Map<Eigen::VectorXf> matStmp(dstemp, minMN);
        Eigen::Map<Eigen::MatrixXf> matS(ds, maxMN, minMN);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
            matS(k, k) = matStmp(k);
        }
        delete[] dstemp;
        dstemp = nullptr;
        S = ArrayOf(NLS_SINGLE, dimsS, ds);
        if (withV) {
            Dimensions dimsV(minMN, minMN);
            Eigen::Map<Eigen::MatrixXcf> matV(vtz, minMN, minMN);
            matV = matV.transpose().eval();
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
        single* superb = new_with_exception<single>(minMN - 1, true);
        single* dstemp = new_with_exception<single>(minMN, true);
        single* u = new_with_exception<single>((size_t)ldu * (size_t)m * (size_t)2, true);
        auto* uz = reinterpret_cast<singlecomplex*>(u);
        single* vt = nullptr;
        singlecomplex* vtz = nullptr;
        if (withV) {
            vt = new_with_exception<single>((size_t)ldvt * (size_t)n * (size_t)2, true);
            vtz = reinterpret_cast<singlecomplex*>(vt);
        }
        int info = LAPACKE_cgesvd(
            LAPACK_COL_MAJOR, JOBU, JOBVT, m, n, Rz, lda, dstemp, uz, ldu, vtz, ldvt, superb);
        if (info > 0) {
            Error(_("LAPACKE_cgesvd error."));
        }
        delete[] superb;
        superb = nullptr;
        U = ArrayOf(NLS_SCOMPLEX, dimsU, u);
        Eigen::Map<Eigen::VectorXf> matStmp(dstemp, minMN);
        if (m > n) {
            single* ds = new_with_exception<single>((size_t)n * (size_t)n, true);
            Eigen::Map<Eigen::MatrixXf> matS(ds, n, n);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_SINGLE, dimsS, ds);
        } else {
            single* ds = new_with_exception<single>((size_t)minMN * (size_t)maxMN, true);
            Eigen::Map<Eigen::MatrixXf> matS(ds, m, m);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)minMN; k++) {
                matS(k, k) = matStmp(k);
            }
            S = ArrayOf(NLS_SINGLE, dimsS, ds);
        }
        delete[] dstemp;
        dstemp = nullptr;
        if (withV) {
            Eigen::Map<Eigen::MatrixXcf> matV(vtz, ldvt, n);
            single* vt2 = new_with_exception<single>((size_t)ldvt * (size_t)n * (size_t)2, true);
            auto* vt2z = reinterpret_cast<singlecomplex*>(vt2);
            Eigen::Map<Eigen::MatrixXcf> matV2(vt2z, n, ldvt);
            matV2 = matV.transpose().eval();
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
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(
            _("Undefined function 'svd' for input arguments of type") + " '" + ClassName(A) + "'.");
    }
    Dimensions DimsA = A.getDimensions();
    indexType m = DimsA.getRows();
    indexType n = DimsA.getColumns();
    if (A.isEmpty()) {
        S = ArrayOf::emptyConstructor(DimsA);
        if (m == 0) {
            U = ArrayOf::emptyConstructor(m, m);
            if (withV) {
                indexType n = DimsA.getColumns();
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
                V = ArrayOf::emptyConstructor(DimsA.getColumns(), DimsA.getColumns());
            }
        }
        if (A.getDataClass() == NLS_SINGLE || A.getDataClass() == NLS_SCOMPLEX) {
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
        if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
            if (A.getDataClass() == NLS_DOUBLE) {
                SVD_double(COPY_A, svdFlag, U, S, V, withV);
            } else {
                SVD_doublecomplex(COPY_A, svdFlag, U, S, V, withV);
            }
        } else {
            if (A.getDataClass() == NLS_SINGLE) {
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
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(
            _("Undefined function 'svd' for input arguments of type") + " '" + ClassName(A) + "'.");
    }
    if (A.isEmpty()) {
        Dimensions dimsS(0, 1);
        s = ArrayOf::emptyConstructor(dimsS);
        if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
            s.promoteType(NLS_DOUBLE);
        } else {
            s.promoteType(NLS_SINGLE);
        }
    } else {
        ArrayOf COPY_A(A);
        COPY_A.ensureSingleOwner();
        if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
            if (A.getDataClass() == NLS_DOUBLE) {
                SVD_double(COPY_A, s);
            } else {
                SVD_doublecomplex(COPY_A, s);
            }
        } else {
            if (A.getDataClass() == NLS_SINGLE) {
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
