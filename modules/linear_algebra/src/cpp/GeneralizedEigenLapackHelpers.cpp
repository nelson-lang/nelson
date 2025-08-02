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
#define _SCL_SECURE_NO_WARNINGS
#endif
#include <cstring>
#include "Types.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/src/misc/lapacke.h>
#include "NewWithException.hpp"
#include "EigenLapackHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
singleGeneralizedEigenDecomposition(
    int n, float* v, float* d, float* a, float* b, bool eigenvectors)
{
    char JOBVL = 'N';
    char JOBVR;
    if (eigenvectors) {
        JOBVR = 'V';
    } else {
        JOBVR = 'N';
    }
    int N = n;
    float* A = a;
    int LDA = n;
    float* B = b;
    int LDB = n;
    float* ALPHAR = (float*)new_with_exception<float>(n, true);
    float* ALPHAI = (float*)new_with_exception<float>(n, true);
    float* BETA = (float*)new_with_exception<float>(n, true);
    float* VL = nullptr;
    int LDVL = 1;
    float* VR = v;
    int LDVR = n;
    float WORKSZE;
    int LWORK = -1;
    int INFO;
    LAPACK_sggev(&JOBVL, &JOBVR, &N, A, &LDA, B, &LDB, ALPHAR, ALPHAI, BETA, VL, &LDVL, VR, &LDVR,
        &WORKSZE, &LWORK, &INFO);
    LWORK = (int)WORKSZE;
    float* WORK = (float*)new_with_exception<float>(LWORK, true);
    LAPACK_sggev(&JOBVL, &JOBVR, &N, A, &LDA, B, &LDB, ALPHAR, ALPHAI, BETA, VL, &LDVL, VR, &LDVR,
        WORK, &LWORK, &INFO);
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
        d[2 * i] = ALPHAR[i] / BETA[i];
        d[2 * i + 1] = ALPHAI[i] / BETA[i];
    }
    delete[] ALPHAR;
    delete[] BETA;
    delete[] ALPHAI;
    delete[] WORK;
}
//=============================================================================
bool
singleGeneralizedEigenDecompositionSymmetric(
    int n, float* v, float* d, float* a, float* b, bool eigenvectors)
{
    int ITYPE = 1;
    char JOBZ;
    if (eigenvectors) {
        JOBZ = 'V';
    } else {
        JOBZ = 'N';
    }
    char UPLO = 'U';
    int N = n;
    float* A = a;
    int LDA = n;
    float* B = b;
    int LDB = n;
    float* W = d;
    float WORKSIZE;
    int LWORK = -1;
    int INFO;
    LAPACK_ssygv(&ITYPE, &JOBZ, &UPLO, &N, A, &LDA, B, &LDB, W, &WORKSIZE, &LWORK, &INFO);
    LWORK = (int)WORKSIZE;
    float* WORK = (float*)new_with_exception<float>(LWORK, true);
    LAPACK_ssygv(&ITYPE, &JOBZ, &UPLO, &N, A, &LDA, B, &LDB, W, WORK, &LWORK, &INFO);
    delete[] WORK;
    if (INFO > N) {
        return false;
    }
    if (eigenvectors) {
        long int nn = (long int)(n * n);
        memcpy(v, a, nn * sizeof(float));
    }
    return true;
}
//=============================================================================
void
doubleGeneralizedEigenDecomposition(
    int n, double* v, double* d, double* a, double* b, bool eigenvectors)
{
    char JOBVL = 'N';
    char JOBVR;
    if (eigenvectors) {
        JOBVR = 'V';
    } else {
        JOBVR = 'N';
    }
    int N = n;
    double* A = a;
    int LDA = n;
    double* B = b;
    int LDB = n;
    double* ALPHAR = (double*)new_with_exception<double>(n, true);
    double* ALPHAI = (double*)new_with_exception<double>(n, true);
    double* BETA = (double*)new_with_exception<double>(n, true);
    double* VL = nullptr;
    int LDVL = 1;
    double* VR = v;
    int LDVR = n;
    double WORKSZE;
    int LWORK = -1;
    int INFO;
    LAPACK_dggev(&JOBVL, &JOBVR, &N, A, &LDA, B, &LDB, ALPHAR, ALPHAI, BETA, VL, &LDVL, VR, &LDVR,
        &WORKSZE, &LWORK, &INFO);
    LWORK = (int)WORKSZE;
    double* WORK = (double*)new_with_exception<double>(LWORK, true);
    LAPACK_dggev(&JOBVL, &JOBVR, &N, A, &LDA, B, &LDB, ALPHAR, ALPHAI, BETA, VL, &LDVL, VR, &LDVR,
        WORK, &LWORK, &INFO);
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
        d[2 * i] = ALPHAR[i] / BETA[i];
        d[2 * i + 1] = ALPHAI[i] / BETA[i];
    }
    delete[] ALPHAR;
    delete[] BETA;
    delete[] ALPHAI;
    delete[] WORK;
}
//=============================================================================
bool
doubleGeneralizedEigenDecompositionSymmetric(
    int n, double* v, double* d, double* a, double* b, bool eigenvectors)
{
    int ITYPE = 1;
    char JOBZ;
    if (eigenvectors) {
        JOBZ = 'V';
    } else {
        JOBZ = 'N';
    }
    char UPLO = 'U';
    int N = n;
    double* A = a;
    int LDA = n;
    double* B = b;
    int LDB = n;
    double* W = d;
    double WORKSIZE;
    int LWORK = -1;
    int INFO;
    LAPACK_dsygv(&ITYPE, &JOBZ, &UPLO, &N, A, &LDA, B, &LDB, W, &WORKSIZE, &LWORK, &INFO);
    LWORK = (int)WORKSIZE;
    double* WORK = (double*)new_with_exception<double>(LWORK, true);
    LAPACK_dsygv(&ITYPE, &JOBZ, &UPLO, &N, A, &LDA, B, &LDB, W, WORK, &LWORK, &INFO);
    delete[] WORK;
    if (INFO > N) {
        return false;
    }
    if (eigenvectors) {
        long int nn = (long int)n * (long int)n;
        memcpy(v, a, nn * sizeof(double));
    }
    return true;
}
//=============================================================================
void
singleComplexGeneralizedEigenDecomposition(int n, std::complex<float>* v, std::complex<float>* d,
    std::complex<float>* a, std::complex<float>* b, bool eigenvectors)
{
    char JOBVL = 'N';
    char JOBVR;
    if (eigenvectors) {
        JOBVR = 'V';
    } else {
        JOBVR = 'N';
    }
    int N = n;
    std::complex<float>* A = a;
    int LDA = n;
    std::complex<float>* B = b;
    int LDB = N;
    std::complex<float>* ALPHA
        = (std::complex<float>*)new_with_exception<std::complex<float>>(n, true);
    std::complex<float>* BETA
        = (std::complex<float>*)new_with_exception<std::complex<float>>(n, true);
    std::complex<float>* VL = nullptr;
    int LDVL = n;
    std::complex<float>* VR = v;
    int LDVR = n;
    long int n8 = 8 * n;
    float* RWORK = (float*)new_with_exception<float>(n8, true);
    std::complex<float> WORKSIZE[2];
    int LWORK = -1;
    int INFO;
    LAPACK_cggev(&JOBVL, &JOBVR, &N, A, &LDA, B, &LDB, ALPHA, BETA, VL, &LDVL, VR, &LDVR,
        &WORKSIZE[0], &LWORK, RWORK, &INFO);
    LWORK = (int)WORKSIZE[0].real();
    std::complex<float>* WORK
        = (std::complex<float>*)new_with_exception<std::complex<float>>(LWORK, true);
    LAPACK_cggev(&JOBVL, &JOBVR, &N, A, &LDA, B, &LDB, ALPHA, BETA, VL, &LDVL, VR, &LDVR, WORK,
        &LWORK, RWORK, &INFO);
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
        d[i] = ALPHA[i] / BETA[i];
    }
    delete[] ALPHA;
    delete[] BETA;
    delete[] RWORK;
    delete[] WORK;
}
//=============================================================================
bool
singleComplexGeneralizedEigenDecompositionSymmetric(int n, std::complex<float>* v, float* d,
    std::complex<float>* a, std::complex<float>* b, bool eigenvectors)
{
    int ITYPE = 1;
    char JOBZ;
    if (eigenvectors) {
        JOBZ = 'V';
    } else {
        JOBZ = 'N';
    }
    char UPLO = 'U';
    int N = n;
    std::complex<float>* A = a;
    int LDA = n;
    std::complex<float>* B = b;
    int LDB = n;
    float* W = d;
    float* RWORK = (float*)new_with_exception<float>(std::max(1, 3 * N - 2), true);
    int INFO;
    int LWORK;
    LWORK = std::max(1, 2 * N - 1);
    std::complex<float>* WORK
        = (std::complex<float>*)new_with_exception<std::complex<float>>(LWORK, true);
    LAPACK_chegv(&ITYPE, &JOBZ, &UPLO, &N, A, &LDA, B, &LDB, d, WORK, &LWORK, RWORK, &INFO);
    delete[] WORK;
    delete[] RWORK;
    if (INFO > N) {
        return false;
    }
    if (eigenvectors) {
        long int nn = (long int)n * (long int)n;
        memcpy(v, a, nn * sizeof(std::complex<float>));
    }
    return true;
}
//=============================================================================
bool
doubleComplexGeneralizedEigenDecompositionSymmetric(int n, std::complex<double>* v, double* d,
    std::complex<double>* a, std::complex<double>* b, bool eigenvectors)
{
    int ITYPE = 1;
    char JOBZ;
    if (eigenvectors) {
        JOBZ = 'V';
    } else {
        JOBZ = 'N';
    }
    char UPLO = 'U';
    int N = n;
    std::complex<double>* A = a;
    int LDA = n;
    std::complex<double>* B = b;
    int LDB = n;
    double* RWORK = (double*)new_with_exception<double>(std::max(1, 3 * N - 2), true);
    int INFO;
    int LWORK;
    LWORK = std::max(1, 2 * N - 1);
    std::complex<double>* WORK
        = (std::complex<double>*)new_with_exception<std::complex<double>>(LWORK, true);
    LAPACK_zhegv(&ITYPE, &JOBZ, &UPLO, &N, A, &LDA, B, &LDB, d, WORK, &LWORK, RWORK, &INFO);
    delete[] WORK;
    delete[] RWORK;
    if (INFO > N) {
        return false;
    }
    if (eigenvectors) {
        long int nn = (long int)n * (long int)n;
        memcpy(v, a, nn * sizeof(std::complex<double>));
    }
    return true;
}
//=============================================================================
void
doubleComplexGeneralizedEigenDecomposition(int n, std::complex<double>* v, std::complex<double>* d,
    std::complex<double>* a, std::complex<double>* b, bool eigenvectors)
{
    char JOBVL = 'N';
    char JOBVR;
    if (eigenvectors) {
        JOBVR = 'V';
    } else {
        JOBVR = 'N';
    }
    int N = n;
    std::complex<double>* A = a;
    int LDA = n;
    std::complex<double>* B = b;
    int LDB = N;
    std::complex<double>* ALPHA
        = (std::complex<double>*)new_with_exception<std::complex<double>>(n, true);
    std::complex<double>* BETA
        = (std::complex<double>*)new_with_exception<std::complex<double>>(n, true);
    std::complex<double>* VL = nullptr;
    int LDVL = n;
    std::complex<double>* VR = v;
    int LDVR = n;
    long int n8 = 8 * n;
    double* RWORK = (double*)new_with_exception<double>(n8, true);
    std::complex<double> WORKSIZE[2];
    int LWORK = -1;
    int INFO;
    LAPACK_zggev(&JOBVL, &JOBVR, &N, A, &LDA, B, &LDB, ALPHA, BETA, VL, &LDVL, VR, &LDVR,
        &WORKSIZE[0], &LWORK, RWORK, &INFO);
    LWORK = (int)WORKSIZE[0].real();
    std::complex<double>* WORK
        = (std::complex<double>*)new_with_exception<std::complex<double>>(LWORK, true);
    LAPACK_zggev(&JOBVL, &JOBVR, &N, A, &LDA, B, &LDB, ALPHA, BETA, VL, &LDVL, VR, &LDVR, WORK,
        &LWORK, RWORK, &INFO);
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
        d[i] = ALPHA[i] / BETA[i];
    }
    delete[] ALPHA;
    delete[] BETA;
    delete[] RWORK;
    delete[] WORK;
}
//=============================================================================
}
//=============================================================================
