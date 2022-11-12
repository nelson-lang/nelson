//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include <cstring>
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/src/misc/lapacke.h>
#include "NewWithException.hpp"
#include "EigenLapackHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
doubleEigenDecomposition(int n, std::complex<double>* v, std::complex<double>* d, double* a,
    bool eigenvectors, bool balance)
{
    char BALANC;
    if (balance) {
        BALANC = 'B';
    } else {
        BALANC = 'N';
    }
    char JOBVL = 'N';
    char JOBVR;
    if (eigenvectors) {
        JOBVR = 'V';
    } else {
        JOBVR = 'N';
    }
    char SENSE = 'N';
    int N = n;
    double* Ain = a;
    int LDA = n;
    double* WR = (double*)new_with_exception<double>(n, false);
    double* WI = (double*)new_with_exception<double>(n, false);
    double* VL = nullptr;
    int LDVL = 1;
    auto* VR = (double*)reinterpret_cast<double*>(v);
    int LDVR = n;
    int ILO;
    int IHI;
    double* SCALE = (double*)new_with_exception<double>(n, false);
    double ABNRM;
    double* RCONDE = (double*)new_with_exception<double>(n, false);
    double* RCONDV = (double*)new_with_exception<double>(n, false);
    int maxN = (N < 6) ? 6 : N;
    int LWORK = maxN * maxN * 2;
    long int n2 = (2 * n - 2);
    int* IWORK = (int*)new_with_exception<int>(n2, false);
    int INFO;
    double* WORK = (double*)new_with_exception<double>(LWORK, false);
    LAPACK_dgeevx(&BALANC, &JOBVL, &JOBVR, &SENSE, &N, Ain, &LDA, WR, WI, VL, &LDVL, VR, &LDVR,
        &ILO, &IHI, SCALE, &ABNRM, RCONDE, RCONDV, WORK, &LWORK, IWORK, &INFO);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (int i = 0; i < N; i++) {
        d[i].real(WR[i]);
        d[i].imag(WI[i]);
    }
    delete[] WORK;
    delete[] WR;
    delete[] WI;
    delete[] SCALE;
    delete[] RCONDE;
    delete[] RCONDV;
    delete[] IWORK;
}
//=============================================================================
void
singleEigenDecomposition(int n, std::complex<float>* v, std::complex<float>* d, float* a,
    bool eigenvectors, bool balance)
{
    char BALANC;
    if (balance) {
        BALANC = 'B';
    } else {
        BALANC = 'N';
    }
    char JOBVL = 'N';
    char JOBVR;
    if (eigenvectors) {
        JOBVR = 'V';
    } else {
        JOBVR = 'N';
    }
    char SENSE = 'N';
    int N = n;
    float* Ain = a;
    int LDA = n;
    float* WR = (float*)new_with_exception<float>(n, false);
    float* WI = (float*)new_with_exception<float>(n, false);
    float* VL = nullptr;
    int LDVL = 1;
    auto* VR = (float*)reinterpret_cast<float*>(v);
    int LDVR = n;
    int ILO;
    int IHI;
    float* SCALE = (float*)new_with_exception<float>(n, false);
    float ABNRM;
    float* RCONDE = (float*)new_with_exception<float>(n, false);
    float* RCONDV = (float*)new_with_exception<float>(n, false);
    int maxN = (N < 6) ? 6 : N;
    int LWORK = maxN * maxN * 2;
    long int n2 = 2 * n - 2;
    int* IWORK = (int*)new_with_exception<int>(n2, false);
    int INFO;
    float* WORK = (float*)new_with_exception<float>(LWORK, true);
    LAPACK_sgeevx(&BALANC, &JOBVL, &JOBVR, &SENSE, &N, Ain, &LDA, WR, WI, VL, &LDVL, VR, &LDVR,
        &ILO, &IHI, SCALE, &ABNRM, RCONDE, RCONDV, WORK, &LWORK, IWORK, &INFO);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (int i = 0; i < N; i++) {
        d[i].real(WR[i]);
        d[i].imag(WI[i]);
    }
    delete[] WORK;
    delete[] WR;
    delete[] WI;
    delete[] SCALE;
    delete[] RCONDE;
    delete[] RCONDV;
    delete[] IWORK;
}
//=============================================================================
void
doubleEigenDecompositionSymmetric(int n, double* v, double* d, double* a, bool eigenvectors)
{
    char JOBZ;
    if (eigenvectors) {
        JOBZ = 'V';
    } else {
        JOBZ = 'N';
    }
    char UPLO = 'U';
    int N = n;
    double* Ain = a;
    int LDA = n;
    int INFO;
    double WORKSZE;
    int LWORK;

    LWORK = -1;
    LAPACK_dsyev(&JOBZ, &UPLO, &N, Ain, &LDA, d, &WORKSZE, &LWORK, &INFO);
    LWORK = (int)WORKSZE;
    double* WORK = (double*)new_with_exception<double>(LWORK, false);
    LAPACK_dsyev(&JOBZ, &UPLO, &N, Ain, &LDA, d, WORK, &LWORK, &INFO);
    delete[] WORK;
    if (eigenvectors) {
        long int nn = (long int)n * (long int)n;
        memcpy(v, a, nn * sizeof(double));
    }
}
//=============================================================================
void
singleEigenDecompositionSymmetric(int n, float* v, float* d, float* a, bool eigenvectors)
{
    char JOBZ;
    if (eigenvectors) {
        JOBZ = 'V';
    } else {
        JOBZ = 'N';
    }
    char UPLO = 'U';
    int N = n;
    float* Ain = a;
    int LDA = n;
    int INFO;
    float WORKSZE;
    int LWORK;

    LWORK = -1;
    LAPACK_ssyev(&JOBZ, &UPLO, &N, Ain, &LDA, d, &WORKSZE, &LWORK, &INFO);
    LWORK = (int)WORKSZE;
    float* WORK = (float*)new_with_exception<float>(LWORK, false);
    LAPACK_ssyev(&JOBZ, &UPLO, &N, Ain, &LDA, d, WORK, &LWORK, &INFO);
    delete[] WORK;
    if (eigenvectors) {
        long int nn = (long int)n * (long int)n;
        memcpy(v, a, nn * sizeof(float));
    }
}
//=============================================================================
void
singleComplexEigenDecompositionSymmetric(
    int n, std::complex<float>* v, float* d, std::complex<float>* a, bool eigenvectors)
{
    char JOBZ;
    if (eigenvectors) {
        JOBZ = 'V';
    } else {
        JOBZ = 'N';
    }
    char UPLO = 'U';
    int N = n;
    int LDA = n;
    float* RWORK = (float*)new_with_exception<float>(std::max(1, 3 * N - 2), false);
    int LWORK;
    int INFO;
    std::complex<float> WORKSZE[2];
    LWORK = -1;
    LAPACK_cheev(&JOBZ, &UPLO, &N, a, &LDA, d, WORKSZE, &LWORK, RWORK, &INFO);
    LWORK = (int)WORKSZE[0].real();
    std::complex<float>* WORK
        = (std::complex<float>*)new_with_exception<std::complex<float>>(LWORK, false);
    LAPACK_cheev(&JOBZ, &UPLO, &N, a, &LDA, d, WORK, &LWORK, RWORK, &INFO);
    delete[] WORK;
    delete[] RWORK;
    if (eigenvectors) {
        long int nn = (long int)n * (long int)n;
        memcpy(v, a, nn * sizeof(std::complex<float>));
    }
}
//=============================================================================
void
singleComplexEigenDecomposition(int n, std::complex<float>* v, std::complex<float>* d,
    std::complex<float>* a, bool eigenvectors, bool balance)
{
    char BALANC;
    if (balance) {
        BALANC = 'B';
    } else {
        BALANC = 'N';
    }
    char JOBVL = 'N';
    char JOBVR;
    if (eigenvectors) {
        JOBVR = 'V';
    } else {
        JOBVR = 'N';
    }
    char SENSE = 'N';
    int N = n;
    std::complex<float>* Ain = a;
    int LDA = n;
    std::complex<float>* W = d;
    std::complex<float>* VL = nullptr;
    int LDVL = 1;
    std::complex<float>* VR = v;
    int LDVR = n;
    int ILO;
    int IHI;
    float* SCALE = (float*)new_with_exception<float>(n, false);
    float ABNRM;
    float* RCONDE = (float*)new_with_exception<float>(n, false);
    float* RCONDV = (float*)new_with_exception<float>(n, false);
    int LWORK;
    float* RWORK = (float*)new_with_exception<float>(2 * n, false);
    int INFO;
    std::complex<float> WORKSZE[2];
    LWORK = -1;
    LAPACK_cgeevx(&BALANC, &JOBVL, &JOBVR, &SENSE, &N, Ain, &LDA, W, VL, &LDVL, VR, &LDVR, &ILO,
        &IHI, SCALE, &ABNRM, RCONDE, RCONDV, WORKSZE, &LWORK, RWORK, &INFO);
    LWORK = (int)WORKSZE[0].real();
    std::complex<float>* WORK
        = (std::complex<float>*)new_with_exception<std::complex<float>>(LWORK, false);
    LAPACK_cgeevx(&BALANC, &JOBVL, &JOBVR, &SENSE, &N, Ain, &LDA, W, VL, &LDVL, VR, &LDVR, &ILO,
        &IHI, SCALE, &ABNRM, RCONDE, RCONDV, WORK, &LWORK, RWORK, &INFO);
    delete[] WORK;
    delete[] SCALE;
    delete[] RCONDE;
    delete[] RCONDV;
    delete[] RWORK;
}
//=============================================================================
void
doubleComplexEigenDecompositionSymmetric(
    int n, std::complex<double>* v, double* d, std::complex<double>* a, bool eigenvectors)
{
    char JOBZ;
    if (eigenvectors) {
        JOBZ = 'V';
    } else {
        JOBZ = 'N';
    }
    char UPLO = 'U';
    int N = n;
    int LDA = n;
    double* RWORK = (double*)new_with_exception<double>(std::max(1, 3 * N - 2), false);
    int LWORK;
    int INFO;
    std::complex<double> WORKSZE;
    LWORK = -1;
    LAPACK_zheev(&JOBZ, &UPLO, &N, a, &LDA, d, &WORKSZE, &LWORK, RWORK, &INFO);
    LWORK = (int)WORKSZE.real();
    std::complex<double>* WORK
        = (std::complex<double>*)new_with_exception<std::complex<double>>(LWORK, false);
    LAPACK_zheev(&JOBZ, &UPLO, &N, a, &LDA, d, WORK, &LWORK, RWORK, &INFO);
    delete[] WORK;
    delete[] RWORK;
    long int nn = (long int)n * (long int)n;
    if (eigenvectors) {
        memcpy(v, a, nn * sizeof(std::complex<double>));
    }
}
//=============================================================================
void
doubleComplexEigenDecomposition(int n, std::complex<double>* v, std::complex<double>* d,
    std::complex<double>* a, bool eigenvectors, bool balance)
{
    char BALANC;
    if (balance) {
        BALANC = 'B';
    } else {
        BALANC = 'N';
    }
    char JOBVL = 'N';
    char JOBVR;
    if (eigenvectors) {
        JOBVR = 'V';
    } else {
        JOBVR = 'N';
    }
    char SENSE = 'N';
    int N = n;
    std::complex<double>* Ain = a;
    int LDA = n;
    std::complex<double>* W = d;
    std::complex<double>* VL = nullptr;
    int LDVL = 1;
    std::complex<double>* VR = v;
    int LDVR = n;
    int ILO;
    int IHI;
    double* SCALE = (double*)new_with_exception<double>(n, true);
    double ABNRM;
    double* RCONDE = (double*)new_with_exception<double>(n, true);
    double* RCONDV = (double*)new_with_exception<double>(n, true);
    int LWORK;
    double* RWORK = (double*)new_with_exception<double>(2 * n, false);
    int INFO;

    std::complex<double> WORKSZE[2];

    LWORK = -1;
    LAPACK_zgeevx(&BALANC, &JOBVL, &JOBVR, &SENSE, &N, Ain, &LDA, W, VL, &LDVL, VR, &LDVR, &ILO,
        &IHI, SCALE, &ABNRM, RCONDE, RCONDV, WORKSZE, &LWORK, RWORK, &INFO);

    LWORK = (int)WORKSZE[0].real();
    std::complex<double>* WORK
        = (std::complex<double>*)new_with_exception<std::complex<double>>(LWORK, false);

    LAPACK_zgeevx(&BALANC, &JOBVL, &JOBVR, &SENSE, &N, Ain, &LDA, W, VL, &LDVL, VR, &LDVR, &ILO,
        &IHI, SCALE, &ABNRM, RCONDE, RCONDV, WORK, &LWORK, RWORK, &INFO);

    delete[] WORK;
    delete[] SCALE;
    delete[] RCONDE;
    delete[] RCONDV;
    delete[] RWORK;
}
//=============================================================================
}
//=============================================================================
