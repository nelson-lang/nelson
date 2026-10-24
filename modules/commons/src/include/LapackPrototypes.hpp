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
#include <complex>
//=============================================================================
#if defined(EIGEN_USE_MKL)
#ifndef MKL_Complex8
#define MKL_Complex8 std::complex<float>
#endif
#ifndef MKL_Complex16
#define MKL_Complex16 std::complex<double>
#endif
#include <mkl_lapack.h>
#endif
//=============================================================================
#ifndef lapack_int
#if defined(MKL_INT)
typedef MKL_INT lapack_int;
#else
typedef int lapack_int;
#endif
#endif
//=============================================================================
#ifndef LAPACK_GLOBAL
#if defined(LAPACK_GLOBAL_PATTERN_LC) || defined(ADD_)
#define LAPACK_GLOBAL(lcname, UCNAME) lcname##_
#elif defined(LAPACK_GLOBAL_PATTERN_UC) || defined(UPPER)
#define LAPACK_GLOBAL(lcname, UCNAME) UCNAME
#elif defined(LAPACK_GLOBAL_PATTERN_MC) || defined(NOCHANGE)
#define LAPACK_GLOBAL(lcname, UCNAME) lcname
#else
#define LAPACK_GLOBAL(lcname, UCNAME) lcname##_
#endif
#endif
//=============================================================================
#define LAPACK_sgebal LAPACK_GLOBAL(sgebal, SGEBAL)
#define LAPACK_dgebal LAPACK_GLOBAL(dgebal, DGEBAL)
#define LAPACK_cgebal LAPACK_GLOBAL(cgebal, CGEBAL)
#define LAPACK_zgebal LAPACK_GLOBAL(zgebal, ZGEBAL)
#define LAPACK_slange LAPACK_GLOBAL(slange, SLANGE)
#define LAPACK_dlange LAPACK_GLOBAL(dlange, DLANGE)
#define LAPACK_clange LAPACK_GLOBAL(clange, CLANGE)
#define LAPACK_zlange LAPACK_GLOBAL(zlange, ZLANGE)
#define LAPACK_sgetrf LAPACK_GLOBAL(sgetrf, SGETRF)
#define LAPACK_dgetrf LAPACK_GLOBAL(dgetrf, DGETRF)
#define LAPACK_cgetrf LAPACK_GLOBAL(cgetrf, CGETRF)
#define LAPACK_zgetrf LAPACK_GLOBAL(zgetrf, ZGETRF)
#define LAPACK_sgetrs LAPACK_GLOBAL(sgetrs, SGETRS)
#define LAPACK_dgetrs LAPACK_GLOBAL(dgetrs, DGETRS)
#define LAPACK_cgetrs LAPACK_GLOBAL(cgetrs, CGETRS)
#define LAPACK_zgetrs LAPACK_GLOBAL(zgetrs, ZGETRS)
#define LAPACK_sgetri LAPACK_GLOBAL(sgetri, SGETRI)
#define LAPACK_dgetri LAPACK_GLOBAL(dgetri, DGETRI)
#define LAPACK_cgetri LAPACK_GLOBAL(cgetri, CGETRI)
#define LAPACK_zgetri LAPACK_GLOBAL(zgetri, ZGETRI)
#define LAPACK_sgecon LAPACK_GLOBAL(sgecon, SGECON)
#define LAPACK_dgecon LAPACK_GLOBAL(dgecon, DGECON)
#define LAPACK_cgecon LAPACK_GLOBAL(cgecon, CGECON)
#define LAPACK_zgecon LAPACK_GLOBAL(zgecon, ZGECON)
#define LAPACK_sgeqrf LAPACK_GLOBAL(sgeqrf, SGEQRF)
#define LAPACK_dgeqrf LAPACK_GLOBAL(dgeqrf, DGEQRF)
#define LAPACK_cgeqrf LAPACK_GLOBAL(cgeqrf, CGEQRF)
#define LAPACK_zgeqrf LAPACK_GLOBAL(zgeqrf, ZGEQRF)
#define LAPACK_sgeqp3 LAPACK_GLOBAL(sgeqp3, SGEQP3)
#define LAPACK_dgeqp3 LAPACK_GLOBAL(dgeqp3, DGEQP3)
#define LAPACK_cgeqp3 LAPACK_GLOBAL(cgeqp3, CGEQP3)
#define LAPACK_zgeqp3 LAPACK_GLOBAL(zgeqp3, ZGEQP3)
#define LAPACK_sorgqr LAPACK_GLOBAL(sorgqr, SORGQR)
#define LAPACK_dorgqr LAPACK_GLOBAL(dorgqr, DORGQR)
#define LAPACK_cungqr LAPACK_GLOBAL(cungqr, CUNGQR)
#define LAPACK_zungqr LAPACK_GLOBAL(zungqr, ZUNGQR)
#define LAPACK_sormqr LAPACK_GLOBAL(sormqr, SORMQR)
#define LAPACK_dormqr LAPACK_GLOBAL(dormqr, DORMQR)
#define LAPACK_cunmqr LAPACK_GLOBAL(cunmqr, CUNMQR)
#define LAPACK_zunmqr LAPACK_GLOBAL(zunmqr, ZUNMQR)
#define LAPACK_sgesvd LAPACK_GLOBAL(sgesvd, SGESVD)
#define LAPACK_dgesvd LAPACK_GLOBAL(dgesvd, DGESVD)
#define LAPACK_cgesvd LAPACK_GLOBAL(cgesvd, CGESVD)
#define LAPACK_zgesvd LAPACK_GLOBAL(zgesvd, ZGESVD)
#define LAPACK_sgesdd LAPACK_GLOBAL(sgesdd, SGESDD)
#define LAPACK_dgesdd LAPACK_GLOBAL(dgesdd, DGESDD)
#define LAPACK_cgesdd LAPACK_GLOBAL(cgesdd, CGESDD)
#define LAPACK_zgesdd LAPACK_GLOBAL(zgesdd, ZGESDD)
#define LAPACK_ssyev LAPACK_GLOBAL(ssyev, SSYEV)
#define LAPACK_dsyev LAPACK_GLOBAL(dsyev, DSYEV)
#define LAPACK_cheev LAPACK_GLOBAL(cheev, CHEEV)
#define LAPACK_zheev LAPACK_GLOBAL(zheev, ZHEEV)
#define LAPACK_sgeev LAPACK_GLOBAL(sgeev, SGEEV)
#define LAPACK_dgeev LAPACK_GLOBAL(dgeev, DGEEV)
#define LAPACK_cgeev LAPACK_GLOBAL(cgeev, CGEEV)
#define LAPACK_zgeev LAPACK_GLOBAL(zgeev, ZGEEV)
#define LAPACK_sgeevx LAPACK_GLOBAL(sgeevx, SGEEVX)
#define LAPACK_dgeevx LAPACK_GLOBAL(dgeevx, DGEEVX)
#define LAPACK_cgeevx LAPACK_GLOBAL(cgeevx, CGEEVX)
#define LAPACK_zgeevx LAPACK_GLOBAL(zgeevx, ZGEEVX)
#define LAPACK_sgees LAPACK_GLOBAL(sgees, SGEES)
#define LAPACK_dgees LAPACK_GLOBAL(dgees, DGEES)
#define LAPACK_cgees LAPACK_GLOBAL(cgees, CGEES)
#define LAPACK_zgees LAPACK_GLOBAL(zgees, ZGEES)
#define LAPACK_sggev LAPACK_GLOBAL(sggev, SGGEV)
#define LAPACK_dggev LAPACK_GLOBAL(dggev, DGGEV)
#define LAPACK_cggev LAPACK_GLOBAL(cggev, CGGEV)
#define LAPACK_zggev LAPACK_GLOBAL(zggev, ZGGEV)
#define LAPACK_ssygv LAPACK_GLOBAL(ssygv, SSYGV)
#define LAPACK_dsygv LAPACK_GLOBAL(dsygv, DSYGV)
#define LAPACK_chegv LAPACK_GLOBAL(chegv, CHEGV)
#define LAPACK_zhegv LAPACK_GLOBAL(zhegv, ZHEGV)
#define LAPACK_sposv LAPACK_GLOBAL(sposv, SPOSV)
#define LAPACK_dposv LAPACK_GLOBAL(dposv, DPOSV)
#define LAPACK_cposv LAPACK_GLOBAL(cposv, CPOSV)
#define LAPACK_zposv LAPACK_GLOBAL(zposv, ZPOSV)
#define LAPACK_spocon LAPACK_GLOBAL(spocon, SPOCON)
#define LAPACK_dpocon LAPACK_GLOBAL(dpocon, DPOCON)
#define LAPACK_cpocon LAPACK_GLOBAL(cpocon, CPOCON)
#define LAPACK_zpocon LAPACK_GLOBAL(zpocon, ZPOCON)
#define LAPACK_sgesvx LAPACK_GLOBAL(sgesvx, SGESVX)
#define LAPACK_dgesvx LAPACK_GLOBAL(dgesvx, DGESVX)
#define LAPACK_cgesvx LAPACK_GLOBAL(cgesvx, CGESVX)
#define LAPACK_zgesvx LAPACK_GLOBAL(zgesvx, ZGESVX)
#define LAPACK_sgelsy LAPACK_GLOBAL(sgelsy, SGELSY)
#define LAPACK_dgelsy LAPACK_GLOBAL(dgelsy, DGELSY)
#define LAPACK_cgelsy LAPACK_GLOBAL(cgelsy, CGELSY)
#define LAPACK_zgelsy LAPACK_GLOBAL(zgelsy, ZGELSY)
#define LAPACK_sgelss LAPACK_GLOBAL(sgelss, SGELSS)
#define LAPACK_dgelss LAPACK_GLOBAL(dgelss, DGELSS)
#define LAPACK_cgelss LAPACK_GLOBAL(cgelss, CGELSS)
#define LAPACK_zgelss LAPACK_GLOBAL(zgelss, ZGELSS)
#define LAPACK_spotrf LAPACK_GLOBAL(spotrf, SPOTRF)
#define LAPACK_dpotrf LAPACK_GLOBAL(dpotrf, DPOTRF)
#define LAPACK_cpotrf LAPACK_GLOBAL(cpotrf, CPOTRF)
#define LAPACK_zpotrf LAPACK_GLOBAL(zpotrf, ZPOTRF)
//=============================================================================
// BLAS level-3 general matrix-matrix product (xgemm), same in MKL and OpenBLAS.
// With EIGEN_USE_BLAS, Eigen already declares these Fortran symbols (with its own
// real-pair signature for the complex kinds). The std::complex prototypes are
// exposed only in translation units that undefine EIGEN_USE_BLAS.
#if !defined(EIGEN_USE_BLAS)
#define BLAS_sgemm LAPACK_GLOBAL(sgemm, SGEMM)
#define BLAS_dgemm LAPACK_GLOBAL(dgemm, DGEMM)
#define BLAS_cgemm LAPACK_GLOBAL(cgemm, CGEMM)
#define BLAS_zgemm LAPACK_GLOBAL(zgemm, ZGEMM)
//=============================================================================
extern "C"
{
    void
    BLAS_sgemm(const char*, const char*, const lapack_int*, const lapack_int*, const lapack_int*,
        const float*, const float*, const lapack_int*, const float*, const lapack_int*,
        const float*, float*, const lapack_int*);
    void
    BLAS_dgemm(const char*, const char*, const lapack_int*, const lapack_int*, const lapack_int*,
        const double*, const double*, const lapack_int*, const double*, const lapack_int*,
        const double*, double*, const lapack_int*);
    void
    BLAS_cgemm(const char*, const char*, const lapack_int*, const lapack_int*, const lapack_int*,
        const std::complex<float>*, const std::complex<float>*, const lapack_int*,
        const std::complex<float>*, const lapack_int*, const std::complex<float>*,
        std::complex<float>*, const lapack_int*);
    void
    BLAS_zgemm(const char*, const char*, const lapack_int*, const lapack_int*, const lapack_int*,
        const std::complex<double>*, const std::complex<double>*, const lapack_int*,
        const std::complex<double>*, const lapack_int*, const std::complex<double>*,
        std::complex<double>*, const lapack_int*);
}
#endif
//=============================================================================
#if !defined(_MKL_LAPACK_H_)
extern "C"
{
    void
    LAPACK_sgebal(
        char*, lapack_int*, float*, lapack_int*, lapack_int*, lapack_int*, float*, lapack_int*);
    void
    LAPACK_dgebal(
        char*, lapack_int*, double*, lapack_int*, lapack_int*, lapack_int*, double*, lapack_int*);
    void
    LAPACK_cgebal(char*, lapack_int*, std::complex<float>*, lapack_int*, lapack_int*, lapack_int*,
        float*, lapack_int*);
    void
    LAPACK_zgebal(char*, lapack_int*, std::complex<double>*, lapack_int*, lapack_int*, lapack_int*,
        double*, lapack_int*);
    float
    LAPACK_slange(char*, lapack_int*, lapack_int*, const float*, lapack_int*, float*);
    double
    LAPACK_dlange(char*, lapack_int*, lapack_int*, const double*, lapack_int*, double*);
    float
    LAPACK_clange(char*, lapack_int*, lapack_int*, const std::complex<float>*, lapack_int*, float*);
    double
    LAPACK_zlange(
        char*, lapack_int*, lapack_int*, const std::complex<double>*, lapack_int*, double*);
    void
    LAPACK_sgetrf(
        const lapack_int*, const lapack_int*, float*, const lapack_int*, lapack_int*, lapack_int*);
    void
    LAPACK_dgetrf(
        const lapack_int*, const lapack_int*, double*, const lapack_int*, lapack_int*, lapack_int*);
    void
    LAPACK_cgetrf(const lapack_int*, const lapack_int*, std::complex<float>*, const lapack_int*,
        lapack_int*, lapack_int*);
    void
    LAPACK_zgetrf(const lapack_int*, const lapack_int*, std::complex<double>*, const lapack_int*,
        lapack_int*, lapack_int*);
    void
    LAPACK_sgetrs(char*, const lapack_int*, const lapack_int*, const float*, const lapack_int*,
        const lapack_int*, float*, const lapack_int*, lapack_int*);
    void
    LAPACK_dgetrs(char*, const lapack_int*, const lapack_int*, const double*, const lapack_int*,
        const lapack_int*, double*, const lapack_int*, lapack_int*);
    void
    LAPACK_cgetrs(char*, const lapack_int*, const lapack_int*, const std::complex<float>*,
        const lapack_int*, const lapack_int*, std::complex<float>*, const lapack_int*, lapack_int*);
    void
    LAPACK_zgetrs(char*, const lapack_int*, const lapack_int*, const std::complex<double>*,
        const lapack_int*, const lapack_int*, std::complex<double>*, const lapack_int*,
        lapack_int*);
    void
    LAPACK_sgetri(lapack_int*, float*, lapack_int*, lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dgetri(
        lapack_int*, double*, lapack_int*, lapack_int*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_cgetri(lapack_int*, std::complex<float>*, lapack_int*, lapack_int*, std::complex<float>*,
        lapack_int*, lapack_int*);
    void
    LAPACK_zgetri(lapack_int*, std::complex<double>*, lapack_int*, lapack_int*,
        std::complex<double>*, lapack_int*, lapack_int*);
    void
    LAPACK_sgecon(char*, lapack_int*, const float*, lapack_int*, float*, float*, float*,
        lapack_int*, lapack_int*);
    void
    LAPACK_dgecon(char*, lapack_int*, const double*, lapack_int*, double*, double*, double*,
        lapack_int*, lapack_int*);
    void
    LAPACK_cgecon(char*, lapack_int*, const std::complex<float>*, lapack_int*, float*, float*,
        std::complex<float>*, float*, lapack_int*);
    void
    LAPACK_zgecon(char*, lapack_int*, const std::complex<double>*, lapack_int*, double*, double*,
        std::complex<double>*, double*, lapack_int*);
    void
    LAPACK_sgeqrf(
        lapack_int*, lapack_int*, float*, lapack_int*, float*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dgeqrf(
        lapack_int*, lapack_int*, double*, lapack_int*, double*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_cgeqrf(lapack_int*, lapack_int*, std::complex<float>*, lapack_int*, std::complex<float>*,
        std::complex<float>*, lapack_int*, lapack_int*);
    void
    LAPACK_zgeqrf(lapack_int*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, std::complex<double>*, lapack_int*, lapack_int*);
    void
    LAPACK_sgeqp3(lapack_int*, lapack_int*, float*, lapack_int*, lapack_int*, float*, float*,
        lapack_int*, lapack_int*);
    void
    LAPACK_dgeqp3(lapack_int*, lapack_int*, double*, lapack_int*, lapack_int*, double*, double*,
        lapack_int*, lapack_int*);
    void
    LAPACK_cgeqp3(lapack_int*, lapack_int*, std::complex<float>*, lapack_int*, lapack_int*,
        std::complex<float>*, std::complex<float>*, lapack_int*, float*, lapack_int*);
    void
    LAPACK_zgeqp3(lapack_int*, lapack_int*, std::complex<double>*, lapack_int*, lapack_int*,
        std::complex<double>*, std::complex<double>*, lapack_int*, double*, lapack_int*);
    void
    LAPACK_sorgqr(lapack_int*, lapack_int*, lapack_int*, float*, lapack_int*, const float*, float*,
        lapack_int*, lapack_int*);
    void
    LAPACK_dorgqr(lapack_int*, lapack_int*, lapack_int*, double*, lapack_int*, const double*,
        double*, lapack_int*, lapack_int*);
    void
    LAPACK_cungqr(lapack_int*, lapack_int*, lapack_int*, std::complex<float>*, lapack_int*,
        const std::complex<float>*, std::complex<float>*, lapack_int*, lapack_int*);
    void
    LAPACK_zungqr(lapack_int*, lapack_int*, lapack_int*, std::complex<double>*, lapack_int*,
        const std::complex<double>*, std::complex<double>*, lapack_int*, lapack_int*);
    void
    LAPACK_sormqr(char*, char*, lapack_int*, lapack_int*, lapack_int*, const float*, lapack_int*,
        const float*, float*, lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dormqr(char*, char*, lapack_int*, lapack_int*, lapack_int*, const double*, lapack_int*,
        const double*, double*, lapack_int*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_cunmqr(char*, char*, lapack_int*, lapack_int*, lapack_int*, const std::complex<float>*,
        lapack_int*, const std::complex<float>*, std::complex<float>*, lapack_int*,
        std::complex<float>*, lapack_int*, lapack_int*);
    void
    LAPACK_zunmqr(char*, char*, lapack_int*, lapack_int*, lapack_int*, const std::complex<double>*,
        lapack_int*, const std::complex<double>*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, lapack_int*);
    void
    LAPACK_sgesvd(char*, char*, lapack_int*, lapack_int*, float*, lapack_int*, float*, float*,
        lapack_int*, float*, lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dgesvd(char*, char*, lapack_int*, lapack_int*, double*, lapack_int*, double*, double*,
        lapack_int*, double*, lapack_int*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_cgesvd(char*, char*, lapack_int*, lapack_int*, std::complex<float>*, lapack_int*, float*,
        std::complex<float>*, lapack_int*, std::complex<float>*, lapack_int*, std::complex<float>*,
        lapack_int*, float*, lapack_int*);
    void
    LAPACK_zgesvd(char*, char*, lapack_int*, lapack_int*, std::complex<double>*, lapack_int*,
        double*, std::complex<double>*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, double*, lapack_int*);
    void
    LAPACK_sgesdd(char*, lapack_int*, lapack_int*, float*, lapack_int*, float*, float*, lapack_int*,
        float*, lapack_int*, float*, lapack_int*, lapack_int*, lapack_int*);
    void
    LAPACK_dgesdd(char*, lapack_int*, lapack_int*, double*, lapack_int*, double*, double*,
        lapack_int*, double*, lapack_int*, double*, lapack_int*, lapack_int*, lapack_int*);
    void
    LAPACK_cgesdd(char*, lapack_int*, lapack_int*, std::complex<float>*, lapack_int*, float*,
        std::complex<float>*, lapack_int*, std::complex<float>*, lapack_int*, std::complex<float>*,
        lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_zgesdd(char*, lapack_int*, lapack_int*, std::complex<double>*, lapack_int*, double*,
        std::complex<double>*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_ssyev(
        char*, char*, lapack_int*, float*, lapack_int*, float*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dsyev(char*, char*, lapack_int*, double*, lapack_int*, double*, double*, lapack_int*,
        lapack_int*);
    void
    LAPACK_cheev(char*, char*, lapack_int*, std::complex<float>*, lapack_int*, float*,
        std::complex<float>*, lapack_int*, float*, lapack_int*);
    void
    LAPACK_zheev(char*, char*, lapack_int*, std::complex<double>*, lapack_int*, double*,
        std::complex<double>*, lapack_int*, double*, lapack_int*);
    void
    LAPACK_sgeev(char*, char*, lapack_int*, float*, lapack_int*, float*, float*, float*,
        lapack_int*, float*, lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dgeev(char*, char*, lapack_int*, double*, lapack_int*, double*, double*, double*,
        lapack_int*, double*, lapack_int*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_cgeev(char*, char*, lapack_int*, std::complex<float>*, lapack_int*, std::complex<float>*,
        std::complex<float>*, lapack_int*, std::complex<float>*, lapack_int*, std::complex<float>*,
        lapack_int*, float*, lapack_int*);
    void
    LAPACK_zgeev(char*, char*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, std::complex<double>*, lapack_int*, std::complex<double>*,
        lapack_int*, std::complex<double>*, lapack_int*, double*, lapack_int*);
    void
    LAPACK_sgeevx(char*, char*, char*, char*, lapack_int*, float*, lapack_int*, float*, float*,
        float*, lapack_int*, float*, lapack_int*, lapack_int*, lapack_int*, float*, float*, float*,
        float*, float*, lapack_int*, lapack_int*, lapack_int*);
    void
    LAPACK_dgeevx(char*, char*, char*, char*, lapack_int*, double*, lapack_int*, double*, double*,
        double*, lapack_int*, double*, lapack_int*, lapack_int*, lapack_int*, double*, double*,
        double*, double*, double*, lapack_int*, lapack_int*, lapack_int*);
    void
    LAPACK_cgeevx(char*, char*, char*, char*, lapack_int*, std::complex<float>*, lapack_int*,
        std::complex<float>*, std::complex<float>*, lapack_int*, std::complex<float>*, lapack_int*,
        lapack_int*, lapack_int*, float*, float*, float*, float*, std::complex<float>*, lapack_int*,
        float*, lapack_int*);
    void
    LAPACK_zgeevx(char*, char*, char*, char*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, std::complex<double>*, lapack_int*, std::complex<double>*,
        lapack_int*, lapack_int*, lapack_int*, double*, double*, double*, double*,
        std::complex<double>*, lapack_int*, double*, lapack_int*);
    typedef lapack_int (*LAPACK_sgees_select)(float*, float*);
    typedef lapack_int (*LAPACK_dgees_select)(double*, double*);
    typedef lapack_int (*LAPACK_cgees_select)(std::complex<float>*);
    typedef lapack_int (*LAPACK_zgees_select)(std::complex<double>*);
    void
    LAPACK_sgees(char*, char*, LAPACK_sgees_select, lapack_int*, float*, lapack_int*, lapack_int*,
        float*, float*, float*, lapack_int*, float*, lapack_int*, lapack_int*, lapack_int*);
    void
    LAPACK_dgees(char*, char*, LAPACK_dgees_select, lapack_int*, double*, lapack_int*, lapack_int*,
        double*, double*, double*, lapack_int*, double*, lapack_int*, lapack_int*, lapack_int*);
    void
    LAPACK_cgees(char*, char*, LAPACK_cgees_select, lapack_int*, std::complex<float>*, lapack_int*,
        lapack_int*, std::complex<float>*, std::complex<float>*, lapack_int*, std::complex<float>*,
        lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_zgees(char*, char*, LAPACK_zgees_select, lapack_int*, std::complex<double>*, lapack_int*,
        lapack_int*, std::complex<double>*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_sggev(char*, char*, lapack_int*, float*, lapack_int*, float*, lapack_int*, float*,
        float*, float*, float*, lapack_int*, float*, lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dggev(char*, char*, lapack_int*, double*, lapack_int*, double*, lapack_int*, double*,
        double*, double*, double*, lapack_int*, double*, lapack_int*, double*, lapack_int*,
        lapack_int*);
    void
    LAPACK_cggev(char*, char*, lapack_int*, std::complex<float>*, lapack_int*, std::complex<float>*,
        lapack_int*, std::complex<float>*, std::complex<float>*, std::complex<float>*, lapack_int*,
        std::complex<float>*, lapack_int*, std::complex<float>*, lapack_int*, float*, lapack_int*);
    void
    LAPACK_zggev(char*, char*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, std::complex<double>*, std::complex<double>*,
        std::complex<double>*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, double*, lapack_int*);
    void
    LAPACK_ssygv(lapack_int*, char*, char*, lapack_int*, float*, lapack_int*, float*, lapack_int*,
        float*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dsygv(lapack_int*, char*, char*, lapack_int*, double*, lapack_int*, double*, lapack_int*,
        double*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_chegv(lapack_int*, char*, char*, lapack_int*, std::complex<float>*, lapack_int*,
        std::complex<float>*, lapack_int*, float*, std::complex<float>*, lapack_int*, float*,
        lapack_int*);
    void
    LAPACK_zhegv(lapack_int*, char*, char*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, double*, std::complex<double>*, lapack_int*, double*,
        lapack_int*);
    void
    LAPACK_sposv(
        char*, lapack_int*, lapack_int*, float*, lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dposv(
        char*, lapack_int*, lapack_int*, double*, lapack_int*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_cposv(char*, lapack_int*, lapack_int*, std::complex<float>*, lapack_int*,
        std::complex<float>*, lapack_int*, lapack_int*);
    void
    LAPACK_zposv(char*, lapack_int*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, lapack_int*);
    void
    LAPACK_spocon(char*, lapack_int*, const float*, lapack_int*, float*, float*, float*,
        lapack_int*, lapack_int*);
    void
    LAPACK_dpocon(char*, lapack_int*, const double*, lapack_int*, double*, double*, double*,
        lapack_int*, lapack_int*);
    void
    LAPACK_cpocon(char*, lapack_int*, const std::complex<float>*, lapack_int*, float*, float*,
        std::complex<float>*, float*, lapack_int*);
    void
    LAPACK_zpocon(char*, lapack_int*, const std::complex<double>*, lapack_int*, double*, double*,
        std::complex<double>*, double*, lapack_int*);
    void
    LAPACK_sgesvx(char*, char*, lapack_int*, lapack_int*, float*, lapack_int*, float*, lapack_int*,
        lapack_int*, char*, float*, float*, float*, lapack_int*, float*, lapack_int*, float*,
        float*, float*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dgesvx(char*, char*, lapack_int*, lapack_int*, double*, lapack_int*, double*,
        lapack_int*, lapack_int*, char*, double*, double*, double*, lapack_int*, double*,
        lapack_int*, double*, double*, double*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_cgesvx(char*, char*, lapack_int*, lapack_int*, std::complex<float>*, lapack_int*,
        std::complex<float>*, lapack_int*, lapack_int*, char*, float*, float*, std::complex<float>*,
        lapack_int*, std::complex<float>*, lapack_int*, float*, float*, float*,
        std::complex<float>*, float*, lapack_int*);
    void
    LAPACK_zgesvx(char*, char*, lapack_int*, lapack_int*, std::complex<double>*, lapack_int*,
        std::complex<double>*, lapack_int*, lapack_int*, char*, double*, double*,
        std::complex<double>*, lapack_int*, std::complex<double>*, lapack_int*, double*, double*,
        double*, std::complex<double>*, double*, lapack_int*);
    void
    LAPACK_sgelsy(const lapack_int*, const lapack_int*, const lapack_int*, float*,
        const lapack_int*, float*, const lapack_int*, lapack_int*, const float*, lapack_int*,
        float*, const lapack_int*, lapack_int*);
    void
    LAPACK_dgelsy(const lapack_int*, const lapack_int*, const lapack_int*, double*,
        const lapack_int*, double*, const lapack_int*, lapack_int*, const double*, lapack_int*,
        double*, const lapack_int*, lapack_int*);
    void
    LAPACK_cgelsy(const lapack_int*, const lapack_int*, const lapack_int*, std::complex<float>*,
        const lapack_int*, std::complex<float>*, const lapack_int*, lapack_int*, const float*,
        lapack_int*, std::complex<float>*, const lapack_int*, float*, lapack_int*);
    void
    LAPACK_zgelsy(const lapack_int*, const lapack_int*, const lapack_int*, std::complex<double>*,
        const lapack_int*, std::complex<double>*, const lapack_int*, lapack_int*, const double*,
        lapack_int*, std::complex<double>*, const lapack_int*, double*, lapack_int*);
    void
    LAPACK_sgelss(const lapack_int*, const lapack_int*, const lapack_int*, float*,
        const lapack_int*, float*, const lapack_int*, float*, const float*, lapack_int*, float*,
        const lapack_int*, lapack_int*);
    void
    LAPACK_dgelss(const lapack_int*, const lapack_int*, const lapack_int*, double*,
        const lapack_int*, double*, const lapack_int*, double*, const double*, lapack_int*, double*,
        const lapack_int*, lapack_int*);
    void
    LAPACK_cgelss(const lapack_int*, const lapack_int*, const lapack_int*, std::complex<float>*,
        const lapack_int*, std::complex<float>*, const lapack_int*, float*, const float*,
        lapack_int*, std::complex<float>*, const lapack_int*, float*, lapack_int*);
    void
    LAPACK_zgelss(const lapack_int*, const lapack_int*, const lapack_int*, std::complex<double>*,
        const lapack_int*, std::complex<double>*, const lapack_int*, double*, const double*,
        lapack_int*, std::complex<double>*, const lapack_int*, double*, lapack_int*);
    void
    LAPACK_spotrf(char*, lapack_int*, float*, lapack_int*, lapack_int*);
    void
    LAPACK_dpotrf(char*, lapack_int*, double*, lapack_int*, lapack_int*);
    void
    LAPACK_cpotrf(char*, lapack_int*, std::complex<float>*, lapack_int*, lapack_int*);
    void
    LAPACK_zpotrf(char*, lapack_int*, std::complex<double>*, lapack_int*, lapack_int*);
}
#endif
//=============================================================================
