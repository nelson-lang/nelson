//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsSlicot_exports.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
//=============================================================================
#ifndef C2F
#define C2F(name) name##_
#endif
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(mb05od)(const char* BALANC, int* N, int* NDIAG, double* DELTA,
        double* A, int* LDA, int* MDIG, int* IDIG, int* IWORK, double* DWORK, int* LDWORK,
        int* IWARN, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(mb04md)(
        int* N, double* MAXRED, double* A, int* LDA, double* SCALE, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(mb04gd)(
        int* M, int* N, double* A, int* LDA, int* JPVT, double* TAU, double* DWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(mb03rd)(const char* JOBX, const char* SORT, int* N, double* PMAX,
        double* A, int* LDA, double* X, int* LDX, int* NBLCKS, double* BLSIZE, double* WR,
        double* WI, double* TOL, double* DWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(mb03pd)(const char* JOBRQ, int* M, int* N, double* A, int* LDA,
        int* JPVT, double* RCOND, double* SVLMAX, double* TAU, int* RANK, double* SVAL,
        double* DWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(mc01td)(const char* DICO, int* DP, const double* P, int* STABLE,
        int* NZ, double* DWORK, int* IWARN, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(mb02md)(const char* JOB, int* M, int* N, int* L, int* RANK, double* C,
        int* LDC, double* S, double* X, int* LDX, double* TOL, int* IWORK, double* DWORK,
        int* LDWORK, int* IWARN, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(ag08bd)(const char* EQUIL, int* L, int* N, int* M, int* P, double* A,
        int* LDA, double* E, int* LDE, const double* B, int* LDB, const double* C, int* LDC,
        const double* D, int* LDD, int* NFZ, int* NRANK, int* NIZ, int* DINFZ, int* NKROR,
        int* NINFE, int* NKROL, int* INFZ, int* KRONR, int* INFE, int* KRONL, double* TOL,
        int* IWORK, double* DWORK, int* LDWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(ab08nd)(const char* EQUIL, int* N, int* M, int* P, const double* A,
        int* LDA, const double* B, int* LDB, const double* C, int* LDC, const double* D, int* LDD,
        int* NU, int* RANK, int* DINFZ, int* NKROR, int* NKROL, int* INFZ, int* KRONR, int* KRONL,
        double* AF, int* LDAF, double* BF, int* LDBF, double* TOL, int* IWORK, double* DWORK,
        int* LDWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(ab07nd)(int* N, int* M, double* A, int* LDA, double* B, int* LDB,
        double* C, int* LDC, double* D, int* LDD, double* RCOND, int* IWORK, double* DWORK,
        int* LDWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(ab04md)(const char* TYPE, int* N, int* M, int* P, double* ALPHA,
        double* BETA, double* A, int* LDA, double* B, int* LDB, double* C, int* LDC, double* D,
        int* LDD, int* IWORK, double* DWORK, int* LDWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(ab01od)(const char* STAGES, const char* JOBU, const char* JOBV, int* N,
        int* M, double* A, int* LDA, double* B, int* LDB, double* U, int* LDU, double* V, int* LDV,
        int* NCONT, int* INDCON, int* KSTAIR, double* TOL, int* IWORK, double* DWORK, int* LDWORK,
        int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(sb01bd)(const char* DICO, int* N, int* M, int* NP, double* ALPHA,
        double* A, int* LDA, const double* B, int* LDB, double* WR, double* WI, int* NFP, int* NAP,
        int* NUP, double* F, int* LDF, double* Z, int* LDZ, double* TOL, double* DWORK, int* LDWORK,
        int* IWARN, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(tg01ad)(const char* JOB, int* L, int* N, int* M, int* P,
        double* THRESH, double* A, int* LDA, double* E, int* LDE, double* B, int* LDB, double* C,
        int* LDC, double* LSCALE, double* RSCALE, double* DWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(tb01id)(const char* JOB, int* N, int* M, int* P, double* MAXRED,
        double* A, int* LDA, double* B, int* LDB, double* C, int* LDC, double* SCALE, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(sg02ad)(const char* DICO, const char* JOBB, const char* FACT,
        const char* UPLO, const char* JOBL, const char* SCAL, const char* SORT, const char* ACC,
        int* N, int* M, int* P, const double* A, int* LDA, const double* E, int* LDE,
        const double* B, int* LDB, const double* Q, int* LDQ, const double* R, int* LDR,
        const double* L, int* LDL, double* RCONDU, double* X, int* LDX, double* ALFAR,
        double* ALFAI, double* BETA, double* S, int* LDS, double* T, int* LDT, double* U, int* LDU,
        double* TOL, int* IWORK, double* DWORK, int* LDWORK, int* BWORK, int* IWARN, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(sb10jd)(int* N, int* M, int* NP, double* A, int* LDA, double* B,
        int* LDB, double* C, int* LDC, double* D, int* LDD, double* E, int* LDE, int* NSYS,
        double* DWORK, int* LDWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(sb04qd)(int* N, int* M, double* A, int* LDA, double* B, int* LDB,
        double* C, int* LDC, double* Z, int* LDZ, int* IWORK, double* DWORK, int* LDWORK,
        int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(sb04md)(int* N, int* M, double* A, int* LDA, double* B, int* LDB,
        double* C, int* LDC, double* Z, int* LDZ, int* IWORK, double* DWORK, int* LDWORK,
        int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(sb03od)(const char* DICO, const char* FACT, const char* TRANS, int* N,
        int* M, const double* A, int* LDA, double* Q, int* LDQ, double* B, int* LDB, double* SCALE,
        double* WR, double* WI, double* DWORK, int* LDWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(sb03md)(const char* DICO, const char* JOB, const char* FACT,
        const char* TRANA, int* N, const double* A, int* LDA, double* U, int* LDU, double* C,
        int* LDC, double* SCALE, double* SEP, double* FERR, double* WR, double* WI, int* IWORK,
        double* DWORK, int* LDWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(mb03od)(const char* JOBQR, int* M, int* N, double* A, int* LDA,
        int* JPVT, double* RCOND, double* SVLMAX, double* TAU, int* RANK, double* SVAL,
        double* DWORK, int* LDWORK, int* INFO);
    //=============================================================================
    NLSSLICOT_IMPEXP int C2F(sb02od)(const char* DICO, const char* JOBB, const char* FACT,
        const char* UPLO, const char* JOBL, const char* SORT, int* N, int* M, int* P,
        const double* A, int* LDA, const double* B, int* LDB, const double* Q, int* LDQ,
        const double* R, int* LDR, const double* L, int* LDL, double* RCOND, double* X, int* LDX,
        double* ALFAR, double* ALFAI, double* BETA, double* S, int* LDS, double* T, int* LDT,
        double* U, int* LDU, double* TOL, int* IWORK, double* DWORK, int* LDWORK, int* BWORK,
        int* INFO);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
