//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "SlicotWrapper.hpp"
#include "dynamic_library.hpp"
#include "characters_encoding.hpp"
//=============================================================================
static bool slicotLoaded = false;
static Nelson::library_handle slicot_handle = nullptr;
//=============================================================================
using PROC_mb05od = int (*)(const char* BALANC, int* N, int* NDIAG, double* DELTA, double* A,
    int* LDA, int* MDIG, int* IDIG, int* IWORK, double* DWORK, int* LDWORK, int* IWARN, int* INFO);
using PROC_mb04md = int (*)(int* N, double* MAXRED, double* A, int* LDA, double* SCALE, int* INFO);
using PROC_mb04gd = int (*)(
    int* M, int* N, double* A, int* LDA, int* JPVT, double* TAU, double* DWORK, int* INFO);
using PROC_mb03rd = int (*)(const char* JOBX, const char* SORT, int* N, double* PMAX, double* A,
    int* LDA, double* X, int* LDX, int* NBLCKS, double* BLSIZE, double* WR, double* WI, double* TOL,
    double* DWORK, int* INFO);
using PROC_mb03pd = int (*)(const char* JOBRQ, int* M, int* N, double* A, int* LDA, int* JPVT,
    double* RCOND, double* SVLMAX, double* TAU, int* RANK, double* SVAL, double* DWORK, int* INFO);
using PROC_mc01td = int (*)(const char* DICO, int* DP, const double* P, int* STABLE, int* NZ,
    double* DWORK, int* IWARN, int* INFO);
using PROC_mb02md = int (*)(const char* JOB, int* M, int* N, int* L, int* RANK, double* C, int* LDC,
    double* S, double* X, int* LDX, double* TOL, int* IWORK, double* DWORK, int* LDWORK, int* IWARN,
    int* INFO);
using PROC_ag08bd = int (*)(const char* EQUIL, int* L, int* N, int* M, int* P, double* A, int* LDA,
    double* E, int* LDE, const double* B, int* LDB, const double* C, int* LDC, const double* D,
    int* LDD, int* NFZ, int* NRANK, int* NIZ, int* DINFZ, int* NKROR, int* NINFE, int* NKROL,
    int* INFZ, int* KRONR, int* INFE, int* KRONL, double* TOL, int* IWORK, double* DWORK,
    int* LDWORK, int* INFO);
using PROC_ab08nd = int (*)(const char* EQUIL, int* N, int* M, int* P, const double* A, int* LDA,
    const double* B, int* LDB, const double* C, int* LDC, const double* D, int* LDD, int* NU,
    int* RANK, int* DINFZ, int* NKROR, int* NKROL, int* INFZ, int* KRONR, int* KRONL, double* AF,
    int* LDAF, double* BF, int* LDBF, double* TOL, int* IWORK, double* DWORK, int* LDWORK,
    int* INFO);
using PROC_ab07nd
    = int (*)(int* N, int* M, double* A, int* LDA, double* B, int* LDB, double* C, int* LDC,
        double* D, int* LDD, double* RCOND, int* IWORK, double* DWORK, int* LDWORK, int* INFO);
using PROC_ab04md = int (*)(const char* TYPE, int* N, int* M, int* P, double* ALPHA, double* BETA,
    double* A, int* LDA, double* B, int* LDB, double* C, int* LDC, double* D, int* LDD, int* IWORK,
    double* DWORK, int* LDWORK, int* INFO);
using PROC_ab01od = int (*)(const char* STAGES, const char* JOBU, const char* JOBV, int* N, int* M,
    double* A, int* LDA, double* B, int* LDB, double* U, int* LDU, double* V, int* LDV, int* NCONT,
    int* INDCON, int* KSTAIR, double* TOL, int* IWORK, double* DWORK, int* LDWORK, int* INFO);
using PROC_sb01bd = int (*)(const char* DICO, int* N, int* M, int* NP, double* ALPHA, double* A,
    int* LDA, const double* B, int* LDB, double* WR, double* WI, int* NFP, int* NAP, int* NUP,
    double* F, int* LDF, double* Z, int* LDZ, double* TOL, double* DWORK, int* LDWORK, int* IWARN,
    int* INFO);
using PROC_tg01ad = int (*)(const char* JOB, int* L, int* N, int* M, int* P, double* THRESH,
    double* A, int* LDA, double* E, int* LDE, double* B, int* LDB, double* C, int* LDC,
    double* LSCALE, double* RSCALE, double* DWORK, int* INFO);
using PROC_tb01id = int (*)(const char* JOB, int* N, int* M, int* P, double* MAXRED, double* A,
    int* LDA, double* B, int* LDB, double* C, int* LDC, double* SCALE, int* INFO);
using PROC_sg02ad = int (*)(const char* DICO, const char* JOBB, const char* FACT, const char* UPLO,
    const char* JOBL, const char* SCAL, const char* SORT, const char* ACC, int* N, int* M, int* P,
    const double* A, int* LDA, const double* E, int* LDE, const double* B, int* LDB,
    const double* Q, int* LDQ, const double* R, int* LDR, const double* L, int* LDL, double* RCONDU,
    double* X, int* LDX, double* ALFAR, double* ALFAI, double* BETA, double* S, int* LDS, double* T,
    int* LDT, double* U, int* LDU, double* TOL, int* IWORK, double* DWORK, int* LDWORK, int* BWORK,
    int* IWARN, int* INFO);
using PROC_sb10jd = int (*)(int* N, int* M, int* NP, double* A, int* LDA, double* B, int* LDB,
    double* C, int* LDC, double* D, int* LDD, double* E, int* LDE, int* NSYS, double* DWORK,
    int* LDWORK, int* INFO);
using PROC_sb04qd = int (*)(int* N, int* M, double* A, int* LDA, double* B, int* LDB, double* C,
    int* LDC, double* Z, int* LDZ, int* IWORK, double* DWORK, int* LDWORK, int* INFO);
using PROC_sb04md = int (*)(int* N, int* M, double* A, int* LDA, double* B, int* LDB, double* C,
    int* LDC, double* Z, int* LDZ, int* IWORK, double* DWORK, int* LDWORK, int* INFO);
using PROC_sb03od = int (*)(const char* DICO, const char* FACT, const char* TRANS, int* N, int* M,
    const double* A, int* LDA, double* Q, int* LDQ, double* B, int* LDB, double* SCALE, double* WR,
    double* WI, double* DWORK, int* LDWORK, int* INFO);
using PROC_sb03md = int (*)(const char* DICO, const char* JOB, const char* FACT, const char* TRANA,
    int* N, const double* A, int* LDA, double* U, int* LDU, double* C, int* LDC, double* SCALE,
    double* SEP, double* FERR, double* WR, double* WI, int* IWORK, double* DWORK, int* LDWORK,
    int* INFO);
using PROC_mb03od = int (*)(const char* JOBQR, int* M, int* N, double* A, int* LDA, int* JPVT,
    double* RCOND, double* SVLMAX, double* TAU, int* RANK, double* SVAL, double* DWORK, int* LDWORK,
    int* INFO);
using PROC_sb02od = int (*)(const char* DICO, const char* JOBB, const char* FACT, const char* UPLO,
    const char* JOBL, const char* SORT, int* N, int* M, int* P, const double* A, int* LDA,
    const double* B, int* LDB, const double* Q, int* LDQ, const double* R, int* LDR,
    const double* L, int* LDL, double* RCOND, double* X, int* LDX, double* ALFAR, double* ALFAI,
    double* BETA, double* S, int* LDS, double* T, int* LDT, double* U, int* LDU, double* TOL,
    int* IWORK, double* DWORK, int* LDWORK, int* BWORK, int* INFO);
//=============================================================================
PROC_mb05od mb05odPtr = nullptr;
PROC_mb04md mb04mdPtr = nullptr;
PROC_mb04gd mb04gdPtr = nullptr;
PROC_mb03rd mb03rdPtr = nullptr;
PROC_mb03pd mb03pdPtr = nullptr;
PROC_mc01td mc01tdPtr = nullptr;
PROC_mb02md mb02mdPtr = nullptr;
PROC_ag08bd ag08bdPtr = nullptr;
PROC_ab08nd ab08ndPtr = nullptr;
PROC_ab07nd ab07ndPtr = nullptr;
PROC_ab04md ab04mdPtr = nullptr;
PROC_ab01od ab01odPtr = nullptr;
PROC_sb01bd sb01bdPtr = nullptr;
PROC_tg01ad tg01adPtr = nullptr;
PROC_tb01id tb01idPtr = nullptr;
PROC_sg02ad sg02adPtr = nullptr;
PROC_sb10jd sb10jdPtr = nullptr;
PROC_sb04qd sb04qdPtr = nullptr;
PROC_sb04md sb04mdPtr = nullptr;
PROC_sb03od sb03odPtr = nullptr;
PROC_sb03md sb03mdPtr = nullptr;
PROC_mb03od mb03odPtr = nullptr;
PROC_sb02od sb02odPtr = nullptr;
//=============================================================================
int C2F(mb05od)(const char* BALANC, int* N, int* NDIAG, double* DELTA, double* A, int* LDA,
    int* MDIG, int* IDIG, int* IWORK, double* DWORK, int* LDWORK, int* IWARN, int* INFO)
{
    return mb05odPtr(
        BALANC, N, NDIAG, DELTA, A, LDA, MDIG, IDIG, IWORK, DWORK, LDWORK, IWARN, INFO);
}
//=============================================================================
int C2F(mb04md)(int* N, double* MAXRED, double* A, int* LDA, double* SCALE, int* INFO)
{
    return mb04mdPtr(N, MAXRED, A, LDA, SCALE, INFO);
}
//=============================================================================
int C2F(mb04gd)(
    int* M, int* N, double* A, int* LDA, int* JPVT, double* TAU, double* DWORK, int* INFO)
{
    return mb04gdPtr(M, N, A, LDA, JPVT, TAU, DWORK, INFO);
}
//=============================================================================
int C2F(mb03rd)(const char* JOBX, const char* SORT, int* N, double* PMAX, double* A, int* LDA,
    double* X, int* LDX, int* NBLCKS, double* BLSIZE, double* WR, double* WI, double* TOL,
    double* DWORK, int* INFO)
{
    return mb03rdPtr(JOBX, SORT, N, PMAX, A, LDA, X, LDX, NBLCKS, BLSIZE, WR, WI, TOL, DWORK, INFO);
}
//=============================================================================
int C2F(mb03pd)(const char* JOBRQ, int* M, int* N, double* A, int* LDA, int* JPVT, double* RCOND,
    double* SVLMAX, double* TAU, int* RANK, double* SVAL, double* DWORK, int* INFO)
{
    return mb03pdPtr(JOBRQ, M, N, A, LDA, JPVT, RCOND, SVLMAX, TAU, RANK, SVAL, DWORK, INFO);
}
//=============================================================================
int C2F(mc01td)(const char* DICO, int* DP, const double* P, int* STABLE, int* NZ, double* DWORK,
    int* IWARN, int* INFO)
{
    return mc01tdPtr(DICO, DP, P, STABLE, NZ, DWORK, IWARN, INFO);
}
//=============================================================================
int C2F(mb02md)(const char* JOB, int* M, int* N, int* L, int* RANK, double* C, int* LDC, double* S,
    double* X, int* LDX, double* TOL, int* IWORK, double* DWORK, int* LDWORK, int* IWARN, int* INFO)
{
    return mb02mdPtr(JOB, M, N, L, RANK, C, LDC, S, X, LDX, TOL, IWORK, DWORK, LDWORK, IWARN, INFO);
}
//=============================================================================
int C2F(ag08bd)(const char* EQUIL, int* L, int* N, int* M, int* P, double* A, int* LDA, double* E,
    int* LDE, const double* B, int* LDB, const double* C, int* LDC, const double* D, int* LDD,
    int* NFZ, int* NRANK, int* NIZ, int* DINFZ, int* NKROR, int* NINFE, int* NKROL, int* INFZ,
    int* KRONR, int* INFE, int* KRONL, double* TOL, int* IWORK, double* DWORK, int* LDWORK,
    int* INFO)
{
    return ag08bdPtr(EQUIL, L, N, M, P, A, LDA, E, LDE, B, LDB, C, LDC, D, LDD, NFZ, NRANK, NIZ,
        DINFZ, NKROR, NINFE, NKROL, INFZ, KRONR, INFE, KRONL, TOL, IWORK, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(ab08nd)(const char* EQUIL, int* N, int* M, int* P, const double* A, int* LDA,
    const double* B, int* LDB, const double* C, int* LDC, const double* D, int* LDD, int* NU,
    int* RANK, int* DINFZ, int* NKROR, int* NKROL, int* INFZ, int* KRONR, int* KRONL, double* AF,
    int* LDAF, double* BF, int* LDBF, double* TOL, int* IWORK, double* DWORK, int* LDWORK,
    int* INFO)
{
    return ab08ndPtr(EQUIL, N, M, P, A, LDA, B, LDB, C, LDC, D, LDD, NU, RANK, DINFZ, NKROR, NKROL,
        INFZ, KRONR, KRONL, AF, LDAF, BF, LDBF, TOL, IWORK, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(ab07nd)(int* N, int* M, double* A, int* LDA, double* B, int* LDB, double* C, int* LDC,
    double* D, int* LDD, double* RCOND, int* IWORK, double* DWORK, int* LDWORK, int* INFO)
{
    return ab07ndPtr(N, M, A, LDA, B, LDB, C, LDC, D, LDD, RCOND, IWORK, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(ab04md)(const char* TYPE, int* N, int* M, int* P, double* ALPHA, double* BETA, double* A,
    int* LDA, double* B, int* LDB, double* C, int* LDC, double* D, int* LDD, int* IWORK,
    double* DWORK, int* LDWORK, int* INFO)
{
    return ab04mdPtr(
        TYPE, N, M, P, ALPHA, BETA, A, LDA, B, LDB, C, LDC, D, LDD, IWORK, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(ab01od)(const char* STAGES, const char* JOBU, const char* JOBV, int* N, int* M, double* A,
    int* LDA, double* B, int* LDB, double* U, int* LDU, double* V, int* LDV, int* NCONT,
    int* INDCON, int* KSTAIR, double* TOL, int* IWORK, double* DWORK, int* LDWORK, int* INFO)
{
    return ab01odPtr(STAGES, JOBU, JOBV, N, M, A, LDA, B, LDB, U, LDU, V, LDV, NCONT, INDCON,
        KSTAIR, TOL, IWORK, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(sb01bd)(const char* DICO, int* N, int* M, int* NP, double* ALPHA, double* A, int* LDA,
    const double* B, int* LDB, double* WR, double* WI, int* NFP, int* NAP, int* NUP, double* F,
    int* LDF, double* Z, int* LDZ, double* TOL, double* DWORK, int* LDWORK, int* IWARN, int* INFO)
{
    return sb01bdPtr(DICO, N, M, NP, ALPHA, A, LDA, B, LDB, WR, WI, NFP, NAP, NUP, F, LDF, Z, LDZ,
        TOL, DWORK, LDWORK, IWARN, INFO);
}
//=============================================================================
int C2F(tg01ad)(const char* JOB, int* L, int* N, int* M, int* P, double* THRESH, double* A,
    int* LDA, double* E, int* LDE, double* B, int* LDB, double* C, int* LDC, double* LSCALE,
    double* RSCALE, double* DWORK, int* INFO)
{
    return tg01adPtr(
        JOB, L, N, M, P, THRESH, A, LDA, E, LDE, B, LDB, C, LDC, LSCALE, RSCALE, DWORK, INFO);
}
//=============================================================================
int C2F(tb01id)(const char* JOB, int* N, int* M, int* P, double* MAXRED, double* A, int* LDA,
    double* B, int* LDB, double* C, int* LDC, double* SCALE, int* INFO)
{
    return tb01idPtr(JOB, N, M, P, MAXRED, A, LDA, B, LDB, C, LDC, SCALE, INFO);
}
//=============================================================================
int C2F(sg02ad)(const char* DICO, const char* JOBB, const char* FACT, const char* UPLO,
    const char* JOBL, const char* SCAL, const char* SORT, const char* ACC, int* N, int* M, int* P,
    const double* A, int* LDA, const double* E, int* LDE, const double* B, int* LDB,
    const double* Q, int* LDQ, const double* R, int* LDR, const double* L, int* LDL, double* RCONDU,
    double* X, int* LDX, double* ALFAR, double* ALFAI, double* BETA, double* S, int* LDS, double* T,
    int* LDT, double* U, int* LDU, double* TOL, int* IWORK, double* DWORK, int* LDWORK, int* BWORK,
    int* IWARN, int* INFO)
{
    return sg02adPtr(DICO, JOBB, FACT, UPLO, JOBL, SCAL, SORT, ACC, N, M, P, A, LDA, E, LDE, B, LDB,
        Q, LDQ, R, LDR, L, LDL, RCONDU, X, LDX, ALFAR, ALFAI, BETA, S, LDS, T, LDT, U, LDU, TOL,
        IWORK, DWORK, LDWORK, BWORK, IWARN, INFO);
}
//=============================================================================
int C2F(sb10jd)(int* N, int* M, int* NP, double* A, int* LDA, double* B, int* LDB, double* C,
    int* LDC, double* D, int* LDD, double* E, int* LDE, int* NSYS, double* DWORK, int* LDWORK,
    int* INFO)
{
    return sb10jdPtr(N, M, NP, A, LDA, B, LDB, C, LDC, D, LDD, E, LDE, NSYS, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(sb04qd)(int* N, int* M, double* A, int* LDA, double* B, int* LDB, double* C, int* LDC,
    double* Z, int* LDZ, int* IWORK, double* DWORK, int* LDWORK, int* INFO)
{
    return sb04qdPtr(N, M, A, LDA, B, LDB, C, LDC, Z, LDZ, IWORK, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(sb04md)(int* N, int* M, double* A, int* LDA, double* B, int* LDB, double* C, int* LDC,
    double* Z, int* LDZ, int* IWORK, double* DWORK, int* LDWORK, int* INFO)
{
    return sb04mdPtr(N, M, A, LDA, B, LDB, C, LDC, Z, LDZ, IWORK, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(sb03od)(const char* DICO, const char* FACT, const char* TRANS, int* N, int* M,
    const double* A, int* LDA, double* Q, int* LDQ, double* B, int* LDB, double* SCALE, double* WR,
    double* WI, double* DWORK, int* LDWORK, int* INFO)
{
    return sb03odPtr(
        DICO, FACT, TRANS, N, M, A, LDA, Q, LDQ, B, LDB, SCALE, WR, WI, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(sb03md)(const char* DICO, const char* JOB, const char* FACT, const char* TRANA, int* N,
    const double* A, int* LDA, double* U, int* LDU, double* C, int* LDC, double* SCALE, double* SEP,
    double* FERR, double* WR, double* WI, int* IWORK, double* DWORK, int* LDWORK, int* INFO)
{
    return sb03mdPtr(DICO, JOB, FACT, TRANA, N, A, LDA, U, LDU, C, LDC, SCALE, SEP, FERR, WR, WI,
        IWORK, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(mb03od)(const char* JOBQR, int* M, int* N, double* A, int* LDA, int* JPVT, double* RCOND,
    double* SVLMAX, double* TAU, int* RANK, double* SVAL, double* DWORK, int* LDWORK, int* INFO)
{
    return mb03odPtr(
        JOBQR, M, N, A, LDA, JPVT, RCOND, SVLMAX, TAU, RANK, SVAL, DWORK, LDWORK, INFO);
}
//=============================================================================
int C2F(sb02od)(const char* DICO, const char* JOBB, const char* FACT, const char* UPLO,
    const char* JOBL, const char* SORT, int* N, int* M, int* P, const double* A, int* LDA,
    const double* B, int* LDB, const double* Q, int* LDQ, const double* R, int* LDR,
    const double* L, int* LDL, double* RCOND, double* X, int* LDX, double* ALFAR, double* ALFAI,
    double* BETA, double* S, int* LDS, double* T, int* LDT, double* U, int* LDU, double* TOL,
    int* IWORK, double* DWORK, int* LDWORK, int* BWORK, int* INFO)
{
    return sb02odPtr(DICO, JOBB, FACT, UPLO, JOBL, SORT, N, M, P, A, LDA, B, LDB, Q, LDQ, R, LDR, L,
        LDL, RCOND, X, LDX, ALFAR, ALFAI, BETA, S, LDS, T, LDT, U, LDU, TOL, IWORK, DWORK, LDWORK,
        BWORK, INFO);
}
//=============================================================================
static bool
loadSlicotSymbols()
{
    mb05odPtr = reinterpret_cast<PROC_mb05od>(Nelson::get_function(slicot_handle, "mb05od_"));
    if (!mb05odPtr) {
        return false;
    }

    mb04mdPtr = reinterpret_cast<PROC_mb04md>(Nelson::get_function(slicot_handle, "mb04md_"));
    if (!mb04mdPtr) {
        return false;
    }

    mb04gdPtr = reinterpret_cast<PROC_mb04gd>(Nelson::get_function(slicot_handle, "mb04gd_"));
    if (!mb04gdPtr) {
        return false;
    }

    mb03rdPtr = reinterpret_cast<PROC_mb03rd>(Nelson::get_function(slicot_handle, "mb03rd_"));
    if (!mb03rdPtr) {
        return false;
    }

    mb03pdPtr = reinterpret_cast<PROC_mb03pd>(Nelson::get_function(slicot_handle, "mb03pd_"));
    if (!mb03pdPtr) {
        return false;
    }

    mc01tdPtr = reinterpret_cast<PROC_mc01td>(Nelson::get_function(slicot_handle, "mc01td_"));
    if (!mc01tdPtr) {
        return false;
    }

    mb02mdPtr = reinterpret_cast<PROC_mb02md>(Nelson::get_function(slicot_handle, "mb02md_"));
    if (!mb02mdPtr) {
        return false;
    }

    ag08bdPtr = reinterpret_cast<PROC_ag08bd>(Nelson::get_function(slicot_handle, "ag08bd_"));
    if (!ag08bdPtr) {
        return false;
    }

    ab08ndPtr = reinterpret_cast<PROC_ab08nd>(Nelson::get_function(slicot_handle, "ab08nd_"));
    if (!ab08ndPtr) {
        return false;
    }

    ab07ndPtr = reinterpret_cast<PROC_ab07nd>(Nelson::get_function(slicot_handle, "ab07nd_"));
    if (!ab07ndPtr) {
        return false;
    }

    ab04mdPtr = reinterpret_cast<PROC_ab04md>(Nelson::get_function(slicot_handle, "ab04md_"));
    if (!ab04mdPtr) {
        return false;
    }

    ab01odPtr = reinterpret_cast<PROC_ab01od>(Nelson::get_function(slicot_handle, "ab01od_"));
    if (!ab01odPtr) {
        return false;
    }

    sb01bdPtr = reinterpret_cast<PROC_sb01bd>(Nelson::get_function(slicot_handle, "sb01bd_"));
    if (!sb01bdPtr) {
        return false;
    }

    tg01adPtr = reinterpret_cast<PROC_tg01ad>(Nelson::get_function(slicot_handle, "tg01ad_"));
    if (!tg01adPtr) {
        return false;
    }

    tb01idPtr = reinterpret_cast<PROC_tb01id>(Nelson::get_function(slicot_handle, "tb01id_"));
    if (!tb01idPtr) {
        return false;
    }

    sg02adPtr = reinterpret_cast<PROC_sg02ad>(Nelson::get_function(slicot_handle, "sg02ad_"));
    if (!sg02adPtr) {
        return false;
    }

    sb10jdPtr = reinterpret_cast<PROC_sb10jd>(Nelson::get_function(slicot_handle, "sb10jd_"));
    if (!sb10jdPtr) {
        return false;
    }

    sb04qdPtr = reinterpret_cast<PROC_sb04qd>(Nelson::get_function(slicot_handle, "sb04qd_"));
    if (!sb04qdPtr) {
        return false;
    }

    sb04mdPtr = reinterpret_cast<PROC_sb04md>(Nelson::get_function(slicot_handle, "sb04md_"));
    if (!sb04mdPtr) {
        return false;
    }

    sb03odPtr = reinterpret_cast<PROC_sb03od>(Nelson::get_function(slicot_handle, "sb03od_"));
    if (!sb03odPtr) {
        return false;
    }

    sb03mdPtr = reinterpret_cast<PROC_sb03md>(Nelson::get_function(slicot_handle, "sb03md_"));
    if (!sb03mdPtr) {
        return false;
    }

    mb03odPtr = reinterpret_cast<PROC_mb03od>(Nelson::get_function(slicot_handle, "mb03od_"));
    if (!mb03odPtr) {
        return false;
    }

    sb02odPtr = reinterpret_cast<PROC_sb02od>(Nelson::get_function(slicot_handle, "sb02od_"));
    if (!sb02odPtr) {
        return false;
    }

    return true;
}
//=============================================================================
namespace Nelson {
bool
loadSlicotLibrary()
{
#ifdef _MSC_VER
    std::wstring slicotLibraryName = L"libslicot.dll";
    return loadSlicotLibrary(slicotLibraryName);
#else
    std::string slicotLibraryName = "libslicot" + get_dynamic_library_extension();
    bool res = loadSlicotLibrary(utf8_to_wstring(slicotLibraryName));
    if (!res) {
        std::string slicotLibraryName = "libslicot" + get_dynamic_library_extension() + ".3";
        res = loadSlicotLibrary(utf8_to_wstring(slicotLibraryName));
    }
    return res;
#endif
}
//=============================================================================
bool
loadSlicotLibrary(const std::wstring& slicotLibraryName)
{
#ifdef _MSC_VER
    slicot_handle = load_dynamic_libraryW(slicotLibraryName);
#else
    slicot_handle = load_dynamic_library(wstring_to_utf8(slicotLibraryName));
#endif
    if (slicot_handle != nullptr) {
        slicotLoaded = loadSlicotSymbols();
    } else {
        slicotLoaded = false;
    }
    return slicotLoaded;
}
//=============================================================================
bool
freeSlicotLibrary()
{
    if (slicotLoaded) {
        mb05odPtr = nullptr;
        mb04mdPtr = nullptr;
        mb04gdPtr = nullptr;
        mb03rdPtr = nullptr;
        mb03pdPtr = nullptr;
        mc01tdPtr = nullptr;
        mb02mdPtr = nullptr;
        ag08bdPtr = nullptr;
        ab08ndPtr = nullptr;
        ab07ndPtr = nullptr;
        ab04mdPtr = nullptr;
        ab01odPtr = nullptr;
        sb01bdPtr = nullptr;
        tg01adPtr = nullptr;
        tb01idPtr = nullptr;
        sg02adPtr = nullptr;
        sb10jdPtr = nullptr;
        sb04qdPtr = nullptr;
        sb04mdPtr = nullptr;
        sb03odPtr = nullptr;
        sb03mdPtr = nullptr;
        mb03odPtr = nullptr;
        sb02odPtr = nullptr;
        close_dynamic_library(slicot_handle);
        slicotLoaded = false;
    }
    return slicotLoaded;
}
//=============================================================================
}
//=============================================================================
