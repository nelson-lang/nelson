//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen.hpp"
#include <Eigen/src/misc/lapacke.h>
#include "LeastSquareSolver.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define MSGBUFLEN 2048
//=============================================================================
template <class T>
void
changeStride(T* dst, int dstStride, T* src, int srcStride, int rowCount, int colCount)
{
    for (int i = 0; i < colCount; i++) {
        memcpy(dst + i * dstStride, src + i * srcStride, rowCount * sizeof(T));
    }
}
//=========================================================================================
ArrayOf
solveLeastSquareDouble(
    ArrayOf& matA, ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    Dimensions dimsA = matA.getDimensions();
    Dimensions dimsB = matB.getDimensions();
    Dimensions dimsC = Dimensions(dimsA.getColumns(), dimsB.getColumns());
    double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.getElementCount());
    int m = (int)dimsA.getRows();
    int n = (int)dimsA.getColumns();
    int k = (int)dimsB.getColumns();
    double* c = ptrC;
    double* a = (double*)matA.getReadWriteDataPointer();
    double* b = (double*)matB.getReadWriteDataPointer();
    if ((m == 0) || (n == 0)) {
        return ArrayOf(NLS_DOUBLE, dimsC, ptrC);
    }
    int M = m;
    int N = n;
    int NRHS = k;
    double* A = a;
    int LDA = m;
    int Bsize = (M > N) ? M : N;
    double* B = new_with_exception<double>((size_t)Bsize * (size_t)NRHS, true);
    changeStride<double>(B, Bsize, b, m, m, NRHS);
    int LDB = Bsize;
    int* JPVT = (int*)new_with_exception<int>(N, true);
    double RCOND = std::numeric_limits<double>::epsilon();
    int RANK;
    double WORKSIZE;
    int INFO;
    int LWORK = -1;
    LAPACK_dgelsy(&M, &N, &NRHS, A, &LDA, B, &LDB, JPVT, &RCOND, &RANK, &WORKSIZE, &LWORK, &INFO);
    LWORK = (int)WORKSIZE;
    double* WORK = new_with_exception<double>(LWORK);
    LAPACK_dgelsy(&M, &N, &NRHS, A, &LDA, B, &LDB, JPVT, &RCOND, &RANK, WORK, &LWORK, &INFO);
    bool doRankWarning = false;
    if (M > N) {
        if (RANK < N) {
            doRankWarning = true;
        }
    } else if (RANK < M) {
        doRankWarning = true;
    }
    if (doRankWarning) {
        char msgBuffer[MSGBUFLEN];
        snprintf(msgBuffer, MSGBUFLEN, "%s RANK = %d",
            _("Matrix is rank deficient to machine precision:").c_str(), (int)RANK);
        warningMessage = msgBuffer;
        warningId = WARNING_RANK_DEFICIENT_MATRIX;
    }
    changeStride<double>(c, n, B, Bsize, n, k);
    delete[] B;
    delete[] JPVT;
    delete[] WORK;
    return ArrayOf(NLS_DOUBLE, dimsC, ptrC);
}
//=============================================================================
ArrayOf
solveLeastSquareSingle(
    ArrayOf& matA, ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    Dimensions dimsA = matA.getDimensions();
    Dimensions dimsB = matB.getDimensions();
    Dimensions dimsC = Dimensions(dimsA.getColumns(), dimsB.getColumns());
    single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dimsC.getElementCount());
    int m = (int)dimsA.getRows();
    int n = (int)dimsA.getColumns();
    int k = (int)dimsB.getColumns();
    single* c = ptrC;
    single* a = (single*)matA.getReadWriteDataPointer();
    single* b = (single*)matB.getReadWriteDataPointer();
    if ((m == 0) || (n == 0)) {
        return ArrayOf(NLS_SINGLE, dimsC, ptrC);
    }
    int M = m;
    int N = n;
    int NRHS = k;
    single* A = a;
    int LDA = m;
    int Bsize = (M > N) ? M : N;
    single* B = new_with_exception<single>((size_t)Bsize * (size_t)NRHS, true);
    changeStride<single>(B, Bsize, b, m, m, NRHS);
    int LDB = Bsize;
    int* JPVT = (int*)new_with_exception<int>(N, true);
    single RCOND = std::numeric_limits<single>::epsilon();
    int RANK;
    single WORKSIZE;
    int INFO;
    int LWORK = -1;
    LAPACK_sgelsy(&M, &N, &NRHS, A, &LDA, B, &LDB, JPVT, &RCOND, &RANK, &WORKSIZE, &LWORK, &INFO);
    LWORK = (int)WORKSIZE;
    single* WORK = new_with_exception<single>(LWORK);
    LAPACK_sgelsy(&M, &N, &NRHS, A, &LDA, B, &LDB, JPVT, &RCOND, &RANK, WORK, &LWORK, &INFO);
    bool doRankWarning = false;
    if (M > N) {
        if (RANK < N) {
            doRankWarning = true;
        }
    } else if (RANK < M) {
        doRankWarning = true;
    }
    if (doRankWarning) {
        char msgBuffer[MSGBUFLEN];
        snprintf(msgBuffer, MSGBUFLEN, "%s RANK = %d",
            _("Matrix is rank deficient to machine precision:").c_str(), (int)RANK);
        warningMessage = msgBuffer;
        warningId = WARNING_RANK_DEFICIENT_MATRIX;
    }
    changeStride<single>(c, n, B, Bsize, n, k);
    delete[] B;
    delete[] JPVT;
    delete[] WORK;
    return ArrayOf(NLS_SINGLE, dimsC, ptrC);
}
//=============================================================================
ArrayOf
solveLeastSquareSingleComplex(
    ArrayOf& matA, ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    Dimensions dimsA = matA.getDimensions();
    Dimensions dimsB = matB.getDimensions();
    Dimensions dimsC = Dimensions(dimsA.getColumns(), dimsB.getColumns());
    single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, dimsC.getElementCount());
    std::complex<single>* ptrCz = reinterpret_cast<std::complex<single>*>(ptrC);
    int m = (int)dimsA.getRows();
    int n = (int)dimsA.getColumns();
    int k = (int)dimsB.getColumns();
    single* c = ptrC;
    single* a = (single*)matA.getReadWriteDataPointer();
    single* b = (single*)matB.getReadWriteDataPointer();
    if ((m == 0) || (n == 0)) {
        return ArrayOf(NLS_SINGLE, dimsC, ptrC);
    }
    int M = m;
    int N = n;
    int NRHS = k;
    single* A = a;
    int LDA = m;
    int Bsize = (M > N) ? M : N;
    std::complex<single>* B = new_with_exception<std::complex<single>>(Bsize * NRHS, true);
    single* ptrB = reinterpret_cast<single*>(B);
    changeStride<single>(ptrB, 2 * Bsize, b, 2 * m, 2 * m, NRHS);
    int LDB = Bsize;
    int* JPVT = new_with_exception<int>(N, true);
    single RCOND = std::numeric_limits<single>::epsilon();
    int RANK;
    std::complex<single> WORKSIZE;
    single* RWORK = new_with_exception<single>(N * 2);
    int INFO;
    int LWORK = -1;
    std::complex<single>* Az = reinterpret_cast<std::complex<single>*>(A);
    std::complex<single>* Bz = reinterpret_cast<std::complex<single>*>(B);
    LAPACK_cgelsy(
        &M, &N, &NRHS, Az, &LDA, Bz, &LDB, JPVT, &RCOND, &RANK, &WORKSIZE, &LWORK, RWORK, &INFO);
    LWORK = (int)WORKSIZE.real();
    std::complex<single>* WORK = new_with_exception<std::complex<single>>(LWORK);
    LAPACK_cgelsy(
        &M, &N, &NRHS, Az, &LDA, Bz, &LDB, JPVT, &RCOND, &RANK, WORK, &LWORK, RWORK, &INFO);
    bool doRankWarning = false;
    if (M > N) {
        if (RANK < N) {
            doRankWarning = true;
        }
    } else if (RANK < M) {
        doRankWarning = true;
    }
    if (doRankWarning) {
        char msgBuffer[MSGBUFLEN];
        snprintf(msgBuffer, MSGBUFLEN, "%s RANK = %d",
            _("Matrix is rank deficient to machine precision:").c_str(), (int)RANK);
        warningMessage = msgBuffer;
        warningId = WARNING_RANK_DEFICIENT_MATRIX;
    }
    changeStride<single>(c, 2 * n, ptrB, 2 * Bsize, 2 * n, k);
    delete[] B;
    delete[] JPVT;
    delete[] WORK;
    delete[] RWORK;
    return ArrayOf(NLS_SCOMPLEX, dimsC, ptrC);
}
//=============================================================================
ArrayOf
solveLeastSquareDoubleComplex(
    ArrayOf& matA, ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    Dimensions dimsA = matA.getDimensions();
    Dimensions dimsB = matB.getDimensions();
    Dimensions dimsC = Dimensions(dimsA.getColumns(), dimsB.getColumns());
    double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimsC.getElementCount());
    std::complex<double>* ptrCz = reinterpret_cast<std::complex<double>*>(ptrC);
    int m = (int)dimsA.getRows();
    int n = (int)dimsA.getColumns();
    int k = (int)dimsB.getColumns();
    double* c = ptrC;
    double* a = (double*)matA.getReadWriteDataPointer();
    double* b = (double*)matB.getReadWriteDataPointer();
    if ((m == 0) || (n == 0)) {
        return ArrayOf(NLS_SINGLE, dimsC, ptrC);
    }
    int M = m;
    int N = n;
    int NRHS = k;
    double* A = a;
    int LDA = m;
    int Bsize = (M > N) ? M : N;
    std::complex<double>* B = new_with_exception<std::complex<double>>(Bsize * NRHS, true);
    double* ptrB = reinterpret_cast<double*>(B);
    changeStride<double>(ptrB, 2 * Bsize, b, 2 * m, 2 * m, NRHS);
    int LDB = Bsize;
    int* JPVT = new_with_exception<int>(N, true);
    double RCOND = std::numeric_limits<double>::epsilon();
    int RANK;
    std::complex<double> WORKSIZE;
    double* RWORK = new_with_exception<double>(N * 2);
    int INFO;
    int LWORK = -1;
    std::complex<double>* Az = reinterpret_cast<std::complex<double>*>(A);
    std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>(B);
    LAPACK_zgelsy(
        &M, &N, &NRHS, Az, &LDA, Bz, &LDB, JPVT, &RCOND, &RANK, &WORKSIZE, &LWORK, RWORK, &INFO);
    LWORK = (int)WORKSIZE.real();
    std::complex<double>* WORK = new_with_exception<std::complex<double>>(LWORK);
    LAPACK_zgelsy(
        &M, &N, &NRHS, Az, &LDA, Bz, &LDB, JPVT, &RCOND, &RANK, WORK, &LWORK, RWORK, &INFO);
    bool doRankWarning = false;
    if (M > N) {
        if (RANK < N) {
            doRankWarning = true;
        }
    } else if (RANK < M) {
        doRankWarning = true;
    }
    if (doRankWarning) {
        char msgBuffer[MSGBUFLEN];
        snprintf(msgBuffer, MSGBUFLEN, "%s RANK = %d",
            _("Matrix is rank deficient to machine precision:").c_str(), (int)RANK);
        warningMessage = msgBuffer;
        warningId = WARNING_RANK_DEFICIENT_MATRIX;
    }
    changeStride<double>(c, 2 * n, ptrB, 2 * Bsize, 2 * n, k);
    delete[] B;
    delete[] JPVT;
    delete[] WORK;
    delete[] RWORK;
    return ArrayOf(NLS_DCOMPLEX, dimsC, ptrC);
}
//=============================================================================
}
//=============================================================================
