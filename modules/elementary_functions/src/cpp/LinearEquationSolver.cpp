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
#include <limits>
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
#include <Eigen/src/misc/lapacke.h>
#include "LinearEquationSolver.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
//=============================================================================
ArrayOf
solveLinearEquationDouble(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    ArrayOf res;
    indexType m = matA.getDimensionLength(0);
    indexType n = matB.getDimensionLength(1);
    Dimensions outDim(m, n);
    warningId.clear();
    warningMessage.clear();

    double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, m * n);
    res = ArrayOf(NLS_DOUBLE, outDim, ptrC);

    char FACT = 'E';
    char TRANS = 'N';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)n;
    auto* A = (double*)matA.getDataPointer();
    lapack_int LDA = (lapack_int)m;
    double* AF = (double*)new_with_exception<double>((size_t)(LDA * N), true);
    lapack_int LDAF = (lapack_int)m;
    lapack_int* IPIV = (lapack_int*)new_with_exception<lapack_int>(N, true);
    char EQUED;
    double* R = (double*)new_with_exception<double>(N, true);
    double* C = (double*)new_with_exception<double>(N, true);
    auto* B = (double*)matB.getDataPointer();
    lapack_int LDB = (lapack_int)m;
    double* X = ptrC;
    lapack_int LDX = (lapack_int)m;
    double RCOND;
    double* FERR = (double*)new_with_exception<double>(n, true);
    double* BERR = (double*)new_with_exception<double>(n, true);
    double* WORK = (double*)new_with_exception<double>((size_t)(4 * N), true);
    lapack_int* IWORK = (lapack_int*)new_with_exception<lapack_int>((size_t)(4 * N), true);
    lapack_int INFO = 0;

    LAPACK_dgesvx(&FACT, &TRANS, &N, &NRHS, A, &LDA, AF, &LDAF, IPIV, &EQUED, R, C, B, &LDB, X,
        &LDX, &RCOND, FERR, BERR, WORK, IWORK, &INFO);
    if ((INFO == N) || (INFO == N + 1) || (RCOND < std::numeric_limits<double>::epsilon())) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RCOND = %e",
            _("Matrix is singular to working precision:").c_str(), RCOND);
        warningMessage = msgBuffer;
        warningId = WARNING_NEARLY_SINGULAR_MATRIX;
    }
    delete[] AF;
    delete[] IPIV;
    delete[] R;
    delete[] C;
    delete[] WORK;
    delete[] IWORK;
    delete[] FERR;
    delete[] BERR;
    return res;
}
//=============================================================================
ArrayOf
solveLinearEquationDoubleComplex(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    ArrayOf res;
    indexType m = matA.getDimensionLength(0);
    indexType n = matB.getDimensionLength(1);
    Dimensions outDim(m, n);
    warningId.clear();
    warningMessage.clear();

    double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, m * n);
    res = ArrayOf(NLS_DCOMPLEX, outDim, ptrC);

    char FACT = 'E';
    char TRANS = 'N';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)n;
    lapack_int LDA = (lapack_int)m;
    char EQUED;
    double* R = (double*)new_with_exception<double>(N, true);
    double* C = (double*)new_with_exception<double>(N, true);
    lapack_int LDB = (lapack_int)m;
    lapack_int LDX = (lapack_int)m;
    double RCOND;
    double* FERR = (double*)new_with_exception<double>(NRHS, true);
    double* BERR = (double*)new_with_exception<double>(NRHS, true);

    auto* A = reinterpret_cast<std::complex<double>*>((double*)matA.getDataPointer());
    lapack_int INFO = 0;
    std::complex<double>* AF
        = (std::complex<double>*)new_with_exception<std::complex<double>>((size_t)(LDA * N), true);
    lapack_int LDAF = (lapack_int)m;
    lapack_int* IPIV = (lapack_int*)new_with_exception<lapack_int>(N, true);
    auto* B = reinterpret_cast<std::complex<double>*>((double*)matB.getDataPointer());

    auto* X = reinterpret_cast<std::complex<double>*>(ptrC);

    std::complex<double>* WORK
        = (std::complex<double>*)new_with_exception<std::complex<double>>((size_t)(2 * N), true);

    double* RWORK = new_with_exception<double>((size_t)(2 * N), true);

    LAPACK_zgesvx(&FACT, &TRANS, &N, &NRHS, A, &LDA, AF, &LDAF, IPIV, &EQUED, R, C, B, &LDB, X,
        &LDX, &RCOND, FERR, BERR, WORK, RWORK, &INFO);

    if ((INFO == N) || (INFO == N + 1) || (RCOND < std::numeric_limits<double>::epsilon())) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RCOND = %e",
            _("Matrix is singular to working precision:").c_str(), RCOND);
        warningMessage = msgBuffer;
        warningId = WARNING_NEARLY_SINGULAR_MATRIX;
    }

    delete[] R;
    delete[] C;
    delete[] FERR;
    delete[] BERR;
    delete[] AF;
    delete[] IPIV;
    delete[] WORK;
    delete[] RWORK;

    return res;
}
//=============================================================================
ArrayOf
solveLinearEquationSingle(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    ArrayOf res;
    indexType m = matA.getDimensionLength(0);
    indexType n = matB.getDimensionLength(1);
    Dimensions outDim(m, n);
    warningId.clear();
    warningMessage.clear();

    single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, m * n);
    res = ArrayOf(NLS_SINGLE, outDim, ptrC);

    char FACT = 'E';
    char TRANS = 'N';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)n;
    auto* A = (single*)matA.getDataPointer();
    lapack_int LDA = (lapack_int)m;
    single* AF = (single*)new_with_exception<single>(LDA * N, true);
    lapack_int LDAF = (lapack_int)m;
    lapack_int* IPIV = (lapack_int*)new_with_exception<lapack_int>(N, true);
    char EQUED;
    single* R = (single*)new_with_exception<single>(N, true);
    single* C = (single*)new_with_exception<single>(N, true);
    auto* B = (single*)matB.getDataPointer();
    lapack_int LDB = (lapack_int)m;
    single* X = ptrC;
    lapack_int LDX = (lapack_int)m;
    single RCOND;
    single* FERR = (single*)new_with_exception<single>(n, true);
    single* BERR = (single*)new_with_exception<single>(n, true);
    single* WORK = (single*)new_with_exception<single>(4 * N, true);
    lapack_int* IWORK = (lapack_int*)new_with_exception<lapack_int>(4 * N, true);
    lapack_int INFO = 0;

    LAPACK_sgesvx(&FACT, &TRANS, &N, &NRHS, A, &LDA, AF, &LDAF, IPIV, &EQUED, R, C, B, &LDB, X,
        &LDX, &RCOND, FERR, BERR, WORK, IWORK, &INFO);
    if ((INFO == N) || (INFO == N + 1) || (RCOND < std::numeric_limits<single>::epsilon())) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RCOND = %e",
            _("Matrix is singular to working precision:").c_str(), RCOND);
        warningMessage = msgBuffer;
        warningId = WARNING_NEARLY_SINGULAR_MATRIX;
    }
    delete[] AF;
    delete[] IPIV;
    delete[] R;
    delete[] C;
    delete[] WORK;
    delete[] IWORK;
    delete[] FERR;
    delete[] BERR;
    return res;
}
//=============================================================================
ArrayOf
solveLinearEquationSingleComplex(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    ArrayOf res;
    indexType m = matA.getDimensionLength(0);
    indexType n = matB.getDimensionLength(1);
    Dimensions outDim(m, n);
    warningId.clear();
    warningMessage.clear();

    double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, m * n);
    res = ArrayOf(NLS_SCOMPLEX, outDim, ptrC);

    char FACT = 'E';
    char TRANS = 'N';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)n;
    lapack_int LDA = (lapack_int)m;
    char EQUED;
    single* R = (single*)new_with_exception<single>(N, true);
    single* C = (single*)new_with_exception<single>(N, true);
    lapack_int LDB = (lapack_int)m;
    lapack_int LDX = (lapack_int)m;
    single RCOND;
    single* FERR = (single*)new_with_exception<single>(NRHS, true);
    single* BERR = (single*)new_with_exception<single>(NRHS, true);

    auto* A = reinterpret_cast<std::complex<single>*>((single*)matA.getDataPointer());
    lapack_int INFO = 0;
    std::complex<single>* AF
        = (std::complex<single>*)new_with_exception<std::complex<single>>(LDA * N, true);
    lapack_int LDAF = (lapack_int)m;
    lapack_int* IPIV = (lapack_int*)new_with_exception<lapack_int>(N, true);
    auto* B = reinterpret_cast<std::complex<single>*>((single*)matB.getDataPointer());

    auto* X = reinterpret_cast<std::complex<single>*>(ptrC);

    std::complex<single>* WORK
        = (std::complex<single>*)new_with_exception<std::complex<single>>(2 * N, true);

    single* RWORK = new_with_exception<single>(2 * N, true);

    LAPACK_cgesvx(&FACT, &TRANS, &N, &NRHS, A, &LDA, AF, &LDAF, IPIV, &EQUED, R, C, B, &LDB, X,
        &LDX, &RCOND, FERR, BERR, WORK, RWORK, &INFO);

    if ((INFO == N) || (INFO == N + 1) || (RCOND < std::numeric_limits<single>::epsilon())) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RCOND = %e",
            _("Matrix is singular to working precision:").c_str(), RCOND);
        warningMessage = msgBuffer;
        warningId = WARNING_NEARLY_SINGULAR_MATRIX;
    }

    delete[] R;
    delete[] C;
    delete[] FERR;
    delete[] BERR;
    delete[] AF;
    delete[] IPIV;
    delete[] WORK;
    delete[] RWORK;

    return res;
}
//=============================================================================
}
//=============================================================================
