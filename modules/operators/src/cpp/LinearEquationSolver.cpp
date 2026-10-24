//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <algorithm>
#include <complex>
#include <cmath>
#include <limits>
#include <vector>
#include "lapack_eigen_config.hpp"
#include "LapackPrototypes.hpp"
#include "LinearEquationSolver.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
#include "Warning.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
//=============================================================================
static void
setNearSingularWarning(double rcond, std::wstring& warningId, std::string& warningMessage);
//=============================================================================
template <class T>
static bool
needsTriangularConditionEstimate(const T* factor, indexType n, T threshold)
{
    T maxDiag = 0;
    T minDiag = std::numeric_limits<T>::infinity();
    for (indexType k = 0; k < n; ++k) {
        T value = std::abs(factor[k + k * n]);
        maxDiag = std::max(maxDiag, value);
        minDiag = std::min(minDiag, value);
    }
    return maxDiag == T(0) || minDiag <= threshold * maxDiag;
}
//=============================================================================
template <class T>
static bool
needsTriangularConditionEstimate(const std::complex<T>* factor, indexType n, T threshold)
{
    T maxDiag = 0;
    T minDiag = std::numeric_limits<T>::infinity();
    for (indexType k = 0; k < n; ++k) {
        T value = std::abs(factor[k + k * n]);
        maxDiag = std::max(maxDiag, value);
        minDiag = std::min(minDiag, value);
    }
    return maxDiag == T(0) || minDiag <= threshold * maxDiag;
}
//=============================================================================
template <class T>
static bool
isSymmetricMatrix(const T* data, indexType n)
{
    for (indexType col = 1; col < n; ++col) {
        for (indexType row = 0; row < col; ++row) {
            if (data[row + col * n] != data[col + row * n]) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
isHermitianMatrix(const std::complex<T>* data, indexType n)
{
    for (indexType col = 0; col < n; ++col) {
        if (data[col + col * n].imag() != T(0)) {
            return false;
        }
        for (indexType row = 0; row < col; ++row) {
            if (data[row + col * n] != std::conj(data[col + row * n])) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
static bool
solvePositiveDefiniteDouble(const ArrayOf& matA, const ArrayOf& matB, ArrayOf& res,
    std::wstring& warningId, std::string& warningMessage)
{
    indexType m = matA.dimensionLength(0);
    auto* A = (double*)matA.dataPointer();
    if (!isSymmetricMatrix<double>(A, m)) {
        return false;
    }

    indexType nrhs = matB.dimensionLength(1);
    Dimensions outDim(m, nrhs);
    double* factor = new_with_exception<double>((size_t)m * (size_t)m, false);
    double* x = new_with_exception<double>((size_t)m * (size_t)nrhs, false);
    memcpy(factor, A, (size_t)m * (size_t)m * sizeof(double));
    memcpy(x, matB.dataPointer(), (size_t)m * (size_t)nrhs * sizeof(double));

    char UPLO = 'U';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int LDA = (lapack_int)m;
    lapack_int LDB = (lapack_int)m;
    lapack_int INFO = 0;
    char norm = '1';
    double* normWork = nullptr;
    double anorm = LAPACK_dlange(&norm, &N, &N, A, &LDA, normWork);
    LAPACK_dposv(&UPLO, &N, &NRHS, factor, &LDA, x, &LDB, &INFO);
    if (INFO != 0) {
        delete[] factor;
        delete[] x;
        return false;
    }
    double rcond = 1.;
    if (needsTriangularConditionEstimate<double>(
            factor, m, std::sqrt(std::sqrt(std::numeric_limits<double>::epsilon())))) {
        std::vector<double> work((size_t)3 * (size_t)N);
        std::vector<lapack_int> iwork((size_t)N);
        LAPACK_dpocon(&UPLO, &N, factor, &LDA, &anorm, &rcond, work.data(), iwork.data(), &INFO);
    }
    delete[] factor;
    if (INFO != 0) {
        delete[] x;
        return false;
    }
    res = ArrayOf(NLS_DOUBLE, outDim, x);
    if (rcond < std::numeric_limits<double>::epsilon()) {
        setNearSingularWarning(rcond, warningId, warningMessage);
    }
    return true;
}
//=============================================================================
static bool
solvePositiveDefiniteSingle(const ArrayOf& matA, const ArrayOf& matB, ArrayOf& res,
    std::wstring& warningId, std::string& warningMessage)
{
    indexType m = matA.dimensionLength(0);
    auto* A = (single*)matA.dataPointer();
    if (!isSymmetricMatrix<single>(A, m)) {
        return false;
    }

    indexType nrhs = matB.dimensionLength(1);
    Dimensions outDim(m, nrhs);
    single* factor = new_with_exception<single>((size_t)m * (size_t)m, false);
    single* x = new_with_exception<single>((size_t)m * (size_t)nrhs, false);
    memcpy(factor, A, (size_t)m * (size_t)m * sizeof(single));
    memcpy(x, matB.dataPointer(), (size_t)m * (size_t)nrhs * sizeof(single));

    char UPLO = 'U';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int LDA = (lapack_int)m;
    lapack_int LDB = (lapack_int)m;
    lapack_int INFO = 0;
    char norm = '1';
    single* normWork = nullptr;
    single anorm = LAPACK_slange(&norm, &N, &N, A, &LDA, normWork);
    LAPACK_sposv(&UPLO, &N, &NRHS, factor, &LDA, x, &LDB, &INFO);
    if (INFO != 0) {
        delete[] factor;
        delete[] x;
        return false;
    }
    single rcond = 1;
    if (needsTriangularConditionEstimate<single>(
            factor, m, std::sqrt(std::sqrt(std::numeric_limits<single>::epsilon())))) {
        std::vector<single> work((size_t)3 * (size_t)N);
        std::vector<lapack_int> iwork((size_t)N);
        LAPACK_spocon(&UPLO, &N, factor, &LDA, &anorm, &rcond, work.data(), iwork.data(), &INFO);
    }
    delete[] factor;
    if (INFO != 0) {
        delete[] x;
        return false;
    }
    res = ArrayOf(NLS_SINGLE, outDim, x);
    if (rcond < std::numeric_limits<single>::epsilon()) {
        setNearSingularWarning(rcond, warningId, warningMessage);
    }
    return true;
}
//=============================================================================
static bool
solvePositiveDefiniteDoubleComplex(const ArrayOf& matA, const ArrayOf& matB, ArrayOf& res,
    std::wstring& warningId, std::string& warningMessage)
{
    using Z = std::complex<double>;
    indexType m = matA.dimensionLength(0);
    auto* A = reinterpret_cast<Z*>((double*)matA.dataPointer());
    if (!isHermitianMatrix<double>(A, m)) {
        return false;
    }

    indexType nrhs = matB.dimensionLength(1);
    Dimensions outDim(m, nrhs);
    Z* factor = new_with_exception<Z>((size_t)m * (size_t)m, false);
    Z* x = new_with_exception<Z>((size_t)m * (size_t)nrhs, false);
    memcpy(factor, A, (size_t)m * (size_t)m * sizeof(Z));
    memcpy(x, matB.dataPointer(), (size_t)m * (size_t)nrhs * sizeof(Z));

    char UPLO = 'U';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int LDA = (lapack_int)m;
    lapack_int LDB = (lapack_int)m;
    lapack_int INFO = 0;
    char norm = '1';
    std::vector<double> normWork((size_t)std::max<lapack_int>(1, N));
    double anorm = LAPACK_zlange(&norm, &N, &N, A, &LDA, normWork.data());
    LAPACK_zposv(&UPLO, &N, &NRHS, factor, &LDA, x, &LDB, &INFO);
    if (INFO != 0) {
        delete[] factor;
        delete[] x;
        return false;
    }
    double rcond = 1.;
    if (needsTriangularConditionEstimate<double>(
            factor, m, std::sqrt(std::sqrt(std::numeric_limits<double>::epsilon())))) {
        std::vector<Z> work((size_t)2 * (size_t)N);
        std::vector<double> rwork((size_t)N);
        LAPACK_zpocon(&UPLO, &N, factor, &LDA, &anorm, &rcond, work.data(), rwork.data(), &INFO);
    }
    delete[] factor;
    if (INFO != 0) {
        delete[] x;
        return false;
    }
    res = ArrayOf(NLS_DCOMPLEX, outDim, reinterpret_cast<double*>(x));
    if (rcond < std::numeric_limits<double>::epsilon()) {
        setNearSingularWarning(rcond, warningId, warningMessage);
    }
    return true;
}
//=============================================================================
static bool
solvePositiveDefiniteSingleComplex(const ArrayOf& matA, const ArrayOf& matB, ArrayOf& res,
    std::wstring& warningId, std::string& warningMessage)
{
    using Z = std::complex<single>;
    indexType m = matA.dimensionLength(0);
    auto* A = reinterpret_cast<Z*>((single*)matA.dataPointer());
    if (!isHermitianMatrix<single>(A, m)) {
        return false;
    }

    indexType nrhs = matB.dimensionLength(1);
    Dimensions outDim(m, nrhs);
    Z* factor = new_with_exception<Z>((size_t)m * (size_t)m, false);
    Z* x = new_with_exception<Z>((size_t)m * (size_t)nrhs, false);
    memcpy(factor, A, (size_t)m * (size_t)m * sizeof(Z));
    memcpy(x, matB.dataPointer(), (size_t)m * (size_t)nrhs * sizeof(Z));

    char UPLO = 'U';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int LDA = (lapack_int)m;
    lapack_int LDB = (lapack_int)m;
    lapack_int INFO = 0;
    char norm = '1';
    std::vector<single> normWork((size_t)std::max<lapack_int>(1, N));
    single anorm = LAPACK_clange(&norm, &N, &N, A, &LDA, normWork.data());
    LAPACK_cposv(&UPLO, &N, &NRHS, factor, &LDA, x, &LDB, &INFO);
    if (INFO != 0) {
        delete[] factor;
        delete[] x;
        return false;
    }
    single rcond = 1;
    if (needsTriangularConditionEstimate<single>(
            factor, m, std::sqrt(std::sqrt(std::numeric_limits<single>::epsilon())))) {
        std::vector<Z> work((size_t)2 * (size_t)N);
        std::vector<single> rwork((size_t)N);
        LAPACK_cpocon(&UPLO, &N, factor, &LDA, &anorm, &rcond, work.data(), rwork.data(), &INFO);
    }
    delete[] factor;
    if (INFO != 0) {
        delete[] x;
        return false;
    }
    res = ArrayOf(NLS_SCOMPLEX, outDim, reinterpret_cast<single*>(x));
    if (rcond < std::numeric_limits<single>::epsilon()) {
        setNearSingularWarning(rcond, warningId, warningMessage);
    }
    return true;
}
//=============================================================================
static void
setNearSingularWarning(double rcond, std::wstring& warningId, std::string& warningMessage)
{
    snprintf(msgBuffer, MSGBUFLEN, "%s RCOND = %e",
        Nelson::resolveMessageTemplate("Nelson:operators:matrixSingularWorkingPrecision",
            "Matrix is singular to working precision:")
            .c_str(),
        rcond);
    warningMessage = msgBuffer;
    warningId = WARNING_NEARLY_SINGULAR_MATRIX;
}
//=============================================================================
static bool
solveGeneralDoubleFast(const ArrayOf& matA, const ArrayOf& matB, ArrayOf& res,
    std::wstring& warningId, std::string& warningMessage)
{
    indexType m = matA.dimensionLength(0);
    indexType nrhs = matB.dimensionLength(1);
    if (m < 64) {
        return false;
    }
    Dimensions outDim(m, nrhs);
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int LDA = N;
    lapack_int LDB = N;

    double* factor = new_with_exception<double>((size_t)m * (size_t)m, false);
    double* x = new_with_exception<double>((size_t)m * (size_t)nrhs, false);
    lapack_int* ipiv = new_with_exception<lapack_int>(N, false);
    memcpy(factor, matA.dataPointer(), (size_t)m * (size_t)m * sizeof(double));
    memcpy(x, matB.dataPointer(), (size_t)m * (size_t)nrhs * sizeof(double));

    lapack_int info = 0;
    LAPACK_dgetrf(&N, &N, factor, &LDA, ipiv, &info);
    if (info < 0) {
        delete[] factor;
        delete[] x;
        delete[] ipiv;
        return false;
    }
    if (info > 0) {
        res = ArrayOf(NLS_DOUBLE, outDim, x);
        setNearSingularWarning(0., warningId, warningMessage);
        delete[] factor;
        delete[] ipiv;
        return true;
    }

    double rcond = 1.;
    if (needsTriangularConditionEstimate<double>(
            factor, m, std::sqrt(std::sqrt(std::numeric_limits<double>::epsilon())))) {
        char norm = '1';
        double* normWork = nullptr;
        double anorm = LAPACK_dlange(&norm, &N, &N, (double*)matA.dataPointer(), &LDA, normWork);
        std::vector<double> work((size_t)4 * (size_t)N);
        std::vector<lapack_int> iwork((size_t)N);
        LAPACK_dgecon(&norm, &N, factor, &LDA, &anorm, &rcond, work.data(), iwork.data(), &info);
        if (info != 0) {
            delete[] factor;
            delete[] x;
            delete[] ipiv;
            return false;
        }
    }

    char trans = 'N';
    LAPACK_dgetrs(&trans, &N, &NRHS, factor, &LDA, ipiv, x, &LDB, &info);
    delete[] factor;
    delete[] ipiv;
    if (info != 0) {
        delete[] x;
        return false;
    }
    res = ArrayOf(NLS_DOUBLE, outDim, x);
    if (rcond < std::numeric_limits<double>::epsilon()) {
        setNearSingularWarning(rcond, warningId, warningMessage);
    }
    return true;
}
//=============================================================================
static bool
solveGeneralSingleFast(const ArrayOf& matA, const ArrayOf& matB, ArrayOf& res,
    std::wstring& warningId, std::string& warningMessage)
{
    indexType m = matA.dimensionLength(0);
    indexType nrhs = matB.dimensionLength(1);
    if (m < 64) {
        return false;
    }
    Dimensions outDim(m, nrhs);
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int LDA = N;
    lapack_int LDB = N;

    single* factor = new_with_exception<single>((size_t)m * (size_t)m, false);
    single* x = new_with_exception<single>((size_t)m * (size_t)nrhs, false);
    lapack_int* ipiv = new_with_exception<lapack_int>(N, false);
    memcpy(factor, matA.dataPointer(), (size_t)m * (size_t)m * sizeof(single));
    memcpy(x, matB.dataPointer(), (size_t)m * (size_t)nrhs * sizeof(single));

    lapack_int info = 0;
    LAPACK_sgetrf(&N, &N, factor, &LDA, ipiv, &info);
    if (info < 0) {
        delete[] factor;
        delete[] x;
        delete[] ipiv;
        return false;
    }
    if (info > 0) {
        res = ArrayOf(NLS_SINGLE, outDim, x);
        setNearSingularWarning(0., warningId, warningMessage);
        delete[] factor;
        delete[] ipiv;
        return true;
    }

    single rcond = 1;
    if (needsTriangularConditionEstimate<single>(
            factor, m, std::sqrt(std::sqrt(std::numeric_limits<single>::epsilon())))) {
        char norm = '1';
        single* normWork = nullptr;
        single anorm = LAPACK_slange(&norm, &N, &N, (single*)matA.dataPointer(), &LDA, normWork);
        std::vector<single> work((size_t)4 * (size_t)N);
        std::vector<lapack_int> iwork((size_t)N);
        LAPACK_sgecon(&norm, &N, factor, &LDA, &anorm, &rcond, work.data(), iwork.data(), &info);
        if (info != 0) {
            delete[] factor;
            delete[] x;
            delete[] ipiv;
            return false;
        }
    }

    char trans = 'N';
    LAPACK_sgetrs(&trans, &N, &NRHS, factor, &LDA, ipiv, x, &LDB, &info);
    delete[] factor;
    delete[] ipiv;
    if (info != 0) {
        delete[] x;
        return false;
    }
    res = ArrayOf(NLS_SINGLE, outDim, x);
    if (rcond < std::numeric_limits<single>::epsilon()) {
        setNearSingularWarning(rcond, warningId, warningMessage);
    }
    return true;
}
//=============================================================================
static bool
solveGeneralDoubleComplexFast(const ArrayOf& matA, const ArrayOf& matB, ArrayOf& res,
    std::wstring& warningId, std::string& warningMessage)
{
    using Z = std::complex<double>;
    indexType m = matA.dimensionLength(0);
    indexType nrhs = matB.dimensionLength(1);
    if (m < 64) {
        return false;
    }
    Dimensions outDim(m, nrhs);
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int LDA = N;
    lapack_int LDB = N;
    auto* A = reinterpret_cast<Z*>((double*)matA.dataPointer());

    Z* factor = new_with_exception<Z>((size_t)m * (size_t)m, false);
    Z* x = new_with_exception<Z>((size_t)m * (size_t)nrhs, false);
    lapack_int* ipiv = new_with_exception<lapack_int>(N, false);
    memcpy(factor, A, (size_t)m * (size_t)m * sizeof(Z));
    memcpy(x, matB.dataPointer(), (size_t)m * (size_t)nrhs * sizeof(Z));

    lapack_int info = 0;
    LAPACK_zgetrf(&N, &N, factor, &LDA, ipiv, &info);
    if (info < 0) {
        delete[] factor;
        delete[] x;
        delete[] ipiv;
        return false;
    }
    if (info > 0) {
        res = ArrayOf(NLS_DCOMPLEX, outDim, reinterpret_cast<double*>(x));
        setNearSingularWarning(0., warningId, warningMessage);
        delete[] factor;
        delete[] ipiv;
        return true;
    }

    double rcond = 1.;
    if (needsTriangularConditionEstimate<double>(
            factor, m, std::sqrt(std::sqrt(std::numeric_limits<double>::epsilon())))) {
        char norm = '1';
        std::vector<double> normWork((size_t)std::max<lapack_int>(1, N));
        double anorm = LAPACK_zlange(&norm, &N, &N, A, &LDA, normWork.data());
        std::vector<Z> work((size_t)2 * (size_t)N);
        std::vector<double> rwork((size_t)2 * (size_t)N);
        LAPACK_zgecon(&norm, &N, factor, &LDA, &anorm, &rcond, work.data(), rwork.data(), &info);
        if (info != 0) {
            delete[] factor;
            delete[] x;
            delete[] ipiv;
            return false;
        }
    }

    char trans = 'N';
    LAPACK_zgetrs(&trans, &N, &NRHS, factor, &LDA, ipiv, x, &LDB, &info);
    delete[] factor;
    delete[] ipiv;
    if (info != 0) {
        delete[] x;
        return false;
    }
    res = ArrayOf(NLS_DCOMPLEX, outDim, reinterpret_cast<double*>(x));
    if (rcond < std::numeric_limits<double>::epsilon()) {
        setNearSingularWarning(rcond, warningId, warningMessage);
    }
    return true;
}
//=============================================================================
static bool
solveGeneralSingleComplexFast(const ArrayOf& matA, const ArrayOf& matB, ArrayOf& res,
    std::wstring& warningId, std::string& warningMessage)
{
    using Z = std::complex<single>;
    indexType m = matA.dimensionLength(0);
    indexType nrhs = matB.dimensionLength(1);
    if (m < 64) {
        return false;
    }
    Dimensions outDim(m, nrhs);
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)nrhs;
    lapack_int LDA = N;
    lapack_int LDB = N;
    auto* A = reinterpret_cast<Z*>((single*)matA.dataPointer());

    Z* factor = new_with_exception<Z>((size_t)m * (size_t)m, false);
    Z* x = new_with_exception<Z>((size_t)m * (size_t)nrhs, false);
    lapack_int* ipiv = new_with_exception<lapack_int>(N, false);
    memcpy(factor, A, (size_t)m * (size_t)m * sizeof(Z));
    memcpy(x, matB.dataPointer(), (size_t)m * (size_t)nrhs * sizeof(Z));

    lapack_int info = 0;
    LAPACK_cgetrf(&N, &N, factor, &LDA, ipiv, &info);
    if (info < 0) {
        delete[] factor;
        delete[] x;
        delete[] ipiv;
        return false;
    }
    if (info > 0) {
        res = ArrayOf(NLS_SCOMPLEX, outDim, reinterpret_cast<single*>(x));
        setNearSingularWarning(0., warningId, warningMessage);
        delete[] factor;
        delete[] ipiv;
        return true;
    }

    single rcond = 1;
    if (needsTriangularConditionEstimate<single>(
            factor, m, std::sqrt(std::sqrt(std::numeric_limits<single>::epsilon())))) {
        char norm = '1';
        std::vector<single> normWork((size_t)std::max<lapack_int>(1, N));
        single anorm = LAPACK_clange(&norm, &N, &N, A, &LDA, normWork.data());
        std::vector<Z> work((size_t)2 * (size_t)N);
        std::vector<single> rwork((size_t)2 * (size_t)N);
        LAPACK_cgecon(&norm, &N, factor, &LDA, &anorm, &rcond, work.data(), rwork.data(), &info);
        if (info != 0) {
            delete[] factor;
            delete[] x;
            delete[] ipiv;
            return false;
        }
    }

    char trans = 'N';
    LAPACK_cgetrs(&trans, &N, &NRHS, factor, &LDA, ipiv, x, &LDB, &info);
    delete[] factor;
    delete[] ipiv;
    if (info != 0) {
        delete[] x;
        return false;
    }
    res = ArrayOf(NLS_SCOMPLEX, outDim, reinterpret_cast<single*>(x));
    if (rcond < std::numeric_limits<single>::epsilon()) {
        setNearSingularWarning(rcond, warningId, warningMessage);
    }
    return true;
}
//=============================================================================
ArrayOf
solveLinearEquationDouble(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    ArrayOf res;
    indexType m = matA.dimensionLength(0);
    indexType n = matB.dimensionLength(1);
    Dimensions outDim(m, n);
    warningId.clear();
    warningMessage.clear();

    if (solvePositiveDefiniteDouble(matA, matB, res, warningId, warningMessage)) {
        return res;
    }
    if (solveGeneralDoubleFast(matA, matB, res, warningId, warningMessage)) {
        return res;
    }

    double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, m * n);
    res = ArrayOf(NLS_DOUBLE, outDim, ptrC);

    char FACT = 'E';
    char TRANS = 'N';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)n;
    // FACT = 'E' lets LAPACK equilibrate A and B in place; work on copies so the
    // caller's matrices (e.g. the R factor returned by qr) are never mutated.
    auto* A = (double*)new_with_exception<double>((size_t)m * (size_t)m, false);
    memcpy(A, matA.dataPointer(), (size_t)m * (size_t)m * sizeof(double));
    lapack_int LDA = (lapack_int)m;
    double* AF = (double*)new_with_exception<double>((size_t)(LDA * N), true);
    lapack_int LDAF = (lapack_int)m;
    lapack_int* IPIV = (lapack_int*)new_with_exception<lapack_int>(N, true);
    char EQUED;
    double* R = (double*)new_with_exception<double>(N, true);
    double* C = (double*)new_with_exception<double>(N, true);
    auto* B = (double*)new_with_exception<double>((size_t)m * (size_t)n, false);
    memcpy(B, matB.dataPointer(), (size_t)m * (size_t)n * sizeof(double));
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
        setNearSingularWarning(RCOND, warningId, warningMessage);
    }
    delete[] A;
    delete[] B;
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
    indexType m = matA.dimensionLength(0);
    indexType n = matB.dimensionLength(1);
    Dimensions outDim(m, n);
    warningId.clear();
    warningMessage.clear();

    if (solvePositiveDefiniteDoubleComplex(matA, matB, res, warningId, warningMessage)) {
        return res;
    }
    if (solveGeneralDoubleComplexFast(matA, matB, res, warningId, warningMessage)) {
        return res;
    }

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

    // FACT = 'E' lets LAPACK equilibrate A and B in place; work on copies so the
    // caller's matrices (e.g. the R factor returned by qr) are never mutated.
    auto* A = new_with_exception<std::complex<double>>((size_t)m * (size_t)m, false);
    memcpy(A, matA.dataPointer(), (size_t)m * (size_t)m * sizeof(std::complex<double>));
    lapack_int INFO = 0;
    std::complex<double>* AF
        = (std::complex<double>*)new_with_exception<std::complex<double>>((size_t)(LDA * N), true);
    lapack_int LDAF = (lapack_int)m;
    lapack_int* IPIV = (lapack_int*)new_with_exception<lapack_int>(N, true);
    auto* B = new_with_exception<std::complex<double>>((size_t)m * (size_t)n, false);
    memcpy(B, matB.dataPointer(), (size_t)m * (size_t)n * sizeof(std::complex<double>));

    auto* X = reinterpret_cast<std::complex<double>*>(ptrC);

    std::complex<double>* WORK
        = (std::complex<double>*)new_with_exception<std::complex<double>>((size_t)(2 * N), true);

    double* RWORK = new_with_exception<double>((size_t)(2 * N), true);

    LAPACK_zgesvx(&FACT, &TRANS, &N, &NRHS, A, &LDA, AF, &LDAF, IPIV, &EQUED, R, C, B, &LDB, X,
        &LDX, &RCOND, FERR, BERR, WORK, RWORK, &INFO);

    if ((INFO == N) || (INFO == N + 1) || (RCOND < std::numeric_limits<double>::epsilon())) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RCOND = %e",
            Nelson::resolveMessageTemplate("Nelson:operators:matrixSingularWorkingPrecision",
                "Matrix is singular to working precision:")
                .c_str(),
            RCOND);
        warningMessage = msgBuffer;
        warningId = WARNING_NEARLY_SINGULAR_MATRIX;
    }

    delete[] A;
    delete[] B;
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
    indexType m = matA.dimensionLength(0);
    indexType n = matB.dimensionLength(1);
    Dimensions outDim(m, n);
    warningId.clear();
    warningMessage.clear();

    if (solvePositiveDefiniteSingle(matA, matB, res, warningId, warningMessage)) {
        return res;
    }
    if (solveGeneralSingleFast(matA, matB, res, warningId, warningMessage)) {
        return res;
    }

    single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, m * n);
    res = ArrayOf(NLS_SINGLE, outDim, ptrC);

    char FACT = 'E';
    char TRANS = 'N';
    lapack_int N = (lapack_int)m;
    lapack_int NRHS = (lapack_int)n;
    // FACT = 'E' lets LAPACK equilibrate A and B in place; work on copies so the
    // caller's matrices (e.g. the R factor returned by qr) are never mutated.
    auto* A = (single*)new_with_exception<single>((size_t)m * (size_t)m, false);
    memcpy(A, matA.dataPointer(), (size_t)m * (size_t)m * sizeof(single));
    lapack_int LDA = (lapack_int)m;
    single* AF = (single*)new_with_exception<single>((size_t)(LDA * N), true);
    lapack_int LDAF = (lapack_int)m;
    lapack_int* IPIV = (lapack_int*)new_with_exception<lapack_int>(N, true);
    char EQUED;
    single* R = (single*)new_with_exception<single>(N, true);
    single* C = (single*)new_with_exception<single>(N, true);
    auto* B = (single*)new_with_exception<single>((size_t)m * (size_t)n, false);
    memcpy(B, matB.dataPointer(), (size_t)m * (size_t)n * sizeof(single));
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
        setNearSingularWarning(RCOND, warningId, warningMessage);
    }
    delete[] A;
    delete[] B;
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
    indexType m = matA.dimensionLength(0);
    indexType n = matB.dimensionLength(1);
    Dimensions outDim(m, n);
    warningId.clear();
    warningMessage.clear();

    if (solvePositiveDefiniteSingleComplex(matA, matB, res, warningId, warningMessage)) {
        return res;
    }
    if (solveGeneralSingleComplexFast(matA, matB, res, warningId, warningMessage)) {
        return res;
    }

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

    // FACT = 'E' lets LAPACK equilibrate A and B in place; work on copies so the
    // caller's matrices (e.g. the R factor returned by qr) are never mutated.
    auto* A = new_with_exception<std::complex<single>>((size_t)m * (size_t)m, false);
    memcpy(A, matA.dataPointer(), (size_t)m * (size_t)m * sizeof(std::complex<single>));
    lapack_int INFO = 0;
    std::complex<single>* AF
        = (std::complex<single>*)new_with_exception<std::complex<single>>((size_t)(LDA * N), true);
    lapack_int LDAF = (lapack_int)m;
    lapack_int* IPIV = (lapack_int*)new_with_exception<lapack_int>(N, true);
    auto* B = new_with_exception<std::complex<single>>((size_t)m * (size_t)n, false);
    memcpy(B, matB.dataPointer(), (size_t)m * (size_t)n * sizeof(std::complex<single>));

    auto* X = reinterpret_cast<std::complex<single>*>(ptrC);

    std::complex<single>* WORK
        = (std::complex<single>*)new_with_exception<std::complex<single>>(2 * N, true);

    single* RWORK = new_with_exception<single>(2 * N, true);

    LAPACK_cgesvx(&FACT, &TRANS, &N, &NRHS, A, &LDA, AF, &LDAF, IPIV, &EQUED, R, C, B, &LDB, X,
        &LDX, &RCOND, FERR, BERR, WORK, RWORK, &INFO);

    if ((INFO == N) || (INFO == N + 1) || (RCOND < std::numeric_limits<single>::epsilon())) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RCOND = %e",
            Nelson::resolveMessageTemplate("Nelson:operators:matrixSingularWorkingPrecision",
                "Matrix is singular to working precision:")
                .c_str(),
            RCOND);
        warningMessage = msgBuffer;
        warningId = WARNING_NEARLY_SINGULAR_MATRIX;
    }

    delete[] A;
    delete[] B;
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
} // namespace Nelson
//=============================================================================
