//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <cfloat>
#include "BlasThreadScope.hpp"
#include "lapack_eigen_config.hpp"
#undef EIGEN_USE_MKL
#undef EIGEN_USE_MKL_VML
#include "LapackPrototypes.hpp"
#include "ReciprocalConditionNumber.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_Double(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    Dimensions dimsA = A.dimensions();
    const auto* input = (const double*)A.dataPointer();
    if (A.isScalar()) {
        double value = input[0];
        if (std::isnan(value)) {
            rcond = ArrayOf::doubleScalar(std::nan(""));
        } else if (std::isinf(value) || value == 0.) {
            rcond = ArrayOf::doubleScalar(0.);
        } else {
            rcond = ArrayOf::doubleScalar(1.);
        }
    } else {
        char norm = '1';
        int m = static_cast<int>(dimsA.rows());
        int n = static_cast<int>(dimsA.columns());
        int lda = m;
        BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n), 4);
        std::vector<double> workNorm((size_t)std::max(1, m));
        double normA = LAPACK_dlange(&norm, &m, &n, input, &lda, workNorm.data());
        if (std::isnan(normA)) {
            return ArrayOf::doubleScalar(std::nan(""));
        }
        size_t nbElements = (size_t)m * (size_t)n;
        std::vector<double> factor(input, input + nbElements);
        int info = 0;
        std::vector<int> ipiv((size_t)std::min(m, n));
        LAPACK_dgetrf(&m, &n, factor.data(), &lda, ipiv.data(), &info);
        if (info < 0) {
            Error("Nelson:rcond:lapackError", "LAPACK_%s error code: %d.", "dgetrf", info);
        }
        info = 0;
        std::vector<double> work((size_t)4 * (size_t)n);
        std::vector<int> iwork((size_t)n);
        double res = 0.;
        LAPACK_dgecon(
            &norm, &n, factor.data(), &lda, &normA, &res, work.data(), iwork.data(), &info);
        if (res < DBL_EPSILON && precionWarning) {
            Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
        } else if (info < 0 && info != -5) {
            Error("Nelson:rcond:lapackError", "LAPACK_%s error code: %d.", "dgecon", info);
        }
        rcond = ArrayOf::doubleScalar(res);
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_DoubleComplex(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    auto* input = reinterpret_cast<const doublecomplex*>((const double*)A.dataPointer());
    if (A.isScalar()) {
        doublecomplex value = input[0];
        if (std::isnan(value.real()) || std::isnan(value.imag())) {
            rcond = ArrayOf::doubleScalar(std::nan(""));
        } else if (std::isinf(value.real()) || std::isinf(value.imag())
            || (value.real() == 0. && value.imag() == 0.)) {
            rcond = ArrayOf::doubleScalar(0.);
        } else {
            rcond = ArrayOf::doubleScalar(1.);
        }
    } else {
        char norm = '1';
        int m = static_cast<int>(A.rows());
        int n = static_cast<int>(A.columns());
        int lda = m;
        BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n), 4);
        std::vector<double> workNorm((size_t)std::max(1, m));
        double normA = LAPACK_zlange(&norm, &m, &n, input, &lda, workNorm.data());
        if (std::isnan(normA)) {
            return ArrayOf::doubleScalar(std::nan(""));
        }
        size_t nbElements = (size_t)m * (size_t)n;
        std::vector<doublecomplex> factor(input, input + nbElements);
        int info = 0;
        std::vector<int> ipiv((size_t)std::min(m, n));
        LAPACK_zgetrf(&m, &n, factor.data(), &lda, ipiv.data(), &info);
        if (info < 0) {
            Error("Nelson:rcond:lapackError", "LAPACK_%s error code: %d.", "zgetrf", info);
        }
        info = 0;
        std::vector<doublecomplex> work((size_t)4 * (size_t)n);
        std::vector<double> rwork((size_t)2 * (size_t)n);
        double res = 0.;
        LAPACK_zgecon(
            &norm, &n, factor.data(), &lda, &normA, &res, work.data(), rwork.data(), &info);
        if (res < DBL_EPSILON && precionWarning) {
            Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
        } else if (info < 0 && info != -5) {
            Error("Nelson:rcond:lapackError", "LAPACK_%s error code: %d.", "zgecon", info);
        }
        rcond = ArrayOf::doubleScalar(res);
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_Single(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    Dimensions dimsA = A.dimensions();
    const auto* input = (const single*)A.dataPointer();
    if (A.isScalar()) {
        single value = input[0];
        if (std::isnan(value)) {
            rcond = ArrayOf::singleScalar(std::nanf(""));
        } else if (std::isinf(value) || value == 0.) {
            rcond = ArrayOf::singleScalar(0.);
        } else {
            rcond = ArrayOf::singleScalar(1.);
        }
    } else {
        char norm = '1';
        int m = static_cast<int>(dimsA.rows());
        int n = static_cast<int>(dimsA.columns());
        int lda = m;
        BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n), 4);
        std::vector<single> workNorm((size_t)std::max(1, m));
        single normA = LAPACK_slange(&norm, &m, &n, input, &lda, workNorm.data());
        if (std::isnan(normA)) {
            return ArrayOf::singleScalar(std::nanf(""));
        }
        size_t nbElements = (size_t)m * (size_t)n;
        std::vector<single> factor(input, input + nbElements);
        int info = 0;
        std::vector<int> ipiv((size_t)std::min(m, n));
        LAPACK_sgetrf(&m, &n, factor.data(), &lda, ipiv.data(), &info);
        if (info < 0) {
            Error("Nelson:rcond:lapackError", "LAPACK_%s error code: %d.", "sgetrf", info);
        }
        info = 0;
        std::vector<single> work((size_t)4 * (size_t)n);
        std::vector<int> iwork((size_t)n);
        single res = 0.;
        LAPACK_sgecon(
            &norm, &n, factor.data(), &lda, &normA, &res, work.data(), iwork.data(), &info);
        if (res < FLT_EPSILON && precionWarning) {
            Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
        } else if (info < 0 && info != -5) {
            Error("Nelson:rcond:lapackError", "LAPACK_%s error code: %d.", "sgecon", info);
        }
        rcond = ArrayOf::singleScalar(res);
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_SingleComplex(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    Dimensions dimsA = A.dimensions();
    auto* input = reinterpret_cast<const singlecomplex*>((const single*)A.dataPointer());
    if (A.isScalar()) {
        singlecomplex value = input[0];
        if (std::isnan(value.real()) || std::isnan(value.imag())) {
            rcond = ArrayOf::singleScalar(std::nanf(""));
        } else if (std::isinf(value.real()) || std::isinf(value.imag())
            || (value.real() == 0. && value.imag() == 0.)) {
            rcond = ArrayOf::singleScalar(0.);
        } else {
            rcond = ArrayOf::singleScalar(1.);
        }
    } else {
        char norm = '1';
        int m = static_cast<int>(dimsA.rows());
        int n = static_cast<int>(dimsA.columns());
        int lda = m;
        BlasThreadScope blasThreadScope(useSingleThreadBlasForSmallMatrix(m, n), 4);
        std::vector<single> workNorm((size_t)std::max(1, m));
        single normA = LAPACK_clange(&norm, &m, &n, input, &lda, workNorm.data());
        if (std::isnan(normA)) {
            return ArrayOf::singleScalar(std::nanf(""));
        }
        size_t nbElements = (size_t)m * (size_t)n;
        std::vector<singlecomplex> factor(input, input + nbElements);
        int info = 0;
        std::vector<int> ipiv((size_t)std::min(m, n));
        LAPACK_cgetrf(&m, &n, factor.data(), &lda, ipiv.data(), &info);
        if (info < 0) {
            Error("Nelson:rcond:lapackError", "LAPACK_%s error code: %d.", "cgetrf", info);
        }
        info = 0;
        std::vector<singlecomplex> work((size_t)4 * (size_t)n);
        std::vector<single> rwork((size_t)2 * (size_t)n);
        single res = 0.;
        LAPACK_cgecon(
            &norm, &n, factor.data(), &lda, &normA, &res, work.data(), rwork.data(), &info);
        if (res < FLT_EPSILON && precionWarning) {
            Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
        } else if (info < 0 && info != -5) {
            Error("Nelson:rcond:lapackError", "LAPACK_%s error code: %d.", "cgecon", info);
        }
        rcond = ArrayOf::singleScalar(res);
    }
    return rcond;
}
//=============================================================================
ArrayOf
ReciprocalConditionNumber(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    bool isSupportedTypes = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE
                                || A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "rcond", ClassName(A));
    }
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
    if (A.isEmpty()) {
        if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
            rcond = ArrayOf::doubleScalar(std::numeric_limits<double>::infinity());
        } else {
            rcond = ArrayOf::singleScalar(std::numeric_limits<single>::infinity());
        }
    } else {
        if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
            if (A.dataClass() == NLS_DOUBLE) {
                rcond = ReciprocalConditionNumber_Double(A, precionWarning);
            } else {
                rcond = ReciprocalConditionNumber_DoubleComplex(A, precionWarning);
            }
        } else {
            if (A.dataClass() == NLS_SINGLE) {
                rcond = ReciprocalConditionNumber_Single(A, precionWarning);
            } else {
                rcond = ReciprocalConditionNumber_SingleComplex(A, precionWarning);
            }
        }
    }
    return rcond;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
