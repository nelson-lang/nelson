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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <cfloat>
#include "lapack_eigen_config.hpp"
#undef EIGEN_USE_MKL
#undef EIGEN_USE_MKL_VML
#include <Eigen/src/misc/lapacke.h>
#include <Eigen/Dense>
#include "ReciprocalConditionNumber.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Warning.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_Double(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    Dimensions dimsA = A.getDimensions();
    Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)dimsA.getRows(),
        (Eigen::Index)dimsA.getColumns());
    if (matA.hasNaN()) {
        rcond = ArrayOf::doubleConstructor(std::nan(""));
    } else {
        if (A.isScalar()) {
            if (std::isinf(matA(0))) {
                rcond = ArrayOf::doubleConstructor(0.);
            } else {
                if (matA(0) == 0.) {
                    rcond = ArrayOf::doubleConstructor(0.);
                } else {
                    rcond = ArrayOf::doubleConstructor(1.);
                }
            }
        } else {
            // need to copy A because LAPACK_dgetrf modify values.
            ArrayOf R(A);
            R.ensureSingleOwner();
            double normA = 0;
            char norm = '1';
            int m = static_cast<int>(dimsA.getRows());
            int n = static_cast<int>(dimsA.getColumns());
            int lda = m;
            normA = LAPACKE_dlange(
                LAPACK_COL_MAJOR, norm, m, n, (const double*)R.getDataPointer(), lda);
            int info = 0;
            int* ipiv = new_with_exception<int>(std::min(m, n), false);
            LAPACK_dgetrf(&m, &n, (double*)R.getDataPointer(), &lda, ipiv, &info);
            delete[] ipiv;
            ipiv = nullptr;
            if (info < 0) {
                Error(fmt::sprintf(_("LAPACK_dgetrf error code: %d."), info));
            }
            info = 0;
            double* work = new_with_exception<double>(4 * n, false);
            int* iwork = new_with_exception<int>(n, false);
            double res = 0.;
            LAPACK_dgecon(&norm, &n, (const double*)R.getDataPointer(), &lda, &normA, &res, work,
                iwork, &info);
            delete[] iwork;
            iwork = nullptr;
            delete[] work;
            work = nullptr;
            if (res < DBL_EPSILON && precionWarning) {
                Warning("Nelson:singularMatrix", _("Matrix is singular to working precision."));
            } else if (info < 0 && info != -5) {
                Error(fmt::sprintf(_("LAPACK_dgecon error code: %d."), info));
            }
            rcond = ArrayOf::doubleConstructor(res);
        }
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_DoubleComplex(ArrayOf A, bool precionWarning)
{
    ArrayOf rcond;
    auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
    Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    if (matA.hasNaN()) {
        rcond = ArrayOf::doubleConstructor(std::nan(""));
    } else {
        if (A.isScalar()) {
            doublecomplex dcplx = matA(0);
            if (std::isinf(dcplx.real()) || std::isinf(dcplx.imag())) {
                rcond = ArrayOf::doubleConstructor(0.);
            } else {
                if (dcplx.real() == 0. && dcplx.imag() == 0.) {
                    rcond = ArrayOf::doubleConstructor(0.);
                } else {
                    rcond = ArrayOf::doubleConstructor(1.);
                }
            }
        } else {
            ArrayOf R(A);
            R.ensureSingleOwner();
            double normA = 0;
            char norm = '1';
            int m = static_cast<int>(R.getRows());
            int n = static_cast<int>(R.getColumns());
            int lda = m;
            auto* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
            normA = LAPACKE_zlange(LAPACK_COL_MAJOR, norm, m, n, Rz, lda);
            int info = 0;
            int* ipiv = new_with_exception<int>(std::min(m, n), false);
            LAPACK_zgetrf(&m, &n, Rz, &lda, ipiv, &info);
            delete[] ipiv;
            ipiv = nullptr;
            if (info < 0) {
                Error(fmt::sprintf(_("LAPACK_zgetrf error code: %d."), info));
            }
            info = 0;
            doublecomplex* work = new_with_exception<doublecomplex>(4 * n, false);
            double* rwork = new_with_exception<double>(2 * n, false);
            double res = 0.;
            LAPACK_zgecon(&norm, &n, Rz, &lda, &normA, &res, work, rwork, &info);
            delete[] rwork;
            rwork = nullptr;
            delete[] work;
            work = nullptr;
            if (res < DBL_EPSILON && precionWarning) {
                Warning("Nelson:singularMatrix", _("Matrix is singular to working precision."));
            } else if (info < 0 && info != -5) {
                Error(fmt::sprintf(_("LAPACK_zgecon error code: %d."), info));
            }
            rcond = ArrayOf::doubleConstructor(res);
        }
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_Single(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    Dimensions dimsA = A.getDimensions();
    Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), (Eigen::Index)dimsA.getRows(),
        (Eigen::Index)dimsA.getColumns());
    if (matA.hasNaN()) {
        rcond = ArrayOf::singleConstructor(std::nanf(""));
    } else {
        if (A.isScalar()) {
            if (std::isinf(matA(0))) {
                rcond = ArrayOf::singleConstructor(0.);
            } else {
                if (matA(0) == 0.) {
                    rcond = ArrayOf::singleConstructor(0.);
                } else {
                    rcond = ArrayOf::singleConstructor(1.);
                }
            }
        } else {
            ArrayOf R(A);
            R.ensureSingleOwner();
            single normA = 0;
            char norm = '1';
            int m = static_cast<int>(dimsA.getRows());
            int n = static_cast<int>(dimsA.getColumns());
            int lda = m;
            normA = LAPACKE_slange(
                LAPACK_COL_MAJOR, norm, m, n, (const single*)R.getDataPointer(), lda);
            int info = 0;
            int* ipiv = new_with_exception<int>(std::min(m, n), false);
            LAPACK_sgetrf(&m, &n, (single*)R.getDataPointer(), &lda, ipiv, &info);
            delete[] ipiv;
            ipiv = nullptr;
            if (info < 0) {
                Error(fmt::sprintf(_("LAPACK_sgetrf error code: %d."), info));
            }
            info = 0;
            single* work = new_with_exception<single>(4 * n, false);
            int* iwork = new_with_exception<int>(n, false);
            single res = 0.;
            LAPACK_sgecon(
                &norm, &n, (single*)R.getDataPointer(), &lda, &normA, &res, work, iwork, &info);
            delete[] iwork;
            iwork = nullptr;
            delete[] work;
            work = nullptr;
            if (res < FLT_EPSILON && precionWarning) {
                Warning("Nelson:singularMatrix", _("Matrix is singular to working precision."));
            } else if (info < 0 && info != -5) {
                Error(fmt::sprintf(_("LAPACK_sgecon error code: %d."), info));
            }
            rcond = ArrayOf::singleConstructor(res);
        }
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_SingleComplex(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    Dimensions dimsA = A.getDimensions();
    auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(
        Az, (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
    if (matA.hasNaN()) {
        rcond = ArrayOf::singleConstructor(std::nanf(""));
    } else {
        if (A.isScalar()) {
            singlecomplex scplx = matA(0);
            if (std::isinf(scplx.real()) || std::isinf(scplx.imag())) {
                rcond = ArrayOf::singleConstructor(0.);
            } else {
                if (scplx.real() == 0. && scplx.imag() == 0.) {
                    rcond = ArrayOf::singleConstructor(0.);
                } else {
                    rcond = ArrayOf::singleConstructor(1.);
                }
            }
        } else {
            ArrayOf R(A);
            R.ensureSingleOwner();
            auto* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
            single normA = 0;
            char norm = '1';
            int m = static_cast<int>(dimsA.getRows());
            int n = static_cast<int>(dimsA.getColumns());
            int lda = m;
            normA = LAPACKE_clange(LAPACK_COL_MAJOR, norm, m, n, Rz, lda);
            int info = 0;
            int* ipiv = new_with_exception<int>(std::min(m, n), false);
            LAPACK_cgetrf(&m, &n, Rz, &lda, ipiv, &info);
            delete[] ipiv;
            ipiv = nullptr;
            if (info < 0) {
                Error(fmt::sprintf(_("LAPACK_cgetrf error code: %d."), info));
            }
            info = 0;
            singlecomplex* work = new_with_exception<singlecomplex>(4 * n, false);
            single* rwork = new_with_exception<single>(2 * n, false);
            single res = 0.;
            LAPACK_cgecon(&norm, &n, Rz, &lda, &normA, &res, work, rwork, &info);
            delete[] rwork;
            rwork = nullptr;
            delete[] work;
            work = nullptr;
            if (res < FLT_EPSILON && precionWarning) {
                Warning("Nelson:singularMatrix", _("Matrix is singular to working precision."));
            } else if (info < 0 && info != -5) {
                Error(fmt::sprintf(_("LAPACK_cgecon error code: %d."), info));
            }
            rcond = ArrayOf::singleConstructor(res);
        }
    }
    return rcond;
}
//=============================================================================
ArrayOf
ReciprocalConditionNumber(const ArrayOf& A, bool precionWarning)
{
    ArrayOf rcond;
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(_("Undefined function 'rcond' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
            rcond = ArrayOf::doubleConstructor(std::numeric_limits<double>::infinity());
        } else {
            rcond = ArrayOf::singleConstructor(std::numeric_limits<single>::infinity());
        }
    } else {
        if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
            if (A.getDataClass() == NLS_DOUBLE) {
                rcond = ReciprocalConditionNumber_Double(A, precionWarning);
            } else {
                rcond = ReciprocalConditionNumber_DoubleComplex(A, precionWarning);
            }
        } else {
            if (A.getDataClass() == NLS_SINGLE) {
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
