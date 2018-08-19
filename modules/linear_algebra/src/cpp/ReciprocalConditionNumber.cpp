//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "ReciprocalConditionNumber.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
#include <Eigen/src/misc/lapacke.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_Double(const ArrayOf A)
{
    ArrayOf rcond;
    Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(),
        (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
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
            int m = (int)A.getDimensions().getRows();
            int n = (int)A.getDimensions().getColumns();
            int lda = m;
            normA = LAPACKE_dlange(
                LAPACK_COL_MAJOR, norm, m, n, (const double*)R.getDataPointer(), lda);
            int info = 0;
            int* ipiv = new_with_exception<int>(std::min(m, n));
            LAPACK_dgetrf(&m, &n, (double*)R.getDataPointer(), &lda, ipiv, &info);
            delete[] ipiv;
            ipiv = nullptr;
            if (info < 0) {
                Error(_("LAPACK_dgetrf error."));
            }
            info = 0;
            double* work = new_with_exception<double>(4 * n);
            int* iwork = new_with_exception<int>(n);
            double res = 0.;
            LAPACK_dgecon(&norm, &n, (const double*)R.getDataPointer(), &lda, &normA, &res, work,
                iwork, &info);
            delete[] iwork;
            iwork = nullptr;
            delete[] work;
            work = nullptr;
            if (info < 0) {
                Error(_("LAPACK_dgecon error."));
            }
            rcond = ArrayOf::doubleConstructor(res);
        }
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_DoubleComplex(ArrayOf A)
{
    ArrayOf rcond;
    doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
    Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
        (Eigen::Index)A.getDimensions().getColumns());
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
            int m = (int)R.getDimensions().getRows();
            int n = (int)R.getDimensions().getColumns();
            int lda = m;
            doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
            normA = LAPACKE_zlange(LAPACK_COL_MAJOR, norm, m, n, Rz, lda);
            int info = 0;
            int* ipiv = new_with_exception<int>(std::min(m, n));
            LAPACK_zgetrf(&m, &n, Rz, &lda, ipiv, &info);
            delete[] ipiv;
            ipiv = nullptr;
            if (info < 0) {
                Error(_("LAPACK_zgetrf error."));
            }
            info = 0;
            doublecomplex* work = new_with_exception<doublecomplex>(4 * n);
            double* rwork = new_with_exception<double>(2 * n);
            double res = 0.;
            LAPACK_zgecon(&norm, &n, Rz, &lda, &normA, &res, work, rwork, &info);
            delete[] rwork;
            rwork = nullptr;
            delete[] work;
            work = nullptr;
            if (info < 0) {
                Error(_("LAPACK_zgecon error."));
            }
            rcond = ArrayOf::doubleConstructor(res);
        }
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_Single(ArrayOf A)
{
    ArrayOf rcond;
    Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(),
        (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
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
            int m = (int)A.getDimensions().getRows();
            int n = (int)A.getDimensions().getColumns();
            int lda = m;
            normA = LAPACKE_slange(
                LAPACK_COL_MAJOR, norm, m, n, (const single*)R.getDataPointer(), lda);
            int info = 0;
            int* ipiv = new_with_exception<int>(std::min(m, n));
            LAPACK_sgetrf(&m, &n, (single*)R.getDataPointer(), &lda, ipiv, &info);
            delete[] ipiv;
            ipiv = nullptr;
            if (info < 0) {
                Error(_("LAPACK_sgetrf error."));
            }
            info = 0;
            single* work = new_with_exception<single>(4 * n);
            int* iwork = new_with_exception<int>(n);
            single res = 0.;
            LAPACK_sgecon(
                &norm, &n, (single*)R.getDataPointer(), &lda, &normA, &res, work, iwork, &info);
            delete[] iwork;
            iwork = nullptr;
            delete[] work;
            work = nullptr;
            if (info < 0) {
                Error(_("LAPACK_sgecon error."));
            }
            rcond = ArrayOf::singleConstructor(res);
        }
    }
    return rcond;
}
//=============================================================================
static ArrayOf
ReciprocalConditionNumber_SingleComplex(ArrayOf A)
{
    ArrayOf rcond;
    singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
        (Eigen::Index)A.getDimensions().getColumns());
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
            singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
            single normA = 0;
            char norm = '1';
            int m = (int)R.getDimensions().getRows();
            int n = (int)R.getDimensions().getColumns();
            int lda = m;
            normA = LAPACKE_clange(LAPACK_COL_MAJOR, norm, m, n, Rz, lda);
            int info = 0;
            int* ipiv = new_with_exception<int>(std::min(m, n));
            LAPACK_cgetrf(&m, &n, Rz, &lda, ipiv, &info);
            delete[] ipiv;
            ipiv = nullptr;
            if (info < 0) {
                Error(_("LAPACK_cgetrf error."));
            }
            info = 0;
            singlecomplex* work = new_with_exception<singlecomplex>(4 * n);
            single* rwork = new_with_exception<single>(2 * n);
            single res = 0.;
            LAPACK_cgecon(&norm, &n, Rz, &lda, &normA, &res, work, rwork, &info);
            delete[] rwork;
            rwork = nullptr;
            delete[] work;
            work = nullptr;
            if (info < 0) {
                Error(_("LAPACK_cgecon error."));
            }
            rcond = ArrayOf::singleConstructor(res);
        }
    }
    return rcond;
}
//=============================================================================
ArrayOf
ReciprocalConditionNumber(ArrayOf A)
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
                rcond = ReciprocalConditionNumber_Double(A);
            } else {
                rcond = ReciprocalConditionNumber_DoubleComplex(A);
            }
        } else {
            if (A.getDataClass() == NLS_SINGLE) {
                rcond = ReciprocalConditionNumber_Single(A);
            } else {
                rcond = ReciprocalConditionNumber_SingleComplex(A);
            }
        }
    }
    return rcond;
}
//=============================================================================
}
//=============================================================================
