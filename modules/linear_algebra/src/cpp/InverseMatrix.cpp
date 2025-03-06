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
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#undef EIGEN_USE_MKL_VML
#undef EIGEN_USE_MKL
#include <Eigen/Dense>
#include <Eigen/src/misc/lapacke.h>
#include "InverseMatrix.hpp"
#include "ClassName.hpp"
#include "ReciprocalConditionNumber.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
InverseDouble(const ArrayOf& A, double rcond)
{
    ArrayOf res = A;
    res.ensureSingleOwner();
    double* ptrD = (double*)res.getDataPointer();
    Eigen::Map<Eigen::MatrixXd> matA(
        (double*)A.getDataPointer(), (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    if (matA.isDiagonal()) {
        if (std::isnan(rcond)) {
            Eigen::Map<Eigen::MatrixXd> matR(
                ptrD, (Eigen::Index)res.getRows(), (Eigen::Index)res.getColumns());
            matR.setConstant(std::nan("NaN"));
            return res;
        }
        int N = (int)A.getColumns();
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType k = 0; k < (ompIndexType)N; k++) {
            ptrD[k + (k * N)] = 1 / ptrD[k + (k * N)];
        }
    } else {
        int N = (int)A.getColumns();
        int* IPIV = new_with_exception<int>(N);
        int LWORK = std::max(1, 4 * N);
        int INFO = 0;

        LAPACK_dgetrf(&N, &N, ptrD, &N, IPIV, &INFO);
        if (INFO == 0) {
            double* WORK = new_with_exception<double>(LWORK);
            LAPACK_dgetri(&N, ptrD, &N, IPIV, WORK, &LWORK, &INFO);
            delete[] WORK;
            delete[] IPIV;
        } else {
            delete[] IPIV;
            Eigen::Map<Eigen::MatrixXd> matR(
                ptrD, (Eigen::Index)res.getRows(), (Eigen::Index)res.getColumns());
            if (rcond == 0) {
                matR.setConstant(std::numeric_limits<double>::infinity());
            } else {
                matR = matA.inverse();
            }
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
InverseDoubleComplex(ArrayOf A, double rcond)
{
    ArrayOf res(A);
    res.ensureSingleOwner();
    auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
    auto* Rz = reinterpret_cast<doublecomplex*>((double*)res.getDataPointer());
    Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    Eigen::Map<Eigen::MatrixXcd> matR(
        Rz, (Eigen::Index)res.getRows(), (Eigen::Index)res.getColumns());
    if (matA.isDiagonal()) {
        if (std::isnan(rcond)) {
            doublecomplex cst(std::nan("NaN"), std::nan("NaN"));
            matR.setConstant(cst);
            return res;
        }
        int N = (int)A.getColumns();
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType k = 0; k < (ompIndexType)N; k++) {
            Rz[k + (k * N)] = std::complex<double>(1, 0) / Rz[k + (k * N)];
        }
    } else {
        int N = (int)A.getColumns();
        int* IPIV = new_with_exception<int>(N);
        int LWORK = std::max(1, 4 * N);
        int INFO = 0;
        LAPACK_zgetrf(&N, &N, Rz, &N, IPIV, &INFO);
        if (INFO == 0) {
            doublecomplex* WORK = new_with_exception<doublecomplex>(LWORK);
            LAPACK_zgetri(&N, Rz, &N, IPIV, WORK, &LWORK, &INFO);
            delete[] WORK;
            delete[] IPIV;
        } else {
            delete[] IPIV;
            if (rcond == 0) {
                doublecomplex cst(std::numeric_limits<double>::infinity(), 0);
                matR.setConstant(cst);
            } else {
                matR = matA.inverse();
            }
        }
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
InverseSingle(const ArrayOf& A, single rcond)
{
    ArrayOf res = A;
    res.ensureSingleOwner();
    single* ptrD = (single*)res.getDataPointer();
    Eigen::Map<Eigen::MatrixXf> matA(
        (single*)A.getDataPointer(), (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    if (matA.isDiagonal()) {
        if (std::isnan(rcond)) {
            Eigen::Map<Eigen::MatrixXf> matR(
                ptrD, (Eigen::Index)res.getRows(), (Eigen::Index)res.getColumns());
            matR.setConstant(std::nanf("NaN"));
            return res;
        }
        int N = (int)A.getColumns();
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType k = 0; k < (ompIndexType)N; k++) {
            ptrD[k + (k * N)] = 1 / ptrD[k + (k * N)];
        }
    } else {
        int N = (int)A.getColumns();
        int* IPIV = new_with_exception<int>(N);
        int LWORK = std::max(1, 4 * N);
        int INFO = 0;

        LAPACK_sgetrf(&N, &N, ptrD, &N, IPIV, &INFO);
        if (INFO == 0) {
            single* WORK = new_with_exception<single>(LWORK);
            LAPACK_sgetri(&N, ptrD, &N, IPIV, WORK, &LWORK, &INFO);
            delete[] WORK;
            delete[] IPIV;
        } else {
            delete[] IPIV;
            Eigen::Map<Eigen::MatrixXf> matR(
                ptrD, (Eigen::Index)res.getRows(), (Eigen::Index)res.getColumns());
            if (rcond == 0) {
                matR.setConstant(std::numeric_limits<single>::infinity());
            } else {
                matR = matA.inverse();
            }
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
InverseSingleComplex(ArrayOf A, single rcond)
{
    ArrayOf res(A);
    res.ensureSingleOwner();
    auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
    auto* Rz = reinterpret_cast<singlecomplex*>((single*)res.getDataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    Eigen::Map<Eigen::MatrixXcf> matR(
        Rz, (Eigen::Index)res.getRows(), (Eigen::Index)res.getColumns());
    if (matA.isDiagonal()) {
        if (std::isnan(rcond)) {
            singlecomplex cst(std::nanf("NaN"), std::nanf("NaN"));
            matR.setConstant(cst);
            return res;
        }
        int N = (int)A.getColumns();
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType k = 0; k < (ompIndexType)N; k++) {
            Rz[k + (k * N)] = std::complex<single>(1, 0) / Rz[k + (k * N)];
        }
    } else {
        int N = (int)A.getColumns();
        int* IPIV = new_with_exception<int>(N);
        int LWORK = std::max(1, 4 * N);
        int INFO = 0;
        LAPACK_cgetrf(&N, &N, Rz, &N, IPIV, &INFO);
        if (INFO == 0) {
            singlecomplex* WORK = new_with_exception<singlecomplex>(LWORK);
            LAPACK_cgetri(&N, Rz, &N, IPIV, WORK, &LWORK, &INFO);
            delete[] WORK;
            delete[] IPIV;
        } else {
            delete[] IPIV;
            if (rcond == 0) {
                singlecomplex cst(std::numeric_limits<single>::infinity(), 0);
                matR.setConstant(cst);
            } else {
                matR = matA.inverse();
            }
        }
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    }
    return res;
}
//=============================================================================
ArrayOf
InverseMatrix(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        needToOverload = true;
        return {};
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        ArrayOf RES(A);
        RES.ensureSingleOwner();
        return RES;
    }
    ArrayOf rCondArray = ReciprocalConditionNumber(A, true);
    double rcond = rCondArray.getContentAsDoubleScalar();
    ArrayOf R;
    if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
        if (A.getDataClass() == NLS_DOUBLE) {
            R = InverseDouble(A, rcond);
        } else {
            R = InverseDoubleComplex(A, rcond);
        }
    } else {
        if (A.getDataClass() == NLS_SINGLE) {
            R = InverseSingle(A, static_cast<single>(rcond));
        } else {
            R = InverseSingleComplex(A, static_cast<single>(rcond));
        }
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
