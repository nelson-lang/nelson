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
#include "lapack_eigen_config.hpp"
#include "ExpMatrix.hpp"
#include "ClassName.hpp"
#include <unsupported/Eigen/MatrixFunctions>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ExpMatrix(ArrayOf A)
{
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(_("Undefined function 'expm' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        ArrayOf RES(A);
        RES.ensureSingleOwner();
        return RES;
    }
    if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
        if (A.getDataClass() == NLS_DOUBLE) {
            ArrayOf R(A);
            R.ensureSingleOwner();
            Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)A.getRows(),
                (Eigen::Index)A.getColumns());
            Eigen::Map<Eigen::MatrixXd> matR((double*)R.getDataPointer(), (Eigen::Index)R.getRows(),
                (Eigen::Index)R.getColumns());
            if (!matA.allFinite()) {
                matR.setConstant(std::nan("NaN"));
            } else {
                matR = matA.exp();
            }
            return R;
        } // NLS_DCOMPLEX

        ArrayOf R(A);
        R.ensureSingleOwner();
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        auto* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(
            Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        Eigen::Map<Eigen::MatrixXcd> matR(
            Rz, (Eigen::Index)R.getRows(), (Eigen::Index)R.getColumns());
        if (!matA.allFinite()) {
            doublecomplex cst(std::nan("NaN"), std::nan("NaN"));
            matR.setConstant(cst);
        } else {
            // [V, D] = eig(A);
            // expm = V * diag(exp(diag(D))) * inv(V);
            Eigen::ComplexEigenSolver<Eigen::MatrixXcd> es(matA);
            auto evects = es.eigenvectors();
            auto evals = es.eigenvalues();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < static_cast<ompIndexType>(evals.rows()); ++i) {
                evals(i) = std::exp(evals(i));
            }
            auto evalsdiag = evals.asDiagonal();
            matR = evects * evalsdiag * evects.inverse();
        }
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    }
    if (A.getDataClass() == NLS_SINGLE) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        Eigen::Map<Eigen::MatrixXf> matA(
            (single*)A.getDataPointer(), (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        Eigen::Map<Eigen::MatrixXf> matR(
            (single*)R.getDataPointer(), (Eigen::Index)R.getRows(), (Eigen::Index)R.getColumns());
        if (!matA.allFinite()) {
            matA.setConstant(std::nanf("NaN"));
        } else {
            matR = matA.exp();
        }
        return R;
    } // NLS_SCOMPLEX

    ArrayOf R(A);
    R.ensureSingleOwner();
    auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
    auto* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    Eigen::Map<Eigen::MatrixXcf> matR(Rz, (Eigen::Index)R.getRows(), (Eigen::Index)R.getColumns());
    if (!matA.allFinite()) {
        singlecomplex cst(std::nanf("NaN"), std::nanf("NaN"));
        matR.setConstant(cst);
    } else {
        // [V, D] = eig(A);
        // expm = V * diag(exp(diag(D))) * inv(V);
        Eigen::ComplexEigenSolver<Eigen::MatrixXcf> es(matA);
        auto evects = es.eigenvectors();
        auto evals = es.eigenvalues();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(evals.rows()); ++i) {
            evals(i) = std::exp(evals(i));
        }
        auto evalsdiag = evals.asDiagonal();
        matR = evects * evalsdiag * evects.inverse();
    }
    if (R.allReal()) {
        R.promoteType(NLS_SINGLE);
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
