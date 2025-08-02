//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SchurDecompostion.hpp"
#include "ClassName.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Eigenvalues>
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
SchurDecomposition(ArrayOf A, bool asComplex, ArrayOf& T)
{
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(_("Undefined function 'schur' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    Dimensions dimsA = A.getDimensions();
    if (A.isEmpty()) {
        T = ArrayOf::emptyConstructor(dimsA);
        T.promoteType(A.getDataClass());
        return;
    }
    if (asComplex || A.isComplex()) {
        if (A.getDataClass() == NLS_SINGLE || A.getDataClass() == NLS_SCOMPLEX) {
            A.promoteType(NLS_SCOMPLEX);
            auto* Az = reinterpret_cast<singlecomplex*>((double*)A.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matA(
                Az, (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
            if (!matA.allFinite()) {
                Error(_("Input argument must not contain NaN or Inf."));
            }
            ArrayOf T_temp(A);
            T_temp.ensureSingleOwner();
            auto* Tz = reinterpret_cast<singlecomplex*>((double*)T_temp.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matT(
                Tz, (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
            Eigen::ComplexSchur<Eigen::MatrixXcf> schur(matA);
            matT = schur.matrixT();
            T = T_temp;
        } else {
            A.promoteType(NLS_DCOMPLEX);
            auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matA(
                Az, (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
            if (!matA.allFinite()) {
                Error(_("Input argument must not contain NaN or Inf."));
            }
            ArrayOf T_temp(A);
            T_temp.ensureSingleOwner();
            auto* Tz = reinterpret_cast<doublecomplex*>((double*)T_temp.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matT(
                Tz, (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
            Eigen::ComplexSchur<Eigen::MatrixXcd> schur(matA);
            matT = schur.matrixT();
            T = T_temp;
        }
    } else {
        ArrayOf T_temp(A);
        T_temp.ensureSingleOwner();
        if (A.getDataClass() == NLS_SINGLE) {
            Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(),
                (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
            if (!matA.allFinite()) {
                Error(_("Input argument must not contain NaN or Inf."));
            }
            Eigen::Map<Eigen::MatrixXf> matT((single*)T_temp.getDataPointer(),
                (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
            Eigen::RealSchur<Eigen::MatrixXf> schur(matA);
            matT = schur.matrixT();
        } else {
            Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(),
                (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
            if (!matA.allFinite()) {
                Error(_("Input argument must not contain NaN or Inf."));
            }
            Eigen::Map<Eigen::MatrixXd> matT((double*)T_temp.getDataPointer(),
                (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
            Eigen::RealSchur<Eigen::MatrixXd> schur(matA);
            matT = schur.matrixT();
        }
        T = T_temp;
    }
    if (T.allReal()) {
        if (T.getDataClass() == NLS_SINGLE || T.getDataClass() == NLS_SCOMPLEX) {
            T.promoteType(NLS_SINGLE);
        } else {
            T.promoteType(NLS_DOUBLE);
        }
    }
}
//=============================================================================
void
SchurDecomposition(ArrayOf A, bool asComplex, ArrayOf& U, ArrayOf& T)
{
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(_("Undefined function 'schur' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        Dimensions dimsA = A.getDimensions();
        U = ArrayOf::emptyConstructor(dimsA);
        U.promoteType(A.getDataClass());
        T = ArrayOf::emptyConstructor(dimsA);
        T.promoteType(A.getDataClass());
        return;
    }
    if (asComplex || A.isComplex()) {
        A.promoteType(NLS_DCOMPLEX);
        auto* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(
            Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        if (!matA.allFinite()) {
            Error(_("Input argument must not contain NaN or Inf."));
        }
        ArrayOf U_temp(A);
        U_temp.ensureSingleOwner();
        ArrayOf T_temp(A);
        T_temp.ensureSingleOwner();
        auto* Uz = reinterpret_cast<doublecomplex*>((double*)U_temp.getDataPointer());
        auto* Tz = reinterpret_cast<doublecomplex*>((double*)T_temp.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matU(
            Uz, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        Eigen::Map<Eigen::MatrixXcd> matT(
            Tz, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        Eigen::ComplexSchur<Eigen::MatrixXcd> schur(matA);
        matU = schur.matrixU();
        matT = schur.matrixT();
        U = U_temp;
        T = T_temp;
    } else {
        Eigen::Map<Eigen::MatrixXd> matA(
            (double*)A.getDataPointer(), (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        if (!matA.allFinite()) {
            Error(_("Input argument must not contain NaN or Inf."));
        }
        ArrayOf U_temp(A);
        U_temp.ensureSingleOwner();
        ArrayOf T_temp(A);
        T_temp.ensureSingleOwner();
        Eigen::Map<Eigen::MatrixXd> matU((double*)U_temp.getDataPointer(),
            (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        Eigen::Map<Eigen::MatrixXd> matT((double*)T_temp.getDataPointer(),
            (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        Eigen::RealSchur<Eigen::MatrixXd> schur(matA);
        matU = schur.matrixU();
        matT = schur.matrixT();
        U = U_temp;
        T = T_temp;
    }
    if (U.allReal()) {
        if (U.getDataClass() == NLS_SINGLE || U.getDataClass() == NLS_SCOMPLEX) {
            U.promoteType(NLS_SINGLE);
        } else {
            U.promoteType(NLS_DOUBLE);
        }
    }
    if (T.allReal()) {
        if (T.getDataClass() == NLS_SINGLE || T.getDataClass() == NLS_SCOMPLEX) {
            T.promoteType(NLS_SINGLE);
        } else {
            T.promoteType(NLS_DOUBLE);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
