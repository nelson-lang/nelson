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
#include "SchurDecompostion.hpp"
#include "ClassName.hpp"
#include "lapack_eigen.hpp"
#include <Eigen/Eigenvalues>
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
    if (A.isEmpty()) {
        Dimensions dims = A.getDimensions();
        T = ArrayOf::emptyConstructor(dims);
        T.promoteType(A.getDataClass());
        return;
    }
    if (asComplex || A.isComplex()) {
        if (A.getDataClass() == NLS_SINGLE || A.getDataClass() == NLS_SCOMPLEX) {
            A.promoteType(NLS_SCOMPLEX);
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((double*)A.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            if (!matA.allFinite()) {
                Error(_("Input argument must not contain NaN or Inf."));
            }
            ArrayOf T_temp(A);
            T_temp.ensureSingleOwner();
            singlecomplex* Tz = reinterpret_cast<singlecomplex*>((double*)T_temp.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matT(Tz, (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            Eigen::ComplexSchur<Eigen::MatrixXcf> schur(matA);
            matT = schur.matrixT();
            T = T_temp;
        } else {
            A.promoteType(NLS_DCOMPLEX);
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            if (!matA.allFinite()) {
                Error(_("Input argument must not contain NaN or Inf."));
            }
            ArrayOf T_temp(A);
            T_temp.ensureSingleOwner();
            doublecomplex* Tz = reinterpret_cast<doublecomplex*>((double*)T_temp.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matT(Tz, (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            Eigen::ComplexSchur<Eigen::MatrixXcd> schur(matA);
            matT = schur.matrixT();
            T = T_temp;
        }
    } else {
        ArrayOf T_temp(A);
        T_temp.ensureSingleOwner();
        if (A.getDataClass() == NLS_SINGLE) {
            Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(),
                (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            if (!matA.allFinite()) {
                Error(_("Input argument must not contain NaN or Inf."));
            }
            Eigen::Map<Eigen::MatrixXf> matT((single*)T_temp.getDataPointer(),
                (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            Eigen::RealSchur<Eigen::MatrixXf> schur(matA);
            matT = schur.matrixT();
        } else {
            Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(),
                (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            if (!matA.allFinite()) {
                Error(_("Input argument must not contain NaN or Inf."));
            }
            Eigen::Map<Eigen::MatrixXd> matT((double*)T_temp.getDataPointer(),
                (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
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
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
            (Eigen::Index)A.getDimensions().getColumns());
        if (!matA.allFinite()) {
            Error(_("Input argument must not contain NaN or Inf."));
        }
        ArrayOf U_temp(A);
        U_temp.ensureSingleOwner();
        ArrayOf T_temp(A);
        T_temp.ensureSingleOwner();
        doublecomplex* Uz = reinterpret_cast<doublecomplex*>((double*)U_temp.getDataPointer());
        doublecomplex* Tz = reinterpret_cast<doublecomplex*>((double*)T_temp.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matU(Uz, (Eigen::Index)A.getDimensions().getRows(),
            (Eigen::Index)A.getDimensions().getColumns());
        Eigen::Map<Eigen::MatrixXcd> matT(Tz, (Eigen::Index)A.getDimensions().getRows(),
            (Eigen::Index)A.getDimensions().getColumns());
        Eigen::ComplexSchur<Eigen::MatrixXcd> schur(matA);
        matU = schur.matrixU();
        matT = schur.matrixT();
        U = U_temp;
        T = T_temp;
    } else {
        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(),
            (Eigen::Index)A.getDimensions().getRows(),
            (Eigen::Index)A.getDimensions().getColumns());
        if (!matA.allFinite()) {
            Error(_("Input argument must not contain NaN or Inf."));
        }
        ArrayOf U_temp(A);
        U_temp.ensureSingleOwner();
        ArrayOf T_temp(A);
        T_temp.ensureSingleOwner();
        Eigen::Map<Eigen::MatrixXd> matU((double*)U_temp.getDataPointer(),
            (Eigen::Index)A.getDimensions().getRows(),
            (Eigen::Index)A.getDimensions().getColumns());
        Eigen::Map<Eigen::MatrixXd> matT((double*)T_temp.getDataPointer(),
            (Eigen::Index)A.getDimensions().getRows(),
            (Eigen::Index)A.getDimensions().getColumns());
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
}
//=============================================================================
