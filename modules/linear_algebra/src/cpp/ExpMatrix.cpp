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
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "ExpMatrix.hpp"
#include "ClassName.hpp"
#include "lapack_eigen.hpp"
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
            Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(),
                (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            Eigen::Map<Eigen::MatrixXd> matR((double*)R.getDataPointer(),
                (Eigen::Index)R.getDimensions().getRows(),
                (Eigen::Index)R.getDimensions().getColumns());
            if (!matA.allFinite()) {
                matR.setConstant(std::nan("NaN"));
            } else {
                matR = matA.exp();
            }
            return R;
        } else // NLS_DCOMPLEX
        {
            ArrayOf R(A);
            R.ensureSingleOwner();
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
            doublecomplex* Rz = reinterpret_cast<doublecomplex*>((double*)R.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            Eigen::Map<Eigen::MatrixXcd> matR(Rz, (Eigen::Index)R.getDimensions().getRows(),
                (Eigen::Index)R.getDimensions().getColumns());
            if (!matA.allFinite()) {
                doublecomplex cst(std::nan("NaN"), std::nan("NaN"));
                matR.setConstant(cst);
            } else {
                // [V, D] = eig(A);
                // expm = V * diag(exp(diag(D))) * inv(V);
                Eigen::ComplexEigenSolver<Eigen::MatrixXcd> es(matA);
                auto evects = es.eigenvectors();
                auto evals = es.eigenvalues();
                for (indexType i = 0; i < static_cast<indexType>(evals.rows()); ++i) {
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
    } else {
        if (A.getDataClass() == NLS_SINGLE) {
            ArrayOf R(A);
            R.ensureSingleOwner();
            Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(),
                (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            Eigen::Map<Eigen::MatrixXf> matR((single*)R.getDataPointer(),
                (Eigen::Index)R.getDimensions().getRows(),
                (Eigen::Index)R.getDimensions().getColumns());
            if (!matA.allFinite()) {
                matA.setConstant(std::nanf("NaN"));
            } else {
                matR = matA.exp();
            }
            return R;
        } else // NLS_SCOMPLEX
        {
            ArrayOf R(A);
            R.ensureSingleOwner();
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
            singlecomplex* Rz = reinterpret_cast<singlecomplex*>((single*)R.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            Eigen::Map<Eigen::MatrixXcf> matR(Rz, (Eigen::Index)R.getDimensions().getRows(),
                (Eigen::Index)R.getDimensions().getColumns());
            if (!matA.allFinite()) {
                singlecomplex cst(std::nanf("NaN"), std::nanf("NaN"));
                matR.setConstant(cst);
            } else {
                // [V, D] = eig(A);
                // expm = V * diag(exp(diag(D))) * inv(V);
                Eigen::ComplexEigenSolver<Eigen::MatrixXcf> es(matA);
                auto evects = es.eigenvectors();
                auto evals = es.eigenvalues();
                for (indexType i = 0; i < static_cast<indexType>(evals.rows()); ++i) {
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
    }
}
//=============================================================================
}
//=============================================================================
