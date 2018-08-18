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
#include "LogMatrix.hpp"
#include "ClassName.hpp"
#include "lapack_eigen.hpp"
#include <unsupported/Eigen/MatrixFunctions>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
logmComplex(ArrayOf& A)
{
    ArrayOf R(A);
    R.ensureSingleOwner();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    std::complex<T>* Rz = reinterpret_cast<std::complex<T>*>((T*)R.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(Az,
        (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(Rz,
        (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
    if (!matA.allFinite()) {
        Error(_("Input must be finite."));
    } else {
        // [V, D] = eig(A);
        // sqrtm = V * diag(log(diag(D))) * inv(V);
        Eigen::ComplexEigenSolver<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>>
            solver(matA.template cast<std::complex<T>>());
        auto evects = solver.eigenvectors();
        auto evals = solver.eigenvalues();
        for (indexType i = 0; i < static_cast<indexType>(evals.rows()); ++i) {
            evals(i) = std::log(evals(i));
        }
        auto evalsdiag = evals.asDiagonal();
        matR = evects * evalsdiag * evects.inverse();
    }
    return R;
}
//=============================================================================
ArrayOf
LogMatrix(ArrayOf A)
{
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(_("Undefined function 'sqrtm' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        ArrayOf res(A);
        res.ensureSingleOwner();
        return res;
    }
    ArrayOf res;
    if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
        if (A.getDataClass() == NLS_DOUBLE) {
            A.promoteType(NLS_DCOMPLEX);
        }
        res = logmComplex<double>(A);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } else {
        if (A.getDataClass() == NLS_SINGLE) {
            A.promoteType(NLS_SCOMPLEX);
        }
        res = logmComplex<single>(A);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
