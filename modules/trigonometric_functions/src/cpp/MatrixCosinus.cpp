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
#include "MatrixCosinus.hpp"
#include "ClassName.hpp"
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
template <class T>
ArrayOf
cosmComplex(const ArrayOf& A)
{
    T* ptrR = (T*)ArrayOf::allocateArrayOf(A.getDataClass(), A.getLength(), stringVector(), false);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    std::complex<T>* Rz = reinterpret_cast<std::complex<T>*>((T*)ptrR);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(Az,
        (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(Rz,
        (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
    // [V, D] = eig(A);
    // cosm = V * diag(cos(diag(D))) * inv(V);
    Eigen::ComplexEigenSolver<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>>
        solver(matA.template cast<std::complex<T>>());
    auto evects = solver.eigenvectors();
    auto evals = solver.eigenvalues();
    for (indexType i = 0; i < static_cast<indexType>(evals.rows()); ++i) {
        evals(i) = cos(evals(i));
    }
    auto evalsdiag = evals.asDiagonal();
    matR = evects * evalsdiag * evects.inverse();
    return ArrayOf(A.getDataClass(), A.getDimensions(), ptrR);
}
//=============================================================================
ArrayOf
MatrixCos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return ArrayOf();
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf R = cosmComplex<single>(A);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.promoteType(NLS_SCOMPLEX);
        R = cosmComplex<single>(R);
        R.promoteType(NLS_SINGLE);
        return R;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf R = cosmComplex<double>(A);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.promoteType(NLS_DCOMPLEX);
        R = cosmComplex<double>(R);
        R.promoteType(NLS_DOUBLE);
        return R;
    } break;
    }
    return ArrayOf();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
