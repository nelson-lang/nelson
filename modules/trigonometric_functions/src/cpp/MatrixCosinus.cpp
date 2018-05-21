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
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
#include "MatrixCosinus.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
    template <class T>
    ArrayOf cosmComplex(ArrayOf &A)
    {
        ArrayOf R(A);
        R.ensureSingleOwner();
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
        std::complex<T>* Rz = reinterpret_cast<std::complex<T>*>((T*)R.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(Az, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
        Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(Rz, (Eigen::Index)R.getDimensions().getRows(), (Eigen::Index)R.getDimensions().getColumns());
        // [V, D] = eig(A);
        // cosm = V * diag(cos(diag(D))) * inv(V);
        Eigen::ComplexEigenSolver<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> solver(matA.template cast<std::complex<T>>());
        auto evects = solver.eigenvectors();
        auto evals = solver.eigenvalues();
        for (indexType i = 0; i < static_cast<indexType>(evals.rows()); ++i)
        {
            evals(i) = cos(evals(i));
        }
        auto evalsdiag = evals.asDiagonal();
        matR = evects * evalsdiag * evects.inverse();
        return R;
    }
    //=============================================================================
    ArrayOf MatrixCos(ArrayOf A)
    {
        if (!A.isSquare())
        {
            throw Exception(_("Square matrix expected."));
        }
        if (A.isEmpty())
        {
            ArrayOf R(A);
            R.ensureSingleOwner();
            return R;
        }
        if (A.isSparse())
        {
            throw Exception(_("Undefined function 'cosm' for input arguments of type") + " '" + ClassName(A) + "'.");
        }
        switch (A.getDataClass())
        {
            case NLS_CELL_ARRAY:
            case NLS_STRUCT_ARRAY:
            case NLS_LOGICAL:
            case NLS_UINT8:
            case NLS_INT8:
            case NLS_UINT16:
            case NLS_INT16:
            case NLS_UINT32:
            case NLS_INT32:
            case NLS_UINT64:
            case NLS_INT64:
            case NLS_CHAR:
            {
                throw Exception(_("Undefined function 'cosm' for input arguments of type") + " '" + ClassName(A) + "'.");
            }
            break;
            case NLS_SCOMPLEX:
            {
                ArrayOf R = cosmComplex<single>(A);
                if (R.allReal())
                {
                    R.promoteType(NLS_SINGLE);
                }
                return R;
            }
            case NLS_SINGLE:
            {
                A.promoteType(NLS_SCOMPLEX);
                ArrayOf R = cosmComplex<single>(A);
                R.promoteType(NLS_SINGLE);
                return R;
            }
            break;
            case NLS_DCOMPLEX:
            {
                ArrayOf R = cosmComplex<double>(A);
                if (R.allReal())
                {
                    R.promoteType(NLS_DOUBLE);
                }
                return R;
            }
            break;
            case NLS_DOUBLE:
            {
                A.promoteType(NLS_DCOMPLEX);
                ArrayOf R = cosmComplex<double>(A);
                R.promoteType(NLS_DOUBLE);
                return R;
            }
            break;
            default:
            {
                throw Exception(_W("Invalid conversion."));
            }
            break;
        }
        return ArrayOf();
    }
    //=============================================================================
}
//=============================================================================
