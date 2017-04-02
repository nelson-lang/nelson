//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
//#include "lapack_eigen.hpp"
#include <Eigen/Dense>
#include <Eigen/SVD>
#include "SVD.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    void SVD(ArrayOf A, ArrayOf &s)
    {
    }
    //=============================================================================
    void SVD(ArrayOf A, ArrayOf &S, ArrayOf &V)
    {
    }
    //=============================================================================
    void SVD(ArrayOf A, ArrayOf &U, ArrayOf &S, ArrayOf &V)
    {
        bool isSupportedTypes = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE ||
                                 A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX) && !A.isSparse();
        if (!isSupportedTypes)
        {
            throw Exception(_("Undefined function 'svd' for input arguments of type") + " '" + ClassName(A) + "'.");
        }
        if (A.isEmpty())
        {
        }
        else
        {
            if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX)
            {
                if (A.getDataClass() == NLS_DOUBLE)
                {
                    Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
                    Dimensions dimsU(matA.rows(), matA.rows());
                    Dimensions dimsS(matA.rows(), matA.cols());
                    Dimensions dimsV(matA.cols(), matA.cols());
                    double *ptU = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsU.getElementCount());
                    double *ptS = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsS.getElementCount());
                    double *ptV = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsV.getElementCount());
                    Eigen::Map<Eigen::MatrixXd> matU(ptU, dimsU.getRows(), dimsU.getColumns());
                    Eigen::Map<Eigen::MatrixXd> matS(ptS, dimsS.getRows(), dimsS.getColumns());
                    Eigen::Map<Eigen::MatrixXd> matV(ptV, dimsV.getRows(), dimsV.getColumns());
                    Eigen::BDCSVD<Eigen::MatrixXd> svd(matA, Eigen::ComputeFullU | Eigen::ComputeFullV);
                    matU = svd.matrixU();
                    matV = svd.matrixV();
                    Eigen::MatrixXd matS1 = svd.singularValues();
                    size_t r = 0;
                    for (size_t c = 0; c < dimsS.getColumns(); c++)
                    {
                        if (r < matS1.size())
                        {
                            matS(r, c) = matS1(r);
                            r++;
                        }
                        else
                        {
                            break;
                        }
                    }
                    U = ArrayOf(NLS_DOUBLE, dimsU, ptU);
                    S = ArrayOf(NLS_DOUBLE, dimsS, ptS);
                    V = ArrayOf(NLS_DOUBLE, dimsV, ptV);
                }
            }
        }
    }
    //=============================================================================
}
//=============================================================================
