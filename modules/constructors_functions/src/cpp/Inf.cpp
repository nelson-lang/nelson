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
#include <Eigen/Dense>
#include "Inf.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf Inf(void)
    {
        return Inf(1, 1);
    }
    //=============================================================================
    ArrayOf Inf(uint32 m, uint32 n)
    {
        double *mat = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, m * n, Nelson::stringVector(), false);
        Eigen::Map<Eigen::MatrixXd> matInf(mat, m, n);
        Eigen::MatrixXd matOne(m, n);
        Eigen::MatrixXd matZero(m, n);
        matZero.setConstant(0.);
        matOne.setConstant(1.);
        matInf = matOne.cwiseQuotient(matZero);
        Dimensions dimMat(m, n);
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimMat, mat);
        return res;
    }

};
//=============================================================================