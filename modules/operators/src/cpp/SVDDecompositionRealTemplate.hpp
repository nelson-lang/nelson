//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
solveSVDDecompositionReal(Class destinationClass, const ArrayOf& matA, const ArrayOf& matB)
{
    ArrayOf res;

    indexType n = matA.getDimensionLength(1);
    indexType k = matB.getDimensionLength(1);
    Dimensions outDim(n, k);

    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matAz(
        (T*)matA.getDataPointer(), matA.getRows(), matA.getColumns());

    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matBz(
        (T*)matB.getDataPointer(), matB.getRows(), matB.getColumns());

    T* ptrC = (T*)ArrayOf::allocateArrayOf(destinationClass, n * k);
    res = ArrayOf(destinationClass, outDim, ptrC);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matCz(
        ptrC, outDim.getRows(), outDim.getColumns());

    matCz = matAz
                .bdcSvd(Eigen::DecompositionOptions::ComputeThinU
                    | Eigen::DecompositionOptions::ComputeThinV)
                .solve(matBz);
    return res;
}
//=============================================================================
}
//=============================================================================
