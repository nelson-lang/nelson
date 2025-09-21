//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
solveSVDDecompositionReal(NelsonType destinationClass, const ArrayOf& matA, const ArrayOf& matB)
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

    Eigen::JacobiSVD<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> svd(
        matAz, Eigen::ComputeFullU | Eigen::ComputeFullV);
    matCz = svd.solve(matBz);
    return res;
}
//=============================================================================
}
//=============================================================================
