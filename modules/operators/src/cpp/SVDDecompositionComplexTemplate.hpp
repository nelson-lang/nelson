//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <complex>
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
solveSVDDecompositionComplex(NelsonType destinationClass, const ArrayOf& matA, const ArrayOf& matB)
{
    ArrayOf res;

    indexType n = matA.getDimensionLength(1);
    indexType k = matB.getDimensionLength(1);
    Dimensions outDim(n, k);

    std::complex<T>* ptrAz = reinterpret_cast<std::complex<T>*>((T*)matA.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matAz(
        ptrAz, matA.getRows(), matA.getColumns());

    std::complex<T>* ptrBz = reinterpret_cast<std::complex<T>*>((T*)matB.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matBz(
        ptrBz, matB.getRows(), matB.getColumns());

    T* ptrC = (T*)ArrayOf::allocateArrayOf(destinationClass, n * k);
    res = ArrayOf(destinationClass, outDim, ptrC);
    std::complex<T>* ptrCz = reinterpret_cast<std::complex<T>*>((T*)ptrC);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matCz(
        ptrCz, outDim.getRows(), outDim.getColumns());

    matCz = matAz
                .bdcSvd(Eigen::DecompositionOptions::ComputeThinU
                    | Eigen::DecompositionOptions::ComputeThinV)
                .solve(matBz);
    return res;
}
//=============================================================================
}
//=============================================================================
