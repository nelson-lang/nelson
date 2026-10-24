//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <Eigen/Sparse>
#include <memory>
#include <utility>
#include "ArrayOf.hpp"
#include "Decomplexify.hpp"
#include "SparseType.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The input matrix is compressed, copied into Nelson sparse storage, then
// deleted. pruneZeros removes explicit zero entries before the copy.
template <class T>
ArrayOf
makeSparseArrayOfFromEigen(NelsonType dataClass, const Dimensions& dimensions,
    std::unique_ptr<Eigen::SparseMatrix<T, 0, signedIndexType>> sparseMatrix,
    bool pruneZeros = false)
{
    if (pruneZeros) {
        sparseMatrix->prune(T(0));
    }
    sparseMatrix->makeCompressed();
    void* copiedSparseMatrix = Eigen_CopySparseMatrix(
        dataClass, dimensions.rows(), dimensions.columns(), sparseMatrix.get());
    ArrayOf result(dataClass, dimensions, copiedSparseMatrix, true);
    // A non-empty result whose imaginary part is all zero becomes a real sparse
    // matrix. An empty result keeps the propagated class. sparse() on a complex
    // dense matrix uses another path and keeps its complex attribute.
    if (!result.isEmpty()) {
        return decomplexify(result);
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOf
makeSparseArrayOfFromEigen(NelsonType dataClass, const Dimensions& dimensions,
    Eigen::SparseMatrix<T, 0, signedIndexType>* sparseMatrix, bool pruneZeros = false)
{
    return makeSparseArrayOfFromEigen<T>(dataClass, dimensions,
        std::unique_ptr<Eigen::SparseMatrix<T, 0, signedIndexType>>(sparseMatrix), pruneZeros);
}
//=============================================================================
template <class T>
ArrayOf
makeSparseResult(NelsonType dataClass, const Dimensions& dimensions,
    std::unique_ptr<Eigen::SparseMatrix<T, 0, signedIndexType>> sparseMatrix,
    bool pruneZeros = false)
{
    return makeSparseArrayOfFromEigen<T>(
        dataClass, dimensions, std::move(sparseMatrix), pruneZeros);
}
//=============================================================================
template <class T>
ArrayOf
makeSparseResult(NelsonType dataClass, const Dimensions& dimensions,
    Eigen::SparseMatrix<T, 0, signedIndexType>* sparseMatrix, bool pruneZeros = false)
{
    return makeSparseArrayOfFromEigen<T>(dataClass, dimensions, sparseMatrix, pruneZeros);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
