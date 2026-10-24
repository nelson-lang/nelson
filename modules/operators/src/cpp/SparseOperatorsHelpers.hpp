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
#include "ArrayOf.hpp"
#include "SparseArrayOf.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> using NelsonSparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
//=============================================================================
template <class T>
const NelsonSparseMatrix<T>*
getSparseMatrixPointer(const ArrayOf& A)
{
    return static_cast<const NelsonSparseMatrix<T>*>(A.sparseDataPointer());
}
//=============================================================================
template <class T>
NelsonSparseMatrix<T>*
getSparseMatrixPointer(ArrayOf& A)
{
    return static_cast<NelsonSparseMatrix<T>*>(A.readWriteDataPointer());
}
//=============================================================================
template <class T>
T
getSparseScalarValue(const ArrayOf& A)
{
    return getSparseMatrixPointer<T>(A)->coeff(0, 0);
}
//=============================================================================
template <class T>
ArrayOf
makeSparseOperatorResult(
    NelsonType dataClass, const Dimensions& dimensions, NelsonSparseMatrix<T>* sparseMatrix)
{
    return makeSparseArrayOfFromEigen<T>(dataClass, dimensions, sparseMatrix, true);
}
//=============================================================================
template <class T>
ArrayOf
copySparseOperatorResult(NelsonType dataClass, const Dimensions& dimensions, const ArrayOf& A)
{
    auto* sparseMatrix = new NelsonSparseMatrix<T>(*getSparseMatrixPointer<T>(A));
    return makeSparseOperatorResult<T>(dataClass, dimensions, sparseMatrix);
}
//=============================================================================
template <class T>
ArrayOf
scalarSparseOperatorProduct(
    NelsonType dataClass, const Dimensions& dimensions, const ArrayOf& A, T scalarValue)
{
    auto* sparseMatrix = new NelsonSparseMatrix<T>(*getSparseMatrixPointer<T>(A));
    if (scalarValue == T(0)) {
        sparseMatrix->setZero();
    } else {
        for (signedIndexType k = 0; k < sparseMatrix->outerSize(); ++k) {
            for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseMatrix, k); it; ++it) {
                it.valueRef() *= scalarValue;
            }
        }
    }
    return makeSparseOperatorResult<T>(dataClass, dimensions, sparseMatrix);
}
//=============================================================================
template <class T>
ArrayOf
sameSizeSparseOperatorAddition(
    NelsonType dataClass, const Dimensions& dimensions, const ArrayOf& A, const ArrayOf& B)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    auto* sparseResult = new NelsonSparseMatrix<T>(*sparseA + *sparseB);
    return makeSparseOperatorResult<T>(dataClass, dimensions, sparseResult);
}
//=============================================================================
template <class T>
ArrayOf
sameSizeSparseOperatorSubtraction(
    NelsonType dataClass, const Dimensions& dimensions, const ArrayOf& A, const ArrayOf& B)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    auto* sparseResult = new NelsonSparseMatrix<T>(*sparseA - *sparseB);
    return makeSparseOperatorResult<T>(dataClass, dimensions, sparseResult);
}
//=============================================================================
template <class T>
ArrayOf
sameSizeSparseOperatorMultiplication(
    NelsonType dataClass, const Dimensions& dimensions, const ArrayOf& A, const ArrayOf& B)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    auto* sparseResult = new NelsonSparseMatrix<T>(sparseA->cwiseProduct(*sparseB));
    return makeSparseOperatorResult<T>(dataClass, dimensions, sparseResult);
}
//=============================================================================
template <class T>
ArrayOf
matrixSparseOperatorMultiplication(
    NelsonType dataClass, indexType rows, indexType cols, const ArrayOf& A, const ArrayOf& B)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    auto* sparseResult = new NelsonSparseMatrix<T>((*sparseA) * (*sparseB));
    return makeSparseOperatorResult<T>(dataClass, Dimensions(rows, cols), sparseResult);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
