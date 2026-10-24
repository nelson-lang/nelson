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
static SparseDoubleMatrix
complexHermitianToRealBlockMatrix(const SparseComplexMatrix& mat)
{
    const signedIndexType n = static_cast<signedIndexType>(mat.rows());
    SparseDoubleMatrix block(2 * n, 2 * n);
    std::vector<Eigen::Triplet<double, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(4) * mat.nonZeros());
    for (int outer = 0; outer < mat.outerSize(); ++outer) {
        for (SparseComplexMatrix::InnerIterator it(mat, outer); it; ++it) {
            const signedIndexType row = static_cast<signedIndexType>(it.row());
            const signedIndexType col = static_cast<signedIndexType>(it.col());
            const double realPart = it.value().real();
            const double imagPart = it.value().imag();
            if (realPart != 0.0) {
                triplets.emplace_back(row, col, realPart);
                triplets.emplace_back(row + n, col + n, realPart);
            }
            if (imagPart != 0.0) {
                triplets.emplace_back(row, col + n, -imagPart);
                triplets.emplace_back(row + n, col, imagPart);
            }
        }
    }
    block.setFromTriplets(triplets.begin(), triplets.end());
    block.makeCompressed();
    return block;
}
//=============================================================================
static void
appendIndependentComplexVectors(const SparseEigenResult& blockResult, indexType n, indexType k,
    SparseGeneralEigenResult& result)
{
    for (Eigen::Index col = 0; col < blockResult.vectors.cols() && result.values.size() < k;
        ++col) {
        Eigen::VectorXcd candidate(n);
        for (indexType row = 0; row < n; ++row) {
            candidate((Eigen::Index)row)
                = doublecomplex(blockResult.vectors((Eigen::Index)row, col),
                    blockResult.vectors((Eigen::Index)(row + n), col));
        }
        double candidateNorm = candidate.norm();
        if (candidateNorm <= std::numeric_limits<double>::epsilon()) {
            continue;
        }
        candidate /= candidateNorm;
        bool dependent = false;
        for (Eigen::Index existing = 0; existing < result.vectors.cols(); ++existing) {
            if (std::abs(result.vectors.col(existing).dot(candidate)) > 1.0 - 1e-8) {
                dependent = true;
                break;
            }
        }
        if (!dependent) {
            Eigen::Index dst = result.vectors.cols();
            result.vectors.conservativeResize(n, dst + 1);
            result.vectors.col(dst) = candidate;
            result.values.emplace_back(blockResult.values[(size_t)col], 0.0);
        }
    }
}
//=============================================================================
static SparseGeneralEigenResult
sparseHermitianComplexEigs(const SparseComplexMatrix& mat, indexType k, const std::string& which)
{
    SparseDoubleMatrix block = complexHermitianToRealBlockMatrix(mat);
    const indexType blockLimit = static_cast<indexType>(block.rows());
    indexType blockK = std::min<indexType>(std::max<indexType>(2 * k, k + 2), blockLimit - 1);
    SparseEigsRequest blockRequest { which, false, 0.0 };
    SparseGeneralEigenResult result;
    result.vectors.resize(mat.rows(), 0);
    for (;;) {
        SparseEigenResult blockResult = sparseSymmetricEigs(block, blockK, blockRequest);
        appendIndependentComplexVectors(blockResult, mat.rows(), k, result);
        if (result.values.size() >= k || blockK == blockLimit - 1) {
            break;
        }
        blockK = blockLimit - 1;
    }
    if (result.values.size() < k) {
        Error(L"Nelson:linear_algebra:arpackFailed", L"ARPACK eigensolver failed.");
    }
    return result;
}
//=============================================================================
