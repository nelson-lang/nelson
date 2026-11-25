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
// Shared by SparseRealPart.cpp / SparseImagPart.cpp: build a real sparse matrix from
// one component (real or imag) of a complex sparse matrix, keeping only the projected
// values that are nonzero. The four complex cases (double/single × real/imag) differ
// only by the element type and the projection, so they are one template.
//=============================================================================
#include <Eigen/Sparse>
#include <vector>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class TComplex, class TReal, class Proj>
ArrayOf
sparseComplexComponent(const ArrayOf& a, NelsonType destClass, Proj proj)
{
    indexType rows = a.dimensionLength(0);
    indexType cols = a.dimensionLength(1);
    try {
        auto* spmatDST = new Eigen::SparseMatrix<TReal, 0, signedIndexType>(rows, cols);
        auto* spmatSRC = (Eigen::SparseMatrix<TComplex, 0, signedIndexType>*)a.sparseDataPointer();
        std::vector<Eigen::Triplet<TReal, signedIndexType>> triplets;
        triplets.reserve(spmatSRC->nonZeros());
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (typename Eigen::SparseMatrix<TComplex, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, k);
                it; ++it) {
                TReal value = proj(it.value());
                if (value != TReal(0)) {
                    triplets.emplace_back(it.row(), it.col(), value);
                }
            }
        }
        spmatDST->setFromTriplets(triplets.begin(), triplets.end());
        spmatDST->makeCompressed();
        return ArrayOf(destClass, a.dimensions(), (void*)spmatDST, true);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
