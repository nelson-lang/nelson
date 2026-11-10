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
//=============================================================================
namespace Nelson {
//=============================================================================
// C = max(A, B) or min(A, B) with a sparse operand is always sparse; the two-matrix kernel
// applies when no dense matrix operand, not two scalars, same sizes and zero background.
inline bool
sparseExtremumKernelApplies(const ArrayOf& A, const ArrayOf& B, bool backgroundIsZero)
{
    bool aScalar = A.isScalar();
    bool bScalar = B.isScalar();
    if ((!A.isSparse() && !aScalar) || (!B.isSparse() && !bScalar) || (aScalar && bScalar)
        || !backgroundIsZero) {
        return false;
    }
    return aScalar || bScalar || A.dimensions().equals(B.dimensions());
}
//=============================================================================
// combine(fill, M) or combine(M, fill) for a sparse M, written straight into a compressed result
template <class T, class Combine>
Eigen::SparseMatrix<T, 0, signedIndexType>*
sparseScalarExtremum(
    const Eigen::SparseMatrix<T, 0, signedIndexType>& M, T fill, bool fillFirst, Combine combine)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    auto apply = [&](T v) { return fillFirst ? combine(fill, v) : combine(v, fill); };
    T background = apply(T(0));
    bool storedOnly = (background == T(0));
    signedIndexType rows = M.rows();
    signedIndexType cols = M.cols();
    auto* R = new SparseMatrix(rows, cols);
    R->resizeNonZeros(storedOnly ? (signedIndexType)M.nonZeros() : rows * cols);
    signedIndexType* outer = R->outerIndexPtr();
    signedIndexType* inner = R->innerIndexPtr();
    T* values = R->valuePtr();
    signedIndexType nz = 0;
    for (signedIndexType c = 0; c < cols; ++c) {
        outer[c] = nz;
        typename SparseMatrix::InnerIterator it(M, c);
        if (storedOnly) {
            for (; it; ++it) {
                T v = apply(it.value());
                if (v != T(0)) {
                    inner[nz] = it.row();
                    values[nz++] = v;
                }
            }
            continue;
        }
        // implicit zeros take the background value
        for (signedIndexType r = 0; r < rows; ++r) {
            T v = background;
            if (it && it.row() == r) {
                v = apply(it.value());
                ++it;
            }
            if (v != T(0)) {
                inner[nz] = r;
                values[nz++] = v;
            }
        }
    }
    outer[cols] = nz;
    R->resizeNonZeros(nz);
    R->data().squeeze();
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
