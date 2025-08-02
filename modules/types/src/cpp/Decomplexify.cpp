//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Sparse>
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
decomplexify(const ArrayOf& A)
{
    ArrayOf res;
    NelsonType classA = A.getDataClass();
    if (classA == NLS_DCOMPLEX || classA == NLS_SCOMPLEX) {
        if (A.allReal()) {
            if (classA == NLS_DCOMPLEX) {
                if (A.isSparse()) {
                    indexType rows = A.getDimensionLength(0);
                    indexType cols = A.getDimensionLength(1);
                    Eigen::SparseMatrix<double, 0, signedIndexType>* spmatDST
                        = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
                    Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spmatSRC
                        = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)
                              A.getSparseDataPointer();
                    for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                        for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator
                                 it(*spmatSRC, k);
                             it; ++it) {
                            spmatDST->coeffRef(it.row(), it.col()) = it.value().real();
                        }
                    }
                    spmatDST->finalize();
                    spmatDST->makeCompressed();
                    void* pRes = (void*)spmatDST;
                    res = ArrayOf(NLS_DOUBLE, A.getDimensions(), pRes, true);
                } else {
                    res = A;
                    res.promoteType(NLS_DOUBLE);
                }
            }
            if (classA == NLS_SCOMPLEX) {
                res = A;
                res.promoteType(NLS_SINGLE);
            }
        } else {
            res = A;
        }
    } else {
        res = A;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
