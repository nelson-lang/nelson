//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "SparseNonZeros.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
indexType
SparseNonZeros(const ArrayOf& a)
{
    indexType nnz = 0;
    if (a.isSparse()) {
        switch (a.dataClass()) {
        case NLS_LOGICAL: {
            Eigen::SparseMatrix<logical, 0, signedIndexType>* spmat
                = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)a.sparseDataPointer();
            if (spmat) {
                nnz = spmat->nonZeros();
            }
        } break;
        case NLS_DOUBLE: {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)a.sparseDataPointer();
            if (spmat) {
                nnz = spmat->nonZeros();
            }
        } break;
        case NLS_SINGLE: {
            Eigen::SparseMatrix<single, 0, signedIndexType>* spmat
                = (Eigen::SparseMatrix<single, 0, signedIndexType>*)a.sparseDataPointer();
            if (spmat) {
                nnz = spmat->nonZeros();
            }
        } break;
        case NLS_DCOMPLEX: {
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spmat
                = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)a.sparseDataPointer();
            if (spmat) {
                nnz = spmat->nonZeros();
            }
        } break;
        case NLS_SCOMPLEX: {
            Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>* spmat
                = (Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>*)a.sparseDataPointer();
            if (spmat) {
                nnz = spmat->nonZeros();
            }
        } break;
        default:
            Error(L"Nelson:types:typeNotManaged", L"Type not supported.");
            break;
        }
    } else {
        Error(L"Nelson:types:typeNotManaged", L"Type not supported.");
    }
    return nnz;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
