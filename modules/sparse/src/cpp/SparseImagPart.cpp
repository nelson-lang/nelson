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
#include "SparseImagPart.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
SparseImagPart(const ArrayOf& a)
{
    ArrayOf res;
    if (!a.isSparse()) {
        Error(_W("Sparse expected."));
    }
    switch (a.getDataClass()) {
    case NLS_DOUBLE:
    case NLS_LOGICAL: {
        indexType rows = a.getDimensionLength(0);
        indexType cols = a.getDimensionLength(1);
        try {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
                = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
            spmat->finalize();
            spmat->makeCompressed();
            void* pRes = (void*)spmat;
            res = ArrayOf(NLS_DOUBLE, a.getDimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
    } break;
    case NLS_DCOMPLEX: {
        indexType rows = a.getDimensionLength(0);
        indexType cols = a.getDimensionLength(1);
        try {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spmatDST
                = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spmatSRC
                = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)a.getSparseDataPointer();
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                     it; ++it) {
                    if (it.value().imag() != 0.) {
                        spmatDST->coeffRef(it.row(), it.col()) = it.value().imag();
                    }
                }
            }
            spmatDST->finalize();
            spmatDST->makeCompressed();
            void* pRes = (void*)spmatDST;
            res = ArrayOf(NLS_DOUBLE, a.getDimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
    } break;
    default: {
        Error(_("Cannot do imag with current type '") + ClassName(a) + "'.");
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
