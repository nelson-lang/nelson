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
#include "SparseRealPart.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SparseType.hpp"
#include "SparseComplexComponent.hpp"
#include <Eigen/Sparse>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
SparseRealPart(ArrayOf a)
{
    ArrayOf res;
    if (!a.isSparse()) {
        Error(L"Nelson:types:sparseExpected", L"Sparse expected.");
    }
    switch (a.dataClass()) {
    case NLS_LOGICAL: {
        try {
            indexType rows = a.dimensionLength(0);
            indexType cols = a.dimensionLength(1);
            Eigen::SparseMatrix<double, 0, signedIndexType>* spmatDST
                = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
            Eigen::SparseMatrix<logical, 0, signedIndexType>* spmatSRC
                = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)a.sparseDataPointer();
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    if (it.value() != 0) {
                        spmatDST->coeffRef(it.row(), it.col()) = it.value() == 1;
                    }
                }
            }
            spmatDST->finalize();
            spmatDST->makeCompressed();
            void* pRes = (void*)spmatDST;
            res = ArrayOf(NLS_DOUBLE, a.dimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
    } break;
    case NLS_DOUBLE: {
        return ArrayOf(a);
    } break;
    case NLS_SINGLE: {
        return ArrayOf(a);
    } break;
    case NLS_DCOMPLEX: {
        return sparseComplexComponent<doublecomplex, double>(
            a, NLS_DOUBLE, [](const doublecomplex& z) { return z.real(); });
    } break;
    case NLS_SCOMPLEX: {
        return sparseComplexComponent<singlecomplex, single>(
            a, NLS_SINGLE, [](const singlecomplex& z) { return z.real(); });
    } break;
    default: {
        Error("Nelson:types:realUnsupportedType", "Cannot do real with current type '%s'.",
            ClassName(a));
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
