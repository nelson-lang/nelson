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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
SparseImagPart(const ArrayOf& a)
{
    ArrayOf res;
    if (!a.isSparse()) {
        Error(L"Nelson:types:sparseExpected", L"Sparse expected.");
    }
    switch (a.dataClass()) {
    case NLS_DOUBLE:
    case NLS_LOGICAL: {
        indexType rows = a.dimensionLength(0);
        indexType cols = a.dimensionLength(1);
        try {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
                = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
            spmat->finalize();
            spmat->makeCompressed();
            void* pRes = (void*)spmat;
            res = ArrayOf(NLS_DOUBLE, a.dimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
    } break;
    case NLS_SINGLE: {
        indexType rows = a.dimensionLength(0);
        indexType cols = a.dimensionLength(1);
        try {
            Eigen::SparseMatrix<single, 0, signedIndexType>* spmat
                = new Eigen::SparseMatrix<single, 0, signedIndexType>(rows, cols);
            spmat->finalize();
            spmat->makeCompressed();
            void* pRes = (void*)spmat;
            res = ArrayOf(NLS_SINGLE, a.dimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
    } break;
    case NLS_DCOMPLEX: {
        indexType rows = a.dimensionLength(0);
        indexType cols = a.dimensionLength(1);
        try {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spmatDST
                = new Eigen::SparseMatrix<double, 0, signedIndexType>(rows, cols);
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spmatSRC
                = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)a.sparseDataPointer();
            std::vector<Eigen::Triplet<double, signedIndexType>> triplets;
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    if (it.value().imag() != 0.) {
                        triplets.emplace_back(it.row(), it.col(), it.value().imag());
                    }
                }
            }
            spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            spmatDST->makeCompressed();
            void* pRes = (void*)spmatDST;
            res = ArrayOf(NLS_DOUBLE, a.dimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
    } break;
    case NLS_SCOMPLEX: {
        indexType rows = a.dimensionLength(0);
        indexType cols = a.dimensionLength(1);
        try {
            Eigen::SparseMatrix<single, 0, signedIndexType>* spmatDST
                = new Eigen::SparseMatrix<single, 0, signedIndexType>(rows, cols);
            Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>* spmatSRC
                = (Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>*)a.sparseDataPointer();
            std::vector<Eigen::Triplet<single, signedIndexType>> triplets;
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    if (it.value().imag() != 0.f) {
                        triplets.emplace_back(it.row(), it.col(), it.value().imag());
                    }
                }
            }
            spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            spmatDST->makeCompressed();
            void* pRes = (void*)spmatDST;
            res = ArrayOf(NLS_SINGLE, a.dimensions(), pRes, true);
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
    } break;
    default: {
        Error("Nelson:types:imagUnsupportedType", "Cannot do imag with current type '%s'.",
            ClassName(a));
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
