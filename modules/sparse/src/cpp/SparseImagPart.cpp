//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "SparseImagPart.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
SparseImagPart(ArrayOf a)
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
        } catch (const std::bad_alloc& e) {
            e.what();
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
                    spmatDST->coeffRef(it.row(), it.col()) = it.value().imag();
                }
            }
            spmatDST->finalize();
            spmatDST->makeCompressed();
            void* pRes = (void*)spmatDST;
            res = ArrayOf(NLS_DOUBLE, a.getDimensions(), pRes, true);
        } catch (const std::bad_alloc& e) {
            e.what();
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
}
//=============================================================================
