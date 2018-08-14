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
#include "SparseNonZeros.hpp"
#include "Error.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
indexType
SparseNonZeros(ArrayOf a)
{
    indexType nnz = 0;
    if (a.isSparse()) {
        switch (a.getDataClass()) {
        case NLS_LOGICAL: {
            Eigen::SparseMatrix<logical, 0, signedIndexType>* spmat
                = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)a.getSparseDataPointer();
            if (spmat) {
                nnz = spmat->nonZeros();
            }
        } break;
        case NLS_DOUBLE: {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)a.getSparseDataPointer();
            if (spmat) {
                nnz = spmat->nonZeros();
            }
        } break;
        case NLS_DCOMPLEX: {
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spmat
                = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)a.getSparseDataPointer();
            if (spmat) {
                nnz = spmat->nonZeros();
            }
        } break;
        default:
            Error(_W("type not supported."));
            break;
        }
    } else {
        Error(_W("type not supported."));
    }
    return nnz;
}
//=============================================================================
}
//=============================================================================
