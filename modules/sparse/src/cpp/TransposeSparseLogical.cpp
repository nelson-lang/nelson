//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "TransposeSparseLogical.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
TransposeSparseLogical(ArrayOf A)
{
    ArrayOf C;
    if (A.isEmpty()) {
        Dimensions dimsC(A.getDimensions().getColumns(), A.getDimensions().getRows());
        C = ArrayOf(NLS_LOGICAL, dimsC, (void*)nullptr, true);
    } else {
        Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatA
            = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.getSparseDataPointer();
        Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatC;
        try {
            spMatC = new Eigen::SparseMatrix<logical, 0, signedIndexType>(
                spMatA->cols(), spMatA->rows());
        } catch (const std::bad_alloc&) {
            spMatC = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
        *spMatC = spMatA->transpose();
        Dimensions dimsC = Dimensions(spMatC->rows(), spMatC->cols());
        C = ArrayOf(NLS_LOGICAL, dimsC, (void*)spMatC, true);
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
