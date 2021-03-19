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
#include <Eigen/Sparse>
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
decomplexify(const ArrayOf& A)
{
    ArrayOf res;
    Class classA = A.getDataClass();
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
