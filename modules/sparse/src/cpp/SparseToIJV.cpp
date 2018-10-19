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
#include "SparseToIJV.hpp"
#include "SparseNonZeros.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
void
SparseToIJV(ArrayOf spA, ArrayOf& I, ArrayOf& J, ArrayOf& V, ArrayOf& M, ArrayOf& N, ArrayOf& NNZ,
    bool& needToOverload)
{
    needToOverload = false;
    if (spA.isSparse()) {
        Dimensions dims = spA.getDimensions();
        indexType nnz = SparseNonZeros(spA);
        indexType* ptrI;
        indexType* ptrJ;
        try {
            ptrI = new indexType[nnz];
            ptrJ = new indexType[nnz];
        } catch (const std::bad_alloc& e) {
            e.what();
            Error(ERROR_MEMORY_ALLOCATION);
        }
        int nz = 0;
        void* ptrV = Eigen_SparseToIJV(spA.getDataClass(), dims.getRows(), dims.getColumns(),
            spA.getSparseDataPointer(), ptrI, ptrJ, nz);
        double* pdI = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nnz);
        for (indexType k = 0; k < nnz; k++) {
            pdI[k] = (double)ptrI[k];
        }
        I = ArrayOf(NLS_DOUBLE, Dimensions(nnz, 1), (void*)pdI);
        delete[] ptrI;
        double* pdJ = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, nnz);
        for (indexType k = 0; k < nnz; k++) {
            pdJ[k] = (double)ptrJ[k];
        }
        J = ArrayOf(NLS_DOUBLE, Dimensions(nnz, 1), (void*)pdJ);
        delete[] ptrJ;
        V = ArrayOf(spA.getDataClass(), Dimensions(nnz, 1), ptrV);
        M = ArrayOf::doubleConstructor((double)dims.getRows());
        N = ArrayOf::doubleConstructor((double)dims.getColumns());
        NNZ = ArrayOf::doubleConstructor((double)spA.nzmax());
    } else {
        needToOverload = true;
    }
}
//=============================================================================

}
//=============================================================================
