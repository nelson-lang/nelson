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
#include "SparseToIJV.hpp"
#include "SparseNonZeros.hpp"
#include "SparseType.hpp"
#include <Eigen/Sparse>
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
SparseToIJV(const ArrayOf& spA, ArrayOf& I, ArrayOf& J, ArrayOf& V, ArrayOf& M, ArrayOf& N,
    ArrayOf& NNZ, bool& needToOverload)
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
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
        int nz = 0;
        void* ptrV = Eigen_SparseToIJV(spA.getDataClass(), dims.getRows(), dims.getColumns(),
            spA.getSparseDataPointer(), ptrI, ptrJ, nz);
        double* pdI = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, nnz, stringVector(), false));
        for (indexType k = 0; k < nnz; k++) {
            pdI[k] = static_cast<double>(ptrI[k]);
        }
        I = ArrayOf(NLS_DOUBLE, Dimensions(nnz, 1), (void*)pdI);
        delete[] ptrI;
        double* pdJ = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, nnz, stringVector(), false));
        for (indexType k = 0; k < nnz; k++) {
            pdJ[k] = static_cast<double>(ptrJ[k]);
        }
        J = ArrayOf(NLS_DOUBLE, Dimensions(nnz, 1), (void*)pdJ);
        delete[] ptrJ;
        V = ArrayOf(spA.getDataClass(), Dimensions(nnz, 1), ptrV);
        M = ArrayOf::doubleConstructor(static_cast<double>(dims.getRows()));
        N = ArrayOf::doubleConstructor(static_cast<double>(dims.getColumns()));
        NNZ = ArrayOf::doubleConstructor(static_cast<double>(spA.nzmax()));
    } else {
        needToOverload = true;
    }
}
//=============================================================================

} // namespace Nelson
//=============================================================================
