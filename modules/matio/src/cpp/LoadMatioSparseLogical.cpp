//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "LoadMatioSparseLogical.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
#include "SparseConstructors.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioSparseLogical(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    mat_sparse_t* sparseData = (mat_sparse_t*)matVariable->data;
    indexType nzmax = (indexType)sparseData->nzmax;
    ArrayOf I;
    ArrayOf J;
    ArrayOf V;
    if (dims.isEmpty(false)) {
        I = ArrayOf::emptyConstructor();
        J = ArrayOf::emptyConstructor();
        V = ArrayOf::emptyConstructor(dims);
        V.promoteType(NLS_LOGICAL);
    } else {
        indexType nbR = (indexType)sparseData->nir;
        Dimensions dimsR = Dimensions(1, nbR);
        void* ptrR = nullptr;
        try {
            ptrR = ArrayOf::allocateArrayOf(NLS_INT32, nbR, stringVector(), false);
        } catch (Exception&) {
            return false;
        }
        memcpy(ptrR, sparseData->ir, nbR * sizeof(int32));
        auto* iptR = static_cast<int32*>(ptrR);
        for (int k = 0; k < nbR; k++) {
            iptR[k] = iptR[k] + 1;
        }
        I = ArrayOf(NLS_INT32, dimsR, ptrR);

        Dimensions dimsC = Dimensions(1, nbR);
        void* ptrC = nullptr;
        try {
            ptrC = ArrayOf::allocateArrayOf(NLS_INT32, nbR, stringVector(), false);
        } catch (Exception&) {
            return false;
        }
        auto* iptC = static_cast<int32*>(ptrC);
        int ii = 0;
        for (uint32 p = 1; p < (uint32)sparseData->njc; p++) {
            for (unsigned n = 0; n < unsigned(sparseData->jc[p] - sparseData->jc[p - 1]); n++) {
                iptC[ii] = p;
                ii++;
            }
        }
        J = ArrayOf(NLS_INT32, dimsC, ptrC);

        indexType nbV = (indexType)sparseData->ndata;
        Dimensions dimsV = Dimensions(1, nbV);
        void* ptrV = nullptr;
        try {
            ptrV = ArrayOf::allocateArrayOf(NLS_LOGICAL, nbV, stringVector(), false);
        } catch (Exception&) {
            return false;
        }
        V = ArrayOf(NLS_LOGICAL, dimsV, ptrV);
        memset(ptrV, logical(1), sparseData->ndata * sizeof(logical));
    }
    VariableValue = SparseConstructor(I, J, V, dims[0], dims[1], nzmax);
    bSuccess = true;
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
