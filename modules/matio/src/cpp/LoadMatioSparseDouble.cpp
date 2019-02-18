//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include <cstring>
#include "LoadMatioSparseDouble.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
#include "SparseConstructors.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioSparseDouble(matvar_t* matVariable, ArrayOf& VariableValue)
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
        if (matVariable->isComplex) {
            V.promoteType(NLS_DCOMPLEX);
        } else {
            V.promoteType(NLS_DOUBLE);
        }
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
        int32* iptR = (int32*)ptrR;
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
        int32* iptC = (int32*)ptrC;
        int ii = 0;
        for (int p = 1; p < sparseData->njc; p++) {
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
            if (matVariable->isComplex) {
                ptrV = ArrayOf::allocateArrayOf(NLS_DCOMPLEX, nbV, stringVector(), false);
            } else {
                ptrV = ArrayOf::allocateArrayOf(NLS_DOUBLE, nbV, stringVector(), false);
            }
        } catch (Exception&) {
            return false;
        }
        if (matVariable->isComplex) {
            V = ArrayOf(NLS_DCOMPLEX, dimsV, ptrV);
            mat_complex_split_t* cplx = (mat_complex_split_t*)sparseData->data;
            double* ptrDouble = (double*)ptrV;
            double* ptrRe = (double*)(cplx->Re);
            double* ptrIm = (double*)(cplx->Im);
            indexType i = 0;
            for (indexType k = 0; k < nbV; k++) {
                ptrDouble[i] = ptrRe[k];
                ptrDouble[i + 1] = ptrIm[k];
                i = i + 2;
            }
        } else {
            V = ArrayOf(NLS_DOUBLE, dimsV, ptrV);
            memcpy(ptrV, sparseData->data, sparseData->ndata * sizeof(double));
        }
    }
    VariableValue = SparseConstructor(I, J, V, dims[0], dims[1], nzmax);
    bSuccess = true;
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
