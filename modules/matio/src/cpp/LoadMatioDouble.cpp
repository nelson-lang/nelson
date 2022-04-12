//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "LoadMatioDouble.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioDouble(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    NelsonType destinationClass = matVariable->isComplex ? NLS_DCOMPLEX : NLS_DOUBLE;
    Dimensions dims = getMatVarDimensions(matVariable);
    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        VariableValue.promoteType(destinationClass);
        bSuccess = true;
    } else {
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(
                destinationClass, dims.getElementCount(), stringVector(), false);
        } catch (Exception&) {
            return false;
        }
        if (matVariable->isComplex != 0) {
            mat_complex_split_t* cplx = (mat_complex_split_t*)matVariable->data;
            auto* ptrDouble = static_cast<double*>(ptr);
            double* ptrR = (double*)(cplx->Re);
            double* ptrI = (double*)(cplx->Im);
            indexType i = 0;
            indexType elementCount = dims.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                ptrDouble[i] = ptrR[k];
                ptrDouble[i + 1] = ptrI[k];
                i = i + 2;
            }
            VariableValue = ArrayOf(destinationClass, dims, ptr);
            bSuccess = true;
        } else {
            memcpy(ptr, matVariable->data, matVariable->nbytes);
            VariableValue = ArrayOf(destinationClass, dims, ptr);
            bSuccess = true;
        }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
