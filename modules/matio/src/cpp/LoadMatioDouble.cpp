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
    Class destinationClass = matVariable->isComplex ? NLS_DCOMPLEX : NLS_DOUBLE;
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
            double* ptrDouble = (double*)ptr;
            double* ptrR = (double*)(cplx->Re);
            double* ptrI = (double*)(cplx->Im);
            indexType i = 0;
            for (indexType k = 0; k < dims.getElementCount(); k++) {
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
