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
#include <cstring>
#include "LoadMatioInteger.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
complexIntegerTocomplexDouble(mat_complex_split_t* cplx, indexType nbElements, double* ptrDouble)
{
    T* ptrR = (T*)(cplx->Re);
    T* ptrI = (T*)(cplx->Im);
    indexType i = 0;
    for (indexType k = 0; k < nbElements; k++) {
        ptrDouble[i] = (double)ptrR[k];
        ptrDouble[i + 1] = (double)ptrI[k];
        i = i + 2;
    }
}
//=============================================================================
bool
LoadMatioInteger(matvar_t* matVariable, Class destinationClass, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        VariableValue.promoteType(destinationClass);
        bSuccess = true;
    } else {
        void* ptr = nullptr;
        try {
            if (matVariable->isComplex != 0) {
                ptr = ArrayOf::allocateArrayOf(
                    NLS_DCOMPLEX, dims.getElementCount(), stringVector(), false);
            } else {
                ptr = ArrayOf::allocateArrayOf(
                    destinationClass, dims.getElementCount(), stringVector(), false);
            }
        } catch (Exception&) {
            return false;
        }
        if (matVariable->isComplex != 0) {
            auto* ptrDouble = static_cast<double*>(ptr);
            mat_complex_split_t* cplx = (mat_complex_split_t*)matVariable->data;
            switch (destinationClass) {
            case NLS_INT8: {
                complexIntegerTocomplexDouble<int8>(cplx, dims.getElementCount(), ptrDouble);
            } break;
            case NLS_INT16: {
                complexIntegerTocomplexDouble<int16>(cplx, dims.getElementCount(), ptrDouble);
            } break;
            case NLS_INT32: {
                complexIntegerTocomplexDouble<int32>(cplx, dims.getElementCount(), ptrDouble);
            } break;
            case NLS_INT64: {
                complexIntegerTocomplexDouble<int64>(cplx, dims.getElementCount(), ptrDouble);
            } break;
            case NLS_UINT8: {
                complexIntegerTocomplexDouble<uint8>(cplx, dims.getElementCount(), ptrDouble);
            } break;
            case NLS_UINT16: {
                complexIntegerTocomplexDouble<uint16>(cplx, dims.getElementCount(), ptrDouble);
            } break;
            case NLS_UINT32: {
                complexIntegerTocomplexDouble<uint32>(cplx, dims.getElementCount(), ptrDouble);
            } break;
            case NLS_UINT64: {
                complexIntegerTocomplexDouble<uint64>(cplx, dims.getElementCount(), ptrDouble);
            } break;
            }
            VariableValue = ArrayOf(NLS_DCOMPLEX, dims, ptr);
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
