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
#include "SaveMatioCharacterArray.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioCharacterArray(std::string variableName, ArrayOf variableValue)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank = variableDims.getLength();
    size_t* dims;
    try {
        dims = new size_t[rank];
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
    for (indexType k = 0; k < rank; k++) {
        dims[k] = variableDims[k];
    }
    void* ptrValue = nullptr;
    ArrayOf asUint16;
	if (!variableDims.isEmpty(false)) {
        if (sizeof(charType) == sizeof(uint16)) {
            ptrValue = (void*)variableValue.getDataPointer();
        } else {
            asUint16 = variableValue;
            asUint16.promoteType(NLS_UINT16);
            ptrValue = (void*)asUint16.getDataPointer();
        }
    }
    matvar_t* matVariable = Mat_VarCreate(variableName.c_str(), MAT_C_CHAR, MAT_T_UTF16, (int)rank, dims, ptrValue, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
}
//=============================================================================
