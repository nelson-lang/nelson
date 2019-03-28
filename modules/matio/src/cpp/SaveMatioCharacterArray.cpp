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
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioCharacterArray(const std::string &variableName, ArrayOf variableValue, mat_ft matVersion)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    void* ptrValue = nullptr;
    matio_types matType = MAT_T_UTF8;
    ArrayOf asUint;
    if (!variableDims.isEmpty(false)) {
        if (matVersion == MAT_FT_MAT5 || matVersion == MAT_FT_MAT4) {
            asUint = variableValue;
            asUint.promoteType(NLS_UINT8);
            ptrValue = const_cast<void*>(asUint.getDataPointer());
            matType = MAT_T_UTF8;
        } else {
            if (sizeof(charType) == sizeof(uint16)) {
                ptrValue = const_cast<void*>(variableValue.getDataPointer());
            } else {
                asUint = variableValue;
                asUint.promoteType(NLS_UINT16);
                ptrValue = const_cast<void*>(asUint.getDataPointer());
            }
            matType = MAT_T_UTF16;
        }
    }
    matvar_t* matVariable
        = Mat_VarCreate(variableName.c_str(), MAT_C_CHAR, matType, (int)rank, dims, ptrValue, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
}  // namespace Nelson
//=============================================================================
