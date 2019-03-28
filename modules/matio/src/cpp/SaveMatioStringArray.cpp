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
#include "SaveMatioStringArray.hpp"
#include "SaveMatioCharacterArray.hpp"
#include "SaveMatioDouble.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioStringArray(const std::string& variableName, ArrayOf variableValue, mat_ft matVersion)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    void* ptrValue = nullptr;
    if (!variableDims.isEmpty(false)) {
        ptrValue = const_cast<void*>(variableValue.getDataPointer());
    }

    indexType nbElements = variableDims.getElementCount();
    matvar_t** cellElements = nullptr;
    try {
        cellElements = (matvar_t**)new matvar_t*[nbElements];
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
    auto* elements = (ArrayOf*)variableValue.getDataPointer();
    for (indexType i = 0; i < nbElements; ++i) {
        if (elements[i].getDataClass() == NLS_CHAR) {
            cellElements[i] = SaveMatioCharacterArray(variableName, elements[i], matVersion);
        } else {
            ArrayOf NanAsArrayOf = ArrayOf::doubleConstructor(std::nan("NaN"));
            cellElements[i] = SaveMatioDouble(variableName, NanAsArrayOf);
        }
        if (cellElements[i] == nullptr) {
            delete[] cellElements;
            delete[] dims;
            return nullptr;
        }
    }
    matvar_t* matVariable = Mat_VarCreate(
        variableName.c_str(), MAT_C_CELL, MAT_T_CELL, (int)rank, dims, cellElements, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
