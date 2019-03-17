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
#include "SaveMatioStruct.hpp"
#include "SaveMatioVariable.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioStruct(std::string variableName, ArrayOf variableValue, mat_ft matVersion)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    stringVector fieldnames = variableValue.getFieldNames();
    size_t nbFielnames = fieldnames.size();
    indexType nbStructElements = nbFielnames * variableDims.getElementCount() + 1;
    matvar_t** structElements = nullptr;
    try {
        structElements = new matvar_t*[nbStructElements];
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
    for (indexType i = 0; i < nbStructElements; ++i) {
        structElements[i] = nullptr;
    }

    ArrayOf* elements = (ArrayOf*)variableValue.getDataPointer();
    for (indexType i = 0; i < variableDims.getElementCount(); ++i) {
        for (indexType j = 0; j < (indexType)nbFielnames; ++j) {
            ArrayOf element = elements[i * nbFielnames + j];
            structElements[i * nbFielnames + j]
                = SaveMatioVariable(fieldnames[j], element, matVersion);
        }
    }
    matvar_t* matVariable = Mat_VarCreate(
        variableName.c_str(), MAT_C_STRUCT, MAT_T_STRUCT, (int)rank, dims, structElements, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
}
//=============================================================================
