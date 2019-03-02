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
#include "SaveMatioDouble.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioInteger(std::string variableName, ArrayOf variableValue)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    void* ptrValue = nullptr;
    if (!variableDims.isEmpty(false)) {
        ptrValue = (void*)variableValue.getDataPointer();
    }
    matio_classes matClass;
    matio_types matType;
    switch (variableValue.getDataClass()) {
    case NLS_UINT8: {
        matClass = MAT_C_UINT8;
        matType = MAT_T_UINT8;
    } break;
    case NLS_UINT16: {
        matClass = MAT_C_UINT16;
        matType = MAT_T_UINT16;
    } break;
    case NLS_UINT32: {
        matClass = MAT_C_UINT32;
        matType = MAT_T_UINT32;
    } break;
    case NLS_UINT64: {
        matClass = MAT_C_UINT64;
        matType = MAT_T_UINT64;
    } break;
    case NLS_INT8: {
        matClass = MAT_C_INT8;
        matType = MAT_T_INT8;
    } break;
    case NLS_INT16: {
        matClass = MAT_C_INT16;
        matType = MAT_T_INT16;
    } break;
    case NLS_INT32: {
        matClass = MAT_C_INT32;
        matType = MAT_T_INT32;
    } break;
    case NLS_INT64: {
        matClass = MAT_C_INT64;
        matType = MAT_T_INT64;
    } break;
    default: {
        delete[] dims;
        return nullptr;
    } break;
    }
    matvar_t* matVariable
        = Mat_VarCreate(variableName.c_str(), matClass, matType, (int)rank, dims, ptrValue, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
}
//=============================================================================
