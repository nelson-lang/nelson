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
#include "LoadMatioVariable.hpp"
#include "Exception.hpp"
#include "LoadMatioCharacters.hpp"
#include "LoadMatioDouble.hpp"
#include "LoadMatioSingle.hpp"
#include "LoadMatioInteger.hpp"
#include "LoadMatioLogical.hpp"
#include "LoadMatioSparseDouble.hpp"
#include "LoadMatioSparseLogical.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioVariable(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return false;
    }
    switch (matVariable->class_type) {
    case MAT_C_EMPTY: {
    } break;
    case MAT_C_OBJECT: {
        // NOT MANAGED by MATIO 1.5.13 :(
    } break;
    case MAT_C_FUNCTION: {
        // NOT MANAGED by MATIO 1.5.13 :(
    } break;
    case MAT_C_OPAQUE: {
        // NOT MANAGED by MATIO 1.5.13 :(
    } break;
    case MAT_C_SPARSE: {
        if (matVariable->isLogical) {
            bSuccess = LoadMatioSparseLogical(matVariable, VariableValue);
        } else {
            bSuccess = LoadMatioSparseDouble(matVariable, VariableValue);
        }
    } break;
    case MAT_C_CELL: {
    } break;
    case MAT_C_STRUCT: {
    } break;
    case MAT_C_CHAR: {
        bSuccess = LoadMatioCharacters(matVariable, VariableValue);
    } break;
    case MAT_C_DOUBLE: {
        bSuccess = LoadMatioDouble(matVariable, VariableValue);
    } break;
    case MAT_C_SINGLE: {
        bSuccess = LoadMatioSingle(matVariable, VariableValue);
    } break;
    case MAT_C_INT8: {
        bSuccess = LoadMatioInteger(matVariable, NLS_INT8, VariableValue);
    } break;
    case MAT_C_INT16: {
        bSuccess = LoadMatioInteger(matVariable, NLS_INT16, VariableValue);
    } break;
    case MAT_C_INT32: {
        bSuccess = LoadMatioInteger(matVariable, NLS_INT32, VariableValue);
    } break;
    case MAT_C_INT64: {
        bSuccess = LoadMatioInteger(matVariable, NLS_INT64, VariableValue);
    } break;
    case MAT_C_UINT8: {
        if (matVariable->isLogical) {
            bSuccess = LoadMatioLogical(matVariable, VariableValue);
        } else {
            bSuccess = LoadMatioInteger(matVariable, NLS_UINT8, VariableValue);
        }
    } break;
    case MAT_C_UINT16: {
        bSuccess = LoadMatioInteger(matVariable, NLS_UINT16, VariableValue);
    } break;
    case MAT_C_UINT32: {
        bSuccess = LoadMatioInteger(matVariable, NLS_UINT32, VariableValue);
    } break;
    case MAT_C_UINT64: {
        bSuccess = LoadMatioInteger(matVariable, NLS_UINT64, VariableValue);
    } break;
    default: {
        bSuccess = false;
    } break;
    }
    return bSuccess;
}
//=============================================================================
}
//=============================================================================
