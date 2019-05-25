//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "SaveMatioVariable.hpp"
#include "SaveMatioDouble.hpp"
#include "SaveMatioSingle.hpp"
#include "SaveMatioDoubleComplex.hpp"
#include "SaveMatioSingleComplex.hpp"
#include "SaveMatioLogical.hpp"
#include "SaveMatioCell.hpp"
#include "SaveMatioCharacterArray.hpp"
#include "SaveMatioStringArray.hpp"
#include "SaveMatioInteger.hpp"
#include "SaveMatioStruct.hpp"
#include "SaveMatioSparseLogical.hpp"
#include "SaveMatioSparseDouble.hpp"
#include "SaveMatioSparseDoubleComplex.hpp"
#include "SaveMatioHandle.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioVariable(const std::string& variableName, ArrayOf variableValue, mat_ft matVersion)
{
    matvar_t* matVariable = nullptr;
    switch (variableValue.getDataClass()) {
    case NLS_HANDLE: {
        matVariable = SaveMatioHandle(variableName, variableValue, matVersion);
    } break;
    case NLS_CELL_ARRAY: {
        matVariable = SaveMatioCell(variableName, variableValue, matVersion);
    } break;
    case NLS_STRUCT_ARRAY: {
        matVariable = SaveMatioStruct(variableName, variableValue, matVersion);
    } break;
    case NLS_STRING_ARRAY: {
        // MATIO 1.5.13 does not know string array type
        // Workaround: String array converted to Cell array
        matVariable = SaveMatioStringArray(variableName, variableValue, matVersion);
    } break;
    case NLS_LOGICAL: {
        if (variableValue.isSparse()) {
            matVariable = SaveMatioSparseLogical(variableName, variableValue);
        } else {
            matVariable = SaveMatioLogical(variableName, variableValue);
        }
    } break;
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        matVariable = SaveMatioInteger(variableName, variableValue);
    } break;
    case NLS_SINGLE: {
        matVariable = SaveMatioSingle(variableName, variableValue);
    } break;
    case NLS_DOUBLE: {
        if (variableValue.isSparse()) {
            matVariable = SaveMatioSparseDouble(variableName, variableValue);
        } else {
            matVariable = SaveMatioDouble(variableName, variableValue);
        }
    } break;
    case NLS_SCOMPLEX: {
        matVariable = SaveMatioSingleComplex(variableName, variableValue);
    } break;
    case NLS_DCOMPLEX: {
        if (variableValue.isSparse()) {
            matVariable = SaveMatioSparseDoubleComplex(variableName, variableValue);
        } else {
            matVariable = SaveMatioDoubleComplex(variableName, variableValue);
        }
    } break;
    case NLS_CHAR: {
        matVariable = SaveMatioCharacterArray(variableName, variableValue, matVersion);
    } break;
    default: { } break; }
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
