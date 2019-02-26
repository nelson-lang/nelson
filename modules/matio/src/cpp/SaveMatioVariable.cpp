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
#include "SaveMatioVariable.hpp"
#include "SaveMatioDouble.hpp"
#include "SaveMatioSingle.hpp"
#include "SaveMatioLogical.hpp"
#include "SaveMatioCell.hpp"
#include "SaveMatioCharacterArray.hpp"
#include "SaveMatioStringArray.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioVariable(std::string variableName, ArrayOf variableValue, mat_ft matVersion)
{
    matvar_t* matVariable = nullptr;
    switch (variableValue.getDataClass()) {
    case NLS_HANDLE: {
    } break;
    case NLS_CELL_ARRAY: {
        matVariable = SaveMatioCell(variableName, variableValue, matVersion);
    } break;
    case NLS_STRUCT_ARRAY: {
    } break;
    case NLS_STRING_ARRAY: {
		// MATIO 1.5.13 does not know string array
		// Workaround: String array converted to Cell array
        matVariable = SaveMatioStringArray(variableName, variableValue, matVersion);
    } break;
    case NLS_LOGICAL: {
        if (variableValue.isSparse()) {
        } else {
            matVariable = SaveMatioLogical(variableName, variableValue, matVersion);
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

	} break;
    case NLS_SINGLE: {
        matVariable = SaveMatioSingle(variableName, variableValue, matVersion);
    } break;
    case NLS_DOUBLE: {
        if (variableValue.isSparse()) {
        } else {
            matVariable = SaveMatioDouble(variableName, variableValue, matVersion);
        }
    } break;
    case NLS_SCOMPLEX: {
    } break;
    case NLS_DCOMPLEX : {
    } break;
    case NLS_CHAR: {
        matVariable = SaveMatioCharacterArray(variableName, variableValue, matVersion);
    } break;
    default: { } break;
    }
    return matVariable;
}
//=============================================================================
}
//=============================================================================
