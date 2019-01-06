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
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include "h5Save.hpp"
#include "IsValidVariableName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
saveDoubleMatrix(hid_t file, std::string variableName, ArrayOf VariableValue);
//=============================================================================
static bool
saveDoubleSparseMatrix(hid_t file, std::string variableName, ArrayOf VariableValue);
//=============================================================================
void
h5Save(Evaluator* eval, const std::wstring& filename, wstringVector names, bool append,
    bool nocompression)
{
    wstringVector variablesName;
    for (indexType k = 0; k < names.size(); k++) {

        if (!IsValidVariableName(names[k])) {
            Error(_W("Invalid variable name:") + names[k]);
        }
        if (!eval->getContext()->isVariable(names[k])) {
            Error(_W("Variable does not exist:") + names[k]);
        }
    }

	hid_t file_id = H5I_INVALID_HID;

    variablesName = names;
    if (variablesName.empty()) {
        eval->getContext()->getCurrentScope()->getVariablesList(false, variablesName);
    }


	for (indexType k = 0; k < variablesName.size(); k++) {
        ArrayOf variableValue;
        std::string variableName = wstring_to_utf8(variablesName[k]);
        eval->getContext()->getCurrentScope()->lookupVariable(variableName, variableValue);
        switch (variableValue.getDataClass()) {
        case NLS_HANDLE: {
        } break;
        case NLS_CELL_ARRAY: {
        } break;
        case NLS_STRUCT_ARRAY: {
        } break;
        case NLS_STRING_ARRAY: {
        } break;
        case NLS_LOGICAL: {
        } break;
        case NLS_UINT8: {
        } break;
        case NLS_INT8: {
        } break;
        case NLS_UINT16: {
        } break;
        case NLS_INT16: {
        } break;
        case NLS_UINT32: {
        } break;
        case NLS_INT32: {
        } break;
        case NLS_UINT64: {
        } break;
        case NLS_INT64: {
        } break;
        case NLS_SINGLE: {
        } break;
        case NLS_DOUBLE: {
            if (variableValue.isSparse()) {
                saveDoubleSparseMatrix(file_id, variableName, variableValue);
            } else {
                saveDoubleMatrix(file_id, variableName, variableValue);
            }
        } break;
        case NLS_SCOMPLEX: {
        } break;
        case NLS_DCOMPLEX: {
        } break;
        case NLS_CHAR: {
        } break;
		default: {
		} break;
		}
	}
}
//=============================================================================
bool
saveDoubleMatrix(hid_t file, std::string variableName, ArrayOf VariableValue)
{
    bool bSuccess = false;
    return bSuccess;
}
//=============================================================================
bool
saveDoubleSparseMatrix(hid_t file, std::string variableName, ArrayOf VariableValue)
{
    bool bSuccess = false;
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
