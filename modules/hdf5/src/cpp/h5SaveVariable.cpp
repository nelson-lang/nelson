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
#include "h5SaveVariable.hpp"
#include "h5SaveInteger.hpp"
#include "h5SaveDouble.hpp"
#include "h5SaveSingle.hpp"
#include "h5SaveString.hpp"
#include "h5SaveStruct.hpp"
#include "h5SaveCell.hpp"
#include "h5SaveLogical.hpp"
#include "h5SaveHandle.hpp"
#include "h5SaveFunctionHandle.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveVariable(hid_t fid, const std::string& location, const std::string& variableName,
    ArrayOf VariableValue, bool useCompression)
{
    bool bSuccess = false;
    switch (VariableValue.getDataClass()) {
    case NLS_HANDLE: {
        bSuccess = h5SaveHandle(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_CELL_ARRAY: {
        bSuccess = h5SaveCell(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_STRUCT_ARRAY: {
        if (VariableValue.isFunctionHandle()) {
            bSuccess
                = h5SaveFunctionHandle(fid, location, variableName, VariableValue, useCompression);
        } else {
            bSuccess = h5SaveStruct(fid, location, variableName, VariableValue, useCompression);
        }
    } break;
    case NLS_STRING_ARRAY: {
        bSuccess = h5SaveStringArray(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_LOGICAL: {
        bSuccess = h5SaveLogical(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        bSuccess = h5SaveInteger(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        bSuccess = h5SaveSingle(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        bSuccess = h5SaveDouble(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_CHAR: {
        bSuccess = h5SaveCharacterArray(fid, location, variableName, VariableValue, useCompression);
    } break;
    default: {
        bSuccess = false;
    } break;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
