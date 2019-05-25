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
#include "h5LoadVariable.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "StringToClass.hpp"
#include "h5LoadLogical.hpp"
#include "h5LoadDouble.hpp"
#include "h5LoadSingle.hpp"
#include "h5LoadInteger.hpp"
#include "h5LoadString.hpp"
#include "h5LoadHandle.hpp"
#include "h5LoadCell.hpp"
#include "h5LoadStruct.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadVariable(
    hid_t fid, const std::string& location, const std::string& variableName, ArrayOf& VariableValue)
{
    bool bSuccess = true;
    bool isObject = isNelsonObject(fid, location, variableName);
    std::string className = getNelsonClass(fid, location, variableName);
    Dimensions dims = getNelsonDimensions(fid, location, variableName);
    bool isEmpty = isNelsonEmpty(fid, location, variableName);
    bool isComplex = isNelsonComplex(fid, location, variableName);
    bool isSparse = isNelsonSparse(fid, location, variableName);
    uint64 nzmax = 0;
    if (isSparse) {
        nzmax = getNelsonNzmax(fid, location, variableName);
    }

    Class clDest;
    if (isObject) {
        clDest = NLS_STRUCT_ARRAY;
    } else {
        bool haveError;
        clDest = StringToClass(className, haveError);
        if (haveError) {
            return false;
        }
    }
    switch (clDest) {
    case NLS_HANDLE: {
        bSuccess = h5LoadHandle(fid, location, variableName, VariableValue);
    } break;
    case NLS_CELL_ARRAY: {
        bSuccess = h5LoadCell(fid, location, variableName, isEmpty, dims, VariableValue);
    } break;
    case NLS_STRUCT_ARRAY: {
        bSuccess = h5LoadStruct(
            fid, location, variableName, isEmpty, dims, isObject, className, VariableValue);
    } break;
    case NLS_STRING_ARRAY: {
        bSuccess = h5LoadStringArray(fid, location, variableName, isEmpty, dims, VariableValue);
    } break;
    case NLS_LOGICAL: {
        bSuccess = h5LoadLogical(
            fid, location, variableName, isEmpty, dims, isSparse, nzmax, VariableValue);
    } break;
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        bSuccess = h5LoadInteger(fid, location, variableName, clDest, isEmpty, dims, VariableValue);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        bSuccess
            = h5LoadSingle(fid, location, variableName, isEmpty, isComplex, dims, VariableValue);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        bSuccess = h5LoadDouble(
            fid, location, variableName, isEmpty, isComplex, dims, isSparse, nzmax, VariableValue);
    } break;
    case NLS_CHAR: {
        bSuccess = h5LoadCharacterArray(fid, location, variableName, isEmpty, dims, VariableValue);
    } break;
    default: {
        return false;
    }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
