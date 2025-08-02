//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
#include "h5SaveMissing.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveVariable(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    switch (VariableValue.getDataClass()) {
    case NLS_GO_HANDLE: {
        bSuccess = false;
    } break;

    case NLS_HANDLE: {
        bSuccess = h5SaveHandle(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_CELL_ARRAY: {
        bSuccess = h5SaveCell(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_FUNCTION_HANDLE: {
        bSuccess = h5SaveFunctionHandle(fid, location, variableName, VariableValue, useCompression);
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_STRUCT_ARRAY: {
        bSuccess = h5SaveStruct(fid, location, variableName, VariableValue, useCompression);
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
    case NLS_MISSING_ARRAY: {
        bSuccess = h5SaveMissing(fid, location, variableName, VariableValue, useCompression);
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
