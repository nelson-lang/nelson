//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
#include "SaveMatioFunctionHandle.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioVariable(const std::string& variableName, const ArrayOf& variableValue, mat_ft matVersion)
{
    matvar_t* matVariable = nullptr;
    switch (variableValue.getDataClass()) {
    case NLS_GO_HANDLE: {
    } break;
    case NLS_HANDLE: {
        matVariable = SaveMatioHandle(variableName, variableValue, matVersion);
    } break;
    case NLS_CELL_ARRAY: {
        matVariable = SaveMatioCell(variableName, variableValue, matVersion);
    } break;
    case NLS_FUNCTION_HANDLE: {
        matVariable = SaveMatioFunctionHandle(variableName, variableValue, matVersion);
    } break;
    case NLS_CLASS_ARRAY:
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
    default: {
    } break;
    }
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
