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
#include "ClassdefSerialization.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioVariable(Evaluator* eval, const std::string& variableName, const ArrayOf& variableValue,
    mat_ft matVersion)
{
    matvar_t* matVariable = nullptr;
    ArrayOf savedObject;
    if (classdefPrepareSaveObject(eval, variableValue, savedObject)) {
        return SaveMatioStruct(eval, variableName, savedObject, matVersion);
    }
    if (classdefPreparePersistentObject(variableValue, savedObject)) {
        return SaveMatioStruct(eval, variableName, savedObject, matVersion);
    }
    switch (variableValue.dataClass()) {
    case NLS_GO_HANDLE: {
    } break;
    case NLS_HANDLE: {
        matVariable = SaveMatioHandle(eval, variableName, variableValue, matVersion);
    } break;
    case NLS_CELL_ARRAY: {
        matVariable = SaveMatioCell(eval, variableName, variableValue, matVersion);
    } break;
    case NLS_FUNCTION_HANDLE: {
        matVariable = SaveMatioFunctionHandle(eval, variableName, variableValue, matVersion);
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_STRUCT_ARRAY: {
        matVariable = SaveMatioStruct(eval, variableName, variableValue, matVersion);
    } break;
    case NLS_STRING_ARRAY: {
        // matio 1.5.13 has no string array type: save it as a cell array.
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
