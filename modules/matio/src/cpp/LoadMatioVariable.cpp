//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LoadMatioVariable.hpp"
#include "Warning.hpp"
#include "Exception.hpp"
#include "LoadMatioCharacters.hpp"
#include "LoadMatioDouble.hpp"
#include "LoadMatioSingle.hpp"
#include "LoadMatioInteger.hpp"
#include "LoadMatioLogical.hpp"
#include "LoadMatioSparseDouble.hpp"
#include "LoadMatioSparseLogical.hpp"
#include "LoadMatioCell.hpp"
#include "LoadMatioEmpty.hpp"
#include "LoadMatioFunction.hpp"
#include "LoadMatioObject.hpp"
#include "LoadMatioOpaque.hpp"
#include "LoadMatioStruct.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioVariable(matvar_t* matVariable, bool fromCellOrStruct, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        Warning(WARNING_MATIO_TYPE_NOT_SUPPORTED, _W("Cannot read variable."));
        VariableValue = ArrayOf::emptyStructWithoutFields();
        return true;
    }
    switch (matVariable->class_type) {
    case MAT_C_EMPTY: {
        bSuccess = LoadMatioEmpty(matVariable, fromCellOrStruct, VariableValue);
    } break;
    case MAT_C_OBJECT: {
        // NOT MANAGED by MATIO 1.5.13 :(
        bSuccess = LoadMatioObject(matVariable, VariableValue);
    } break;
    case MAT_C_FUNCTION: {
        // NOT MANAGED by MATIO 1.5.13 :(
        bSuccess = LoadMatioFunction(matVariable, VariableValue);
    } break;
    case MAT_C_OPAQUE: {
        // NOT MANAGED by MATIO 1.5.13 :(
        bSuccess = LoadMatioOpaque(matVariable, VariableValue);
    } break;
    case MAT_C_SPARSE: {
        if (matVariable->isLogical) {
            bSuccess = LoadMatioSparseLogical(matVariable, VariableValue);
        } else {
            bSuccess = LoadMatioSparseDouble(matVariable, VariableValue);
        }
    } break;
    case MAT_C_CELL: {
        bSuccess = LoadMatioCell(matVariable, VariableValue);
    } break;
    case MAT_C_STRUCT: {
        bSuccess = LoadMatioStruct(matVariable, VariableValue);
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
} // namespace Nelson
//=============================================================================
