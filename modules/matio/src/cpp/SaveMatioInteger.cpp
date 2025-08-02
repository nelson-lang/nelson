//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SaveMatioDouble.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioInteger(const std::string& variableName, const ArrayOf& variableValue)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    void* ptrValue = nullptr;
    if (!variableDims.isEmpty(false)) {
        ptrValue = const_cast<void*>(variableValue.getDataPointer());
    }
    matio_classes matClass;
    matio_types matType;
    switch (variableValue.getDataClass()) {
    case NLS_UINT8: {
        matClass = MAT_C_UINT8;
        matType = MAT_T_UINT8;
    } break;
    case NLS_UINT16: {
        matClass = MAT_C_UINT16;
        matType = MAT_T_UINT16;
    } break;
    case NLS_UINT32: {
        matClass = MAT_C_UINT32;
        matType = MAT_T_UINT32;
    } break;
    case NLS_UINT64: {
        matClass = MAT_C_UINT64;
        matType = MAT_T_UINT64;
    } break;
    case NLS_INT8: {
        matClass = MAT_C_INT8;
        matType = MAT_T_INT8;
    } break;
    case NLS_INT16: {
        matClass = MAT_C_INT16;
        matType = MAT_T_INT16;
    } break;
    case NLS_INT32: {
        matClass = MAT_C_INT32;
        matType = MAT_T_INT32;
    } break;
    case NLS_INT64: {
        matClass = MAT_C_INT64;
        matType = MAT_T_INT64;
    } break;
    default: {
        delete[] dims;
        return nullptr;
    } break;
    }
    matvar_t* matVariable
        = Mat_VarCreate(variableName.c_str(), matClass, matType, (int)rank, dims, ptrValue, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
