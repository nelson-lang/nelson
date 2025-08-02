//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SaveMatioLogical.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioLogical(const std::string& variableName, const ArrayOf& variableValue)
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
    matvar_t* matVariable = Mat_VarCreate(
        variableName.c_str(), MAT_C_UINT8, MAT_T_UINT8, (int)rank, dims, ptrValue, MAT_F_LOGICAL);
    delete[] dims;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
