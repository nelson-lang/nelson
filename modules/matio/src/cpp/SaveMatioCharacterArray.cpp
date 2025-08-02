//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SaveMatioCharacterArray.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioCharacterArray(
    const std::string& variableName, const ArrayOf& variableValue, mat_ft matVersion)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    void* ptrValue = nullptr;
    matio_types matType = MAT_T_UTF8;
    ArrayOf asUint;
    if (!variableDims.isEmpty(false)) {
        if (matVersion == MAT_FT_MAT5 || matVersion == MAT_FT_MAT4) {
            asUint = variableValue;
            asUint.promoteType(NLS_UINT8);
            ptrValue = const_cast<void*>(asUint.getDataPointer());
            matType = MAT_T_UTF8;
        } else {
            if (sizeof(charType) == sizeof(uint16)) {
                ptrValue = const_cast<void*>(variableValue.getDataPointer());
            } else {
                asUint = variableValue;
                asUint.promoteType(NLS_UINT16);
                ptrValue = const_cast<void*>(asUint.getDataPointer());
            }
            matType = MAT_T_UTF16;
        }
    }
    matvar_t* matVariable
        = Mat_VarCreate(variableName.c_str(), MAT_C_CHAR, matType, (int)rank, dims, ptrValue, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
