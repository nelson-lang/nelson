//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SaveMatioCell.hpp"
#include "SaveMatioVariable.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioCell(const std::string& variableName, const ArrayOf& variableValue, mat_ft matVersion)
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

    indexType nbElements = variableDims.getElementCount();
    matvar_t** cellElements = nullptr;
    try {
        cellElements = (matvar_t**)new matvar_t*[nbElements];
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
    auto* elements = (ArrayOf*)variableValue.getDataPointer();
    for (indexType i = 0; i < nbElements; ++i) {
        cellElements[i] = SaveMatioVariable(variableName, elements[i], matVersion);
        if (cellElements[i] == nullptr) {
            delete[] cellElements;
            delete[] dims;
            return nullptr;
        }
    }
    matvar_t* matVariable = Mat_VarCreate(
        variableName.c_str(), MAT_C_CELL, MAT_T_CELL, (int)rank, dims, cellElements, 0);
    delete[] dims;
    return matVariable;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
