//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "LoadMatioStruct.hpp"
#include "LoadMatioVariable.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioStruct(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    unsigned int nbFields = Mat_VarGetNumberOfFields(matVariable);
    char* const* names = Mat_VarGetStructFieldnames(matVariable);
    stringVector fieldnames;
    fieldnames.reserve((size_t)nbFields);
    for (unsigned int k = 0; k < nbFields; k++) {
        fieldnames.push_back(names[k]);
    }

    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyStructConstructor(fieldnames, dims);
        bSuccess = true;
    } else if (dims.equals(Dimensions(1, 1)) && fieldnames.empty()) {
        VariableValue = ArrayOf::emptyStructWithoutFields();
        bSuccess = true;
    } else {
        indexType nbElements = dims.getElementCount();
        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements * fieldnames.size()];
        } catch (const std::bad_alloc&) {
            return false;
        }
        VariableValue = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        indexType offset = 0;
        for (indexType j = 0; j < nbElements; j++) {
            for (const auto& name : fieldnames) {
                matvar_t* fieldMatVar = Mat_VarGetStructFieldByName(matVariable, name.c_str(), j);
                bool bSuccess = LoadMatioVariable(fieldMatVar, true, elements[offset]);
                if (!bSuccess) {
                    return false;
                }
                offset++;
            }
        }
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
