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
#include "LoadMatioCell.hpp"
#include "LoadMatioVariable.hpp"
#include "NelsonException.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioCell(Evaluator* eval, matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::empty(dims);
        VariableValue.promoteType(NLS_CELL_ARRAY);
        bSuccess = true;
    } else {
        indexType nbElements = dims.elementCount();
        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements];
            ArrayOf cell = ArrayOf(NLS_CELL_ARRAY, dims, elements);
            for (indexType k = 0; k < nbElements; k++) {
                matvar_t* elementMatVar = Mat_VarGetCell(matVariable, (int)k);
                if (!LoadMatioVariable(eval, elementMatVar, true, elements[k])) {
                    return false;
                }
            }
            VariableValue = cell;
            bSuccess = true;
        } catch (NelsonException&) {
            return false;
        }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
