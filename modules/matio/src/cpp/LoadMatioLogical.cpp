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
#include "LoadMatioLogical.hpp"
#include "Exception.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioLogical(matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    NelsonType destinationClass = NLS_LOGICAL;
    Dimensions dims = getMatVarDimensions(matVariable);
    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyConstructor(dims);
        VariableValue.promoteType(destinationClass);
        bSuccess = true;
    } else {
        void* ptr = nullptr;
        try {
            ptr = ArrayOf::allocateArrayOf(
                destinationClass, dims.getElementCount(), stringVector(), false);
        } catch (Exception&) {
            return false;
        }
        memcpy(ptr, matVariable->data, matVariable->nbytes);
        VariableValue = ArrayOf(destinationClass, dims, ptr);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
