//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsEqualHandle.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsEqualHandle(const ArrayOf& A, const ArrayOf& B)
{
    if (A.dataClass() == B.dataClass()) {
        if (A.handleCategory() != B.handleCategory()) {
            return false;
        }
        Dimensions dimsA = A.dimensions();
        Dimensions dimsB = B.dimensions();
        if (dimsA.equals(dimsB)) {
            const HandleRef* ptrA = (const HandleRef*)A.dataPointer();
            const HandleRef* ptrB = (const HandleRef*)B.dataPointer();
            indexType elementCount = A.elementCount();
            for (indexType k = 0; k < elementCount; k++) {
                if (!ptrA[k] || !ptrB[k] || ptrA[k]->id() != ptrB[k]->id()) {
                    return false;
                }
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
