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
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsEqualHandle(const ArrayOf& A, const ArrayOf& B)
{
    if (A.getDataClass() == B.getDataClass()) {
        if (A.getHandleCategory() != B.getHandleCategory()) {
            return false;
        }
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (dimsA.equals(dimsB)) {
            const nelson_handle* ptrA = (const nelson_handle*)A.getDataPointer();
            const nelson_handle* ptrB = (const nelson_handle*)B.getDataPointer();
            indexType elementCount = A.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                if (ptrA[k] != ptrB[k]) {
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
