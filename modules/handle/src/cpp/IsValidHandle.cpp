//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsValidHandle.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
IsValidHandle(const ArrayOf& A)
{
    ArrayOf res;
    if (A.isHandle()) {
        Dimensions dimsA = A.dimensions();
        auto* qp = (HandleRef*)A.dataPointer();
        if (qp != nullptr) {
            logical* resArray = static_cast<logical*>(
                ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), false));
            indexType elementCount = dimsA.elementCount();
            for (indexType k = 0; k < elementCount; k++) {
                resArray[k] = qp[k] && qp[k]->isValid();
            }
            res = ArrayOf(NLS_LOGICAL, dimsA, resArray);
        } else {
            res = ArrayOf::empty(dimsA);
        }
    } else {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
