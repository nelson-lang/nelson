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
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
IsValidHandle(const ArrayOf& A)
{
    ArrayOf res;
    if (A.isHandle()) {
        Dimensions dimsA = A.getDimensions();
        auto* qp = (nelson_handle*)A.getDataPointer();
        if (qp != nullptr) {
            logical* resArray = static_cast<logical*>(ArrayOf::allocateArrayOf(
                NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
            indexType elementCount = dimsA.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj != nullptr) {
                    if (hlObj->getPointer() != nullptr) {
                        resArray[k] = true;
                    } else {
                        resArray[k] = false;
                    }
                } else {
                    resArray[k] = false;
                }
            }
            res = ArrayOf(NLS_LOGICAL, dimsA, resArray);
        } else {
            res = ArrayOf::emptyConstructor(dimsA);
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
