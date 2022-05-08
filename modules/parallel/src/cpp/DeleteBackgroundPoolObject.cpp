//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DeleteBackgroundPoolObject.hpp"
#include "BackgroundPoolObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
DeleteBackgroundPoolObject(const ArrayOf& A)
{
    bool res = false;
    if (A.isHandle()) {
        if (!A.isEmpty()) {
            Dimensions dims = A.getDimensions();
            nelson_handle* qp = (nelson_handle*)(A.getDataPointer());
            size_t elementCount = static_cast<size_t>(dims.getElementCount());
            for (size_t k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    if (hlObj->getCategory() != BACKGROUNDPOOL_CATEGORY_STR) {
                        Error(_W("backgroundPool handle expected."));
                    }
                    auto* backgroundPoolObj = (BackgroundPoolObject*)hlObj;
                    delete backgroundPoolObj;
                    HandleManager::getInstance()->removeHandle(hl);
                    res = true;
                }
            }
        } else {
            Error(_W("backgroundPool valid handle expected."));
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
