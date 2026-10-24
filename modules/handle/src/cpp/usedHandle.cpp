//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "usedHandle.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
usedHandle(const std::string& category)
{
    ArrayOf res;
    std::vector<HandleRef> used
        = HandleManager::getInstance()->getAllHandleRefsOfCategory(category);
    size_t nbHandles = used.size();
    if (nbHandles > 0) {
        Dimensions dims(1, nbHandles);
        HandleRef* nh = static_cast<HandleRef*>(
            ArrayOf::allocateArrayOf(NLS_HANDLE, nbHandles, stringVector(), false));
        for (size_t k = 0; k < nbHandles; k++) {
            nh[k] = used[k];
        }
        res = ArrayOf(NLS_HANDLE, dims, (void*)nh);
    } else {
        res = ArrayOf::empty(0, 0);
        res.promoteType(NLS_HANDLE);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
