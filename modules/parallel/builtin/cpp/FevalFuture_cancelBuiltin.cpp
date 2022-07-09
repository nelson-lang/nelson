//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalFuture_cancelBuiltin.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
#include "FevalFutureObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::FevalFuture_cancelBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(_W("FevalFuture handle expected."));
    }
    if (param1.getHandleCategory() != FEVALFUTURE_CATEGORY_STR) {
        Error(_W("FevalFuture handle expected."));
    }
    indexType nbElements = param1.getElementCount();

    if (nbElements > 0) {
        auto* ptr = (nelson_handle*)param1.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(ptr[k]);
            auto* objFevalFuture = (FevalFutureObject*)hlObj;
            if (objFevalFuture != nullptr) {
                objFevalFuture->cancel();
            }
        }
    }
    return retval;
}
//=============================================================================
