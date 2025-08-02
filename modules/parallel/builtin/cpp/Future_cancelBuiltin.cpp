//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Future_cancelBuiltin.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FevalFutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "FutureObjectHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::Future_cancelBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(_W("FevalFuture handle expected."));
    }
    bool isSupportedType = (param1.getHandleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR);
    if (!isSupportedType) {
        Error(_W("Future handle expected."));
    }
    std::vector<FutureObject*> futures = ArrayOfToFutures(param1);
    for (auto f : futures) {
        if (f) {
            f->cancel();
        }
    }
    return retval;
}
//=============================================================================
