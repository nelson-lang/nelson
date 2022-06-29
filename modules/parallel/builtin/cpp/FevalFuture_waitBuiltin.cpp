//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include "FevalFuture_waitBuiltin.hpp"
#include "FevalFutureObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "WaitFutures.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::FevalFuture_waitBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // wait(F)
    // wait(F,state)
    // tf = wait(F,state,timeout)

    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 3);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(_W("FevalFuture handle expected."));
    }
    if (param1.getHandleCategory() != FEVALFUTURE_CATEGORY_STR) {
        Error(_W("FevalFuture handle expected."));
    }
    indexType nbElements = param1.getElementCount();
    double timeout = std::numeric_limits<double>::infinity();
    THREAD_STATE expectedState = THREAD_STATE::FINISHED;
    std::vector<Nelson::FevalFutureObject*> futures;

    if (argIn.size() > 1) {
        std::wstring stateStr = argIn[1].getContentAsWideString();
        if (stateStr == L"running") {
            expectedState = THREAD_STATE::RUNNING;
        } else if (stateStr == L"finished") {
            expectedState = THREAD_STATE::FINISHED;
        } else {
            Error(_W("Valid states to wait for are: 'running' or 'finished'."));
        }
    }
    if (argIn.size() > 2) {
        timeout = argIn[2].getContentAsDoubleScalar();
        if (timeout < 0) {
            Error(_W("Expected timeout to be non-negative real numerical scalar."));
        }
    }
    if (nbElements > 0) {
        auto* ptr = (nelson_handle*)param1.getDataPointer();
        bool bContinueToWait = true;
        futures.reserve(nbElements);
        for (indexType k = 0; k < nbElements; k++) {
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(ptr[k]);
            auto* objFevalFuture = (FevalFutureObject*)hlObj;
            if (objFevalFuture == nullptr) {
                Error(_W("Cannot wait for completion of Futures that are in state 'unavailable'."));
            }
            THREAD_STATE state = objFevalFuture->state;
            if (state == THREAD_STATE::UNAVAILABLE) {
                Error(_W("Cannot wait for completion of Futures that are in state 'unavailable'."));
            }
            futures.push_back(objFevalFuture);
        }
    }

    bool res = WaitFutures(eval, futures, expectedState, timeout);
    if (nLhs == 1) {
        retval << ArrayOf::logicalConstructor(res);
    }
    return retval;
}
//=============================================================================
