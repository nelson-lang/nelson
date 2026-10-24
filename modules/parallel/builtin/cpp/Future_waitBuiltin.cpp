//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include "Future_waitBuiltin.hpp"
#include "FevalFutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "WaitFutures.hpp"
#include "FutureObjectHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::Future_waitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // wait(F)
    // wait(F,state)
    // tf = wait(F,state,timeout)

    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 3);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        OverloadRequired("wait");
    }
    bool isSupportedType = (param1.handleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR)
        || (param1.handleCategory() == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR)
        || (param1.handleCategory() == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR);

    if (!isSupportedType) {
        OverloadRequired("wait");
    }
    double timeout = std::numeric_limits<double>::infinity();
    THREAD_STATE expectedState = THREAD_STATE::FINISHED;

    if (argIn.size() > 1) {
        std::wstring stateStr = argIn[1].asWideString();
        if (stateStr == L"running") {
            expectedState = THREAD_STATE::RUNNING;
        } else if (stateStr == L"finished") {
            expectedState = THREAD_STATE::FINISHED;
        } else {
            Error(L"Nelson:future:invalidWaitState",
                L"Valid states to wait for are: 'running' or 'finished'.");
        }
    }
    if (argIn.size() > 2) {
        timeout = argIn[2].asDoubleScalar();
        if (timeout < 0) {
            Error(L"Nelson:parallel:invalidTimeout",
                L"Expected timeout to be non-negative real numerical scalar.");
        }
    }

    std::vector<FutureObject*> futures = ArrayOfToFutures(param1);
    for (auto f : futures) {
        if (f) {
            THREAD_STATE state = f->state;
            if (state == THREAD_STATE::UNAVAILABLE) {
                Error(L"Nelson:future:unavailableState",
                    L"Cannot wait for completion of Futures that are in state 'unavailable'.");
            }
        } else {
            Error(L"Nelson:future:unavailableState",
                L"Cannot wait for completion of Futures that are in state 'unavailable'.");
        }
    }
    bool res = WaitFutures(eval, futures, expectedState, timeout);
    if (nLhs == 1) {
        retval << ArrayOf::logicalScalar(res);
    }
    return retval;
}
//=============================================================================
