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
#include "i18n.hpp"
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
        Error(_W("FevalFuture handle expected."));
    }
    bool isSupportedType = (param1.getHandleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR);

    if (!isSupportedType) {
        Error(_W("Future handle expected."));
    }
    double timeout = std::numeric_limits<double>::infinity();
    THREAD_STATE expectedState = THREAD_STATE::FINISHED;

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

    std::vector<FutureObject*> futures = ArrayOfToFutures(param1);
    for (auto f : futures) {
        if (f) {
            THREAD_STATE state = f->state;
            if (state == THREAD_STATE::UNAVAILABLE) {
                Error(_W("Cannot wait for completion of Futures that are in state 'unavailable'."));
            }
        } else {
            Error(_W("Cannot wait for completion of Futures that are in state 'unavailable'."));
        }
    }
    bool res = WaitFutures(eval, futures, expectedState, timeout);
    if (nLhs == 1) {
        retval << ArrayOf::logicalConstructor(res);
    }
    return retval;
}
//=============================================================================
