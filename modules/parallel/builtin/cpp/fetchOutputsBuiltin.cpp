//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <tuple>
#include "fetchOutputsBuiltin.hpp"
#include "Error.hpp"
#include "FevalFutureObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::fetchOutputsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) { 
            Error(_W("FevalFuture handle expected."));
    }
    if (!param1.isScalar()) { 
            Error(_W("FevalFuture handle expected."));
    }
    if (param1.getHandleCategory() != FEVALFUTURE_CATEGORY_STR) {
            Error(_W("FevalFuture handle expected."));
    }
    auto* fevalFutureObject = (FevalFutureObject*)param1.getContentAsHandleScalar();
    if (fevalFutureObject) {
        THREAD_STATE state = fevalFutureObject->getState();
        bool wait = (state == THREAD_STATE::RUNNING) || (state == THREAD_STATE::QUEUED);
        while (wait) {
            state = fevalFutureObject->getState();
            wait = (state == THREAD_STATE::RUNNING) || (state == THREAD_STATE::QUEUED);
        }
        bool valid = false;
        std::tuple<ArrayOfVector, Exception> resultOrFuture = fevalFutureObject->get(valid);
        retval = std::get<0>(resultOrFuture);
        Exception e = std::get<1>(resultOrFuture);
        if (!e.getMessage().empty()) {
            std::wstring errorMessage
                = _W("One or more futures resulted in an error.") + L"\n\n" + e.getMessage();
            Error(errorMessage, L"Nelson:parallel:future:ExecutionError");
        }
    } else {
        Error(_W("FevalFuture not available"));
    }
    return retval;
}
//=============================================================================
