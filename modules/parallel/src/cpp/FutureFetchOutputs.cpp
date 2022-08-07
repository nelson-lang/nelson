//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FutureFetchOutputs.hpp"
#include "Error.hpp"
#include "WaitFutures.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
FutureFetchOutputs(Evaluator* eval, FevalFutureObject* fevalFutureObject)
{
    WaitFinishedOrFailedFuture(eval, fevalFutureObject);

    ArrayOfVector _result;
    Exception _exception;

    switch (fevalFutureObject->state) {
    case THREAD_STATE::FINISHED: {
        _result = fevalFutureObject->getResult();
        _exception = fevalFutureObject->getException();
    } break;
    case THREAD_STATE::FAILED: {
        _exception = fevalFutureObject->getException();
    } break;
    default: {
        Error(_W("Not managed."));
    } break;
    }
    if (!_exception.getMessage().empty()) {
        std::wstring errorMessage
            = _W("One or more futures resulted in an error.") + L"\n\n" + _exception.getMessage();
        Error(errorMessage, L"Nelson:parallel:future:ExecutionError");
    }
    return _result;
}
//=============================================================================
}
//=============================================================================
