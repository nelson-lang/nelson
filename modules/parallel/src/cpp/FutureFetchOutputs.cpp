//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FutureFetchOutputs.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "WaitFutures.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
FutureFetchOutputs(Evaluator* eval, const std::vector<FutureObject*>& futures, bool uniformOutput)
{
    constexpr double noTimeout = std::numeric_limits<double>::infinity();
    WaitFutures(eval, futures, THREAD_STATE::FINISHED, noTimeout);

    ArrayOfVector _results(futures.size());
    Exception _exception;

    size_t nLhs = 0;
    bool once = true;
    for (auto future : futures) {
        if (future) {
            ArrayOfVector _result = future->getResult();
            _exception = future->getException();
            if (!_exception.getMessage().empty()) {
                std::wstring errorMessage = _W("One or more futures resulted in an error.")
                    + L"\n\n" + _exception.getMessage();
                Error(errorMessage, L"Nelson:parallel:future:ExecutionError");
            }
            if (once) {
                nLhs = _result.size();
                once = false;
            } else {
                if (nLhs != _result.size()) {
                    std::wstring errorMessage = _W("Unable to concatenate outputs.");
                    Error(errorMessage, L"Nelson:parallel:future:ConcatenateOutputs");
                }
            }
        }
    }

    ArrayOfVector argsToConcate;
    for (size_t k = 0; k < nLhs; ++k) {
        for (auto future : futures) {
            if (future) {
                argsToConcate << future->getResult()[k];
            }
        }
        _results.push_back(eval->vertcatOperator(argsToConcate));
        argsToConcate.clear();
    }
    return _results;
}
//=============================================================================
}
//=============================================================================
