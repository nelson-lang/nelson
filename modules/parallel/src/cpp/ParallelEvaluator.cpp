//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ParallelEvaluator.hpp"
#include "Error.hpp"
#include "EvaluateInterface.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Evaluator*
createParallelEvaluator(EvaluateInterface* evaluatorInterface, size_t ID)
{
    Context* context = nullptr;
    Evaluator* evaluator = nullptr;
    try {
        context = new Context();
        evaluator = new Evaluator(context, evaluatorInterface, false, ID);
    } catch (const std::bad_alloc&) {
        if (context) {
            delete context;
        }
        if (evaluator) {
            delete evaluator;
        }
        Error(ERROR_MEMORY_ALLOCATION);
    }
    return evaluator;
}
//=============================================================================
Evaluator*
deleteParallelEvaluator(Evaluator* evaluator, bool evaluatorWasCanceled)
{
    if (evaluator) {
        evaluator->setState(NLS_STATE_QUIT);
        evaluator->resetState();

        if (!evaluatorWasCanceled) {
            delete evaluator;
            evaluator = nullptr;
        }
    }
    return evaluator;
}
//=============================================================================
}
//=============================================================================
