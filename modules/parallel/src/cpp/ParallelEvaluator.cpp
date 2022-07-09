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
    if (evaluator && evaluatorWasCanceled) {
        Context* context = evaluator->getContext();
        delete evaluator;
        evaluator = nullptr;
        if (context) {
            delete context;
            context = nullptr;
        }
        return evaluator;
    }
    return nullptr;
}
//=============================================================================
}
//=============================================================================
