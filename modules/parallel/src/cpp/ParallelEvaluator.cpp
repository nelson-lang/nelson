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
createParallelEvaluator()
{
    Context* context = nullptr;
    EvaluateInterface* evaluatorIO = nullptr;
    Evaluator* evaluator = nullptr;
    try {
        context = new Context();
        evaluatorIO = new EvaluateInterface();
        evaluator = new Evaluator(context, evaluatorIO, false);
    } catch (const std::bad_alloc&) {
        if (context) {
            delete context;
        }
        if (evaluatorIO) {
            delete evaluatorIO;
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
        EvaluateInterface* io = (EvaluateInterface*)evaluator->getInterface();
        delete evaluator;
        evaluator = nullptr;
        if (context) {
            delete context;
            context = nullptr;
        }
        if (io) {
            delete io;
            io = nullptr;
        }
        return evaluator;
    }
    return nullptr;
}
//=============================================================================
}
//=============================================================================
