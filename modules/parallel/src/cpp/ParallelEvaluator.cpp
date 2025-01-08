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
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::mutex ParallelEvaluator::evaluatorMutex;
//=============================================================================
std::shared_ptr<Evaluator>
ParallelEvaluator::create(EvaluateInterface* evaluatorInterface, size_t ID)
{
    std::lock_guard<std::mutex> lock(evaluatorMutex);
    try {
        auto context = std::make_shared<Context>();
        return std::shared_ptr<Evaluator>(
            new Evaluator(context.get(), evaluatorInterface, false, ID), [context](Evaluator* e) {
                if (e) {
                    e->setState(NLS_STATE_QUIT);
                    e->resetState();
                    delete e;
                }
            });
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
        return nullptr;
    } catch (const std::exception& e) {
        Error(_("Failed to create evaluator: ") + e.what());
        return nullptr;
    }
}
//=============================================================================
void
ParallelEvaluator::destroy(std::shared_ptr<Evaluator>& evaluator, bool evaluatorWasCanceled)
{
    if (!evaluator) {
        return;
    }

    std::lock_guard<std::mutex> lock(evaluatorMutex);

    try {
        evaluator->setState(NLS_STATE_QUIT);
        evaluator->resetState();

        if (!evaluatorWasCanceled) {
            evaluator.reset();
        }
    } catch (const std::exception&) {
    }
}
//=============================================================================
}
//=============================================================================
