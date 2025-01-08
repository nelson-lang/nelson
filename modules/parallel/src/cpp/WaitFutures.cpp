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
#include <thread>
#include <chrono>
#include "WaitFutures.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
allAreFinished(const std::vector<FutureObject*>& futures)
{
    for (const auto& f : futures) {
        FutureStateGuard guard(f->stateMutex);
        if (f->state != THREAD_STATE::FINISHED) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
allAreRunningOrFinished(const std::vector<FutureObject*>& futures)
{
    for (const auto& f : futures) {
        FutureStateGuard guard(f->stateMutex);
        if (f->state != THREAD_STATE::RUNNING && f->state != THREAD_STATE::FINISHED) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
WaitFutures(Evaluator* eval, const std::vector<FutureObject*>& futures, THREAD_STATE expectedState,
    double timeoutSeconds)
{
    if (futures.empty()) {
        return true;
    }

    const auto timeout_ns = std::isinf(timeoutSeconds) ? std::numeric_limits<int64_t>::max()
                                                       : static_cast<int64_t>(timeoutSeconds * 1e9);

    const auto begin_time = std::chrono::steady_clock::now();

    while (true) {
        bool conditionMet = (expectedState == THREAD_STATE::FINISHED)
            ? allAreFinished(futures)
            : allAreRunningOrFinished(futures);

        if (conditionMet) {
            return true;
        }

        const auto current_time = std::chrono::steady_clock::now();
        const auto difftime_ns
            = std::chrono::duration_cast<std::chrono::nanoseconds>(current_time - begin_time)
                  .count();

        if (difftime_ns > timeout_ns) {
            return false;
        }

        if (eval != nullptr
            && NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())) {
            return false;
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(50));

        if (eval != nullptr && eval->haveEventsLoop()) {
            ProcessEventsDynamicFunctionWithoutWait();
        }
    }
}
//=============================================================================
}
//=============================================================================
